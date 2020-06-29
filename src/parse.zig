const std = @import("std");
const Allocator = std.mem.Allocator;

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const eq = tokenize.eq;
const TokenList = tokenize.TokenList;
const TokenIterator = tokenize.TokenIterator;

const StatementType = enum { C //, V, F, E, D, A, P, BlockOpen, BlockClose
};

const Statement = union(StatementType) {
    C: struct { constants: TokenList },

    fn deinit(self: *Statement, allocator: *Allocator) void {
        switch (self.*) {
            .C => self.*.C.constants.deinit(),
        }
        allocator.destroy(self);
    }
};

const StatementIterator = struct {
    allocator: *Allocator,
    tokens: TokenIterator,

    fn init(allocator: *Allocator, buffer: Token) StatementIterator {
        return StatementIterator{ .allocator = allocator, .tokens = TokenIterator{ .buffer = buffer } };
    }

    fn next(self: *StatementIterator) !?*Statement {
        // get next token
        var token = (try self.tokens.next()) orelse {
            // we have seen the last Token, so we have seen the last Statement, end of iteration
            return null;
        };
        // if the token is a label, read one more token
        var label: ?Token = null;
        while (label == null) {
            if (token[0] == '$') break;
            label = token;
            token = (try self.tokens.next()) orelse return Error.Incomplete;
        }
        if (token[0] != '$') return Error.UnexpectedToken;
        if (token.len != 2) return Error.IllegalToken;
        // handle the $x command
        switch (token[1]) {
            'c' => {
                // TODO: move to new fn 'read TokenList until terminator token'
                var l = TokenList.init(self.allocator);
                while (true) {
                    token = (try self.tokens.next()) orelse return Error.Incomplete;
                    if (eq(token, "$.")) break;
                    // TODO: check against duplicates
                    _ = try l.push(token);
                }
                const result = try self.allocator.create(Statement);
                result.* = Statement{ .C = .{ .constants = l } };
                return result;
            },
            else => return Error.IllegalToken,
        }
    }
};

const expect = std.testing.expect;

test "parse constant declaration" {
    var statements = StatementIterator.init(std.testing.allocator, "$c wff |- $.");
    // TODO: abbreviation for simpler expect statements, preferably allowing for list literals
    const s = try statements.next();
    expect(s != null); // TODO: refine
    s.?.deinit(std.testing.allocator);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

// TODO: add more tests for checking error values

test "parse empty file" {
    var statements = StatementIterator.init(std.testing.allocator, "");
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}
