const std = @import("std");
const Allocator = std.mem.Allocator;

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const eq = tokenize.eq;
const eqs = tokenize.eqs;
const TokenList = tokenize.TokenList;
const TokenIterator = tokenize.TokenIterator;

const StatementType = enum { C, V, F, D //, E, A, P, BlockOpen, BlockClose
};

const Statement = union(StatementType) {
    C: struct { constants: TokenList },
    V: struct { variables: TokenList },
    F: struct { kind: Token, variable: Token },
    D: struct { variables: TokenList },

    fn deinit(self: *Statement, allocator: *Allocator) void {
        switch (self.*) {
            .C => self.*.C.constants.deinit(),
            .V => self.*.V.variables.deinit(),
            .F => {},
            .D => self.*.D.variables.deinit(),
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
            'c' => return self.statement(.{
                .C = .{ .constants = try self.nextUntil("$.") },
            }),
            'v' => return self.statement(.{
                .V = .{ .variables = try self.nextUntil("$.") },
            }),
            'f' => {
                var t = try self.nextUntil("$.");
                defer t.deinit();
                if (t.count() < 2) return Error.Incomplete;
                if (t.count() > 2) return Error.UnexpectedToken;
                return self.statement(.{
                    .F = .{ .kind = t.at(0).*, .variable = t.at(1).* },
                });
            },
            'd' => return self.statement(.{
                .D = .{ .variables = try self.nextUntil("$.") },
            }),
            else => return Error.IllegalToken,
        }
    }

    fn nextUntil(self: *StatementIterator, terminator: Token) !TokenList {
        var result = TokenList.init(self.allocator);
        while (true) {
            const token = (try self.tokens.next()) orelse return Error.Incomplete;
            if (eq(token, terminator)) break;
            _ = try result.push(token);
        }
        return result;
    }

    fn statement(self: *StatementIterator, s: Statement) !*Statement {
        const result = try self.allocator.create(Statement);
        result.* = s;
        return result;
    }
};

const expect = std.testing.expect;

fn forNext(statements: *StatementIterator, f: var) !void {
    const s = try statements.next();
    _ = f.do(s);
    s.?.deinit(std.testing.allocator);
}

test "parse $f declaration" {
    var statements = StatementIterator.init(std.testing.allocator, "$f wff ph $.");
    _ = try forNext(&statements, struct {
        fn do(s: var) void {
            expect(eq(s.?.F.kind, "wff"));
            expect(eq(s.?.F.variable, "ph"));
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "parse constant declaration" {
    var statements = StatementIterator.init(std.testing.allocator, "$c wff |- $.");
    _ = try forNext(&statements, struct {
        fn do(s: var) void {
            expect(eqs(s.?.C.constants, &[_]Token{ "wff", "|-" }));
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

// TODO: add more tests for checking error values

test "parse empty file" {
    var statements = StatementIterator.init(std.testing.allocator, "");
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}
