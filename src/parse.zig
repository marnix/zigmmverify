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

const StatementType = enum { C, V, F, E, D, A // , P, BlockOpen, BlockClose
};

const Statement = union(StatementType) {
    C: struct { constants: TokenList },
    V: struct { variables: TokenList },
    F: struct { kind: Token, variable: Token },
    E: struct { label: Token, expression: TokenList },
    D: struct { variables: TokenList },
    A: struct { label: Token, expression: TokenList },

    fn deinit(self: *Statement, allocator: *Allocator) void {
        switch (self.*) {
            .C => self.*.C.constants.deinit(),
            .V => self.*.V.variables.deinit(),
            .F => {},
            .E => self.*.E.expression.deinit(),
            .D => self.*.D.variables.deinit(),
            .A => self.*.A.expression.deinit(),
        }
        allocator.destroy(self);
    }
};

const StatementIterator = struct {
    allocator: *Allocator,
    tokens: TokenIterator,
    optStatement: ?*Statement = null,

    fn init(allocator: *Allocator, buffer: Token) StatementIterator {
        return StatementIterator{ .allocator = allocator, .tokens = TokenIterator{ .buffer = buffer } };
    }

    fn next(self: *StatementIterator) !?*Statement {
        // return any statement detected in the previous call
        if (self.optStatement) |s| {
            self.optStatement = null;
            return s;
        }
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
        var result: *Statement = undefined;
        switch (token[1]) { // handle the $x command
            'c' => {
                result = try self.statement(.{
                    .C = .{ .constants = try self.nextUntil("$.") },
                });
            },
            'v' => {
                result = try self.statement(.{
                    .V = .{ .variables = try self.nextUntil("$.") },
                });
            },
            'f' => {
                var t = try self.nextUntil("$.");
                defer t.deinit();
                if (t.count() < 2) return Error.Incomplete;
                if (t.count() > 2) return Error.UnexpectedToken;
                result = try self.statement(.{
                    .F = .{ .kind = t.at(0).*, .variable = t.at(1).* },
                });
            },
            'e' => {
                defer label = null;
                result = try self.statement(.{
                    .E = .{
                        .label = label orelse return Error.MissingLabel,
                        .expression = try self.nextUntil("$."),
                    },
                });
            },
            'd' => {
                result = try self.statement(.{
                    .D = .{ .variables = try self.nextUntil("$.") },
                });
            },
            'a' => {
                defer label = null;
                result = try self.statement(.{
                    .A = .{
                        .label = label orelse return Error.MissingLabel,
                        .expression = try self.nextUntil("$."),
                    },
                });
            },
            else => return Error.IllegalToken,
        }
        if (label) |_| {
            self.optStatement = result;
            return Error.UnexpectedLabel;
        }
        return result;
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

test "check error for label on $d" {
    var statements = StatementIterator.init(std.testing.allocator, "xfreeinA $d A x $.");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: var) void {
            expect(eqs(s.?.D.variables, &[_]Token{ "A", "x" }));
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
