usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const eq = tokenize.eq;
const eqs = tokenize.eqs;
const TokenList = tokenize.TokenList;
const TokenIterator = tokenize.TokenIterator;

pub const StatementType = enum { C, V, F, E, D, A, P, BlockOpen, BlockClose };

pub const Statement = union(StatementType) {
    C: struct { constants: TokenList },
    V: struct { variables: TokenList },
    F: struct { label: Token, tokens: TokenList }, // runtime checked: exactly 2 tokens
    E: struct { label: Token, tokens: TokenList },
    D: struct { variables: TokenList },
    A: struct { label: Token, tokens: TokenList },
    P: struct { label: Token, tokens: TokenList, proof: TokenList },
    BlockOpen,
    BlockClose: struct {},

    pub fn deinit(self: *Statement, allocator: *Allocator) void {
        switch (self.*) {
            .C => self.*.C.constants.deinit(),
            .V => self.*.V.variables.deinit(),
            .F => self.*.F.tokens.deinit(),
            .E => self.*.E.tokens.deinit(),
            .D => self.*.D.variables.deinit(),
            .A => self.*.A.tokens.deinit(),
            .P => {
                self.*.P.tokens.deinit();
                self.*.P.proof.deinit();
            },
            .BlockOpen, .BlockClose => {},
        }
        allocator.destroy(self);
    }

    pub fn deinitLeavingProof(self: *Statement, allocator: *Allocator) TokenList {
        defer allocator.destroy(self);
        self.*.P.tokens.deinit();
        return self.*.P.proof;
    }
};

pub const StatementIterator = struct {
    allocator: *Allocator,
    tokens: TokenIterator,
    optStatement: ?*Statement = null,

    pub fn init(allocator: *Allocator, buffer: []const u8) StatementIterator {
        return StatementIterator{ .allocator = allocator, .tokens = TokenIterator{ .buffer = buffer } };
    }

    pub fn next(self: *StatementIterator) !?*Statement {
        // return any statement detected in the previous call
        if (self.optStatement) |s| {
            self.optStatement = null;
            return s;
        }
        // get next token
        var token: Token = undefined;
        while (true) {
            token = (try self.nextToken()) orelse {
                // we have seen the last Token, so we have seen the last Statement, end of iteration
                return null;
            };
            if (eq(token, "$[")) {
                (try self.nextUntil("$]")).deinit();
            } else break;
        }
        // if the token is a label, read one more token
        var optLabel: ?Token = null;
        while (optLabel == null) {
            if (token[0] == '$') break;
            optLabel = token;
            token = (try self.nextToken()) orelse return Error.Incomplete;
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
                defer optLabel = null;
                const label = optLabel orelse return Error.MissingLabel;
                var tokens = try self.nextUntil("$.");
                errdefer tokens.deinit();
                if (tokens.items.len < 2) return Error.Incomplete;
                if (tokens.items.len > 2) return Error.UnexpectedToken;
                result = try self.statement(.{
                    .F = .{
                        .label = label,
                        .tokens = tokens,
                    },
                });
            },
            'e' => {
                defer optLabel = null;
                result = try self.statement(.{
                    .E = .{
                        .label = optLabel orelse return Error.MissingLabel,
                        .tokens = try self.nextUntil("$."),
                    },
                });
            },
            'd' => {
                result = try self.statement(.{
                    .D = .{ .variables = try self.nextUntil("$.") },
                });
            },
            'a' => {
                defer optLabel = null;
                result = try self.statement(.{
                    .A = .{
                        .label = optLabel orelse return Error.MissingLabel,
                        .tokens = try self.nextUntil("$."),
                    },
                });
            },
            'p' => {
                defer optLabel = null;
                result = try self.statement(.{
                    .P = .{
                        .label = optLabel orelse return Error.MissingLabel,
                        .tokens = try self.nextUntil("$="),
                        .proof = try self.nextUntil("$."),
                    },
                });
            },
            '{' => result = try self.statement(.{ .BlockOpen = .{} }),
            '}' => result = try self.statement(.{ .BlockClose = .{} }),
            else => return Error.IllegalToken,
        }
        if (optLabel) |_| {
            self.optStatement = result;
            return Error.UnexpectedLabel;
        }
        return result;
    }

    pub fn nextToken(self: *StatementIterator) !?Token {
        while (true) {
            const result = try self.tokens.next();
            if (result) |token| {
                if (eq(token, "$(")) {
                    (try self.nextUntil("$)")).deinit();
                    continue;
                }
            }
            return result;
        }
    }

    fn nextUntil(self: *StatementIterator, terminator: Token) (Error || Allocator.Error)!TokenList {
        var result = TokenList.init(self.allocator);
        while (true) {
            const token = (try self.nextToken()) orelse return Error.Incomplete;
            if (eq(token, terminator)) break;
            _ = try result.append(token);
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

fn forNext(statements: *StatementIterator, f: anytype) !void {
    const s = try statements.next();
    _ = f.do(s);
    s.?.deinit(std.testing.allocator);
}

test "parse empty file with empty include file" {
    var statements = StatementIterator.init(std.testing.allocator, "$[ empty.mm $]");
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "$c with label" {
    var statements = StatementIterator.init(std.testing.allocator, "c $c T $.");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(s != null);
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "$v with label" {
    var statements = StatementIterator.init(std.testing.allocator, "v $v a $.");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(s != null);
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "$f without label" {
    var statements = StatementIterator.init(std.testing.allocator, "$f");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.MissingLabel);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "$e without label" {
    var statements = StatementIterator.init(std.testing.allocator, "$e");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.MissingLabel);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "$d with label" {
    var statements = StatementIterator.init(std.testing.allocator, "dxy $d x y $.");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(s != null);
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "$a without label" {
    var statements = StatementIterator.init(std.testing.allocator, "$a");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.MissingLabel);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "$p without label" {
    var statements = StatementIterator.init(std.testing.allocator, "$p");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.MissingLabel);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "${ with label" {
    var statements = StatementIterator.init(std.testing.allocator, "block ${");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(s != null);
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "$} with label" {
    var statements = StatementIterator.init(std.testing.allocator, "endblock $}");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(s != null);
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "unknown statement type (token skipped)" {
    var statements = StatementIterator.init(std.testing.allocator, "x $x");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.IllegalToken);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "too short $f statement" {
    var statements = StatementIterator.init(std.testing.allocator, "w $f wff $.");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.Incomplete);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "`$xy` token after label" {
    var statements = StatementIterator.init(std.testing.allocator, "$xy");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.IllegalToken);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "`$xy` token after label" {
    var statements = StatementIterator.init(std.testing.allocator, "aLabel $xy");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.IllegalToken);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "`$` token without label" {
    var statements = StatementIterator.init(std.testing.allocator, "$");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.IllegalToken);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "`$` token after label" {
    var statements = StatementIterator.init(std.testing.allocator, "aLabel $");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.IllegalToken);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "non-$ token after label" {
    var statements = StatementIterator.init(std.testing.allocator, "aLabel nonCommand");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.UnexpectedToken);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "parse $p declaration" {
    var statements = StatementIterator.init(std.testing.allocator, "idwffph $p wff ph $= ? $.");
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(eqs(s.?.P.tokens, &[_]Token{ "wff", "ph" }));
            expect(eqs(s.?.P.proof, &[_]Token{"?"}));
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "parse $f declaration" {
    var statements = StatementIterator.init(std.testing.allocator, "wph $f wff ph $.");
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(eq(s.?.F.label, "wph"));
            expect(eqs(s.?.F.tokens, &[_]Token{ "wff", "ph" }));
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "check error for label on $d" {
    var statements = StatementIterator.init(std.testing.allocator, "xfreeinA $d A x $.");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(eqs(s.?.D.variables, &[_]Token{ "A", "x" }));
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "check error for unknown command" {
    var statements = StatementIterator.init(std.testing.allocator, "$Q");
    if (statements.next()) |_| unreachable else |err| expect(err == Error.IllegalToken);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "parse constant declaration" {
    var statements = StatementIterator.init(std.testing.allocator, "$c wff |- $.");
    _ = try forNext(&statements, struct {
        fn do(s: anytype) void {
            expect(eqs(s.?.C.constants, &[_]Token{ "wff", "|-" }));
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "parse comment, also including $[" {
    var statements = StatementIterator.init(std.testing.allocator, "$( a $[ b.mm $] c $)");
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "parse empty file" {
    var statements = StatementIterator.init(std.testing.allocator, "");
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}
