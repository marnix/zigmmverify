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
};

pub const StatementIterator = struct {
    allocator: *Allocator,
    tokens: TokenIterator,
    optStatement: ?*Statement = null,

    pub fn init(allocator: *Allocator, buffer: Token) StatementIterator {
        return StatementIterator{ .allocator = allocator, .tokens = TokenIterator{ .buffer = buffer } };
    }

    pub fn next(self: *StatementIterator) !?*Statement {
        // return any statement detected in the previous call
        if (self.optStatement) |s| {
            self.optStatement = null;
            return s;
        }
        // get next token
        var token = (try self.nextToken()) orelse {
            // we have seen the last Token, so we have seen the last Statement, end of iteration
            return null;
        };
        // if the token is a label, read one more token
        var label: ?Token = null;
        while (label == null) {
            if (token[0] == '$') break;
            label = token;
            token = (try self.nextToken()) orelse return Error.Incomplete;
        }
        if (token[0] != '$') return Error.UnexpectedToken; //TODO: Test
        if (token.len != 2) return Error.IllegalToken; //TODO: Test
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
                defer label = null;
                var t = try self.nextUntil("$.");
                if (t.count() < 2) return Error.Incomplete; //TODO: Test
                if (t.count() > 2) return Error.UnexpectedToken; //TODO: Test
                result = try self.statement(.{
                    .F = .{
                        .label = label orelse return Error.MissingLabel, //TODO: Test
                        .tokens = t,
                    },
                });
            },
            'e' => {
                defer label = null;
                result = try self.statement(.{
                    .E = .{
                        .label = label orelse return Error.MissingLabel, //TODO: Test
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
                defer label = null;
                result = try self.statement(.{
                    .A = .{
                        .label = label orelse return Error.MissingLabel, //TODO: Test
                        .tokens = try self.nextUntil("$."),
                    },
                });
            },
            'p' => {
                defer label = null;
                result = try self.statement(.{
                    .P = .{
                        .label = label orelse return Error.MissingLabel, //TODO: Test
                        .tokens = try self.nextUntil("$="),
                        .proof = try self.nextUntil("$."),
                    },
                });
            },
            '{' => return self.statement(.{ .BlockOpen = .{} }),
            '}' => return self.statement(.{ .BlockClose = .{} }),
            else => return Error.IllegalToken, //TODO: Test
        }
        if (label) |_| {
            self.optStatement = result;
            return Error.UnexpectedLabel; //TODO: Test
        }
        return result;
    }

    fn nextToken(self: *StatementIterator) !?Token {
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

test "parse $p declaration" {
    var statements = StatementIterator.init(std.testing.allocator, "idwffph $p wff ph $= ? $.");
    _ = try forNext(&statements, struct {
        fn do(s: var) void {
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
        fn do(s: var) void {
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
        fn do(s: var) void {
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
        fn do(s: var) void {
            expect(eqs(s.?.C.constants, &[_]Token{ "wff", "|-" }));
        }
    });
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "parse comment" {
    var statements = StatementIterator.init(std.testing.allocator, "$( a $[ b.mm $] c $)");
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

test "parse empty file" {
    var statements = StatementIterator.init(std.testing.allocator, "");
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}
