const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const errors = @import("errors.zig");
const Error = errors.Error;

const read = @import("read.zig");
const readBuffer = read.readBuffer;

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

    pub fn deinit(self: *Statement, allocator: Allocator) void {
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

    pub fn deinitLeavingProof(self: *Statement, allocator: Allocator) TokenList {
        defer allocator.destroy(self);
        self.*.P.tokens.deinit();
        return self.*.P.proof;
    }
};

pub const StatementIterator = struct {
    allocator: Allocator,
    dir: std.fs.Dir,
    tokens: TokenIterator,
    optStatement: ?*Statement = null,
    nestedIterator: ?*StatementIterator = null,

    pub fn init(allocator: Allocator, dir: std.fs.Dir, buffer: []const u8) StatementIterator {
        return StatementIterator{ .allocator = allocator, .dir = dir, .tokens = TokenIterator{ .buffer = buffer } };
    }

    pub fn next(self: *StatementIterator) (Error || Allocator.Error || std.os.ReadError)!?*Statement {
        // return any statement detected in the previous call
        if (self.optStatement) |s| {
            self.optStatement = null;
            return s;
        }
        // get next token
        var token: Token = undefined;
        while (true) {
            // find token in nested iterator
            if (self.nestedIterator) |it| {
                const optStatement = try it.next();
                if (optStatement) |stat| return stat;
                // end of nested iterator, clean up
                self.allocator.free(it.tokens.buffer);
                self.allocator.destroy(it);
                self.nestedIterator = null;
            }

            // find token in myself
            token = (try self.nextToken()) orelse {
                // we have seen the last Token, so we have seen the last Statement, end of iteration
                return null;
            };
            if (!eq(token, "$[")) break;

            // create nested iterator for include file
            const f = try self.nextUntil("$]");
            if (f.items.len != 1) return Error.IncorrectFileName; // TODO: test
            defer f.deinit();
            const include_file_name = f.items[0];

            const buffer = try readBuffer(self.allocator, self.dir, include_file_name);
            self.nestedIterator = try self.allocator.create(StatementIterator);
            self.nestedIterator.?.* = StatementIterator.init(self.allocator, self.dir, buffer);
            // ...and go on to immediately use this new nested iterator
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
const alltests = @import("alltests.zig");
const TestFS = alltests.TestFS;

fn forNext(statements: *StatementIterator, f: anytype) !void {
    const s = try statements.next();
    _ = try f.do(s);
    s.?.deinit(std.testing.allocator);
}

pub fn _StatementIterator_init(buffer: []const u8) StatementIterator {
    const unused_root_dir = undefined; // no file access done in this test
    return StatementIterator.init(std.testing.allocator, unused_root_dir, buffer);
}

test "parse constant declaration from include file" {
    var testFS = TestFS.init();
    defer testFS.deinit();
    try testFS.writeFile("constants.mm", "$c wff |- $.");

    var statements = StatementIterator.init(std.testing.allocator, testFS.tmpDir.dir, "$[ constants.mm $]");
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(eqs(s.?.C.constants, &[_]Token{ "wff", "|-" }));
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "parse file only including empty include file" {
    var testFS = TestFS.init();
    defer testFS.deinit();
    try testFS.writeFile("empty.mm", "");

    var statements = StatementIterator.init(std.testing.allocator, testFS.tmpDir.dir, "$[ empty.mm $]");
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "include non-existing file" {
    var testFS = TestFS.init();
    defer testFS.deinit();

    var statements = StatementIterator.init(std.testing.allocator, testFS.tmpDir.dir, "$[ nonexisting.mm $]");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.IncorrectFileName);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "include without file name" {
    var statements = _StatementIterator_init("$[ $]");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.IncorrectFileName);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "$c with label" {
    var statements = _StatementIterator_init("c $c T $.");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(s != null);
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "$v with label" {
    var statements = _StatementIterator_init("v $v a $.");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(s != null);
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "$f without label" {
    var statements = _StatementIterator_init("$f");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.MissingLabel);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "$e without label" {
    var statements = _StatementIterator_init("$e");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.MissingLabel);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "$d with label" {
    var statements = _StatementIterator_init("dxy $d x y $.");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(s != null);
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "$a without label" {
    var statements = _StatementIterator_init("$a");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.MissingLabel);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "$p without label" {
    var statements = _StatementIterator_init("$p");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.MissingLabel);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "${ with label" {
    var statements = _StatementIterator_init("block ${");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(s != null);
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "$} with label" {
    var statements = _StatementIterator_init("endblock $}");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(s != null);
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "unknown statement type (token skipped)" {
    var statements = _StatementIterator_init("x $x");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.IllegalToken);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "too short $f statement" {
    var statements = _StatementIterator_init("w $f wff $.");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.Incomplete);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "`$xy` token after label" {
    var statements = _StatementIterator_init("$xy");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.IllegalToken);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "`$xy` token after label" {
    var statements = _StatementIterator_init("aLabel $xy");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.IllegalToken);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "`$` token without label" {
    var statements = _StatementIterator_init("$");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.IllegalToken);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "`$` token after label" {
    var statements = _StatementIterator_init("aLabel $");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.IllegalToken);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "non-$ token after label" {
    var statements = _StatementIterator_init("aLabel nonCommand");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.UnexpectedToken);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "parse $p declaration" {
    var statements = _StatementIterator_init("idwffph $p wff ph $= ? $.");
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(eqs(s.?.P.tokens, &[_]Token{ "wff", "ph" }));
            try expect(eqs(s.?.P.proof, &[_]Token{"?"}));
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "parse $f declaration" {
    var statements = _StatementIterator_init("wph $f wff ph $.");
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(eq(s.?.F.label, "wph"));
            try expect(eqs(s.?.F.tokens, &[_]Token{ "wff", "ph" }));
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "check error for label on $d" {
    var statements = _StatementIterator_init("xfreeinA $d A x $.");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.UnexpectedLabel);
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(eqs(s.?.D.variables, &[_]Token{ "A", "x" }));
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "check error for unknown command" {
    var statements = _StatementIterator_init("$Q");
    if (statements.next()) |_| unreachable else |err| try expect(err == Error.IllegalToken);
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "parse constant declaration" {
    var statements = _StatementIterator_init("$c wff |- $.");
    _ = try forNext(&statements, struct {
        fn do(s: anytype) !void {
            try expect(eqs(s.?.C.constants, &[_]Token{ "wff", "|-" }));
        }
    });
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "parse comment, also including $[" {
    var statements = _StatementIterator_init("$( a $[ b.mm $] c $)");
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}

test "parse empty file" {
    var statements = _StatementIterator_init("");
    try expect((try statements.next()) == null);
    try expect((try statements.next()) == null);
}
