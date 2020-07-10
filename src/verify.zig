usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const TokenSet = tokenize.TokenSet;
const TokenMap = tokenize.TokenMap;
const parse = @import("parse.zig");

const CVToken = struct { token: Token, cv: enum { C, V } };
const Expression = []CVToken;
const Hypothesis = struct { expression: Expression, isF: bool };
const HypothesisList = []Hypothesis;
const InferenceRule = struct {
    const Self = @This();

    hypotheses: HypothesisList,
    conclusion: Expression,

    fn deinit(self: *Self, allocator: *Allocator) void {
        // Later: deinit the hypotheses
        allocator.free(self.conclusion);
    }
};

const Substitution = TokenMap(Expression);
const ProofState = std.SinglyLinkedList(Expression);

// TODO: Find a better name; {Token,Label,Symbol}Interpretation?
const MeaningType = enum { Constant, Variable, Rule };
const Meaning = union(MeaningType) {
    const Self = @This();

    Constant: void,
    Variable: ?Token,
    Rule: InferenceRule,

    fn deinit(self: *Self, allocator: *Allocator) void {
        switch (self.*) {
            .Constant, .Variable => {},
            .Rule => self.*.Rule.deinit(allocator),
        }
    }
};

const VerifyState = struct {
    const Self = @This();

    allocator: *Allocator,

    /// what each active token means
    meanings: TokenMap(Meaning),
    currentScopeDiff: ?*ScopeDiff,

    fn init(allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .meanings = TokenMap(Meaning).init(allocator),
            .currentScopeDiff = null,
        };
    }

    fn deinit(self: *Self) void {
        var it = self.meanings.iterator();
        while (it.next()) |kv| {
            kv.value.deinit(self.allocator);
        }
        self.meanings.deinit();
        while (self.currentScopeDiff) |scopeDiff| {
            scopeDiff.pop();
        }
    }

    /// caller gets ownership of result, needs to hand back to us to be freed by our allocator
    fn fromHypothesis(self: *Self, tokens: TokenList) !InferenceRule {
        const expression = try self.expressionOf(tokens);
        return InferenceRule{
            .hypotheses = &[_]Hypothesis{}, //TODO: Is this safe because it is comptime known?
            .conclusion = expression,
        };
    }

    /// caller gets ownership of result, needs to hand back to us to be freed by our allocator
    fn expressionOf(self: *Self, tokens: TokenList) !Expression {
        var result = try self.allocator.alloc(CVToken, tokens.count());
        errdefer self.allocator.free(result);
        var i: usize = 0;
        var it = @as(TokenList, tokens).iterator(0);
        while (it.next()) |pToken| : (i += 1) {
            const kv = self.meanings.get(pToken.*) orelse return Error.UnexpectedToken;
            result[i] = .{
                .token = pToken.*,
                .cv = switch (kv.value) {
                    .Constant => .C,
                    .Variable => .V,
                    else => return Error.UnexpectedToken,
                },
            };
        }
        return result;
    }
};

/// A ScopeDiff represents how a nested scope differs from its outer scope:
/// which tokens and labels will become inactive again at its ` $} ` statement.
const ScopeDiff = struct {
    const Self = @This();

    state: *VerifyState,
    optOuter: ?*ScopeDiff,
    activeTokens: TokenSet,

    fn push(state: *VerifyState) !void {
        const newScopeDiff = try state.allocator.create(ScopeDiff);
        errdefer state.allocator.destroy(newScopeDiff);
        newScopeDiff.* = Self{
            .state = state,
            .optOuter = state.currentScopeDiff,
            .activeTokens = TokenSet.init(state.allocator),
        };

        state.currentScopeDiff = newScopeDiff;
    }

    fn pop(self: *Self) void {
        self.state.currentScopeDiff = self.optOuter;
        self.deinitActiveTokens();
        self.state.allocator.destroy(self);
    }

    fn deinitActiveTokens(self: *Self) void {
        var it = self.activeTokens.iterator();
        while (it.next()) |kv| {
            if (self.state.meanings.remove(kv.key)) |*kv2| {
                kv2.value.deinit(self.state.allocator);
            } else {
                std.debug.warn("\nTODO: insert meaning for token {0}.\n", .{kv.key});
            }
        }
        self.activeTokens.deinit();
    }
};

pub fn verify(buffer: []const u8, allocator: *Allocator) !void {
    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});

    var n: u64 = 0;
    defer std.debug.warn("\nFound {0} statements!\n", .{n});

    var state = try VerifyState.init(allocator);
    defer state.deinit();

    var statements = parse.StatementIterator.init(allocator, buffer);
    while (try statements.next()) |statement| {
        defer statement.deinit(allocator);
        n += 1;

        switch (statement.*) {
            .C => |cStatement| {
                if (state.currentScopeDiff) |_| return Error.UnexpectedToken; // $c inside ${ $}
                var it = @as(TokenList, cStatement.constants).iterator(0);
                while (it.next()) |constant| {
                    const kv = try state.meanings.put(constant.*, MeaningType.Constant);
                    if (kv) |_| return Error.Duplicate;
                }
            },
            .V => |vStatement| {
                var it = @as(TokenList, vStatement.variables).iterator(0); // TODO: why coercion needed??
                while (it.next()) |variable| {
                    const kv = try state.meanings.put(variable.*, Meaning{ .Variable = null });
                    if (kv) |_| return Error.Duplicate;
                    if (state.currentScopeDiff) |scopeDiff| {
                        _ = try scopeDiff.activeTokens.add(variable.*); // this $v will become inactive at the next $}
                    }
                }
            },
            .F => |fStatement| {
                const kv = try state.meanings.put(fStatement.label, Meaning{ .Rule = try state.fromHypothesis(fStatement.tokens) });
                if (kv) |_| return Error.Duplicate; // check _before_ put
                if (state.currentScopeDiff) |scopeDiff| {
                    _ = try scopeDiff.activeTokens.add(fStatement.label); // this $f will become inactive at the next $}
                }
            },
            .E => |eStatement| {
                const kv = try state.meanings.put(eStatement.label, Meaning{ .Rule = try state.fromHypothesis(eStatement.tokens) });
                if (kv) |_| return Error.Duplicate; // check _before_ put
                if (state.currentScopeDiff) |scopeDiff| {
                    _ = try scopeDiff.activeTokens.add(eStatement.label); // this $e will become inactive at the next $}
                }
            },
            .BlockOpen => {
                try ScopeDiff.push(&state);
            },
            .BlockClose => {
                if (state.currentScopeDiff) |scopeDiff| {
                    scopeDiff.pop();
                } else return Error.UnexpectedToken;
            },
            else => {
                // TODO: implement the other statements, then remove this clause
            },
        }
    }

    if (state.currentScopeDiff) |_| return Error.Incomplete; // unclosed $}
}

const expect = std.testing.expect;
const expectError = std.testing.expectError;
const eq = tokenize.eq;
const eqs = tokenize.eqs;

test "use undeclared variable" {
    expectError(Error.UnexpectedToken, verify("$c wff $. wph $f wff ph $.", std.testing.allocator));
}

test "use undeclared variable" {
    expectError(Error.UnexpectedToken, verify("$c wff $. wph $f wff ph $.", std.testing.allocator));
}

test "use statement label as a token" {
    expectError(Error.UnexpectedToken, verify("$c wff ph $. wph $f wff ph $. wxx $e wph $.", std.testing.allocator));
}

test "simplest correct $f" {
    try verify("$c wff $. $v ph $. wph $f wff ph $.", std.testing.allocator);
}

test "tokenlist to expression" {
    // TODO: Simplify this test case using a few helpers and/or refactorings.
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    _ = try state.meanings.put("wff", MeaningType.Constant);
    _ = try state.meanings.put("ph", Meaning{ .Variable = null });
    var tokens = TokenList.init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.push("wff");
    try tokens.push("ph");
    expect(eqs(tokens, &[_]Token{ "wff", "ph" }));

    const expression = try state.expressionOf(tokens);
    defer std.testing.allocator.free(expression);

    expect(expression.len == 2);
    expect(eq(expression[0].token, "wff"));
    expect(expression[0].cv == .C);
    expect(eq(expression[1].token, "ph"));
    expect(expression[1].cv == .V);
}

//test "no duplicate variable declarations" {
//    expectError(Error.Duplicate, verify("$c ca cb $. $v v $. cav $f ca v $. cbv $f cb v $.", std.testing.allocator));
//}

//test "no duplicate variable declarations, in nested scope (2)" {
//    expectError(Error.Duplicate, verify("$c ca cb $. ${ $v v $. cav $f ca v $. cbv $f cb v $. $}", std.testing.allocator));
//}

test "duplicate variable declarations, in nested scope (2)" {
    try verify("$c ca cb $. $v v $. ${ cav $f ca v $. $} cbv $f cb v $.", std.testing.allocator);
}

test "$v in nested scope" {
    try verify("$c ca $. ${ $v v $. $}", std.testing.allocator);
}

test "$f in nested scope (1)" {
    try verify("$c ca $. ${ $v v $. cav $f ca v $. $}", std.testing.allocator);
}

test "$f in nested scope (2)" {
    try verify("$c ca $. $v v $. ${ cav $f ca v $. $}", std.testing.allocator);
}

test "token is either constant or variable, not both" {
    expectError(Error.Duplicate, verify("$c wff $. $v wff $.", std.testing.allocator));
}

test "no constant allowed in nested scope" {
    expectError(Error.UnexpectedToken, verify("${ $c wff $. $}", std.testing.allocator));
}

test "nested variable" {
    try verify("$v ph $. ${ $v ps $. $} $v ps $.", std.testing.allocator);
}

test "nested duplicate variable" {
    expectError(Error.Duplicate, verify("$v ph $. ${ $v ph $. $}", std.testing.allocator));
}

test "unopened block" {
    expectError(Error.UnexpectedToken, verify("$}", std.testing.allocator));
}

test "unclosed block" {
    expectError(Error.Incomplete, verify("${", std.testing.allocator));
}

test "multiple blocks" {
    try verify("${ $} ${ ${ $} $}", std.testing.allocator);
}

test "duplicate variable" {
    expectError(Error.Duplicate, verify("$v ph ps ph $.", std.testing.allocator));
}

test "single variable" {
    try verify("$v ph $.", std.testing.allocator);
}

test "duplicate constant" {
    expectError(Error.Duplicate, verify("$c wff wff $.", std.testing.allocator));
}
