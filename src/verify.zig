usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const TokenSet = tokenize.TokenSet;
const TokenMap = tokenize.TokenMap;
const parse = @import("parse.zig");

const SinglyLinkedList = std.SinglyLinkedList;
const FELabelList = std.SegmentedList(struct { label: Token, fe: enum { F, E } }, 0);

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
    Variable: struct { usedInFStatement: bool },
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
    activeHypotheses: FELabelList,
    currentScopeDiff: ?*ScopeDiff,

    fn init(allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .meanings = TokenMap(Meaning).init(allocator),
            .activeHypotheses = FELabelList.init(allocator),
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
        self.activeHypotheses.deinit();
    }

    fn addStatementsFrom(self: *Self, buffer: []const u8) !void {
        var n: u64 = 0;
        defer std.debug.warn("\nFound {0} statements!\n", .{n});

        var statements = parse.StatementIterator.init(self.allocator, buffer);
        while (try statements.next()) |statement| {
            defer statement.deinit(self.allocator);
            n += 1;

            switch (statement.*) {
                .C => |cStatement| {
                    if (self.currentScopeDiff) |_| return Error.UnexpectedToken; // $c inside ${ $}
                    var it = @as(TokenList, cStatement.constants).iterator(0);
                    while (it.next()) |constant| {
                        if (self.meanings.get(constant.*)) |_| return Error.Duplicate;
                        const kv = try self.meanings.put(constant.*, MeaningType.Constant);
                    }
                },
                .V => |vStatement| {
                    var it = @as(TokenList, vStatement.variables).iterator(0); // TODO: why coercion needed??
                    while (it.next()) |variable| {
                        if (self.meanings.get(variable.*)) |_| return Error.Duplicate;
                        const kv = try self.meanings.put(variable.*, Meaning{ .Variable = .{ .usedInFStatement = false } });
                        if (self.currentScopeDiff) |scopeDiff| {
                            _ = try scopeDiff.activeTokens.add(variable.*); // this $v will become inactive at the next $}
                        }
                    }
                },
                .F => |fStatement| {
                    if (self.meanings.get(fStatement.label)) |_| return Error.Duplicate;
                    _ = try self.meanings.put(fStatement.label, Meaning{ .Rule = try self.fromHypothesis(fStatement.tokens) });
                    const variable = fStatement.tokens.at(1).*;
                    if (self.meanings.get(variable)) |kv2| {
                        if (kv2.value != .Variable) return Error.UnexpectedToken; // $f k l $. where l is something else than variable, TODO: test
                        if (kv2.value.Variable.usedInFStatement) return Error.Duplicate;
                        kv2.value.Variable.usedInFStatement = true;
                    } else return Error.UnexpectedToken; // $f k x $. without $v x $.  TODO: Test
                    _ = try self.activeHypotheses.push(.{ .label = fStatement.label, .fe = .F });
                    if (self.currentScopeDiff) |scopeDiff| {
                        scopeDiff.nrActiveHypotheses += 1;
                        _ = try scopeDiff.activeTokens.add(fStatement.label); // this $f will become inactive at the next $}
                        assert(!scopeDiff.variablesInFStatements.contains(variable));
                        _ = try scopeDiff.variablesInFStatements.add(variable);
                    }
                },
                .E => |eStatement| {
                    if (self.meanings.get(eStatement.label)) |_| return Error.Duplicate;
                    _ = try self.meanings.put(eStatement.label, Meaning{ .Rule = try self.fromHypothesis(eStatement.tokens) });
                    _ = try self.activeHypotheses.push(.{ .label = eStatement.label, .fe = .E });
                    if (self.currentScopeDiff) |scopeDiff| {
                        scopeDiff.nrActiveHypotheses += 1;
                        _ = try scopeDiff.activeTokens.add(eStatement.label); // this $e will become inactive at the next $}
                    }
                },
                .BlockOpen => {
                    try ScopeDiff.push(self);
                },
                .BlockClose => {
                    if (self.currentScopeDiff) |scopeDiff| {
                        scopeDiff.pop();
                    } else return Error.UnexpectedToken;
                },
                else => {
                    // TODO: implement the other statements, then remove this clause
                },
            }
        }
    }

    /// consumes the passed expression
    fn mandatoryHypothesesOf(self: *Self, expression: Expression) !MHIterator {
        defer self.allocator.free(expression);
        return try MHIterator.init(self, self.allocator, expression);
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
    variablesInFStatements: TokenSet,
    nrActiveHypotheses: usize,

    fn push(state: *VerifyState) !void {
        const newScopeDiff = try state.allocator.create(ScopeDiff);
        errdefer state.allocator.destroy(newScopeDiff);
        newScopeDiff.* = Self{
            .state = state,
            .optOuter = state.currentScopeDiff,
            .activeTokens = TokenSet.init(state.allocator),
            .variablesInFStatements = TokenSet.init(state.allocator),
            .nrActiveHypotheses = 0,
        };

        state.currentScopeDiff = newScopeDiff;
    }

    fn pop(self: *Self) void {
        self.state.currentScopeDiff = self.optOuter;
        self.deinitNrActiveHypotheses();
        self.deinitVariableInFStatements();
        self.deinitActiveTokens();
        self.state.allocator.destroy(self);
    }

    fn deinitNrActiveHypotheses(self: *Self) void {
        while (self.nrActiveHypotheses > 0) : (self.nrActiveHypotheses -= 1) {
            _ = self.state.activeHypotheses.pop();
        }
    }

    fn deinitVariableInFStatements(self: *Self) void {
        var it = self.variablesInFStatements.iterator();
        while (it.next()) |kv| {
            var kv2 = self.state.meanings.get(kv.key) orelse continue;
            assert(kv2.value == .Variable);
            assert(kv2.value.Variable.usedInFStatement == true);
            kv2.value.Variable.usedInFStatement = false;
        }
        self.variablesInFStatements.deinit();
    }

    fn deinitActiveTokens(self: *Self) void {
        var it = self.activeTokens.iterator();
        while (it.next()) |kv| {
            if (self.state.meanings.remove(kv.key)) |*kv2| {
                kv2.value.deinit(self.state.allocator);
            } else {
                std.debug.warn("\nTODO: insert meaning for token {0}.\n", .{kv.key});
                // this is reachable in error paths, unfortunately.
            }
        }
        self.activeTokens.deinit();
    }
};

pub fn verify(buffer: []const u8, allocator: *Allocator) !void {
    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});
    var state = try VerifyState.init(allocator);
    defer state.deinit();
    try state.addStatementsFrom(buffer);
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
    _ = try state.meanings.put("ph", Meaning{ .Variable = .{ .usedInFStatement = false } });
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

test "no duplicate variable declarations" {
    expectError(Error.Duplicate, verify("$c ca cb $. $v v $. cav $f ca v $. cbv $f cb v $.", std.testing.allocator));
}

test "no duplicate variable declarations, in nested scope (2)" {
    expectError(Error.Duplicate, verify("$c ca cb $. ${ $v v $. cav $f ca v $. cbv $f cb v $. $}", std.testing.allocator));
}

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

// ----------------------------------------
// ITERATOR OVER MANDATORY HYPOTHESES

const MHIterator = struct {
    const Self = @This();

    state: *VerifyState,
    allocator: *Allocator,
    mhs: SinglyLinkedList(Token),
    iterator: ?*SinglyLinkedList(Token).Node,

    /// expression remains owned by the caller
    fn init(state: *VerifyState, allocator: *Allocator, expression: Expression) !MHIterator {
        var variables = TokenSet.init(allocator);
        defer variables.deinit();
        for (expression) |cvToken| if (cvToken.cv == .V) {
            _ = try variables.add(cvToken.token);
        };

        var mhs = SinglyLinkedList(Token).init();
        // loop over state.activeHypotheses, in reverse order
        var it = state.activeHypotheses.iterator(state.activeHypotheses.count());
        while (it.prev()) |feLabel| {
            // TODO: if mandatory hypothesis, add to the front of mhs
        }

        return MHIterator{ .state = state, .allocator = allocator, .mhs = mhs, .iterator = mhs.first };
    }

    fn next(self: *Self) ?Token { // TODO: Change return type to InferenceRule?
        if (self.iterator) |node| {
            self.iterator = node.next;
            return node.data;
        } else return null;
    }
};

fn tokenListOf(buffer: []const u8) !TokenList {
    var it = parse.StatementIterator.init(std.testing.allocator, buffer);
    var result = TokenList.init(std.testing.allocator);
    while (true) {
        if (try it.nextToken()) |token| {
            _ = try result.push(token);
        } else break;
    }
    return result;
}

fn expressionOf(state: *VerifyState, buffer: []const u8) !Expression {
    var t = try tokenListOf(buffer);
    defer t.deinit();
    return try state.expressionOf(t);
}

test "iterate over no mandatory hypotheses" {
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    try state.addStatementsFrom("$c T $.");

    var it = try state.mandatoryHypothesesOf(try expressionOf(&state, "T"));
    expect(it.next() == null);
}

test "iterate over single $f hypothesis" {
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    try state.addStatementsFrom("$c wff |- $. $v ph $. wph $f wff ph $.");

    var it = try state.mandatoryHypothesesOf(try expressionOf(&state, "|- ph"));
    // TODO implement: expect(eq(it.next().?, "wph"));
}
