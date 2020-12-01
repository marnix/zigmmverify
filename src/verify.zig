usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const TokenSet = tokenize.TokenSet;
const TokenMap = tokenize.TokenMap;

const parse = @import("parse.zig");

const prove = @import("prove.zig");
const AsRuleMeaningMap = prove.AsRuleMeaningMap;

pub fn copyExpression(allocator: *Allocator, original: Expression) !Expression {
    return try sliceCopy(CVToken, allocator, original);
}

// TODO: move to new utils.zig?
fn sliceCopy(comptime T: type, allocator: *Allocator, original: []const T) ![]const T {
    var copy = try allocator.alloc(T, original.len);
    errdefer allocator.free(copy);
    std.mem.copy(T, copy, original);
    return copy;
}

const SinglyLinkedList = std.SinglyLinkedList;
const FELabel = struct { label: Token, fe: enum { F, E } };
const FELabelList = std.SegmentedList(FELabel, 0);
pub const DVPair = struct { var1: Token, var2: Token };
const DVPairList = std.SegmentedList(DVPair, 0);

pub const CVToken = struct { token: Token, cv: enum { C, V } };
pub const Expression = []const CVToken;

pub fn eqExpr(a: Expression, b: Expression) bool {
    const result = brk: {
        if (a.len != b.len) break :brk false;
        for (a) |ai, i| {
            if (!(eq(ai.token, b[i].token) and ai.cv == b[i].cv)) break :brk false;
        }
        break :brk true;
    };
    if (!result) {
        std.debug.warn("\neqExpr: actual = ", .{});
        warnExpr(a);
        std.debug.warn(", expected = ", .{});
        warnExpr(b);
        std.debug.warn(".\n", .{});
    }
    return result;
}

fn warnExpr(expr: Expression) void {
    var sep: []const u8 = "";
    for (expr) |cvToken| {
        switch (cvToken.cv) {
            .C => {
                std.debug.warn("{1}{0}", .{ cvToken.token, sep });
            },
            .V => {
                std.debug.warn("{1}${0}", .{ cvToken.token, sep });
            },
        }
        sep = " ";
    }
}

pub const Hypothesis = struct {
    const Self = @This();

    expression: Expression,
    isF: bool,

    fn deinit(self: *Self, allocator: *Allocator) void {
        allocator.free(self.expression);
    }
};

pub const InferenceRule = struct {
    const Self = @This();

    activeDVPairs: []DVPair,
    hypotheses: []Hypothesis,
    conclusion: Expression,

    fn deinit(self: *Self, allocator: *Allocator) void {
        allocator.free(self.activeDVPairs);
        for (self.hypotheses) |*hyp| {
            hyp.deinit(allocator);
        }
        allocator.free(self.hypotheses);
        allocator.free(self.conclusion);
    }
};

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

pub const VerifyState = struct {
    const Self = @This();

    allocator: *Allocator,

    /// what each active token means
    meanings: TokenMap(Meaning),
    /// the active hypotheses, and whether they are $f or $e
    activeHypotheses: FELabelList,
    /// the active distinct variable pairs
    activeDVPairs: DVPairList,
    /// the difference between the current nested scope
    /// and its immediately surrounding scope
    currentScopeDiff: ?*ScopeDiff,

    fn init(allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .meanings = TokenMap(Meaning).init(allocator),
            .activeHypotheses = FELabelList.init(allocator),
            .activeDVPairs = DVPairList.init(allocator),
            .currentScopeDiff = null,
        };
    }

    fn deinit(self: *Self) void {
        while (self.currentScopeDiff) |scopeDiff| {
            scopeDiff.pop();
        }
        self.activeDVPairs.deinit();
        self.activeHypotheses.deinit();
        var it = self.meanings.iterator();
        while (it.next()) |kv| {
            kv.value.deinit(self.allocator);
        }
        self.meanings.deinit();
    }

    fn addStatementsFrom(self: *Self, buffer: []const u8) !void {
        const selfAsRuleMeaningMap = AsRuleMeaningMap(*VerifyState){ .child = self, .getter = Self.getRuleMeaningOf };

        var nr_statements: u64 = 0;
        var nr_proofs: u64 = 0;
        defer std.debug.warn("\nFound {0} statements so far, of which {1} are $p.\n", .{ nr_statements, nr_proofs });

        var frameArena = std.heap.ArenaAllocator.init(self.allocator);
        defer frameArena.deinit();
        const frameAllocator = &frameArena.allocator;

        var batch = std.event.Batch(anyerror!void, 1000, .auto_async).init();

        var statements = parse.StatementIterator.init(self.allocator, buffer);
        while (try statements.next()) |statement| {
            defer statement.deinit(self.allocator);
            nr_statements += 1;

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
                    try self.meanings.put(fStatement.label, Meaning{ .Rule = try self.fromHypothesis(fStatement.tokens) });
                    const variable = fStatement.tokens.at(1).*;
                    if (self.meanings.get(variable)) |meaning| {
                        if (meaning != .Variable) return Error.UnexpectedToken; // $f k l $. where l is something else than variable,
                        if (meaning.Variable.usedInFStatement) return Error.Duplicate;
                    } else unreachable; // $f k x $. without $v x $. is already detected in fromHypothesis() call above
                    try self.meanings.put(variable, .{ .Variable = .{ .usedInFStatement = true } });
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
                    try self.meanings.put(eStatement.label, Meaning{ .Rule = try self.fromHypothesis(eStatement.tokens) });
                    _ = try self.activeHypotheses.push(.{ .label = eStatement.label, .fe = .E });
                    if (self.currentScopeDiff) |scopeDiff| {
                        scopeDiff.nrActiveHypotheses += 1;
                        _ = try scopeDiff.activeTokens.add(eStatement.label); // this $e will become inactive at the next $}
                    }
                },
                .A => |aStatement| {
                    if (self.meanings.get(aStatement.label)) |_| return Error.Duplicate;
                    try self.meanings.put(aStatement.label, Meaning{ .Rule = try self.inferenceRuleOf(aStatement.tokens) });
                },
                .P => |pStatement| {
                    nr_proofs += 1;
                    if (self.meanings.get(pStatement.label)) |_| return Error.Duplicate;
                    const rule = try self.inferenceRuleOf(pStatement.tokens);
                    _ = try self.meanings.put(pStatement.label, Meaning{ .Rule = rule });

                    std.event.Loop.startCpuBoundOperation();

                    const frame = try frameAllocator.create(@Frame(VerifyState.verifyProofConclusion));
                    frame.* = async self.verifyProofConclusion(pStatement.label, pStatement.proof, rule.hypotheses, .{
                        .expression = rule.conclusion,
                        .dvPairs = rule.activeDVPairs,
                    });
                    batch.add(frame);
                },
                .D => |dStatement| {
                    var it1 = @as(TokenList, dStatement.variables).iterator(0);
                    var i: usize = 0;
                    while (it1.next()) |pVar1| : (i += 1) {
                        if (self.meanings.get(pVar1.*)) |meaning| {
                            if (meaning != .Variable) return Error.UnexpectedToken; // TODO: test
                        } else return Error.UnexpectedToken; //TODO: test
                        // ...
                        var it2 = @as(TokenList, dStatement.variables).iterator(i + 1);
                        while (it2.next()) |pVar2| {
                            // TODO: error if var1 == var2
                            _ = try self.activeDVPairs.push(.{ .var1 = pVar1.*, .var2 = pVar2.* });
                            if (self.currentScopeDiff) |scopeDiff| {
                                scopeDiff.nrActiveDVPairs += 1;
                            }
                        }
                        // ...
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
            }

        }
        try batch.wait();
    }

    fn verifyProofConclusion(self: *VerifyState, label: []const u8, proof: TokenList, hypotheses: []Hypothesis, conclusion: struct {
        expression: Expression,
        dvPairs: []DVPair,
    }) anyerror!void {
        // std.debug.warn("\nstarting to verify proof of {0}.\n", .{label});
        // defer std.debug.warn("end of verify proof of {0}.\n", .{label});
        const selfAsRuleMeaningMap = AsRuleMeaningMap(*VerifyState){ .child = self, .getter = Self.getRuleMeaningOf };
        var result = try prove.runProof(proof, hypotheses, selfAsRuleMeaningMap, self.allocator);
        defer result.deinit(self.allocator);

        if (!eqExpr(result.expression, conclusion.expression)) return Error.ResultMismatch;

        // if not(every result.dvPairs is in conclusion.dvPairs) return Error.DVRMissing;
        var it = result.dvPairs.iterator(0);
        while (it.next()) |proofDVPair| {
            for (conclusion.dvPairs) |ruleDVPair| {
                if ((eq(proofDVPair.var1, ruleDVPair.var1) and eq(proofDVPair.var2, ruleDVPair.var2)) or
                    (eq(proofDVPair.var1, ruleDVPair.var2) and eq(proofDVPair.var2, ruleDVPair.var1)))
                {
                    // proofDVPair is declared in an active $d statement
                    break;
                }
            } else {
                // proofDVPair is not declared in any active $d statement
                std.debug.warn("$d {0} {1} $. expected but not found in the following list:\n", .{ proofDVPair.var1, proofDVPair.var2 });
                for (conclusion.dvPairs) |ruleDVPair| {
                    std.debug.warn("   $d {0} {1} $.\n", .{ ruleDVPair.var1, ruleDVPair.var2 });
                }
                std.debug.warn("(end of list)\n", .{});
                return Error.DVRMissing; // TODO: Test
            }
        }
    }

    /// caller does not get ownership
    fn getRuleMeaningOf(self: *Self, token: Token) !InferenceRule {
        switch (self.meanings.get(token) orelse return Error.UnexpectedToken) { //TODO: test
            .Rule => |rule| return rule,
            else => return Error.UnexpectedToken, // TODO: test
        }
    }

    /// caller gets ownership of result, needs to hand back to us to be freed by our allocator
    fn inferenceRuleOf(self: *Self, tokens: TokenList) !InferenceRule {
        const conclusion = try self.expressionOf(tokens);
        var it = try self.mandatoryHypothesesOf(conclusion);
        defer it.deinit();

        var dvPairs = try self.allocator.alloc(DVPair, self.activeDVPairs.len);
        var j: usize = 0;
        var it2 = self.activeDVPairs.iterator(0);
        while (it2.next()) |dvPair| : (j += 1) {
            dvPairs[j] = dvPair.*;
        }

        var hypotheses = try self.allocator.alloc(Hypothesis, it.count());
        var i: usize = 0;
        while (it.next()) |feLabel| : (i += 1) {
            const hypExpression = self.meanings.get(feLabel.label).?.Rule.conclusion;
            hypotheses[i] = .{
                .expression = try copyExpression(self.allocator, hypExpression),
                .isF = (feLabel.fe == .F),
            };
        }

        return InferenceRule{
            .activeDVPairs = dvPairs,
            .hypotheses = hypotheses,
            .conclusion = conclusion,
        };
    }

    /// caller keeps owning the passed expression
    fn mandatoryHypothesesOf(self: *Self, expression: Expression) !MHIterator {
        return try MHIterator.init(self, self.allocator, expression);
    }

    /// caller gets ownership of result, needs to hand back to us to be freed by our allocator
    fn fromHypothesis(self: *Self, tokens: TokenList) !InferenceRule {
        const expression = try self.expressionOf(tokens);
        return InferenceRule{
            .activeDVPairs = try self.allocator.alloc(DVPair, 0), // TODO: It seems we just can use &[_]DVPair{} ??
            .hypotheses = try self.allocator.alloc(Hypothesis, 0), // TODO: It seems we just can use &[_]Hypothesis{} ??
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
            const cv = self.meanings.get(pToken.*) orelse return Error.UnexpectedToken;
            result[i] = .{
                .token = pToken.*,
                .cv = switch (cv) {
                    .Constant => .C,
                    .Variable => .V,
                    else => return Error.UnexpectedToken,
                },
            };
        }
        return result;
    }
};

/// A ScopeDiff represents how a nested scope differs from its immediately surrounding scope:
/// which tokens and labels will become inactive again at its ` $} ` statement.
const ScopeDiff = struct {
    const Self = @This();

    state: *VerifyState,
    optOuter: ?*ScopeDiff,
    activeTokens: TokenSet,
    variablesInFStatements: TokenSet,
    nrActiveHypotheses: usize,
    nrActiveDVPairs: usize,

    fn push(state: *VerifyState) !void {
        const newScopeDiff = try state.allocator.create(ScopeDiff);
        errdefer state.allocator.destroy(newScopeDiff);
        newScopeDiff.* = Self{
            .state = state,
            .optOuter = state.currentScopeDiff,
            .activeTokens = TokenSet.init(state.allocator),
            .variablesInFStatements = TokenSet.init(state.allocator),
            .nrActiveHypotheses = 0,
            .nrActiveDVPairs = 0,
        };

        state.currentScopeDiff = newScopeDiff;
    }

    fn pop(self: *Self) void {
        self.state.currentScopeDiff = self.optOuter;
        self.deinitNrActiveDVPairs();
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

    fn deinitNrActiveDVPairs(self: *Self) void {
        while (self.nrActiveDVPairs > 0) : (self.nrActiveDVPairs -= 1) {
            _ = self.state.activeDVPairs.pop();
        }
    }

    fn deinitVariableInFStatements(self: *Self) void {
        var it = self.variablesInFStatements.iterator();
        while (it.next()) |kv| {
            const variable = kv.key;
            if (self.state.meanings.get(variable)) |meaning| {
                assert(meaning == .Variable);
                assert(meaning.Variable.usedInFStatement == true);
            } else unreachable;
            _ = self.state.meanings.put(variable, .{ .Variable = .{ .usedInFStatement = false } }) catch unreachable; // in-place update can't fail?
        }
        self.variablesInFStatements.deinit();
    }

    fn deinitActiveTokens(self: *Self) void {
        var it = self.activeTokens.iterator();
        while (it.next()) |kv| {
            if (self.state.meanings.remove(kv.key)) |*kv2| {
                kv2.value.deinit(self.state.allocator);
            } else unreachable;
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

test "proof with $d violation" {
    expectError(Error.DVRMissing, verify(
        \\$c wff |- $.
        \\$v P Q R $.
        \\wp $f wff P $.
        \\wq $f wff Q $.
        \\wr $f wff R $.
        \\${ $d P Q $. pq.1 $e |- P $. pq $a |- Q $. $}
        \\
        \\${
        \\  qr.1 $e |- Q $. qr $p |- R $= wq wr qr.1 pq $.
        \\$}
    , std.testing.allocator));
}

test "proof with correct $d" {
    // FOR DEBUGGING: const allocator = &std.heap.loggingAllocator(std.testing.allocator, std.io.getStdErr().outStream()).allocator;
    try verify(
        \\$c wff |- $.
        \\$v P Q R $.
        \\wp $f wff P $.
        \\wq $f wff Q $.
        \\wr $f wff R $.
        \\${ $d P Q $. pq.1 $e |- P $. pq $a |- Q $. $}
        \\
        \\${
        \\  $d Q R $.
        \\  qr.1 $e |- Q $. qr $p |- R $= wq wr qr.1 pq $.
        \\$}
    , std.testing.allocator);
}

test "active DVR (found a memory leak)" {
    try verify(
        \\$c wff $.
        \\$v P Q $.
        \\wp $f wff P $.
        \\wq $f wff Q $.
        \\$d P Q $. pq.1 $e wff P $. pq $a wff Q $.
    , std.testing.allocator);
}

test "count number of active $d pairs" {
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    var n: usize = 0;
    try state.addStatementsFrom("$v a b c d e $.");

    try state.addStatementsFrom("$d $.");
    expect(state.activeDVPairs.len == n + 0);
    n = state.activeDVPairs.len;

    try state.addStatementsFrom("$d a $.");
    expect(state.activeDVPairs.len == n + 0);
    n = state.activeDVPairs.len;

    try state.addStatementsFrom("$d a a $.");
    expect(state.activeDVPairs.len == n + 1);
    n = state.activeDVPairs.len;

    try state.addStatementsFrom("$d a b $.");
    expect(state.activeDVPairs.len == n + 1);
    n = state.activeDVPairs.len;

    try state.addStatementsFrom("$d a b c d e $.");
    expect(state.activeDVPairs.len == n + 10);
    n = state.activeDVPairs.len;
}

test "$d with constant" {
    expectError(Error.UnexpectedToken, verify("$c class $. $d class $.", std.testing.allocator));
}

test "$d with undeclared token" {
    expectError(Error.UnexpectedToken, verify("$d class $.", std.testing.allocator));
}

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
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    try state.meanings.put("wff", MeaningType.Constant);
    try state.meanings.put("ph", Meaning{ .Variable = .{ .usedInFStatement = false } });
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

test "$v in nested scope, used in $a (use-after-free reproduction)" {
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();

    try state.addStatementsFrom("$c class setvar $. ${ $v x $. vx.cv $f setvar x $. cv $a class x $.");
    const cv: InferenceRule = state.meanings.get("cv").?.Rule;
    const vx_cv: Hypothesis = cv.hypotheses[0];
    const x: Token = vx_cv.expression[1].token;
    expect(eq(x, "x"));

    try state.addStatementsFrom("$}");
    const cv2: InferenceRule = state.meanings.get("cv").?.Rule;
    const vx_cv2: Hypothesis = cv.hypotheses[0];
    const x2: Token = vx_cv.expression[1].token;
    expect(eq(x2, "x"));
}

test "$f in nested scope (1)" {
    try verify("$c ca $. ${ $v v $. cav $f ca v $. $}", std.testing.allocator);
}

test "$f in nested scope (2)" {
    try verify("$c ca $. $v v $. ${ cav $f ca v $. $}", std.testing.allocator);
}

test "$f using two constants" {
    expectError(Error.UnexpectedToken, verify("$c wff $. wwff $f wff wff $.", std.testing.allocator));
}

test "$f using undeclared variable" {
    expectError(Error.UnexpectedToken, verify("$c wff $. wps $f wff ps $.", std.testing.allocator));
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
    mandatoryVariables: TokenSet,
    mhs: SinglyLinkedList(FELabel),
    len: usize,
    nextDVPair: usize = 0,

    /// expression remains owned by the caller
    fn init(state: *VerifyState, allocator: *Allocator, expression: Expression) !MHIterator {
        // initially mandatory variables: those from the given expression
        var mandatoryVariables = TokenSet.init(allocator);
        for (expression) |cvToken| if (cvToken.cv == .V) {
            _ = try mandatoryVariables.add(cvToken.token);
        };

        var mhs = SinglyLinkedList(FELabel){};
        var len: usize = 0;
        // loop over state.activeHypotheses, in reverse order
        var it = state.activeHypotheses.iterator(state.activeHypotheses.count());
        while (it.prev()) |activeHypothesis| {
            switch (activeHypothesis.fe) {
                .F => {
                    const fRule = state.meanings.get(activeHypothesis.label).?.Rule;
                    assert(fRule.conclusion.len == 2);
                    assert(fRule.conclusion[1].cv == .V);
                    const fVariable = fRule.conclusion[1].token;
                    if (mandatoryVariables.contains(fVariable)) {
                        // include every $f for every mandatory variable
                        var node = try allocator.create(@TypeOf(mhs).Node);
                        node.* = @TypeOf(mhs).Node{ .data = .{ .label = activeHypothesis.label, .fe = .F } };
                        mhs.prepend(node);
                        len += 1;
                    }
                },
                .E => {
                    // include every $e
                    var node = try allocator.create(@TypeOf(mhs).Node);
                    node.* = @TypeOf(mhs).Node{ .data = .{ .label = activeHypothesis.label, .fe = .E } };
                    mhs.prepend(node);
                    len += 1;
                    // the variables of the $e hypothesis are also mandatory
                    const eRule = state.meanings.get(activeHypothesis.label).?.Rule;
                    const eExpression = eRule.conclusion;
                    for (eExpression) |cvToken| {
                        if (state.meanings.get(cvToken.token)) |meaning| switch (meaning) {
                            .Variable => _ = try mandatoryVariables.add(cvToken.token),
                            else => {},
                        };
                    }
                    for (eExpression) |cvToken| if (cvToken.cv == .V) {
                        _ = try mandatoryVariables.add(cvToken.token);
                    };
                },
            }
        }

        return MHIterator{ .state = state, .allocator = allocator, .mandatoryVariables = mandatoryVariables, .mhs = mhs, .len = len };
    }

    fn deinit(self: *Self) void {
        // loop over all nodes so that all get freed
        while (self.next()) |_| {}
        self.mandatoryVariables.deinit();
    }

    fn count(self: *Self) usize {
        return self.len;
    }

    fn next(self: *Self) ?FELabel {
        if (self.mhs.popFirst()) |node| {
            defer self.allocator.destroy(node);
            self.len -= 1;
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

    var expression = try expressionOf(&state, "T");
    defer std.testing.allocator.free(expression);
    var it = try state.mandatoryHypothesesOf(expression);
    defer it.deinit();
    assert(it.count() == 0);
    expect(it.next() == null);
}

test "iterate over single $f hypothesis" {
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    try state.addStatementsFrom("$c wff |- $. $v ph ps $. wph $f wff ph $. wps $f wff ps $.");

    var expression = try expressionOf(&state, "|- ph");
    defer std.testing.allocator.free(expression);
    var it = try state.mandatoryHypothesesOf(expression);
    defer it.deinit();
    var item: ?FELabel = null;
    assert(it.count() == 1);

    item = it.next();
    expect(eq(item.?.label, "wph"));
    expect(item.?.fe == .F);

    expect(it.next() == null);
}

test "iterate with $e hypothesis" {
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    try state.addStatementsFrom("$c wff |- $. $v ph ps ta $. wta $f wff ta $. wph $f wff ph $. hyp $e wff ta $.");

    var expression = try expressionOf(&state, "|- ph");
    defer std.testing.allocator.free(expression);
    var it = try state.mandatoryHypothesesOf(expression);
    defer it.deinit();
    var item: ?FELabel = null;
    assert(it.count() == 3);

    item = it.next();
    expect(eq(item.?.label, "wta"));
    expect(item.?.fe == .F);

    item = it.next();
    expect(eq(item.?.label, "wph"));
    expect(item.?.fe == .F);
    assert(it.count() == 1);

    item = it.next();
    expect(eq(item.?.label, "hyp"));
    expect(item.?.fe == .E);

    expect(it.next() == null);
}

test "inference rule with $f and $e mandatory hypotheses" {
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    try state.addStatementsFrom("$c wff |- $. $v ph ps ta $. wta $f wff ta $. wph $f wff ph $. hyp $e wff ta $.");

    try state.addStatementsFrom("alltrue $a |- ph $.");

    const alltrueRule = state.meanings.get("alltrue").?.Rule;
    expect(alltrueRule.hypotheses.len == 3);
    expect(eq(alltrueRule.hypotheses[1].expression[1].token, "ph"));
    expect(eq(alltrueRule.conclusion[0].token, "|-"));
}

test "multiple proofs" {
    try verify(
        \\$c T $.
        \\${
        \\  h $e T $.
        \\  p $p T $= ( ) A $.
        \\  q $p T $= ( ) A $.
        \\$}
    , std.testing.allocator);
}
