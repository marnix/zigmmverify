usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const TokenSet = tokenize.TokenSet;
const TokenMap = tokenize.TokenMap;
const eq = tokenize.eq;

const parse = @import("parse.zig");
const Statement = parse.Statement;
const StatementIterator = parse.StatementIterator;

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
const FELabelList = std.ArrayList(FELabel);
pub const DVPair = struct { var1: Token, var2: Token };
const DVPairList = std.ArrayList(DVPair);

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

/// TODO: Find a better name; {Token,Label,Symbol}Interpretation?
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

const RuleIteratorItem = struct {
    const Self = @This();

    label: Token,
    rule: InferenceRule,
    proof: ?TokenList = null,

    pub fn deinit(self: *Self) void {
        if (self.proof) |*p| p.deinit();
    }
};

pub const RuleIterator = struct {
    const Self = @This();

    allocator: *Allocator,

    statements: ?parse.StatementIterator,

    /// what each active token means
    meanings: TokenMap(Meaning),
    /// the active hypotheses, and whether they are $f or $e
    activeHypotheses: FELabelList,
    /// the active distinct variable pairs
    activeDVPairs: DVPairList,
    /// the difference between the current nested scope
    /// and its immediately surrounding scope
    currentScopeDiff: ?*ScopeDiff,

    pub fn init(allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .statements = null,
            .meanings = TokenMap(Meaning).init(allocator),
            .activeHypotheses = FELabelList.init(allocator),
            .activeDVPairs = DVPairList.init(allocator),
            .currentScopeDiff = null,
        };
    }

    pub fn deinit(self: *Self) void {
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

    pub fn addStatementsFrom(self: *Self, buffer: []const u8) !void {
        self.statements = StatementIterator.init(self.allocator, buffer);
    }

    pub fn next(self: *Self) !?RuleIteratorItem {
        if (self.statements == null) return null;
        var nextItem: ?RuleIteratorItem = null;
        while (nextItem == null) {
            if (try self.statements.?.next()) |statement| {
                nextItem = try self.add(statement);
            } else {
                self.statements = null;
                break;
            }
        }
        return nextItem;
    }

    /// Consumes statement.
    fn add(self: *Self, statement: *Statement) !?RuleIteratorItem {
        var deinit_statement = true;
        defer if (deinit_statement) statement.deinit(self.allocator);

        var nextItem: ?RuleIteratorItem = null;
        switch (statement.*) {
            .C => |cStatement| {
                if (self.currentScopeDiff) |_| return Error.UnexpectedToken; // $c inside ${ $}
                for (cStatement.constants.items) |constant| {
                    if (self.meanings.get(constant)) |_| return Error.Duplicate;
                    const kv = try self.meanings.put(constant, MeaningType.Constant);
                }
            },
            .V => |vStatement| {
                for (vStatement.variables.items) |variable| {
                    if (self.meanings.get(variable)) |_| return Error.Duplicate;
                    const kv = try self.meanings.put(variable, Meaning{ .Variable = .{ .usedInFStatement = false } });
                    if (self.currentScopeDiff) |scopeDiff| {
                        _ = try scopeDiff.activeTokens.add(variable); // this $v will become inactive at the next $}
                    }
                }
            },
            .F => |fStatement| {
                if (self.meanings.get(fStatement.label)) |_| return Error.Duplicate;
                try self.meanings.put(fStatement.label, Meaning{ .Rule = try self.fromHypothesis(fStatement.tokens) });
                const variable = fStatement.tokens.items[1];
                if (self.meanings.get(variable)) |meaning| {
                    if (meaning != .Variable) return Error.UnexpectedToken; // $f k l $. where l is something else than variable,
                    if (meaning.Variable.usedInFStatement) return Error.Duplicate;
                } else unreachable; // $f k x $. without $v x $. is already detected in fromHypothesis() call above
                try self.meanings.put(variable, .{ .Variable = .{ .usedInFStatement = true } });
                _ = try self.activeHypotheses.append(.{ .label = fStatement.label, .fe = .F });
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
                _ = try self.activeHypotheses.append(.{ .label = eStatement.label, .fe = .E });
                if (self.currentScopeDiff) |scopeDiff| {
                    scopeDiff.nrActiveHypotheses += 1;
                    _ = try scopeDiff.activeTokens.add(eStatement.label); // this $e will become inactive at the next $}
                }
            },
            .A => |aStatement| {
                if (self.meanings.get(aStatement.label)) |_| return Error.Duplicate;
                const rule = try self.inferenceRuleOf(aStatement.tokens);
                try self.meanings.put(aStatement.label, Meaning{ .Rule = rule });
                nextItem = .{ .label = aStatement.label, .rule = rule };
            },
            .P => |pStatement| {
                if (self.meanings.get(pStatement.label)) |_| return Error.Duplicate;
                const rule = try self.inferenceRuleOf(pStatement.tokens);
                _ = try self.meanings.put(pStatement.label, Meaning{ .Rule = rule });
                const label = pStatement.label;
                const proof = statement.deinitLeavingProof(self.allocator);
                deinit_statement = false;
                nextItem = .{ .label = label, .rule = rule, .proof = proof };
            },
            .D => |dStatement| {
                for (dStatement.variables.items) |var1, i| {
                    if (self.meanings.get(var1)) |meaning| {
                        if (meaning != .Variable) return Error.UnexpectedToken; // TODO: test
                    } else return Error.UnexpectedToken; //TODO: test
                    // ...
                    for (dStatement.variables.items[i + 1 ..]) |var2| {
                        // TODO: error if var1 == var2
                        _ = try self.activeDVPairs.append(.{ .var1 = var1, .var2 = var2 });
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
        return nextItem;
    }

    /// caller does not get ownership
    /// (should not need to be public! TODO: fix)
    pub fn getRuleMeaningOf(self: *Self, token: Token) !InferenceRule {
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

        // TODO: copy in a simpler way?
        var dvPairs = try self.allocator.alloc(DVPair, self.activeDVPairs.items.len);
        for (self.activeDVPairs.items) |dvPair, j| {
            dvPairs[j] = dvPair;
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
        var result = try self.allocator.alloc(CVToken, tokens.items.len);
        errdefer self.allocator.free(result);
        for (tokens.items) |token, i| {
            const cv = self.meanings.get(token) orelse return Error.UnexpectedToken;
            result[i] = .{
                .token = token,
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

    iter: *RuleIterator,
    optOuter: ?*ScopeDiff,
    activeTokens: TokenSet,
    variablesInFStatements: TokenSet,
    nrActiveHypotheses: usize,
    nrActiveDVPairs: usize,

    fn push(iter: *RuleIterator) !void {
        const newScopeDiff = try iter.allocator.create(ScopeDiff);
        errdefer iter.allocator.destroy(newScopeDiff);
        newScopeDiff.* = Self{
            .iter = iter,
            .optOuter = iter.currentScopeDiff,
            .activeTokens = TokenSet.init(iter.allocator),
            .variablesInFStatements = TokenSet.init(iter.allocator),
            .nrActiveHypotheses = 0,
            .nrActiveDVPairs = 0,
        };

        iter.currentScopeDiff = newScopeDiff;
    }

    fn pop(self: *Self) void {
        self.iter.currentScopeDiff = self.optOuter;
        self.deinitNrActiveDVPairs();
        self.deinitNrActiveHypotheses();
        self.deinitVariableInFStatements();
        self.deinitActiveTokens();
        self.iter.allocator.destroy(self);
    }

    fn deinitNrActiveHypotheses(self: *Self) void {
        while (self.nrActiveHypotheses > 0) : (self.nrActiveHypotheses -= 1) {
            _ = self.iter.activeHypotheses.pop();
        }
    }

    fn deinitNrActiveDVPairs(self: *Self) void {
        while (self.nrActiveDVPairs > 0) : (self.nrActiveDVPairs -= 1) {
            _ = self.iter.activeDVPairs.pop();
        }
    }

    fn deinitVariableInFStatements(self: *Self) void {
        var it = self.variablesInFStatements.iterator();
        while (it.next()) |kv| {
            const variable = kv.key;
            if (self.iter.meanings.get(variable)) |meaning| {
                assert(meaning == .Variable);
                assert(meaning.Variable.usedInFStatement == true);
            } else unreachable;
            _ = self.iter.meanings.put(variable, .{ .Variable = .{ .usedInFStatement = false } }) catch unreachable; // in-place update can't fail?
        }
        self.variablesInFStatements.deinit();
    }

    fn deinitActiveTokens(self: *Self) void {
        var it = self.activeTokens.iterator();
        while (it.next()) |kv| {
            if (self.iter.meanings.remove(kv.key)) |*kv2| {
                kv2.value.deinit(self.iter.allocator);
            } else unreachable;
        }
        self.activeTokens.deinit();
    }
};

// ----------------------------------------
// ITERATOR OVER MANDATORY HYPOTHESES

const MHIterator = struct {
    const Self = @This();

    allocator: *Allocator,
    mandatoryVariables: TokenSet,
    mhs: SinglyLinkedList(FELabel),
    len: usize,

    /// expression remains owned by the caller
    fn init(iter: *RuleIterator, allocator: *Allocator, expression: Expression) !MHIterator {
        // initially mandatory variables: those from the given expression
        var mandatoryVariables = TokenSet.init(allocator);
        for (expression) |cvToken| if (cvToken.cv == .V) {
            _ = try mandatoryVariables.add(cvToken.token);
        };

        var mhs = SinglyLinkedList(FELabel){};
        var len: usize = 0;
        // loop over iter.activeHypotheses, in reverse order
        var i: usize = iter.activeHypotheses.items.len;
        while (i > 0) {
            i -= 1;
            const activeHypothesis = iter.activeHypotheses.items[i];

            switch (activeHypothesis.fe) {
                .F => {
                    const fRule = iter.meanings.get(activeHypothesis.label).?.Rule;
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
                    const eRule = iter.meanings.get(activeHypothesis.label).?.Rule;
                    const eExpression = eRule.conclusion;
                    for (eExpression) |cvToken| {
                        if (iter.meanings.get(cvToken.token)) |meaning| switch (meaning) {
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

        return MHIterator{ .allocator = allocator, .mandatoryVariables = mandatoryVariables, .mhs = mhs, .len = len };
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

const expect = std.testing.expect;
const expectError = std.testing.expectError;
const eqs = tokenize.eqs;

fn runRuleIterator(buffer: []const u8, allocator: *Allocator) !void {
    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});
    var iter = try RuleIterator.init(allocator);
    defer iter.deinit();
    try runRuleIteratorPart(&iter, buffer);
    if (iter.currentScopeDiff) |_| return Error.Incomplete; // unclosed $}
}

fn runRuleIteratorPart(iter: *RuleIterator, buffer: []const u8) !void {
    try iter.addStatementsFrom(buffer);
    while (try iter.next()) |*item| {}
}

test "active DVR (found a memory leak)" {
    try runRuleIterator(
        \\$c wff $.
        \\$v P Q $.
        \\wp $f wff P $.
        \\wq $f wff Q $.
        \\$d P Q $. pq.1 $e wff P $. pq $a wff Q $.
    , std.testing.allocator);
}

test "count number of active $d pairs" {
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    var n: usize = 0;
    try runRuleIteratorPart(&iter, "$v a b c d e $.");

    try runRuleIteratorPart(&iter, "$d $.");
    expect(iter.activeDVPairs.items.len == n + 0);
    n = iter.activeDVPairs.items.len;

    try runRuleIteratorPart(&iter, "$d a $.");
    expect(iter.activeDVPairs.items.len == n + 0);
    n = iter.activeDVPairs.items.len;

    try runRuleIteratorPart(&iter, "$d a a $.");
    expect(iter.activeDVPairs.items.len == n + 1);
    n = iter.activeDVPairs.items.len;

    try runRuleIteratorPart(&iter, "$d a b $.");
    expect(iter.activeDVPairs.items.len == n + 1);
    n = iter.activeDVPairs.items.len;

    try runRuleIteratorPart(&iter, "$d a b c d e $.");
    expect(iter.activeDVPairs.items.len == n + 10);
    n = iter.activeDVPairs.items.len;
}

test "$d with constant" {
    expectError(Error.UnexpectedToken, runRuleIterator("$c class $. $d class $.", std.testing.allocator));
}

test "$d with undeclared token" {
    expectError(Error.UnexpectedToken, runRuleIterator("$d class $.", std.testing.allocator));
}

test "use undeclared variable" {
    expectError(Error.UnexpectedToken, runRuleIterator("$c wff $. wph $f wff ph $.", std.testing.allocator));
}

test "use undeclared variable" {
    expectError(Error.UnexpectedToken, runRuleIterator("$c wff $. wph $f wff ph $.", std.testing.allocator));
}

test "use statement label as a token" {
    expectError(Error.UnexpectedToken, runRuleIterator("$c wff ph $. wph $f wff ph $. wxx $e wph $.", std.testing.allocator));
}

test "simplest correct $f" {
    try runRuleIterator("$c wff $. $v ph $. wph $f wff ph $.", std.testing.allocator);
}

test "tokenlist to expression" {
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    try iter.meanings.put("wff", MeaningType.Constant);
    try iter.meanings.put("ph", Meaning{ .Variable = .{ .usedInFStatement = false } });
    var tokens = TokenList.init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.append("wff");
    try tokens.append("ph");
    expect(eqs(tokens, &[_]Token{ "wff", "ph" }));

    const expression = try iter.expressionOf(tokens);
    defer std.testing.allocator.free(expression);

    expect(expression.len == 2);
    expect(eq(expression[0].token, "wff"));
    expect(expression[0].cv == .C);
    expect(eq(expression[1].token, "ph"));
    expect(expression[1].cv == .V);
}

test "no duplicate variable declarations" {
    expectError(Error.Duplicate, runRuleIterator("$c ca cb $. $v v $. cav $f ca v $. cbv $f cb v $.", std.testing.allocator));
}

test "no duplicate variable declarations, in nested scope (2)" {
    expectError(Error.Duplicate, runRuleIterator("$c ca cb $. ${ $v v $. cav $f ca v $. cbv $f cb v $. $}", std.testing.allocator));
}

test "duplicate variable declarations, in nested scope (2)" {
    try runRuleIterator("$c ca cb $. $v v $. ${ cav $f ca v $. $} cbv $f cb v $.", std.testing.allocator);
}

test "$v in nested scope" {
    try runRuleIterator("$c ca $. ${ $v v $. $}", std.testing.allocator);
}

test "$v in nested scope, used in $a (use-after-free reproduction)" {
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();

    try runRuleIteratorPart(&iter, "$c class setvar $. ${ $v x $. vx.cv $f setvar x $. cv $a class x $.");
    const cv: InferenceRule = iter.meanings.get("cv").?.Rule;
    const vx_cv: Hypothesis = cv.hypotheses[0];
    const x: Token = vx_cv.expression[1].token;
    expect(eq(x, "x"));

    try runRuleIteratorPart(&iter, "$}");
    const cv2: InferenceRule = iter.meanings.get("cv").?.Rule;
    const vx_cv2: Hypothesis = cv.hypotheses[0];
    const x2: Token = vx_cv.expression[1].token;
    expect(eq(x2, "x"));
}

test "$f in nested scope (1)" {
    try runRuleIterator("$c ca $. ${ $v v $. cav $f ca v $. $}", std.testing.allocator);
}

test "$f in nested scope (2)" {
    try runRuleIterator("$c ca $. $v v $. ${ cav $f ca v $. $}", std.testing.allocator);
}

test "$f using two constants" {
    expectError(Error.UnexpectedToken, runRuleIterator("$c wff $. wwff $f wff wff $.", std.testing.allocator));
}

test "$f using undeclared variable" {
    expectError(Error.UnexpectedToken, runRuleIterator("$c wff $. wps $f wff ps $.", std.testing.allocator));
}

test "token is either constant or variable, not both" {
    expectError(Error.Duplicate, runRuleIterator("$c wff $. $v wff $.", std.testing.allocator));
}

test "no constant allowed in nested scope" {
    expectError(Error.UnexpectedToken, runRuleIterator("${ $c wff $. $}", std.testing.allocator));
}

test "nested variable" {
    try runRuleIterator("$v ph $. ${ $v ps $. $} $v ps $.", std.testing.allocator);
}

test "nested duplicate variable" {
    expectError(Error.Duplicate, runRuleIterator("$v ph $. ${ $v ph $. $}", std.testing.allocator));
}

test "unopened block" {
    expectError(Error.UnexpectedToken, runRuleIterator("$}", std.testing.allocator));
}

test "unclosed block" {
    expectError(Error.Incomplete, runRuleIterator("${", std.testing.allocator));
}

test "multiple blocks" {
    try runRuleIterator("${ $} ${ ${ $} $}", std.testing.allocator);
}

test "duplicate variable" {
    expectError(Error.Duplicate, runRuleIterator("$v ph ps ph $.", std.testing.allocator));
}

test "single variable" {
    try runRuleIterator("$v ph $.", std.testing.allocator);
}

test "duplicate constant" {
    expectError(Error.Duplicate, runRuleIterator("$c wff wff $.", std.testing.allocator));
}

fn tokenListOf(buffer: []const u8) !TokenList {
    var it = @import("parse.zig").StatementIterator.init(std.testing.allocator, buffer);
    var result = TokenList.init(std.testing.allocator);
    while (true) {
        if (try it.nextToken()) |token| {
            _ = try result.append(token);
        } else break;
    }
    return result;
}

fn expressionOf(iter: *RuleIterator, buffer: []const u8) !Expression {
    var t = try tokenListOf(buffer);
    defer t.deinit();
    return try iter.expressionOf(t);
}

test "iterate over no mandatory hypotheses" {
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    try runRuleIteratorPart(&iter, "$c T $.");

    var expression = try expressionOf(&iter, "T");
    defer std.testing.allocator.free(expression);
    var it = try iter.mandatoryHypothesesOf(expression);
    defer it.deinit();
    assert(it.count() == 0);
    expect(it.next() == null);
}

test "iterate over single $f hypothesis" {
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    try runRuleIteratorPart(&iter, "$c wff |- $. $v ph ps $. wph $f wff ph $. wps $f wff ps $.");

    var expression = try expressionOf(&iter, "|- ph");
    defer std.testing.allocator.free(expression);
    var it = try iter.mandatoryHypothesesOf(expression);
    defer it.deinit();
    var item: ?FELabel = null;
    assert(it.count() == 1);

    item = it.next();
    expect(eq(item.?.label, "wph"));
    expect(item.?.fe == .F);

    expect(it.next() == null);
}

test "iterate with $e hypothesis" {
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    try runRuleIteratorPart(&iter, "$c wff |- $. $v ph ps ta $. wta $f wff ta $. wph $f wff ph $. hyp $e wff ta $.");

    var expression = try expressionOf(&iter, "|- ph");
    defer std.testing.allocator.free(expression);
    var it = try iter.mandatoryHypothesesOf(expression);
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
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    try runRuleIteratorPart(&iter, "$c wff |- $. $v ph ps ta $. wta $f wff ta $. wph $f wff ph $. hyp $e wff ta $.");

    try runRuleIteratorPart(&iter, "alltrue $a |- ph $.");

    const alltrueRule = iter.meanings.get("alltrue").?.Rule;
    expect(alltrueRule.hypotheses.len == 3);
    expect(eq(alltrueRule.hypotheses[1].expression[1].token, "ph"));
    expect(eq(alltrueRule.conclusion[0].token, "|-"));
}
