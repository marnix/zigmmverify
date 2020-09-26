usingnamespace @import("globals.zig");

const SegmentedList = std.SegmentedList;

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const TokenMap = tokenize.TokenMap;
const eq = tokenize.eq;

const verify = @import("verify.zig");
const Expression = verify.Expression;
const eqExpr = verify.eqExpr;
const copyExpression = verify.copyExpression;
const DVPair = verify.DVPair;
const Hypothesis = verify.Hypothesis;
const InferenceRule = verify.InferenceRule;

// TODO: move to new utils.zig?
fn assertCoercible(comptime T: type, comptime U: type) void {
    var someT: T = undefined;
    assert(@TypeOf(@as(U, someT)) == U);
}

const Substitution = TokenMap(Expression);

/// A 'rule meaning map' is an instance of a struct with
/// an fn called `get` that takes a Token and returns InferenceRule (or an error).
fn assertIsRuleMeaningMap(map: var) void {
    comptime const typeOfFnGet = @typeInfo(@TypeOf(map.get)).BoundFn;
    assert(typeOfFnGet.args.len == 2);
    assertCoercible(Token, typeOfFnGet.args[1].arg_type.?);
    assertCoercible(typeOfFnGet.return_type.?, anyerror!InferenceRule);
}

/// A wrapper around a value of type `T` and a `getter: fn (T, Token) anyerror!InferenceRule`,
/// which satisfies `assertIsRuleMeaningMap()`.
pub fn AsRuleMeaningMap(comptime T: type) type {
    return struct {
        const Self = @This();

        child: T,
        getter: fn (T, Token) anyerror!InferenceRule,
        fn get(self: Self, token: Token) anyerror!InferenceRule {
            return (self.getter)(self.child, token);
        }
    };
}

const ProofStack = struct {
    const Self = @This();
    allocator: *Allocator,
    expressions: std.SegmentedList(Expression, 64),
    /// we collect these and clean them up at the very end
    ownedExpressions: std.SegmentedList(Expression, 64),
    dvPairs: std.SegmentedList(DVPair, 16),

    fn init(allocator: *Allocator) Self {
        return ProofStack{
            .allocator = allocator,
            .expressions = std.SegmentedList(Expression, 64).init(allocator),
            .ownedExpressions = std.SegmentedList(Expression, 64).init(allocator),
            .dvPairs = std.SegmentedList(DVPair, 16).init(allocator),
        };
    }
    fn deinit(self: *Self) void {
        self.expressions.deinit();
        var it = self.ownedExpressions.iterator(0);
        while (it.next()) |pOwnedExpression| {
            self.allocator.free(pOwnedExpression.*);
        }
        self.ownedExpressions.deinit();
    }

    fn isEmpty(self: *Self) bool {
        return self.expressions.len == 0;
    }
    fn isSingle(self: *Self) bool {
        return self.expressions.len == 1;
    }
    fn top(self: *Self) Expression {
        return self.expressions.at(self.expressions.len - 1).*;
    }

    fn pushExpression(self: *Self, expression: Expression) !void {
        try self.expressions.push(expression);
    }
    fn pushInferenceRule(self: *Self, rule: InferenceRule) !void {
        const nrHyp = rule.hypotheses.len;

        // pop hypotheses
        var hypotheses = try self.allocator.alloc(Expression, nrHyp);
        defer self.allocator.free(hypotheses);
        {
            var j: usize = nrHyp;
            while (j > 0) : (j -= 1) {
                hypotheses[j - 1] = self.expressions.pop() orelse return Error.Incomplete; // TODO: test
            }
        }

        // build substitution based on $f
        var substitution = Substitution.init(self.allocator);
        defer substitution.deinit();
        {
            var i: usize = 0;
            while (i < nrHyp) : (i += 1) {
                const hyp = rule.hypotheses[i];
                if (hyp.isF) {
                    std.debug.assert(hyp.expression.len == 2);
                    if (hypotheses[i].len == 0) return Error.HypothesisMismatch; // TODO: test
                    if (!eq(hyp.expression[0], hypotheses[i][0])) return Error.HypothesisMismatch; // TODO: test
                    _ = try substitution.put(hyp.expression[1], hypotheses[i][1..]);
                }
            }
        }

        // check substitution for $e
        {
            var i: usize = 0;
            while (i < nrHyp) : (i += 1) {
                const hyp = rule.hypotheses[i];
                if (!hyp.isF) {
                    const substitutedHyp = try substitute(hyp.expression, substitution, self.allocator);
                    defer self.allocator.free(substitutedHyp);
                    if (!eqExpr(substitutedHyp, hypotheses[i])) return Error.HypothesisMismatch;
                }
            }
        }

        for (rule.dvPairs) |dvPair| {
            const expr1 = substitution.get(dvPair.var1).?.value;
            const expr2 = substitution.get(dvPair.var2).?.value;
            // TODO: HOW TO DETERMINE THE VARIABLES IN expr1 AND expr2 ?!?
            // for every variable in expr1:
            // :: for every variable in expr2:
            // :: :: add var1,var2 to self.dvPairs
        }

        const expression = try substitute(rule.conclusion, substitution, self.allocator);
        try self.ownedExpressions.push(expression);
        try self.pushExpression(expression);
    }
};

/// caller becomes owner of allocated result
pub fn runProof(proof: TokenList, hypotheses: []Hypothesis, ruleMeaningMap: var, allocator: *Allocator) !Expression {
    assertIsRuleMeaningMap(ruleMeaningMap);

    const Modes = enum { Initial, Uncompressed, CompressedPart1, CompressedPart2 };
    var mode = Modes.Initial;

    var proofStack = ProofStack.init(allocator);
    defer proofStack.deinit();
    var compressedNumber: usize = 0;
    var compressedLabels = TokenList.init(allocator);
    defer compressedLabels.deinit();
    var markedExpressions = std.SegmentedList(Expression, 32).init(allocator);
    defer markedExpressions.deinit();

    var it = @as(TokenList, proof).iterator(0);
    while (it.next()) |t| {
        var reprocessCurrentToken = false;
        while (true) {
            switch (mode) {
                .Initial => {
                    if (eq(t.*, "(")) {
                        mode = .CompressedPart1;
                    } else {
                        mode = .Uncompressed;
                        reprocessCurrentToken = true;
                    }
                },
                .Uncompressed => {
                    try proofStack.pushInferenceRule(try ruleMeaningMap.get(t.*));
                },
                .CompressedPart1 => {
                    if (eq(t.*, ")")) {
                        mode = .CompressedPart2;
                    } else {
                        try compressedLabels.push(t.*);
                    }
                },
                .CompressedPart2 => {
                    for (t.*) |c| {
                        // handle every character of t.*, building numbers
                        if ('U' <= c and c <= 'Y') {
                            compressedNumber = compressedNumber * 5 + (c - 'U' + 1);
                        } else if ('A' <= c and c <= 'T') {
                            compressedNumber = compressedNumber * 20 + (c - 'A' + 1);
                            // we have a complete number now
                            brk: {
                                var i = compressedNumber;
                                std.debug.assert(i > 0);
                                i -= 1;
                                // hypotheses...
                                if (i < hypotheses.len) {
                                    break :brk try proofStack.pushExpression(hypotheses[i].expression);
                                }
                                i -= hypotheses.len;
                                // ...labels between parentheses...
                                if (i < compressedLabels.len) {
                                    break :brk try proofStack.pushInferenceRule(try ruleMeaningMap.get(compressedLabels.at(i).*));
                                }
                                i -= compressedLabels.len;
                                // ...expressions marked with 'Z'
                                if (i < markedExpressions.len) {
                                    break :brk try proofStack.pushExpression(markedExpressions.at(i).*);
                                }
                                return Error.NumberTooLarge; // TODO: test
                            }
                            compressedNumber = 0;
                        } else if (c == 'Z') {
                            // special case: not a number, but a back reference
                            if (compressedNumber != 0) return Error.NumberIncomplete; // 'Z' in the middle of a number, TODO: test
                            if (proofStack.isEmpty()) return Error.NumberZEarly; // 'Z' with empty proof stack, at the very beginning, TODO: test
                            try markedExpressions.push(proofStack.top());
                        }
                    }
                },
            }
            if (!reprocessCurrentToken) break;
        }
    }
    if (mode == .CompressedPart1) return Error.Incomplete; // TODO: test

    if (proofStack.isEmpty()) return Error.Incomplete; // TODO: test
    if (!proofStack.isSingle()) return Error.UnexpectedToken; // TODO: test; better error code?

    return try copyExpression(allocator, proofStack.top());
}

/// caller becomes owner of allocated result
fn substitute(orig: Expression, subst: Substitution, allocator: *Allocator) !Expression {
    var resultAsList = SegmentedList(Token, 128).init(allocator);
    defer resultAsList.deinit();
    for (orig) |token| {
        if (subst.get(token)) |kv| {
            const repl: Expression = kv.value;
            for (repl) |replToken| {
                try resultAsList.push(replToken);
            }
        } else {
            try resultAsList.push(token);
        }
    }

    var result = try allocator.alloc(Token, resultAsList.len);
    var it = resultAsList.iterator(0);
    var i: usize = 0;
    while (it.next()) |pToken| : (i += 1) result[i] = pToken.*;
    return result;
}

// ----------------------------------------------------------------------------

const expect = std.testing.expect;

test "simple substitution" {
    const original = &[_]Token{ "class", "x" };
    var substitution = Substitution.init(std.testing.allocator);
    defer substitution.deinit();
    _ = try substitution.put("x", &[_]Token{"y"});
    const expected = &[_]Token{ "class", "y" };
    const actual = try substitute(original, substitution, std.testing.allocator);
    defer std.testing.allocator.free(actual);
    expect(eqExpr(actual, expected));
}

test "compare equal expressions" {
    const a = &[_]Token{ "class", "x" };
    const b = &[_]Token{ "class", "x" };
    expect(eqExpr(a, b));
}

test "compare unequal expressions" {
    const a = &[_]Token{ "a", "x" };
    const b = &[_]Token{ "b", "x" };
    expect(!eqExpr(a, b));
}
