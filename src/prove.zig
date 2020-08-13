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
const Hypothesis = verify.Hypothesis;
const InferenceRule = verify.InferenceRule;
const VerifyState = verify.VerifyState;

const Substitution = TokenMap(Expression);

pub fn AsRuleMeaningMap(comptime T: type) type {
    return struct {
        const Self = @This();
        child: T,
        getter: fn (T, Token) Error!InferenceRule,
        /// This is just an abbreviation, to make the caller better readable.
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

    fn init(allocator: *Allocator) Self {
        return ProofStack{
            .allocator = allocator,
            .expressions = std.SegmentedList(Expression, 64).init(allocator),
            .ownedExpressions = std.SegmentedList(Expression, 64).init(allocator),
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

    fn pushExpression(self: *Self, expression: Expression, ownedByProofStack: bool) !void {
        try self.expressions.push(expression);
        if (ownedByProofStack) {
            try self.ownedExpressions.push(expression);
        }
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
                    if (!eq(hyp.expression[0].token, hypotheses[i][0].token)) return Error.HypothesisMismatch; // TODO: test
                    _ = try substitution.put(hyp.expression[1].token, hypotheses[i][1..]);
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

        try self.pushExpression(try substitute(rule.conclusion, substitution, self.allocator), true);
    }
};

/// caller becomes owner of allocated result
pub fn runProof(proof: TokenList, hypotheses: []Hypothesis, ruleMeaningMap: var, allocator: *Allocator) !Expression {
    // TODO: assert, in some way, that @TypeOf(ruleMeaningMap) is a type returned by AsRuleMeaningMap()
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
                                    break :brk try proofStack.pushExpression(hypotheses[i].expression, false);
                                }
                                i -= hypotheses.len;
                                // ...labels between parentheses...
                                if (i < compressedLabels.len) {
                                    break :brk try proofStack.pushInferenceRule(try ruleMeaningMap.get(compressedLabels.at(i).*));
                                }
                                i -= compressedLabels.len;
                                // ...expressions marked with 'Z'
                                if (i < markedExpressions.len) {
                                    break :brk try proofStack.pushExpression(markedExpressions.at(i).*, false);
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
    var resultAsList = SegmentedList(verify.CVToken, 128).init(allocator);
    defer resultAsList.deinit();
    for (orig) |cvToken| {
        if (subst.get(cvToken.token)) |kv| {
            if (cvToken.cv != .V) return Error.UnexpectedToken; // TODO: test
            const repl: Expression = kv.value;
            for (repl) |replCVToken| {
                try resultAsList.push(replCVToken);
            }
        } else {
            try resultAsList.push(cvToken);
        }
    }

    var result = try allocator.alloc(verify.CVToken, resultAsList.len);
    var it = resultAsList.iterator(0);
    var i: usize = 0;
    while (it.next()) |pCVToken| : (i += 1) result[i] = pCVToken.*;
    return result;
}

// ----------------------------------------------------------------------------

const expect = std.testing.expect;

test "simple substitution" {
    const original = &[_]verify.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .V } };
    var substitution = Substitution.init(std.testing.allocator);
    defer substitution.deinit();
    _ = try substitution.put("x", &[_]verify.CVToken{.{ .token = "y", .cv = .V }});
    const expected = &[_]verify.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "y", .cv = .V } };
    const actual = try substitute(original, substitution, std.testing.allocator);
    defer std.testing.allocator.free(actual);
    expect(eqExpr(actual, expected));
}

test "compare equal expressions" {
    const a = &[_]verify.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .V } };
    const b = &[_]verify.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .V } };
    expect(eqExpr(a, b));
}

test "compare unequal expressions" {
    const a = &[_]verify.CVToken{ .{ .token = "a", .cv = .C }, .{ .token = "x", .cv = .V } };
    const b = &[_]verify.CVToken{ .{ .token = "b", .cv = .C }, .{ .token = "x", .cv = .V } };
    expect(!eqExpr(a, b));
}

test "compare unequal expressions, constant vs variable" {
    const a = &[_]verify.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .V } };
    const b = &[_]verify.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .C } };
    expect(!eqExpr(a, b));
}
