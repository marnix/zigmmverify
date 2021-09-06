const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const ArrayList = std.ArrayList;

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const TokenMap = tokenize.TokenMap;
const eq = tokenize.eq;

const compose = @import("compose.zig");
const Expression = compose.Expression;
const eqExpr = compose.eqExpr;
const copyExpression = compose.copyExpression;
const DVPair = compose.DVPair;
const Hypothesis = compose.Hypothesis;
const InferenceRule = compose.InferenceRule;
const RuleIterator = compose.RuleIterator;

// TODO: move to new utils.zig?
fn assertCoercible(comptime T: type, comptime U: type) void {
    var someT: T = undefined;
    assert(@TypeOf(@as(U, someT)) == U);
}

const Substitution = TokenMap(Expression);

const ProofStack = struct {
    const Self = @This();
    allocator: *Allocator,
    expressions: std.ArrayList(Expression),
    /// we collect these and clean them up at the very end
    arena: std.heap.ArenaAllocator,
    dvPairs: std.ArrayList(DVPair),

    fn init(allocator: *Allocator) Self {
        return ProofStack{
            .allocator = allocator,
            .expressions = std.ArrayList(Expression).init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
            .dvPairs = std.ArrayList(DVPair).init(allocator),
        };
    }
    fn deinit(self: *Self) void {
        self.expressions.deinit();
        self.arena.deinit();
        // no self.dvPairs.deinit(), because client keeps ownership
    }

    fn isEmpty(self: *Self) bool {
        return self.expressions.items.len == 0;
    }
    fn isSingle(self: *Self) bool {
        return self.expressions.items.len == 1;
    }
    fn top(self: *Self) Expression {
        return self.expressions.items[self.expressions.items.len - 1];
    }

    fn pushExpression(self: *Self, expression: Expression) !void {
        try self.expressions.append(expression);
    }
    fn pushInferenceRule(self: *Self, rule: InferenceRule) !void {
        const nrHyp = rule.hypotheses.len;

        // pop hypotheses
        var hypotheses = try self.allocator.alloc(Expression, nrHyp);
        defer self.allocator.free(hypotheses);
        {
            var j: usize = nrHyp;
            while (j > 0) : (j -= 1) {
                hypotheses[j - 1] = self.expressions.popOrNull() orelse return Error.Incomplete; // TODO: test
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

        // add distinct variable restrictions imposed by this inference rule
        for (rule.activeDVPairs) |dvPair| {
            // get the 'distinct expressions', skipping any optional DVRs
            const expr1 = if (substitution.get(dvPair.var1)) |e| e else continue;
            const expr2 = if (substitution.get(dvPair.var2)) |e| e else continue;
            // create DVRs for every pair of variables in the two expressions
            for (expr1) |cvToken1| if (cvToken1.cv == .V) {
                for (expr2) |cvToken2| if (cvToken2.cv == .V) {
                    // note: don't try to check for duplicates, that is probably not worth it
                    try self.dvPairs.append(.{ .var1 = cvToken1.token, .var2 = cvToken2.token });
                };
            };
        }

        const expression = try substitute(rule.conclusion, substitution, &self.arena.allocator);
        try self.pushExpression(expression);
    }
};

pub const RunProofResult = struct {
    expression: Expression,
    dvPairs: std.ArrayList(DVPair),

    pub fn deinit(self: *@This(), allocator: *Allocator) void {
        allocator.free(self.expression);
        self.dvPairs.deinit();
    }
};

/// caller becomes owner of allocated result
pub fn runProof(proof: TokenList, hypotheses: []Hypothesis, ruleIterator: *RuleIterator, allocator: *Allocator) !RunProofResult {
    const Modes = enum { Initial, Uncompressed, CompressedPart1, CompressedPart2 };
    var mode = Modes.Initial;

    var proofStack = ProofStack.init(allocator);
    defer proofStack.deinit();
    var compressedNumber: usize = 0;
    var compressedLabels = TokenList.init(allocator);
    defer compressedLabels.deinit();
    var markedExpressions = std.ArrayList(Expression).init(allocator);
    defer markedExpressions.deinit();

    for (proof.items) |t| {
        var reprocessCurrentToken = false;
        while (true) {
            switch (mode) {
                .Initial => {
                    if (eq(t, "(")) {
                        mode = .CompressedPart1;
                    } else {
                        mode = .Uncompressed;
                        reprocessCurrentToken = true;
                    }
                },
                .Uncompressed => {
                    try proofStack.pushInferenceRule(try ruleIterator.getRuleMeaningOf(t));
                },
                .CompressedPart1 => {
                    if (eq(t, ")")) {
                        mode = .CompressedPart2;
                    } else {
                        try compressedLabels.append(t);
                    }
                },
                .CompressedPart2 => {
                    for (t) |c| {
                        // handle every character of t, building numbers
                        if ('U' <= c and c <= 'Y') {
                            compressedNumber = compressedNumber * 5 + (c - 'U' + 1);
                            // number is still incomplete
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
                                if (i < compressedLabels.items.len) {
                                    break :brk try proofStack.pushInferenceRule(try ruleIterator.getRuleMeaningOf(compressedLabels.items[i]));
                                }
                                i -= compressedLabels.items.len;
                                // ...expressions marked with 'Z'...
                                if (i < markedExpressions.items.len) {
                                    break :brk try proofStack.pushExpression(markedExpressions.items[i]);
                                }
                                // ...or larger than expected.
                                return Error.NumberTooLarge; // TODO: test
                            }
                            compressedNumber = 0;
                        } else if (c == 'Z') {
                            // special case: not a number, but a back reference
                            if (compressedNumber != 0) return Error.NumberIncomplete; // 'Z' in the middle of a number, TODO: test
                            if (proofStack.isEmpty()) return Error.NumberZEarly; // 'Z' with empty proof stack, at the very beginning, TODO: test
                            try markedExpressions.append(proofStack.top());
                        }
                    }
                },
            }
            if (!reprocessCurrentToken) break;
            reprocessCurrentToken = false;
        }
    }
    if (mode == .CompressedPart1) return Error.Incomplete; // TODO: test

    if (proofStack.isEmpty()) return Error.Incomplete; // TODO: test
    if (!proofStack.isSingle()) return Error.UnexpectedToken; // TODO: test; better error code?

    return RunProofResult{
        .expression = try copyExpression(allocator, proofStack.top()),
        .dvPairs = proofStack.dvPairs,
    };
}

/// caller becomes owner of allocated result
fn substitute(orig: Expression, subst: Substitution, allocator: *Allocator) !Expression {
    var resultAsList = ArrayList(compose.CVToken).init(allocator);
    defer resultAsList.deinit();
    for (orig) |cvToken| {
        if (subst.get(cvToken.token)) |repl| {
            if (cvToken.cv != .V) return Error.UnexpectedToken; // TODO: test
            for (repl) |replCVToken| {
                try resultAsList.append(replCVToken);
            }
        } else {
            try resultAsList.append(cvToken);
        }
    }

    var result = try allocator.alloc(compose.CVToken, resultAsList.items.len);
    // TODO: copy in a simpler way?
    for (resultAsList.items) |cvToken, i| result[i] = cvToken;
    return result;
}

// ----------------------------------------------------------------------------

const expect = std.testing.expect;

test "simple substitution" {
    const original = &[_]compose.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .V } };
    var substitution = Substitution.init(std.testing.allocator);
    defer substitution.deinit();
    _ = try substitution.put("x", &[_]compose.CVToken{.{ .token = "y", .cv = .V }});
    const expected = &[_]compose.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "y", .cv = .V } };
    const actual = try substitute(original, substitution, std.testing.allocator);
    defer std.testing.allocator.free(actual);
    try expect(eqExpr(actual, expected));
}

test "compare equal expressions" {
    const a = &[_]compose.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .V } };
    const b = &[_]compose.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .V } };
    try expect(eqExpr(a, b));
}

test "compare unequal expressions" {
    const a = &[_]compose.CVToken{ .{ .token = "a", .cv = .C }, .{ .token = "x", .cv = .V } };
    const b = &[_]compose.CVToken{ .{ .token = "b", .cv = .C }, .{ .token = "x", .cv = .V } };
    try expect(!eqExpr(a, b));
}

test "compare unequal expressions, constant vs variable" {
    const a = &[_]compose.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .V } };
    const b = &[_]compose.CVToken{ .{ .token = "class", .cv = .C }, .{ .token = "x", .cv = .C } };
    try expect(!eqExpr(a, b));
}
