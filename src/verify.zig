usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const TokenList = tokenize.TokenList;

const compose = @import("compose.zig");
const Expression = compose.Expression;
const eqExpr = compose.eqExpr;
const Hypothesis = compose.Hypothesis;
const DVPair = compose.DVPair;
const InferenceRule = compose.InferenceRule;
const RuleIterator = compose.RuleIterator;

const prove = @import("prove.zig");
const AsRuleMeaningMap = prove.AsRuleMeaningMap;

pub fn verify(buffer: []const u8, allocator: *Allocator) !void {
    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});
    var iter = try RuleIterator.init(allocator);
    defer iter.deinit();
    try verifyPart(&iter, buffer);
    if (iter.currentScopeDiff) |_| return Error.Incomplete; // unclosed $}
}

/// This only is a separate function to make testing easier
fn verifyPart(iter: *RuleIterator, buffer: []const u8) !void {
    var frameArena = std.heap.ArenaAllocator.init(iter.allocator);
    defer frameArena.deinit();
    const frameAllocator = &frameArena.allocator;

    var nr_proofs: u64 = 0;
    defer std.debug.warn("\nFound {0} $p statements so far.\n", .{nr_proofs});

    var batch = std.event.Batch(anyerror!void, 1000, .auto_async).init();

    try iter.addStatementsFrom(buffer);
    while (try iter.next()) |*item| {
        defer item.deinit();
        if (item.proof) |proof| {
            nr_proofs += 1;
            const rule = item.rule;
            std.event.Loop.startCpuBoundOperation();
            const frame = try frameAllocator.create(@Frame(verifyProofConclusion));
            frame.* = async verifyProofConclusion(iter, item.label, proof, rule.hypotheses, .{
                .expression = rule.conclusion,
                .dvPairs = rule.activeDVPairs,
            });
            batch.add(frame);
        }
    }

    try batch.wait();
}

fn verifyProofConclusion(self: *RuleIterator, label: []const u8, proof: TokenList, hypotheses: []Hypothesis, conclusion: struct {
    expression: Expression,
    dvPairs: []DVPair,
}) anyerror!void {
    // std.debug.warn("\nstarting to verify proof of {0}.\n", .{label});
    // defer std.debug.warn("end of verify proof of {0}.\n", .{label});
    const selfAsRuleMeaningMap = prove.AsRuleMeaningMap(*RuleIterator){ .child = self, .getter = RuleIterator.getRuleMeaningOf };
    var result = try prove.runProof(proof, hypotheses, selfAsRuleMeaningMap, self.allocator);
    defer result.deinit(self.allocator);

    if (!eqExpr(result.expression, conclusion.expression)) return Error.ResultMismatch;

    // if not(every result.dvPairs is in conclusion.dvPairs) return Error.DVRMissing;
    for (result.dvPairs.items) |proofDVPair| {
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

const expect = std.testing.expect;
const expectError = std.testing.expectError;
const eq = tokenize.eq;
const eqs = tokenize.eqs;
const Token = tokenize.Token;

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
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    var n: usize = 0;
    try verifyPart(&iter, "$v a b c d e $.");

    try verifyPart(&iter, "$d $.");
    expect(iter.activeDVPairs.items.len == n + 0);
    n = iter.activeDVPairs.items.len;

    try verifyPart(&iter, "$d a $.");
    expect(iter.activeDVPairs.items.len == n + 0);
    n = iter.activeDVPairs.items.len;

    try verifyPart(&iter, "$d a a $.");
    expect(iter.activeDVPairs.items.len == n + 1);
    n = iter.activeDVPairs.items.len;

    try verifyPart(&iter, "$d a b $.");
    expect(iter.activeDVPairs.items.len == n + 1);
    n = iter.activeDVPairs.items.len;

    try verifyPart(&iter, "$d a b c d e $.");
    expect(iter.activeDVPairs.items.len == n + 10);
    n = iter.activeDVPairs.items.len;
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
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    try iter.meanings.put("wff", compose.MeaningType.Constant);
    try iter.meanings.put("ph", compose.Meaning{ .Variable = .{ .usedInFStatement = false } });
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
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();

    try verifyPart(&iter, "$c class setvar $. ${ $v x $. vx.cv $f setvar x $. cv $a class x $.");
    const cv: InferenceRule = iter.meanings.get("cv").?.Rule;
    const vx_cv: Hypothesis = cv.hypotheses[0];
    const x: Token = vx_cv.expression[1].token;
    expect(eq(x, "x"));

    try verifyPart(&iter, "$}");
    const cv2: InferenceRule = iter.meanings.get("cv").?.Rule;
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
    try verifyPart(&iter, "$c T $.");

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
    try verifyPart(&iter, "$c wff |- $. $v ph ps $. wph $f wff ph $. wps $f wff ps $.");

    var expression = try expressionOf(&iter, "|- ph");
    defer std.testing.allocator.free(expression);
    var it = try iter.mandatoryHypothesesOf(expression);
    defer it.deinit();
    var item: ?compose.FELabel = null;
    assert(it.count() == 1);

    item = it.next();
    expect(eq(item.?.label, "wph"));
    expect(item.?.fe == .F);

    expect(it.next() == null);
}

test "iterate with $e hypothesis" {
    var iter = try RuleIterator.init(std.testing.allocator);
    defer iter.deinit();
    try verifyPart(&iter, "$c wff |- $. $v ph ps ta $. wta $f wff ta $. wph $f wff ph $. hyp $e wff ta $.");

    var expression = try expressionOf(&iter, "|- ph");
    defer std.testing.allocator.free(expression);
    var it = try iter.mandatoryHypothesesOf(expression);
    defer it.deinit();
    var item: ?compose.FELabel = null;
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
    try verifyPart(&iter, "$c wff |- $. $v ph ps ta $. wta $f wff ta $. wph $f wff ph $. hyp $e wff ta $.");

    try verifyPart(&iter, "alltrue $a |- ph $.");

    const alltrueRule = iter.meanings.get("alltrue").?.Rule;
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
