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
            frame.* = async verifyProofConclusion(&iter, item.label, proof, rule.hypotheses, .{
                .expression = rule.conclusion,
                .dvPairs = rule.activeDVPairs,
            });
            batch.add(frame);
        }
    }

    try batch.wait();

    if (iter.currentScopeDiff) |_| return Error.Incomplete; // unclosed $}
}

fn verifyProofConclusion(iter: *RuleIterator, label: []const u8, proof: TokenList, hypotheses: []Hypothesis, conclusion: struct {
    expression: Expression,
    dvPairs: []DVPair,
}) anyerror!void {
    // std.debug.warn("\nstarting to verify proof of {0}.\n", .{label});
    // defer std.debug.warn("end of verify proof of {0}.\n", .{label});
    var result = try prove.runProof(proof, hypotheses, iter, iter.allocator);
    defer result.deinit(iter.allocator);

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
