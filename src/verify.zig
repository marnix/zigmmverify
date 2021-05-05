usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const read = @import("read.zig");
const readBuffer = read.readBuffer;

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

pub fn verifyFile(allocator: *Allocator, dir: std.fs.Dir, mm_file_name: []const u8) !void {
    const buffer = try readBuffer(allocator, dir, mm_file_name);
    defer allocator.free(buffer);

    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});
    var iter = try RuleIterator.init(allocator);
    defer iter.deinit();

    var frameArena = std.heap.ArenaAllocator.init(iter.allocator);
    defer frameArena.deinit();
    const frameAllocator = &frameArena.allocator;

    var nr_proofs: u64 = 0;
    defer std.debug.warn("\nFound {0} $p statements so far.\n", .{nr_proofs});

    try iter.addStatementsFrom(dir, buffer);
    while (try iter.next()) |*item| {
        defer item.deinit();
        if (item.proof) |proof| {
            nr_proofs += 1;
            const rule = item.rule;
            std.event.Loop.startCpuBoundOperation();
            const frame = try frameAllocator.create(@Frame(verifyProofConclusion));
            try verifyProofConclusion(&iter, item.label, proof, rule.hypotheses, .{
                .expression = rule.conclusion,
                .dvPairs = rule.activeDVPairs,
            });
        }
    }

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
const alltests = @import("alltests.zig");
const TestFS = alltests.TestFS;

fn _verifyBuffer(buffer: []const u8) !void {
    const test_file_name = "test_file.mm";

    var testFS = TestFS.init();
    defer testFS.deinit();
    try testFS.writeFile(test_file_name, buffer);

    try verifyFile(std.testing.allocator, testFS.tmpDir.dir, test_file_name);
}

test "proof with $d violation" {
    expectError(Error.DVRMissing, _verifyBuffer(
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
    ));
}

test "proof with correct $d" {
    // FOR DEBUGGING: const allocator = &std.heap.loggingAllocator(std.testing.allocator, std.io.getStdErr().outStream()).allocator;
    try _verifyBuffer(
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
    );
}

test "multiple proofs" {
    try _verifyBuffer(
        \\$c T $.
        \\${
        \\  h $e T $.
        \\  p $p T $= ( ) A $.
        \\  q $p T $= ( ) A $.
        \\$}
    );
}
