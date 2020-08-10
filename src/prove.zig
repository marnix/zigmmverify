usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const eq = tokenize.eq;

const verify = @import("verify.zig");
const Expression = verify.Expression;
const Hypothesis = verify.Hypothesis;
const InferenceRule = verify.InferenceRule;
const VerifyState = verify.VerifyState;

pub fn AsRuleMeaningMap(comptime T: type) type {
    return struct {
        const Self = @This();
        child: T,
        getter: fn (T, Token) anyerror!InferenceRule,
        /// This is just an abbreviation, to make the caller better readable.
        fn get(self: Self, token: Token) anyerror!InferenceRule {
            return (self.getter)(self.child, token);
        }
    };
}

pub fn runProof(proof: TokenList, hypotheses: []Hypothesis, ruleMeaningMap: var) !Expression {
    // TODO: assert, in some way, that @TypeOf(ruleMeaningMap) is a type returned by AsRuleMeaningMap()
    const Modes = enum { Initial, Uncompressed, CompressedPart1, CompressedPart2 };
    var mode = Modes.Initial;

    // TODO: Keep proof stack, singly linked list? segmented list?

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
                    const rule: InferenceRule = try ruleMeaningMap.get(t.*);
                    // TODO: Push on proof stack
                },
                .CompressedPart1 => {
                    if (eq(t.*, ")")) {
                        mode = .CompressedPart2;
                    } else {
                        const rule: InferenceRule = try ruleMeaningMap.get(t.*);
                        // TODO: add to list
                    }
                },
                .CompressedPart2 => {
                    // TODO: handle every character of t.*, building numbers
                    // TODO: handle every completed number
                },
            }
            if (!reprocessCurrentToken) break;
        }
    }

    return &[_]verify.CVToken{};
}

// ----------------------------------------------------------------------------

const expect = std.testing.expect;
