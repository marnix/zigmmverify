usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const TokenSet = tokenize.TokenSet;
const TokenMap = tokenize.TokenMap;
const parse = @import("parse.zig");

const ExpressionChild = struct { token: Token, cv: enum { C, V } };
const Expression = []ExpressionChild;
const HypothesesList = []struct { expression: Expression, isF: bool };
const InferenceRule = struct {
    hypotheses: HypothesesList,
    conclusion: Expression,
};

const Substitution = TokenMap(Expression);
const ProofState = std.SinglyLinkedList(Expression);

// TODO: Find a better name; {Token,Label,Symbol}Interpretation?
const MeaningType = enum { C, V };
const Meaning = union(MeaningType) {
    C: void,
    V: void,
};

const VerifyState = struct {
    const Self = @This();

    allocator: *Allocator,

    /// what each active token means
    meanings: TokenMap(Meaning),
    currentScopeDiff: ?*ScopeDiff,

    fn init(allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .meanings = TokenMap(Meaning).init(allocator),
            .currentScopeDiff = null,
        };
    }

    fn deinit(self: *Self) void {
        // TODO: Deinit every meaning
        self.meanings.deinit();
        while (self.currentScopeDiff) |scopeDiff| {
            scopeDiff.pop();
        }
    }

    fn expressionOf(self: *Self, tokens: TokenList) !*Expression {
        var result = try self.allocator.alloc(ExpressionChild, tokens.count());
        var i: usize = 0;
        var it = @as(TokenList, tokens).iterator(0);
        while (it.next()) |cv| {
            result[i] = .{ .token = cv.*, .cv = .C };
        }
        return &result;
    }
};

/// A ScopeDiff represents how a nested scope differs from its outer scope:
/// which tokens and labels will become inactive again at its ` $} ` statement.
const ScopeDiff = struct {
    const Self = @This();

    state: *VerifyState,
    optOuter: ?*ScopeDiff,
    activeTokens: TokenSet,

    fn push(state: *VerifyState) !void {
        const newScopeDiff = try state.allocator.create(ScopeDiff);
        newScopeDiff.* = Self{
            .state = state,
            .optOuter = state.currentScopeDiff,
            .activeTokens = TokenSet.init(state.allocator),
        };

        state.currentScopeDiff = newScopeDiff;
    }

    fn pop(self: *Self) void {
        self.state.currentScopeDiff = self.optOuter;

        var it = self.activeTokens.iterator();
        while (it.next()) |kv| {
            if (self.state.meanings.remove(kv.key)) |_| {} else {
                std.debug.warn("\nTODO: insert meaning for token {0}.\n", .{kv.key});
            }
        }
        self.activeTokens.deinit();

        self.state.allocator.destroy(self);
    }
};

pub fn verify(buffer: []const u8, allocator: *Allocator) !void {
    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});

    var n: u64 = 0;
    defer std.debug.warn("\nFound {0} statements!\n", .{n});

    var state = try VerifyState.init(allocator);
    defer state.deinit();

    var statements = parse.StatementIterator.init(allocator, buffer);
    while (try statements.next()) |statement| {
        defer statement.deinit(allocator);
        n += 1;

        switch (statement.*) {
            .C => |cStatement| {
                if (state.currentScopeDiff) |_| return Error.UnexpectedToken; // $c inside ${ $}
                var it = @as(TokenList, cStatement.constants).iterator(0);
                while (it.next()) |constant| {
                    const kv = try state.meanings.put(constant.*, Meaning{ .C = void_value });
                    if (kv) |_| return Error.Duplicate;
                }
            },
            .V => |vStatement| {
                var it = @as(TokenList, vStatement.variables).iterator(0); // TODO: why coercion needed??
                while (it.next()) |variable| {
                    const kv = try state.meanings.put(variable.*, Meaning{ .V = void_value });
                    if (kv) |_| return Error.Duplicate;
                    if (state.currentScopeDiff) |scopeDiff| {
                        _ = try scopeDiff.activeTokens.add(variable.*); // this $v will become inactive at the next $}
                    }
                }
            },
            .F => |fStatement| {
                if (state.currentScopeDiff) |scopeDiff| {
                    _ = try scopeDiff.activeTokens.add(fStatement.label); // this $f will become inactive at the next $}
                }
            },
            .E => |eStatement| {
                if (state.currentScopeDiff) |scopeDiff| {
                    _ = try scopeDiff.activeTokens.add(eStatement.label); // this $e will become inactive at the next $}
                }
            },
            .BlockOpen => {
                try ScopeDiff.push(&state);
            },
            .BlockClose => {
                if (state.currentScopeDiff) |scopeDiff| {
                    scopeDiff.pop();
                } else return Error.UnexpectedToken;
            },
            else => {
                // TODO: implement the other statements, then remove this clause
            },
        }
    }

    if (state.currentScopeDiff) |_| return Error.Incomplete; // unclosed $}
}

const expect = std.testing.expect;
const expectError = std.testing.expectError;

test "tokenlist to expression" {
    var state = try VerifyState.init(std.testing.allocator);
    defer state.deinit();
    _ = try state.meanings.put("wff", Meaning{ .C = void_value });
    _ = try state.meanings.put("ph", Meaning{ .V = void_value });
    var tokens = TokenList.init(std.testing.allocator);
    defer tokens.deinit();
    try tokens.push("wff");
    try tokens.push("ph");

    const expression = try state.expressionOf(tokens);
    defer std.testing.allocator.destroy(expression);

    expect(expression.len == 2);
    //expect(ExpressionChild{ .token = "wff", .cv = .C } == expression[0]);
    //expect(ExpressionChild{ .token = "wff", .cv = .V } == expression[0]);
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
