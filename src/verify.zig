usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const TokenSet = tokenize.TokenSet;
const TokenMap = tokenize.TokenMap;
const parse = @import("parse.zig");

const Expression = std.SegmentedList(struct { token: Token, cv: enum { C, V } }, 0);
const HypothesesList = std.SegmentedList(struct { expression: Expression, isF: bool }, 0);
const InferenceRule = struct {
    hypotheses: HypothesesList,
    conclusion: Expression,
};

const Substitution = TokenMap(Expression);
const ProofState = std.SinglyLinkedList(Expression);

const FEStatementList = std.SegmentedList(struct {
    label: Token, expression: Expression, ef: enum { F, E }
}, 0);
const InferenceRuleMap = TokenMap(InferenceRule);
const VerifyState = struct {
    const Self = @This();

    allocator: *Allocator,
    constants: TokenSet,
    variables: TokenSet,
    activeFEStatements: FEStatementList,
    activeStatements: InferenceRuleMap,
    currentScope: ?*Scope,

    fn init(allocator: *Allocator) !Self {
        return Self{
            .allocator = allocator,
            .constants = TokenSet.init(allocator),
            .variables = TokenSet.init(allocator),
            .activeFEStatements = FEStatementList.init(allocator),
            .activeStatements = InferenceRuleMap.init(allocator),
            .currentScope = null,
        };
    }

    fn deinit(self: *Self) void {
        self.constants.deinit();
        self.variables.deinit();
        self.activeFEStatements.deinit();
        self.activeStatements.deinit();
        while (self.currentScope) |scope| {
            const optOuter = scope.optOuter;
            scope.deinit();
            self.currentScope = optOuter;
        }
    }
};
const Scope = struct {
    const Self = @This();

    state: *VerifyState,
    optOuter: ?*Scope,
    variables: TokenSet,

    fn init(state: *VerifyState, allocator: *Allocator) Self {
        return Self{
            .state = state,
            .optOuter = state.currentScope,
            .variables = TokenSet.init(allocator),
        };
    }

    fn deinit(self: *Self) void {
        var it = self.variables.iterator();
        while (it.next()) |kv| {
            _ = self.state.variables.remove(kv.key);
        }
        self.variables.deinit();
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
                var it = @as(TokenList, cStatement.constants).iterator(0);
                while (it.next()) |constant| {
                    const kv = try state.constants.put(constant.*, void_value);
                    if (kv) |_| return Error.Duplicate;
                }
            },
            .V => |vStatement| {
                var it = @as(TokenList, vStatement.variables).iterator(0); // TODO: why coercion needed??
                while (it.next()) |variable| {
                    const kv = try state.variables.put(variable.*, void_value);
                    if (kv) |_| return Error.Duplicate;
                    if (state.currentScope) |scope| {
                        _ = try scope.variables.put(variable.*, void_value);
                    }
                }
            },
            .BlockOpen => {
                const newScope = try allocator.create(Scope);
                newScope.* = Scope.init(&state, allocator);
                state.currentScope = newScope;
            },
            .BlockClose => {
                if (state.currentScope) |scope| {
                    const optOuter = scope.optOuter;
                    scope.deinit();
                    state.currentScope = optOuter;
                } else return Error.UnexpectedToken; // TODO: Test
            },
            else => {
                // TODO: implement the other statements, then remove this clause
            },
        }
    }
}

const expect = std.testing.expect;
const expectError = std.testing.expectError;

test "duplicate variable" {
    expectError(Error.Duplicate, verify("$c ph ps ph $.", std.testing.allocator));
}

test "single variable" {
    try verify("$v ph $.", std.testing.allocator);
}

test "duplicate constant" {
    expectError(Error.Duplicate, verify("$c wff wff $.", std.testing.allocator));
}
