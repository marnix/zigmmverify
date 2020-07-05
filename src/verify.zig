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

    cStatements: TokenSet,
    activeFEStatements: FEStatementList,
    activeStatements: InferenceRuleMap,
    scopes: ScopeStack,

    fn init(allocator: *Allocator) !Self {
        var scopes = ScopeStack.init();
        var node = try allocator.create(ScopeStack.Node);
        node.* = ScopeStack.Node{ .data = Scope.init(allocator) };
        scopes.prepend(node);
        return Self{
            .cStatements = TokenSet.init(allocator),
            .activeFEStatements = FEStatementList.init(allocator),
            .activeStatements = InferenceRuleMap.init(allocator),
            .scopes = scopes,
        };
    }

    fn deinit(self: *Self) void {
        self.cStatements.deinit();
        self.activeFEStatements.deinit();
        self.activeStatements.deinit();
        while (self.scopes.popFirst()) |_| {}
    }

    fn currentScope(self: *Self) *Scope {
        return &(self.scopes.first.?.data);
    }
};
const Scope = struct {
    const Self = @This();

    vStatements: TokenSet,
    feStatements: TokenSet,

    fn init(allocator: *Allocator) Self {
        return Self{
            .vStatements = TokenSet.init(allocator),
            .feStatements = TokenSet.init(allocator),
        };
    }

    fn clone(self: Self) !Self {
        return Self{
            .vStatements = try self.vStatements.clone(),
            .feStatements = try self.feStatements.clone(),
        };
    }

    fn deinit(self: *Self) void {
        self.vStatements.deinit();
        self.feStatements.deinit();
    }
};
const ScopeStack = std.SinglyLinkedList(Scope);

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
                    const kv = try state.cStatements.put(constant.*, void_value);
                    if (kv) |_| return Error.Duplicate;
                }
            },
            .V => |vStatement| {
                var it = @as(TokenList, vStatement.variables).iterator(0); // TODO: why coercion needed??
                while (it.next()) |variable| {
                    const kv = try state.currentScope().vStatements.put(variable.*, void_value);
                    if (kv) |_| return Error.Duplicate; // TODO: Test
                }
            },
            .BlockOpen => {
                const firstNode = state.scopes.popFirst() orelse unreachable;
                state.scopes.prepend(firstNode);
                var newScope = try firstNode.data.clone();
                state.scopes.prepend(&ScopeStack.Node{ .data = newScope });
            },
            .BlockClose => {
                if (state.scopes.popFirst()) |firstNode| {
                    firstNode.data.deinit();
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

test "single variable" {
    if (true) return error.SkipZigTest; // TODO Fix memory leak
    try verify("$v ph $.", std.testing.allocator);
}

test "duplicate constant" {
    if (true) return error.SkipZigTest; // TODO Fix memory leak
    expectError(Error.Duplicate, verify("$c wff wff $.", std.testing.allocator));
}
