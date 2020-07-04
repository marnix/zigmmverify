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

    fn init(allocator: *Allocator) Self {
        return VerifyState{
            .cStatements = TokenSet.init(allocator),
            .activeFEStatements = FEStatementList.init(allocator),
            .activeStatements = InferenceRuleMap.init(allocator),
            .scopes = ScopeStack.init(),
        };
    }

    fn deinit(self: *Self) void {
        self.cStatements.deinit();
        self.activeFEStatements.deinit();
        self.activeStatements.deinit();
        while (self.scopes.popFirst()) |_| {}
    }
};
const Scope = struct {
    vStatements: TokenSet,
    feStatements: TokenSet,
};
const ScopeStack = std.SinglyLinkedList(Scope);

pub fn verify(buffer: []const u8, allocator: *Allocator) !void {
    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});

    var n: u64 = 0;
    defer std.debug.warn("\nFound {0} statements!\n", .{n});

    var state = VerifyState.init(allocator);
    defer state.deinit();

    var statements = parse.StatementIterator.init(allocator, buffer);
    while (try statements.next()) |statement| {
        defer statement.deinit(allocator);
        n += 1;

        switch (statement.*) {
            .C => |cStatement| {
                var it = @as(TokenList, cStatement.constants).iterator(0); // TODO: why coercion needed??
                while (it.next()) |constant| {
                    const kv = try state.cStatements.put(constant.*, void_value);
                    if (kv) |_| return Error.DuplicateConstant;
                }
            },
            else => {
                // TODO: implement the other statements, then remove this clause
            },
        }
    }
}

const expect = std.testing.expect;
const expectError = std.testing.expectError;

test "" {
    expectError(Error.DuplicateConstant, verify("$c wff wff $.", std.testing.allocator));
}
