const std = @import("std");
const Allocator = std.mem.Allocator;

const void_value: void = undefined;

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const TokenList = tokenize.TokenList;
const parse = @import("parse.zig");

const Expression = std.SegmentedList(struct { token: Token, cv: enum { C, V } }, 0);
const HypothesesList = std.SegmentedList(struct { expression: Expression, isF: bool }, 0);
const InferenceRule = struct {
    hypotheses: HypothesesList,
    conclusion: Expression,
};

const Substitution = std.StringHashMap(Expression); // key is Token == []const u8
const ProofState = std.SinglyLinkedList(Expression);

const FEStatementList = std.SegmentedList(struct {
    label: Token, expression: Expression, ef: enum { F, E }
}, 0);
const InferenceRuleMap = std.StringHashMap(InferenceRule); // key is Token == []const u8
const VerifyState = struct {
    const Self = @This();

    cStatements: TokenSet,
    activeFEStatements: FEStatementList,
    activeStatements: InferenceRuleMap,
    scopes: ScopeStack,

    fn deinit(self: *Self) void {
        self.cStatements.deinit();
        self.activeFEStatements.deinit();
        self.activeStatements.deinit();
        while (self.scopes.popFirst()) |_| {}
    }
};
const TokenSet = std.StringHashMap(void); // key is Token == []const u8
const Scope = struct {
    vStatements: TokenSet,
    feStatements: TokenSet,
};
const ScopeStack = std.SinglyLinkedList(Scope);

pub fn verify(buffer: []const u8, allocator: *Allocator) !void {
    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});

    var n: u64 = 0;
    defer std.debug.warn("\nFound {0} statements!\n", .{n});

    var state = VerifyState{
        // TODO: move initialization into VerifyState struct
        .cStatements = TokenSet.init(allocator),
        .activeFEStatements = FEStatementList.init(allocator),
        .activeStatements = InferenceRuleMap.init(allocator),
        .scopes = ScopeStack.init(),
    };
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
