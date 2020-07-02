const std = @import("std");
const Allocator = std.mem.Allocator;

const tokenize = @import("tokenize.zig");
const TokenList = tokenize.TokenList;
const parse = @import("parse.zig");

const Expression = std.SegmentedList(struct { token: Token, cv: enum { C, V } }, 0);
const InferenceRule = struct {
    hypotheses: std.SegmentedList(struct { expression: Expression, isF: bool }, 0),
    conclusion: Expression,
};
const Substitution = std.AutoHashMap(Token, Expression);
const ProofState = std.SinglyLinkedList(Expression);
const VerifyState = struct {
    activeFEStatements: std.SegmentedList(struct {
        label: Token, expression: Expression, ef: enum { E, F }
    }, 0),
    activeStatements: std.AutoHashMap(Token, InferenceRule),
    scopes: ScopeStack,
};
const ScopeStack = std.SinglyLinkedList(std.AutoHashMap(Token, void));

pub fn verify(buffer: []u8, allocator: *Allocator) !void {
    var statements = parse.StatementIterator.init(allocator, buffer);
    var n: u64 = 0;
    defer std.debug.warn("\nFound {0} statements!\n", .{n});
    errdefer |err| std.debug.warn("\nError {0} happened...\n", .{err});
    while (try statements.next()) |statement| {
        n += 1;
        // ...handle statement in some way
        statement.deinit(allocator);
    }
}

test "" {}
