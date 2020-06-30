const std = @import("std");
const Allocator = std.mem.Allocator;

const parse = @import("parse.zig");

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
