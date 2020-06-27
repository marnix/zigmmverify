const std = @import("std");
const mm = @import("mm.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const mm_file = try std.fs.cwd().openFile("set.mm", .{});
    defer mm_file.close();
    const size = (try mm_file.stat()).size;
    const mm_buffer = try allocator.alloc(u8, size);
    defer allocator.free(mm_buffer);
    _ = try mm_file.readAll(mm_buffer);

    var tokens = mm.TokenIterator{ .buffer = mm_buffer };
    while (tokens.next() catch |err| return err) |token| {
        // ...handle token in some way
    }
}

test "run main" {
    try main();
}
