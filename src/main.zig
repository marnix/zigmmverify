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

    _ = mm.verify(mm_buffer) catch |err| {
        // ...some nice error reporting
    };
}

test "run main" {
    try main();
}
