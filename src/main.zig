const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const mm_file = try std.fs.cwd().openFile("set.mm", .{});
    defer mm_file.close();
    const size = (try mm_file.stat()).size;
    const buf = try allocator.alloc(u8, size);
    defer allocator.free(buf);
    _ = try mm_file.readAll(buf);
}

test "run main" {
    try main();
}
