usingnamespace @import("globals.zig");

const errors = @import("errors.zig");
const Error = errors.Error;

/// caller owns the result, which was allocated with the provided allocator
pub fn readBuffer(allocator: *Allocator, dir: std.fs.Dir, file_name: []const u8) ![]const u8 {
    const file = dir.openFile(file_name, .{}) catch return Error.IncorrectFileName;
    defer file.close();
    const size = (try file.stat()).size;
    var buffer = try allocator.alloc(u8, size);
    _ = try file.readAll(buffer);
    return buffer;
}
