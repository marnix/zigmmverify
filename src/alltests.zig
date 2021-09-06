const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

pub const TestFS = struct {
    tmpDir: std.testing.TmpDir = undefined,

    pub fn init() TestFS {
        return TestFS{ .tmpDir = std.testing.tmpDir(.{}) };
    }

    pub fn deinit(self: *TestFS) void {
        self.tmpDir.cleanup();
    }

    pub fn writeFile(self: *TestFS, name: []const u8, buffer: []const u8) !void {
        const file = try self.tmpDir.dir.createFile(name, .{});
        defer file.close();
        try file.writeAll(buffer);
    }
};

test "" {
    std.testing.refAllDecls(@import("main.zig"));
}
