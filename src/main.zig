const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const verify = @import("verify.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.child_allocator;

    const fileName = fileName: {
        var argIter = std.process.args();
        _ = argIter.nextPosix().?; // skip command name, is always present
        break :fileName (argIter.nextPosix() orelse return error.SingleCommandLineArgumentExpected);
    };

    _ = verify.verifyFile(allocator, std.fs.cwd(), fileName) catch |err| {
        // ...some nice error reporting
        return err;
    };
}
