pub const io_mode = .evented;

usingnamespace @import("globals.zig");

const verify = @import("verify.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    const fileName = fileName: {
        var argIter = std.process.args();
        _ = argIter.nextPosix().?; // skip command name, is always present
        if (argIter.nextPosix()) |fileName| {
            break :fileName fileName;
        } else {
            return error.SingleCommandLineArgumentExpected;
        }
    };

    _ = verify.verifyFile(fileName, allocator) catch |err| {
        // ...some nice error reporting
        return err;
    };
}
