usingnamespace @import("globals.zig");

test "" {
    std.testing.refAllDecls(@import("main.zig"));
}
