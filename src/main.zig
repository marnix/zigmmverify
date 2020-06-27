const std = @import("std");

pub fn main() void {
    std.debug.assert(false);
}

test "always succeed" {
    std.debug.assert(true);
}
