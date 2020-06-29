const std = @import("std");

const parse = @import("parse.zig");

pub fn verify(buffer: []u8) !void {
    var tokens = @import("tokenize.zig").TokenIterator{ .buffer = buffer };
    while (try tokens.next()) |token| {
        // ...handle token in some way
    }
}
