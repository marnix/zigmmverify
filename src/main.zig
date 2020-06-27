const std = @import("std");

fn eq(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

const MMIterator = struct {
    buffer: []const u8,
    index: u64 = 0,

    pub fn next(self: *MMIterator) ?[]const u8 {
        var len: u64 = 0;
        while (true) {
            // are we after a token (so at end of file or whitespace?
            const atEnd = (self.index + len >= self.buffer.len);
            var afterToken = true;
            if (!atEnd) {
                const c = self.buffer[self.index + len];
                // TODO: error if c is invalid byte
                afterToken = (c <= ' ');
            }
            // return the next token
            if (afterToken and len > 0) {
                const result = self.buffer[self.index .. self.index + len];
                self.index = self.index + len + 1;
                return result;
            }
            // update iterator state
            if (atEnd) {
                return null;
            }
            if (afterToken) {
                assert(len == 0);
                self.index += 1; // start again at the next character
            } else {
                len += 1; // extend the current token
            }
        }
    }
};

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

    const iter = MMIterator{ .buffer = buf };
}

const assert = @import("std").debug.assert;

test "tokenizer on empty buffer" {
    var iter = MMIterator{ .buffer = "" };
    assert(iter.next() == null);
    assert(iter.next() == null);
}

test "tokenizer on whitespace buffer" {
    var iter = MMIterator{ .buffer = "  \t " };
    assert(iter.next() == null);
    assert(iter.next() == null);
}

test "tokenizer with whitespace at start" {
    var iter = MMIterator{ .buffer = " $d $." };
    assert(eq(iter.next().?, "$d"));
    assert(eq(iter.next().?, "$."));
    assert(iter.next() == null);
    assert(iter.next() == null);
}

test "tokenizer with whitespace at end" {
    var iter = MMIterator{ .buffer = "$d $. " };
    assert(eq(iter.next().?, "$d"));
    assert(eq(iter.next().?, "$."));
    assert(iter.next() == null);
    assert(iter.next() == null);
}

test "tokenizer" {
    var iter = MMIterator{ .buffer = "$c wff $." };
    assert(eq(iter.next().?, "$c"));
    assert(eq(iter.next().?, "wff"));
    assert(eq(iter.next().?, "$."));
    assert(iter.next() == null);
    assert(iter.next() == null);
}

test "run main" {
    try main();
}
