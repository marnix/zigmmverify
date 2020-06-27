const std = @import("std");

fn eq(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

const MMIterator = struct {
    buffer: []const u8,
    index: u64 = 0,

    pub fn next(self: *MMIterator) ?[]const u8 {
        var i = self.index;
        var j = i;
        // invariant: self.buffer[i..j] is the current token so far
        while (true) {
            // where are we?
            const atEnd = (j >= self.buffer.len);
            var afterToken = true;
            if (!atEnd) {
                const c = self.buffer[j];
                // TODO: error if c is invalid byte
                afterToken = (c <= ' ');
            }
            // return the next token, if any
            if (afterToken and i < j) {
                self.index = j + 1;
                return self.buffer[i..j];
            }
            // update iterator state
            if (atEnd) {
                return null;
            }
            j += 1; // extend token with current character
            if (afterToken) {
                assert(j - i == 1);
                i += 1; // remove current character again
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
