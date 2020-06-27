const std = @import("std");

fn eq(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

const Error = error{IllegalCharacter};

const MMIterator = struct {
    buffer: []const u8,
    index: u64 = 0,
    optError: ?Error = null,

    pub fn next(self: *MMIterator) Error!?[]const u8 {
        // return any error detected in the previous call
        if (self.optError) |err| {
            self.optError = null;
            return err;
        }
        var i = self.index;
        var j = i;
        // invariant: self.buffer[i..j] is the current token so far
        while (true) {
            // where are we?
            var optError: ?Error = null;
            const atEnd = (j >= self.buffer.len);
            var afterToken = true;
            if (!atEnd) {
                const c = self.buffer[j];
                afterToken = c <= 32;
                if ((c < 32 and !(c == 9 or c == 10 or c == 12 or c == 13)) or c >= 127) {
                    // treat as whitespace now, return the error next time
                    self.optError = Error.IllegalCharacter;
                    afterToken = true;
                }
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

    var iter = MMIterator{ .buffer = buf };
    while (iter.next() catch |err| return err) |token| {
        // ...handle token in some way
    }
}

const assert = @import("std").debug.assert;

test "tokenizer on empty buffer" {
    var iter = MMIterator{ .buffer = "" };
    assert((try iter.next()) == null);
    assert((try iter.next()) == null);
}

test "tokenizer on whitespace buffer" {
    var iter = MMIterator{ .buffer = "  \t " };
    assert((try iter.next()) == null);
    assert((try iter.next()) == null);
}

test "tokenizer with whitespace at start" {
    var iter = MMIterator{ .buffer = " $d $." };
    assert(eq((try iter.next()).?, "$d"));
    assert(eq((try iter.next()).?, "$."));
    assert((try iter.next()) == null);
    assert((try iter.next()) == null);
}

test "tokenizer with whitespace at end" {
    var iter = MMIterator{ .buffer = "$d $. " };
    assert(eq((try iter.next()).?, "$d"));
    assert(eq((try iter.next()).?, "$."));
    assert((try iter.next()) == null);
    assert((try iter.next()) == null);
}

test "tokenizer with skipped illegal 'low' character" {
    var iter = MMIterator{ .buffer = "$d\x03$." };
    assert(eq((try iter.next()).?, "$d"));
    if (iter.next()) |_| unreachable else |err| assert(err == Error.IllegalCharacter);
    assert(eq((try iter.next()).?, "$."));
    assert((try iter.next()) == null);
    assert((try iter.next()) == null);
}

test "tokenizer with skipped illegal 'high' character" {
    var iter = MMIterator{ .buffer = "$( a\x7fc $)" };
    assert(eq((try iter.next()).?, "$("));
    assert(eq((try iter.next()).?, "a"));
    if (iter.next()) |_| unreachable else |err| assert(err == Error.IllegalCharacter);
    assert(eq((try iter.next()).?, "c"));
    assert(eq((try iter.next()).?, "$)"));
    assert((try iter.next()) == null);
    assert((try iter.next()) == null);
}

test "tokenizer" {
    var iter = MMIterator{ .buffer = "$c wff $." };
    assert(eq((try iter.next()).?, "$c"));
    assert(eq((try iter.next()).?, "wff"));
    assert(eq((try iter.next()).?, "$."));
    assert((try iter.next()) == null);
    assert((try iter.next()) == null);
}

test "run main" {
    try main();
}
