const std = @import("std");
const assert = @import("std").debug.assert;

pub fn verify(buffer: []u8) Error!void {
    var tokens = TokenIterator{ .buffer = buffer };
    while (try tokens.next()) |token| {
        // ...handle token in some way
    }
}

pub const Error = error{IllegalCharacter};

pub const TokenIterator = struct {
    buffer: []const u8,
    index: u64 = 0,
    optError: ?Error = null,

    pub fn next(self: *TokenIterator) Error!?[]const u8 {
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

fn eq(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}

test "tokenizer on empty buffer" {
    var tokens = TokenIterator{ .buffer = "" };
    assert((try tokens.next()) == null);
    assert((try tokens.next()) == null);
}

test "tokenizer on whitespace buffer" {
    var tokens = TokenIterator{ .buffer = "  \t " };
    assert((try tokens.next()) == null);
    assert((try tokens.next()) == null);
}

test "tokenizer with whitespace at start" {
    var tokens = TokenIterator{ .buffer = " $d $." };
    assert(eq((try tokens.next()).?, "$d"));
    assert(eq((try tokens.next()).?, "$."));
    assert((try tokens.next()) == null);
    assert((try tokens.next()) == null);
}

test "tokenizer with whitespace at end" {
    var tokens = TokenIterator{ .buffer = "$d $. " };
    assert(eq((try tokens.next()).?, "$d"));
    assert(eq((try tokens.next()).?, "$."));
    assert((try tokens.next()) == null);
    assert((try tokens.next()) == null);
}

test "tokenizer with skipped illegal 'low' character" {
    var tokens = TokenIterator{ .buffer = "$d\x03$." };
    assert(eq((try tokens.next()).?, "$d"));
    if (tokens.next()) |_| unreachable else |err| assert(err == Error.IllegalCharacter);
    assert(eq((try tokens.next()).?, "$."));
    assert((try tokens.next()) == null);
    assert((try tokens.next()) == null);
}

test "tokenizer with skipped illegal 'high' character" {
    var tokens = TokenIterator{ .buffer = "$( a\x7fc $)" };
    assert(eq((try tokens.next()).?, "$("));
    assert(eq((try tokens.next()).?, "a"));
    if (tokens.next()) |_| unreachable else |err| assert(err == Error.IllegalCharacter);
    assert(eq((try tokens.next()).?, "c"));
    assert(eq((try tokens.next()).?, "$)"));
    assert((try tokens.next()) == null);
    assert((try tokens.next()) == null);
}

test "tokenizer" {
    var tokens = TokenIterator{ .buffer = "$c wff $." };
    assert(eq((try tokens.next()).?, "$c"));
    assert(eq((try tokens.next()).?, "wff"));
    assert(eq((try tokens.next()).?, "$."));
    assert((try tokens.next()) == null);
    assert((try tokens.next()) == null);
}
