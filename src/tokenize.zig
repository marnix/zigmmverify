const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const errors = @import("errors.zig");
const Error = errors.Error;

pub const Token = []const u8;
pub const TokenList = std.SegmentedList(Token, 0); // TODO: Create custom?  E.g., AppendOnlyList singly-linked list.

pub const TokenIterator = struct {
    buffer: Token,
    index: u64 = 0,
    optError: ?Error = null,

    // TODO: Let the error union be inferred?
    fn next(self: *TokenIterator) Error!?Token {
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

pub fn eq(a: Token, b: Token) bool {
    return std.mem.eql(u8, a, b);
}

const expect = std.testing.expect;

test "tokenizer on empty buffer" {
    var tokens = TokenIterator{ .buffer = "" };
    expect((try tokens.next()) == null);
    expect((try tokens.next()) == null);
}

test "tokenizer on whitespace buffer" {
    var tokens = TokenIterator{ .buffer = "  \t " };
    expect((try tokens.next()) == null);
    expect((try tokens.next()) == null);
}

test "tokenizer with whitespace at start" {
    var tokens = TokenIterator{ .buffer = " $d $." };
    expect(eq((try tokens.next()).?, "$d"));
    expect(eq((try tokens.next()).?, "$."));
    expect((try tokens.next()) == null);
    expect((try tokens.next()) == null);
}

test "tokenizer with whitespace at end" {
    var tokens = TokenIterator{ .buffer = "$d $. " };
    expect(eq((try tokens.next()).?, "$d"));
    expect(eq((try tokens.next()).?, "$."));
    expect((try tokens.next()) == null);
    expect((try tokens.next()) == null);
}

test "tokenizer with skipped illegal 'low' character" {
    var tokens = TokenIterator{ .buffer = "$d\x03$." };
    expect(eq((try tokens.next()).?, "$d"));
    if (tokens.next()) |_| unreachable else |err| expect(err == Error.IllegalCharacter);
    expect(eq((try tokens.next()).?, "$."));
    expect((try tokens.next()) == null);
    expect((try tokens.next()) == null);
}

test "tokenizer with skipped illegal 'high' character" {
    var tokens = TokenIterator{ .buffer = "$( a\x7fc $)" };
    expect(eq((try tokens.next()).?, "$("));
    expect(eq((try tokens.next()).?, "a"));
    if (tokens.next()) |_| unreachable else |err| expect(err == Error.IllegalCharacter);
    expect(eq((try tokens.next()).?, "c"));
    expect(eq((try tokens.next()).?, "$)"));
    expect((try tokens.next()) == null);
    expect((try tokens.next()) == null);
}

test "tokenizer" {
    var tokens = TokenIterator{ .buffer = "$c wff $." };
    expect(eq((try tokens.next()).?, "$c"));
    expect(eq((try tokens.next()).?, "wff"));
    expect(eq((try tokens.next()).?, "$."));
    expect((try tokens.next()) == null);
    expect((try tokens.next()) == null);
}

test "tokenizer comment without newline" {
    var tokens = TokenIterator{ .buffer = "$( a b c $)\n$c $." };
    expect(eq((try tokens.next()).?, "$("));
    expect(eq((try tokens.next()).?, "a"));
    expect(eq((try tokens.next()).?, "b"));
    expect(eq((try tokens.next()).?, "c"));
    expect(eq((try tokens.next()).?, "$)"));
    expect(eq((try tokens.next()).?, "$c"));
    expect(eq((try tokens.next()).?, "$."));
    expect((try tokens.next()) == null);
    expect((try tokens.next()) == null);
}
