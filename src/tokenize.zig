const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const errors = @import("errors.zig");
const Error = errors.Error;

pub const Token = []const u8;
pub const TokenList = std.ArrayList(Token);
pub const TokenSet = struct {
    const Self = @This();
    map: TokenMap(void),

    pub fn init(allocator: Allocator) Self {
        return Self{ .map = TokenMap(void).init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.map.deinit();
    }

    pub fn add(self: *Self, token: Token) !bool {
        if (self.map.contains(token)) return true; // already present
        try self.map.put(token, undefined);
        return false;
    }

    pub fn remove(self: *Self, token: Token) void {
        _ = self.map.remove(token);
    }

    pub fn contains(self: *Self, token: Token) bool {
        return self.map.contains(token);
    }

    /// The iterator's next() returns a ?TokenMap(void).KV,
    /// of which only the .key field must be used.
    pub fn iterator(self: *Self) TokenMap(void).Iterator {
        return self.map.iterator();
    }
};
pub fn TokenMap(comptime T: type) type {
    return std.StringHashMap(T); // key is Token == []const u8
}

pub const TokenIterator = struct {
    buffer: Token,
    index: u64 = 0,
    optError: ?Error = null,

    pub fn next(self: *TokenIterator) !?Token {
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

pub fn eqs(a: TokenList, b: []const Token) bool {
    // TODO: compare in a simpler way?
    if (a.items.len != b.len) return false;
    var i: usize = 0;
    while (i < b.len) : (i += 1) {
        if (!eq(a.items[i], b[i])) return false;
    }
    return true;
}

const expect = std.testing.expect;

test "tokenizer on empty buffer" {
    var tokens = TokenIterator{ .buffer = "" };
    try expect((try tokens.next()) == null);
    try expect((try tokens.next()) == null);
}

test "tokenizer on whitespace buffer" {
    var tokens = TokenIterator{ .buffer = "  \t " };
    try expect((try tokens.next()) == null);
    try expect((try tokens.next()) == null);
}

test "tokenizer with whitespace at start" {
    var tokens = TokenIterator{ .buffer = " $d $." };
    try expect(eq((try tokens.next()).?, "$d"));
    try expect(eq((try tokens.next()).?, "$."));
    try expect((try tokens.next()) == null);
    try expect((try tokens.next()) == null);
}

test "tokenizer with whitespace at end" {
    var tokens = TokenIterator{ .buffer = "$d $. " };
    try expect(eq((try tokens.next()).?, "$d"));
    try expect(eq((try tokens.next()).?, "$."));
    try expect((try tokens.next()) == null);
    try expect((try tokens.next()) == null);
}

test "tokenizer with skipped illegal 'low' character" {
    var tokens = TokenIterator{ .buffer = "$d\x03$." };
    try expect(eq((try tokens.next()).?, "$d"));
    if (tokens.next()) |_| unreachable else |err| try expect(err == Error.IllegalCharacter);
    try expect(eq((try tokens.next()).?, "$."));
    try expect((try tokens.next()) == null);
    try expect((try tokens.next()) == null);
}

test "tokenizer with skipped illegal 'high' character" {
    var tokens = TokenIterator{ .buffer = "$( a\x7fc $)" };
    try expect(eq((try tokens.next()).?, "$("));
    try expect(eq((try tokens.next()).?, "a"));
    if (tokens.next()) |_| unreachable else |err| try expect(err == Error.IllegalCharacter);
    try expect(eq((try tokens.next()).?, "c"));
    try expect(eq((try tokens.next()).?, "$)"));
    try expect((try tokens.next()) == null);
    try expect((try tokens.next()) == null);
}

test "tokenizer" {
    var tokens = TokenIterator{ .buffer = "$c wff $." };
    try expect(eq((try tokens.next()).?, "$c"));
    try expect(eq((try tokens.next()).?, "wff"));
    try expect(eq((try tokens.next()).?, "$."));
    try expect((try tokens.next()) == null);
    try expect((try tokens.next()) == null);
}

test "tokenizer comment without newline" {
    var tokens = TokenIterator{ .buffer = "$( a b c $)\n$c $." };
    try expect(eq((try tokens.next()).?, "$("));
    try expect(eq((try tokens.next()).?, "a"));
    try expect(eq((try tokens.next()).?, "b"));
    try expect(eq((try tokens.next()).?, "c"));
    try expect(eq((try tokens.next()).?, "$)"));
    try expect(eq((try tokens.next()).?, "$c"));
    try expect(eq((try tokens.next()).?, "$."));
    try expect((try tokens.next()) == null);
    try expect((try tokens.next()) == null);
}
