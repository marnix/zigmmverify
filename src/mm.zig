// TODO: Split into separate files in mm directory

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

pub fn verify(buffer: []u8) Error!void {
    var tokens = TokenIterator{ .buffer = buffer };
    while (try tokens.next()) |token| {
        // ...handle token in some way
    }
}

pub const Error = error{ IllegalCharacter, Incomplete, UnexpectedToken, IllegalToken };

//-----------------------------------------------------------------------------
// PARSING

const StatementType = enum { C //, V, F, E, D, A, P, BlockOpen, BlockClose
};

const Token = []const u8;
const TokenList = std.SegmentedList(Token, 0); // TODO: Create custom?  E.g., AppendOnlyList singly-linked list.

const Statement = union(StatementType) {
    C: struct { constants: TokenList },

    fn deinit(self: *Statement, allocator: *Allocator) void {
        switch (self.*) {
            .C => self.*.C.constants.deinit(),
        }
        allocator.destroy(self);
    }
};

const StatementIterator = struct {
    allocator: *Allocator,
    tokens: TokenIterator,

    fn init(allocator: *Allocator, buffer: Token) StatementIterator {
        return StatementIterator{ .allocator = allocator, .tokens = TokenIterator{ .buffer = buffer } };
    }

    fn next(self: *StatementIterator) !?*Statement {
        // get next token
        var token = (try self.tokens.next()) orelse {
            // we have seen the last Token, so we have seen the last Statement, end of iteration
            return null;
        };
        // if the token is a label, read one more token
        var label: ?Token = null;
        while (label == null) {
            if (token[0] == '$') break;
            label = token;
            token = (try self.tokens.next()) orelse return Error.Incomplete;
        }
        if (token[0] != '$') return Error.UnexpectedToken;
        if (token.len != 2) return Error.IllegalToken;
        // handle the $x command
        switch (token[1]) {
            'c' => {
                // TODO: move to new fn 'read TokenList until terminator token'
                var l = TokenList.init(self.allocator);
                while (true) {
                    token = (try self.tokens.next()) orelse return Error.Incomplete;
                    if (eq(token, "$.")) break;
                    // TODO: check against duplicates
                    _ = try l.push(token);
                }
                const result = try self.allocator.create(Statement);
                result.* = Statement{ .C = .{ .constants = l } };
                return result;
            },
            else => return Error.IllegalToken,
        }
    }
};

test "parse constant declaration" {
    var statements = StatementIterator.init(std.testing.allocator, "$c wff |- $.");
    // TODO: abbreviation for simpler expect statements, preferably allowing for list literals
    const s = try statements.next();
    expect(s != null); // TODO: refine
    s.?.deinit(std.testing.allocator);
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

// TODO: add more tests for checking error values

test "parse empty file" {
    var statements = StatementIterator.init(std.testing.allocator, "");
    expect((try statements.next()) == null);
    expect((try statements.next()) == null);
}

//-----------------------------------------------------------------------------
// TOKENIZING

const TokenIterator = struct {
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

const expect = std.testing.expect;

fn eq(a: Token, b: Token) bool {
    return std.mem.eql(u8, a, b);
}

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
