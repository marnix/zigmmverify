const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const errors = @import("errors.zig");
const Error = errors.Error;

const tokenize = @import("tokenize.zig");
const Token = tokenize.Token;
const eq = tokenize.eq;
const eqs = tokenize.eqs;
const TokenList = tokenize.TokenList;

const expect = std.testing.expect;

pub fn decompress(tokens: TokenList, allocator: *Allocator) !TokenList {
    var result = TokenList.init(allocator);
    return result;
}

// ----------------------------------------------------------------------------

const TokenIterator = tokenize.TokenIterator;

fn decompressString(buffer: []const u8) !TokenList {
    if (!@import("builtin").is_test) @panic("for testing purposes only");
    var tokenList = tokenListOf(buffer);
    defer (&tokenList).deinit();
    return decompress(tokenList, std.testing.allocator);
}

fn tokenListOf(buffer: []const u8) TokenList {
    if (!@import("builtin").is_test) @panic("for testing purposes only");
    var result = TokenList.init(std.testing.allocator);
    var tokens = TokenIterator{ .buffer = buffer };
    while (tokens.next() catch unreachable) |token| {
        _ = result.push(token) catch unreachable;
    }
    return result;
}

test "decompress empty proof" {
    expect(eqs(try decompressString("( )"), &[_]Token{}));
}
