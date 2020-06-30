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

/// If given compressed proof, that is consumed.
/// If given uncompressed proof, that is returned.
pub fn decompress(tokens: *TokenList, allocator: *Allocator) !TokenList {
    if (tokens.count() == 0 or !eq(tokens.at(0).*, "(")) {
        return tokens.*;
    }
    defer tokens.deinit();
    var result = TokenList.init(allocator);
    var it = tokens.iterator(0);
    while (it.next()) |pToken| {
        //...
    }
    return result;
}

// ----------------------------------------------------------------------------

const TokenIterator = tokenize.TokenIterator;

fn tokenListOf(buffer: []const u8) TokenList {
    if (!@import("builtin").is_test) @panic("for testing purposes only");
    var result = TokenList.init(std.testing.allocator);
    var tokens = TokenIterator{ .buffer = buffer };
    while (tokens.next() catch unreachable) |token| {
        _ = result.push(token) catch unreachable;
    }
    return result;
}

test "'decompress' uncompressed proof" {
    var input = tokenListOf("id ? id");
    var output = try decompress(&input, std.testing.allocator);
    expect(eqs(output, &[_]Token{ "id", "?", "id" }));
    defer output.deinit();
}

test "decompress empty compressed proof" {
    var input = tokenListOf("( )");
    var output = try decompress(&input, std.testing.allocator);
    expect(eqs(output, &[_]Token{}));
    defer output.deinit();
}
