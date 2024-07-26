const std = @import("std");

const Allocator = std.mem.Allocator;

const TokenType = enum { openPar, closePar, integerLiteral, symbol };

const Token = union(TokenType) { openPar, closePar, integerLiteral: i64, symbol: []const u8 };

const TokenizeState = struct { rest: []const u8, current: ?Token };

const ParseNodeType = enum { list, integerLiteral, symbol };

const ListNode = struct { item: *ParseNode, rest: ?*@This() };

const ParseNode = union(ParseNodeType) { list: ?*ListNode, integerLiteral: i64, symbol: []const u8 };

const ParseState = struct { allocator: Allocator, tokenizer: TokenizeState };

const ArrayList = std.ArrayList;

inline fn advanceChar(state: *TokenizeState) void {
    state.rest.len -= 1;
    state.rest.ptr += 1;
}

// zig issue:
// when an error value doesn't belong to an error type, zig screams something on the lines of
// src/parse.zig:45:22: error: expected type 'error{OutOfMemory,UnknownToken,EOF,UnmatchedClosePar}', found type 'error{TokenError}'
// which could be a little bit more obvious, stating that the issue was that one of more errors, (in this case the TokenError error) is not in
// the left set.
fn advanceToken(state: *TokenizeState) ParseError!bool {
    skipWhitespace(state);

    if (state.rest.len == 0) {
        state.current = null;
        return false;
    }

    const next = state.rest[0];

    if (next == '(') {
        state.current = Token.openPar;
        advanceChar(state);
    } else if (next == ')') {
        state.current = Token.closePar;
        advanceChar(state);
    } else if (std.ascii.isDigit(next)) {
        nextNumber(state);
    } else if (isSymbolCharacter(next)) {
        nextSymbol(state);
    } else {
        return error.UnknownToken;
    }

    return true;
}

fn skipWhitespace(state: *TokenizeState) void {
    while (state.rest.len > 0) {
        while (state.rest.len > 0 and isWhitespace(state.rest[0])) {
            advanceChar(state);
        }

        if (state.rest.len > 0 and state.rest[0] != ';') {
            break;
        }

        while (state.rest.len > 0 and state.rest[0] != '\n') {
            advanceChar(state);
        }
    }
}

fn nextNumber(state: *TokenizeState) void {
    std.debug.assert(std.ascii.isDigit(state.rest[0]));

    var slice = state.rest;
    slice.len = 0;

    while (state.rest.len > 0 and std.ascii.isDigit(state.rest[0])) {
        advanceChar(state);
        slice.len += 1;
    }

    const value: i64 = std.fmt.parseInt(i64, slice, 10) catch unreachable;

    state.current = Token{ .integerLiteral = value };
}

fn isSymbolCharacter(char: u8) bool {
    if (std.ascii.isAlphanumeric(char)) {
        return true;
    }

    const specials = [_]u8{ '+', '-', '_', '@', '!', '$', '%' };

    for (specials) |special| {
        if (special == char) {
            return true;
        }
    }

    return false;
}

fn nextSymbol(state: *TokenizeState) void {
    std.debug.assert(std.ascii.isAlphabetic(state.rest[0]));

    var symbol = state.rest;
    symbol.len = 0;

    while (isSymbolCharacter(state.rest[0])) {
        advanceChar(state);
        symbol.len += 1;
    }

    state.current = .{ .symbol = symbol };
}

fn isWhitespace(x: u8) bool {
    return x == ' ' or x == '\t' or x == '\n';
}

const ParseError = error{ OutOfMemory, UnknownToken, EOF, UnmatchedClosePar };

fn parseNode(state: *ParseState) ParseError!*ParseNode {
    std.debug.assert(state.tokenizer.current != null);

    const current = state.tokenizer.current orelse unreachable;
    _ = try advanceToken(&state.tokenizer);

    switch (current) {
        .symbol => |value| {
            const node = try state.allocator.create(ParseNode);
            node.* = ParseNode{ .symbol = value };
            return node;
        },

        .integerLiteral => |value| {
            const node = try state.allocator.create(ParseNode);
            node.* = ParseNode{ .integerLiteral = value };
            return node;
        },

        .openPar => {
            const list = try parseList(state);

            const node = try state.allocator.create(ParseNode);
            node.* = ParseNode{ .list = list };
            return node;
        },

        .closePar => {
            return error.UnmatchedClosePar;
        },
    }
}

fn parseList(state: *ParseState) ParseError!?*ListNode {
    switch (state.tokenizer.current orelse unreachable) {
        Token.closePar => {
            _ = try advanceToken(&state.tokenizer);
            return null;
        },

        else => {
            const node = try state.allocator.create(ListNode);
            const item = try parseNode(state);
            const rest = try parseList(state);
            node.* = ListNode{ .item = item, .rest = rest };
            return node;
        },
    }
}

pub fn parse(str: []const u8, allocator: Allocator) ParseError!*ParseNode {
    var state = ParseState{ .allocator = allocator, .tokenizer = .{ .rest = str, .current = null } };

    if (!try advanceToken(&state.tokenizer)) {
        return error.EOF;
    }

    return parseNode(&state);
}

pub fn show(node: *ParseNode, string: *ArrayList(u8)) error{OutOfMemory}!void {
    const w = string.writer();

    switch (node.*) {
        .list => |list| {
            try showList(list, string);
        },
        .symbol => |symbol| {
            try w.print("{s}", .{symbol});
        },
        .integerLiteral => |integer| {
            try w.print("{}", .{integer});
        },
    }
}

pub fn showList(list: ?*ListNode, string: *ArrayList(u8)) error{OutOfMemory}!void {
    const w = string.writer();
    try w.print("(", .{});

    var current = list;

    while (current) |list_| {
        try show(list_.*.item, string);

        if (list_.rest != null) {
            try w.print(" ", .{});
        }

        current = list_.rest;
    }
    try w.print(")", .{});
}
