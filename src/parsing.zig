const std = @import("std");

const Allocator = std.mem.Allocator;

const TokenType = enum { openPar, closePar, integerLiteral, symbol };

const Token = union(TokenType) { openPar, closePar, integerLiteral: i64, symbol: []const u8 };

const Tokenizer = struct {
    next_char: union(enum) { uninitialized, eof, next: u8 },
    next_token: union(enum) { uninitialized, eof, next: Token },
    rest: []const u8,
    allocator: Allocator,

    fn shiftChar(state: *Tokenizer) void {
        if (state.rest.len == 0) {
            state.next_char = .eof;
            return;
        }
        const value = state.rest[0];
        state.rest.len -= 1;
        state.rest.ptr += 1;
        state.next_char = .{ .next = value };
    }

    fn peekChar(state: *Tokenizer) ?u8 {
        if (state.next_char == .uninitialized) {
            state.shiftChar();
        }

        if (state.next_char == .eof) {
            return null;
        }

        const r = state.next_char.next;

        return r;
    }

    fn shift(state: *Tokenizer) !void {
        skipWhitespace(state);

        const next = state.peekChar() orelse {
            state.next_token = .eof;
            return;
        };

        if (next == '(') {
            state.next_token = .{ .next = Token.openPar };
            state.shiftChar();
        } else if (next == ')') {
            state.next_token = .{ .next = Token.closePar };
            state.shiftChar();
        } else if (std.ascii.isDigit(next)) {
            try state.shiftNumber();
        } else if (isSymbolCharacter(next)) {
            try state.shiftSymbol();
        } else {
            return error.UnknownChar;
        }
    }

    fn shiftNumber(state: *Tokenizer) !void {
        std.debug.assert(std.ascii.isDigit(state.next_char.next));

        var content = std.ArrayList(u8).init(state.allocator);
        defer content.deinit();

        while (true) {
            const next = state.peekChar() orelse break;

            if (std.ascii.isDigit(next)) {
                try content.append(next);
                state.shiftChar();
            } else {
                break;
            }
        }

        if (std.fmt.parseInt(i64, content.items, 10)) |parsed| {
            state.next_token = .{ .next = .{ .integerLiteral = parsed } };
        } else |issue| {
            if (issue == std.fmt.ParseIntError.Overflow) {
                return error.Overflow;
            } else {
                unreachable;
            }
        }
    }

    fn shiftSymbol(state: *Tokenizer) !void {
        std.debug.assert(isSymbolCharacter(state.next_char.next));

        var symbol = std.ArrayList(u8).init(state.allocator);
        errdefer symbol.deinit();

        while (true) {
            const next = state.peekChar() orelse break;

            if (isSymbolCharacter(next)) {
                try symbol.append(next);
                state.shiftChar();
            } else {
                break;
            }
        }

        state.next_token = .{ .next = .{ .symbol = symbol.items } };
    }

    fn peek(state: *Tokenizer) !?Token {
        if (state.next_token == .uninitialized) {
            try state.shift();
        }

        if (state.next_token == .eof) {
            return null;
        }

        return state.next_token.next;
    }

    fn peekOrFail(state: *Tokenizer) !Token {
        return (try state.peek()) orelse return error.EOF;
    }

    fn skipWhitespace(state: *Tokenizer) void {
        while (true) {
            while (isWhitespace(state.peekChar() orelse return)) {
                state.shiftChar();
            }

            if (state.peekChar() == ';') {
                state.shiftChar();

                while ((state.peekChar() orelse return) != '\n') {
                    state.shiftChar();
                }
            } else {
                break;
            }
        }
    }
};

pub const ParseNodeType = enum { list, integerLiteral, symbol };

pub const ListNode = struct {
    const Self = @This();

    item: *ParseNode,
    rest: ?*Self,

    pub fn nth(list: ?*Self, index: usize) ?*Self {
        var result = list;

        var i: usize = 0;

        while (result) |result_| : (i += 1) {
            if (i == index) {
                return result;
            }

            result = result_.rest;
        }

        return result;
    }
};

pub const ParseNode = union(ParseNodeType) { list: ?*ListNode, integerLiteral: i64, symbol: []const u8 };

const ParseState = struct { allocator: Allocator, tokenizer: Tokenizer };

const ArrayList = std.ArrayList;

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

fn isWhitespace(x: u8) bool {
    return x == ' ' or x == '\t' or x == '\n';
}

const ParseError = error{ OutOfMemory, UnknownChar, EOF, UnmatchedClosePar, Overflow };

fn parseNode(state: *ParseState) ParseError!*ParseNode {
    const current = try state.tokenizer.peekOrFail();

    try state.tokenizer.shift();

    switch (current) {
        .symbol => |value| {
            const node = try state.allocator.create(ParseNode);
            node.* = .{ .symbol = value };
            return node;
        },

        .integerLiteral => |value| {
            const node = try state.allocator.create(ParseNode);
            node.* = .{ .integerLiteral = value };
            return node;
        },

        .openPar => {
            const list = try parseRestOfList(state);

            const node = try state.allocator.create(ParseNode);
            node.* = .{ .list = list };
            return node;
        },

        .closePar => {
            return error.UnmatchedClosePar;
        },
    }
}

fn parseRestOfList(state: *ParseState) ParseError!?*ListNode {
    switch (try state.tokenizer.peekOrFail()) {
        .closePar => {
            try state.tokenizer.shift();
            return null;
        },

        else => {
            const node = try state.allocator.create(ListNode);
            const item = try parseNode(state);
            const rest = try parseRestOfList(state);
            node.* = ListNode{ .item = item, .rest = rest };
            return node;
        },
    }
}

pub fn parse(str: []const u8, allocator: Allocator) ParseError!*ParseNode {
    var state = ParseState{ .allocator = allocator, .tokenizer = .{ .rest = str, .allocator = allocator, .next_token = .uninitialized, .next_char = .uninitialized } };

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

test "basic test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var string = std.ArrayList(u8).init(std.testing.allocator);
    defer string.deinit();

    const result = parse("(123 (456 789) () neat)", allocator) catch unreachable;

    try show(result, &string);

    try std.testing.expectEqualStrings("(123 (456 789) () neat)", string.items);
}
