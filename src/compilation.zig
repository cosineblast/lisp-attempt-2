const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const parsing = @import("parsing.zig");

const ParseNode = parsing.ParseNode;
const ParseNodeType = parsing.ParseNodeType;

const rt = @import("runtime.zig");

pub const Expression = union(enum) {
    integer: i64, //
    variable: []const u8,
    function_call: struct { name: *Expression, arguments: []*Expression },
    if_expresssion: struct { condition: *Expression, then_branch: *Expression, else_branch: *Expression },
    let_expression: struct {
        name: []const u8,
        value: *Expression,
        body: *Expression,
    },
    lambda: struct { parameters: [][]const u8, body: *Expression },
    true_expression,
    false_expression,

    begin_expression: []*Expression,
    nil,

    fn onHeap(value: Expression, allocator: Allocator) !*Expression {
        const result = try allocator.create(Expression);
        result.* = value;
        return result;
    }
};

const ExpressionType = std.meta.Tag(Expression);

const Traslation = struct {};

const TranslationError = error{ OutOfMemory, InvalidSyntax };

pub fn translate(node: *ParseNode, allocator: Allocator) TranslationError!*Expression {
    switch (node.*) {
        .list => |list| {
            const call = parsing.ListNode.nth(list, 0) orelse return error.InvalidSyntax;

            switch (call.item.*) {
                .symbol => {
                    if (std.mem.eql(u8, "begin", call.item.symbol)) {
                        return translateBegin(node, allocator);
                    } else if (std.mem.eql(u8, "if", call.item.symbol)) {
                        return translateIf(node, allocator);
                    } else if (std.mem.eql(u8, "let", call.item.symbol)) {
                        return translateLet(node, allocator);
                    } else if (std.mem.eql(u8, "lambda", call.item.symbol)) {
                        return translateLambda(node, allocator);
                    } else {
                        return translateCall(node, allocator);
                    }
                },

                else => {
                    return translateCall(node, allocator);
                },
            }
        },

        .symbol => |value| {
            const result = try allocator.create(Expression);
            // possible zig issue:
            // when assigning a value to a tagged enum which was created from undefined
            // or from malloc, the debug mode checks if the current value is okay, but it
            // implodes because the original tag is invalid.

            if (std.mem.eql(u8, "true", value)) {
                result.* = .true_expression;
            } else if (std.mem.eql(u8, "false", value)) {
                result.* = .false_expression;
            } else if (std.mem.eql(u8, "nil", value)) {
                result.* = .nil;
            } else {
                result.* = .{ .variable = value };
            }

            return result;
        },

        .integerLiteral => |i| {
            const result = try allocator.create(Expression);
            result.* = .{ .integer = i };
            return result;
        },
    }
}

fn translateIf(node: *ParseNode, allocator: Allocator) TranslationError!*Expression {
    const list = node.list;

    const condition = parsing.ListNode.nth(list, 1) orelse return error.InvalidSyntax;
    const then_branch = parsing.ListNode.nth(list, 2) orelse return error.InvalidSyntax;
    const else_branch = parsing.ListNode.nth(list, 3) orelse return error.InvalidSyntax;

    if (parsing.ListNode.nth(list, 4) != null) {
        return error.InvalidSyntax;
    }

    const condition_expr = try translate(condition.item, allocator);
    const then_branch_expr = try translate(then_branch.item, allocator);
    const else_branch_expr = try translate(else_branch.item, allocator);

    const result = try allocator.create(Expression);
    result.* = .{ .if_expresssion = .{ .condition = condition_expr, .then_branch = then_branch_expr, .else_branch = else_branch_expr } };
    return result;
}

fn translateBegin(node: *ParseNode, allocator: Allocator) TranslationError!*Expression {
    const list = node.list;

    var current = list;

    var expressions = std.ArrayList(*Expression).init(allocator);

    while (current) |current_| {
        try expressions.append(try translate(current_.item, allocator));
        current = current_.rest;
    }

    const result = try allocator.create(Expression);
    result.* = .{ .begin_expression = expressions.items };
    return result;
}

fn translateLet(node: *ParseNode, allocator: Allocator) TranslationError!*Expression {
    const list = node.list;

    const name = parsing.ListNode.nth(list, 1) orelse return error.InvalidSyntax;
    const value = parsing.ListNode.nth(list, 2) orelse return error.InvalidSyntax;
    const body = parsing.ListNode.nth(list, 3) orelse return error.InvalidSyntax;

    if (name.item.* != ParseNodeType.symbol) {
        return error.InvalidSyntax;
    }

    if (parsing.ListNode.nth(list, 4) != null) {
        return error.InvalidSyntax;
    }

    const value_expr = try translate(value.item, allocator);
    const body_expr = try translate(body.item, allocator);

    const result = try allocator.create(Expression);
    result.* = .{ .let_expression = .{ .name = name.item.symbol, .value = value_expr, .body = body_expr } };
    return result;
}

fn translateLambda(node: *ParseNode, allocator: Allocator) TranslationError!*Expression {
    const list = node.list;

    const args = parsing.ListNode.nth(list, 1) orelse return error.InvalidSyntax;
    const body = parsing.ListNode.nth(list, 2) orelse return error.InvalidSyntax;

    if (parsing.ListNode.nth(list, 3) != null) {
        return error.InvalidSyntax;
    }

    if (args.item.* != ParseNodeType.list) {
        return error.InvalidSyntax;
    }

    var current = args.item.list;

    var names = std.ArrayList([]const u8).init(allocator);

    while (current) |current_| {
        const name = current_.item;
        if (name.* != ParseNodeType.symbol) {
            return error.InvalidSyntax;
        }
        try names.append(name.symbol);
        current = current_.rest;
    }

    const body_expr = try translate(body.item, allocator);

    const result = try allocator.create(Expression);
    result.* = .{ .lambda = .{ .parameters = names.items, .body = body_expr } };
    return result;
}

fn translateCall(node: *ParseNode, allocator: Allocator) TranslationError!*Expression {
    const list = node.list;

    const function_node = parsing.ListNode.nth(list, 0) orelse return error.InvalidSyntax;
    const function_expr = try translate(function_node.item, allocator);

    var arguments = ArrayList(*Expression).init(allocator);
    var current = list.?.rest;

    while (current) |arg| {
        try arguments.append(try translate(arg.item, allocator));
        current = arg.rest;
    }

    const result = try allocator.create(Expression);
    result.* = .{ .function_call = .{ .name = function_expr, .arguments = arguments.items } };
    return result;
}

test "translation does not fail" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const examples = [_]std.meta.Tuple(&.{ []const u8, ExpressionType }){
        .{ "true", .true_expression }, //
        .{ "false", .false_expression },
        .{ "1", .integer },
        .{ "123", .integer },
        .{ "12345", .integer },
        .{ "beep", .variable },
        .{ "nil", .nil },
        .{ "(begin (f 10) false)", .begin_expression },
        .{ "(if true 1 2)", .if_expresssion },
        .{ "(((f 10) 20) 30)", .function_call },
        .{ "(let x 10 (+ x 10))", .let_expression },
        .{ "(lambda (x y z) (+ x y z))", .lambda },
    };

    for (examples) |example| {
        const tree = try parsing.parse(example.@"0", allocator);
        const result = try translate(tree, allocator);

        const result_type: ExpressionType = result.*;

        try std.testing.expectEqual(example.@"1", result_type);
    }
}
