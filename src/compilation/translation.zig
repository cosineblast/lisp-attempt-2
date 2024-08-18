const std = @import("std");

const compilation = @import("../compilation.zig");
const parsing = @import("../parsing.zig");

const ParseNode = parsing.ParseNode;
const ParseNodeType = parsing.ParseNodeType;

const Allocator = std.mem.Allocator;

const Expression = compilation.Expression;

const ArrayList = std.ArrayList;

const nth = parsing.ListNode.nth;

pub fn translate(node: *ParseNode, allocator: Allocator) Error!*Expression {
    switch (node.*) {
        .list => |list| {
            const call = nth(list, 0) orelse return error.InvalidSyntax;

            switch (call.item.*) {
                .symbol => {
                    if (std.mem.eql(u8, "begin", call.item.symbol)) {
                        return translateBegin(node, allocator);
                    } else if (std.mem.eql(u8, "if", call.item.symbol)) {
                        return translateIf(node, allocator);
                    } else if (std.mem.eql(u8, "let*", call.item.symbol)) {
                        return translateLet(node, allocator);
                    } else if (std.mem.eql(u8, "lambda", call.item.symbol)) {
                        return translateLambda(node, allocator);
                    } else if (std.mem.eql(u8, "def", call.item.symbol)) {
                        return translateDef(node, allocator);
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

fn translateIf(node: *ParseNode, allocator: Allocator) Error!*Expression {
    const list = node.list;

    const condition = nth(list, 1) orelse return error.InvalidSyntax;
    const then_branch = nth(list, 2) orelse return error.InvalidSyntax;
    const else_branch = nth(list, 3) orelse return error.InvalidSyntax;

    if (nth(list, 4) != null) {
        return error.InvalidSyntax;
    }

    const condition_expr = try translate(condition.item, allocator);
    const then_branch_expr = try translate(then_branch.item, allocator);
    const else_branch_expr = try translate(else_branch.item, allocator);

    const result = try allocator.create(Expression);
    result.* = .{ .if_expresssion = .{ .condition = condition_expr, .then_branch = then_branch_expr, .else_branch = else_branch_expr } };
    return result;
}

fn translateBegin(node: *ParseNode, allocator: Allocator) Error!*Expression {
    const list = node.list;

    var current = nth(list, 1);

    var expressions = std.ArrayList(*Expression).init(allocator);

    while (current) |current_| {
        try expressions.append(try translate(current_.item, allocator));
        current = current_.rest;
    }

    const result = try allocator.create(Expression);
    result.* = .{ .begin_expression = expressions.items };
    return result;
}

fn translateLet(node: *ParseNode, allocator: Allocator) Error!*Expression {
    const list = node.list;

    const name = nth(list, 1) orelse return error.InvalidSyntax;
    const value = nth(list, 2) orelse return error.InvalidSyntax;
    const body = nth(list, 3) orelse return error.InvalidSyntax;

    if (name.item.* != ParseNodeType.symbol) {
        return error.InvalidSyntax;
    }

    if (nth(list, 4) != null) {
        return error.InvalidSyntax;
    }

    const value_expr = try translate(value.item, allocator);
    const body_expr = try translate(body.item, allocator);

    const result = try allocator.create(Expression);
    result.* = .{ .let_expression = .{ .name = name.item.symbol, .value = value_expr, .body = body_expr } };
    return result;
}

fn translateLambda(node: *ParseNode, allocator: Allocator) Error!*Expression {
    const list = node.list;

    var args: *parsing.ListNode = undefined;
    var body: *parsing.ListNode = undefined;

    const first = nth(list, 1) orelse return error.InvalidSyntax;
    const second = nth(list, 2) orelse return error.InvalidSyntax;

    var self_name: ?[]const u8 = null;

    if (first.item.* == .symbol) {
        args = second;
        body = nth(list, 3) orelse return error.InvalidSyntax;
        self_name = first.item.symbol;

        if (nth(list, 4) != null) {
            return error.InvalidSyntax;
        }
    } else {
        args = first;
        body = second;

        if (nth(list, 3) != null) {
            return error.InvalidSyntax;
        }
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
    result.* = .{ .lambda = .{ .self_name = self_name, .parameters = names.items, .body = body_expr } };
    return result;
}

fn translateCall(node: *ParseNode, allocator: Allocator) Error!*Expression {
    const list = node.list;

    const function_node = nth(list, 0) orelse return error.InvalidSyntax;
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

fn translateDef(node: *ParseNode, allocator: Allocator) Error!*Expression {
    const list = node.list;

    const name = nth(list, 1) orelse return error.InvalidSyntax;
    const value = nth(list, 2) orelse return error.InvalidSyntax;

    if (name.item.* != ParseNodeType.symbol) {
        return error.InvalidSyntax;
    }

    if (nth(list, 3) != null) {
        return error.InvalidSyntax;
    }

    const value_expr = try translate(value.item, allocator);

    const result = try allocator.create(Expression);
    result.* = .{ .def_expression = .{ .name = name.item.symbol, .value = value_expr } };
    return result;
}

const Error = error{ OutOfMemory, InvalidSyntax };
