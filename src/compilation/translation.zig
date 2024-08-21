const std = @import("std");

const compilation = @import("../compilation.zig");
const parsing = @import("../parsing.zig");

const ParseNode = parsing.ParseNode;
const ParseNodeType = parsing.ParseNodeType;

const Allocator = std.mem.Allocator;

const Expression = compilation.Expression;

const ArrayList = std.ArrayList;

const nth = parsing.ListNode.nth;

const Error = error{ OutOfMemory, TranslationError };

pub const Diagnostic = union(enum) {
    too_few_if,
    too_many_if,

    invalid_let_binding: *ParseNode,
    too_few_let,
    too_many_let,

    too_few_lambda,
    too_many_lambda,

    non_list_lambda_parameters,
    invalid_lambda_binding: *ParseNode,

    too_few_def,
    too_many_def,

    invalid_def_binding,

    empty_form,
};

const State = struct {
    allocator: Allocator,
    diagnostic: ?Diagnostic,

    fn fail(self: *State, diagnostic: Diagnostic) error{TranslationError} {
        self.diagnostic = diagnostic;
        return error.TranslationError;
    }

    fn translate(self: *State, node: *ParseNode) Error!*Expression {
        switch (node.*) {
            .list => |list| {
                const call = nth(list, 0) orelse return self.fail(.empty_form);

                switch (call.item.*) {
                    .symbol => {
                        if (std.mem.eql(u8, "begin", call.item.symbol)) {
                            return self.translateBegin(node);
                        } else if (std.mem.eql(u8, "if", call.item.symbol)) {
                            return self.translateIf(node);
                        } else if (std.mem.eql(u8, "let*", call.item.symbol)) {
                            return self.translateLet(node);
                        } else if (std.mem.eql(u8, "lambda", call.item.symbol)) {
                            return self.translateLambda(node);
                        } else if (std.mem.eql(u8, "def", call.item.symbol)) {
                            return self.translateDef(node);
                        } else {
                            return self.translateCall(node);
                        }
                    },

                    else => {
                        return self.translateCall(node);
                    },
                }
            },

            .symbol => |value| {
                const result = try self.allocator.create(Expression);
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
                const result = try self.allocator.create(Expression);
                result.* = .{ .integer = i };
                return result;
            },
        }
    }

    fn translateIf(self: *State, node: *ParseNode) Error!*Expression {
        const list = node.list;

        const condition = nth(list, 1) orelse return self.fail(.too_few_if);
        const then_branch = nth(list, 2) orelse return self.fail(.too_few_if);
        const else_branch = nth(list, 3) orelse return self.fail(.too_few_if);

        if (nth(list, 4) != null) {
            return self.fail(.too_many_if);
        }

        const condition_expr = try self.translate(condition.item);
        const then_branch_expr = try self.translate(then_branch.item);
        const else_branch_expr = try self.translate(else_branch.item);

        const result = try self.allocator.create(Expression);
        result.* = .{ .if_expresssion = .{ .condition = condition_expr, .then_branch = then_branch_expr, .else_branch = else_branch_expr } };
        return result;
    }

    fn translateBegin(self: *State, node: *ParseNode) Error!*Expression {
        const list = node.list;

        var current = nth(list, 1);

        var expressions = std.ArrayList(*Expression).init(self.allocator);

        while (current) |current_| {
            try expressions.append(try self.translate(current_.item));
            current = current_.rest;
        }

        const result = try self.allocator.create(Expression);
        result.* = .{ .begin_expression = expressions.items };
        return result;
    }

    fn translateLet(self: *State, node: *ParseNode) Error!*Expression {
        const list = node.list;

        const name = nth(list, 1) orelse return self.fail(.too_few_let);
        const value = nth(list, 2) orelse return self.fail(.too_few_let);
        const body = nth(list, 3) orelse return self.fail(.too_few_let);

        if (name.item.* != ParseNodeType.symbol) {
            return self.fail(.{ .invalid_let_binding = name.item });
        }

        if (nth(list, 4) != null) {
            return self.fail(.too_many_let);
        }

        const value_expr = try self.translate(value.item);
        const body_expr = try self.translate(body.item);

        const result = try self.allocator.create(Expression);
        result.* = .{ .let_expression = .{ .name = name.item.symbol, .value = value_expr, .body = body_expr } };
        return result;
    }

    fn translateLambda(self: *State, node: *ParseNode) Error!*Expression {
        const list = node.list;

        var args: *parsing.ListNode = undefined;
        var body: *parsing.ListNode = undefined;

        const first = nth(list, 1) orelse return self.fail(.too_few_lambda);
        const second = nth(list, 2) orelse return self.fail(.too_few_lambda);

        var self_name: ?[]const u8 = null;

        if (first.item.* == .symbol) {
            args = second;
            body = nth(list, 3) orelse return self.fail(.too_few_lambda);
            self_name = first.item.symbol;

            if (nth(list, 4) != null) {
                return self.fail(.too_many_lambda);
            }
        } else {
            args = first;
            body = second;

            if (nth(list, 3) != null) {
                return self.fail(.too_many_lambda);
            }
        }

        if (args.item.* != ParseNodeType.list) {
            return self.fail(.non_list_lambda_parameters);
        }

        var current = args.item.list;

        var names = std.ArrayList([]const u8).init(self.allocator);
        errdefer names.deinit();

        while (current) |current_| {
            const name = current_.item;
            if (name.* != ParseNodeType.symbol) {
                return self.fail(.{ .invalid_lambda_binding = name });
            }
            try names.append(name.symbol);
            current = current_.rest;
        }

        const body_expr = try self.translate(body.item);

        const result = try self.allocator.create(Expression);
        result.* = .{ .lambda = .{ .self_name = self_name, .parameters = names.items, .body = body_expr } };
        return result;
    }

    fn translateCall(self: *State, node: *ParseNode) Error!*Expression {
        const list = node.list;

        const function_node = nth(list, 0) orelse return self.fail(.empty_form);
        const function_expr = try self.translate(function_node.item);

        var arguments = ArrayList(*Expression).init(self.allocator);
        var current = list.?.rest;

        while (current) |arg| {
            try arguments.append(try self.translate(arg.item));
            current = arg.rest;
        }

        const result = try self.allocator.create(Expression);
        result.* = .{ .function_call = .{ .name = function_expr, .arguments = arguments.items } };
        return result;
    }

    fn translateDef(self: *State, node: *ParseNode) Error!*Expression {
        const list = node.list;

        const name = nth(list, 1) orelse return self.fail(.too_few_def);
        const value = nth(list, 2) orelse return self.fail(.too_few_def);

        if (name.item.* != ParseNodeType.symbol) {
            return self.fail(.invalid_def_binding);
        }

        if (nth(list, 3) != null) {
            return self.fail(.too_many_def);
        }

        const value_expr = try self.translate(value.item);

        const result = try self.allocator.create(Expression);
        result.* = .{ .def_expression = .{ .name = name.item.symbol, .value = value_expr } };
        return result;
    }
};

pub fn translate(node: *ParseNode, allocator: Allocator, out_diagnostic_: ?*Diagnostic) Error!*Expression {
    var state = State{ .allocator = allocator, .diagnostic = null };

    return state.translate(node) catch |issue| {
        if (issue == error.TranslationError) {
            if (out_diagnostic_) |out_diagnostic| {
                out_diagnostic.* = state.diagnostic.?;
            }
        }

        return issue;
    };
}
