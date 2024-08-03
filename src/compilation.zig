const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const parsing = @import("parsing.zig");

const ParseNode = parsing.ParseNode;
const ParseNodeType = parsing.ParseNodeType;

const rt = @import("runtime.zig");

const Tuple = std.meta.Tuple;

pub const Expression = union(enum) {
    const Call = struct { name: *Expression, arguments: []*Expression };
    const If = struct { condition: *Expression, then_branch: *Expression, else_branch: *Expression };
    const Let = struct {
        name: []const u8,
        value: *Expression,
        body: *Expression,
    };

    const Lambda = struct { parameters: [][]const u8, body: *Expression };

    integer: i64, //
    variable: []const u8,
    function_call: Call,
    if_expresssion: If,
    let_expression: Let,
    lambda: Lambda,
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

    var current = parsing.ListNode.nth(list, 1);

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

pub fn showExpression(expression: *Expression, out: *ArrayList(u8)) !void {
    try std.json.stringify(expression.*, .{}, out.writer());
}

const analysis = struct {
    const Error = error{OutOfMemory};

    fn findFreeVariables(expr: *Expression, bound: *ArrayList([]const u8), free: *ArrayList([]const u8)) Error!void {
        switch (expr.*) {
            .variable => |name| {
                var found: bool = false;
                for (bound.items) |item| {
                    if (std.mem.eql(u8, item, name)) {
                        found = true;
                    }
                }
                if (!found) {
                    try free.append(name);
                }
            },
            .function_call => |call| {
                try findFreeVariables(call.name, bound, free);

                for (call.arguments) |item| {
                    try findFreeVariables(item, bound, free);
                }
            },
            .if_expresssion => |if_expr| {
                try findFreeVariables(if_expr.condition, bound, free);
                try findFreeVariables(if_expr.then_branch, bound, free);
                try findFreeVariables(if_expr.else_branch, bound, free);
            },
            .let_expression => |let_expr| {
                try findFreeVariables(let_expr.value, bound, free);
                try bound.append(let_expr.name);
                try findFreeVariables(let_expr.body, bound, free);
                _ = bound.pop();
            },
            .lambda => |lambda_expr| {
                for (lambda_expr.parameters) |parameter| {
                    try bound.append(parameter);
                }
                try findFreeVariables(lambda_expr.body, bound, free);

                for (0..lambda_expr.parameters.len) |_| {
                    _ = bound.pop();
                }
            },
            .begin_expression => |expressions| {
                for (expressions) |item| {
                    try findFreeVariables(item, bound, free);
                }
            },
            .integer => {},
            .true_expression => {},
            .false_expression => {},
            .nil => {},
        }
    }
};

pub const Compilation = struct { //
    const Self = @This();

    const Binding = struct { name: []const u8, frame_offset: usize };

    const IntRefPair = struct { value: i64, reference: u8 };

    const Error = error{ OutOfMemory, OtherCompilationError };

    const OtherError = union(enum) {
        OutOfMemory,
        UnkownVariable: []const u8,
    };

    allocator: Allocator,
    lambda_builder: rt.LambdaBuilder,
    frame_size: usize,
    local_bindings: ArrayList(Binding),
    integer_literals: ArrayList(IntRefPair),
    true_literal_ref: ?u8,
    false_literal_ref: ?u8,
    nil_literal_ref: ?u8,
    issue: ?OtherError,

    pub fn init(allocator: Allocator) Compilation {
        return .{ //
            .allocator = allocator,
            .lambda_builder = rt.LambdaBuilder.init(allocator),
            .frame_size = 0,
            .local_bindings = ArrayList(Binding).init(allocator),
            .integer_literals = ArrayList(IntRefPair).init(allocator),
            .true_literal_ref = null,
            .false_literal_ref = null,
            .nil_literal_ref = null,
            .issue = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.local_bindings.deinit();
        self.integer_literals.deinit();
    }

    // The expected behavior of all compile functions, is that
    // they will insert code which when ran, will add a single value to the stack
    // which corresponds to the value of the expression in the source
    pub fn compileExpression(self: *Self, expression: *Expression) Error!void {
        const before = self.frame_size;

        switch (expression.*) {
            .integer => |value| {
                try self.compileIntegerLiteral(value);
            },
            .variable => |name| {
                try self.compileVariable(name);
            },
            .function_call => |call| {
                try self.compileFunctionCall(call);
            },
            .if_expresssion => |value| {
                try self.compileIfExpression(value);
            },
            .let_expression => |value| {
                try self.compileLetExpression(value);
            },
            .lambda => |lambda_expr| {
                try self.compileLambdaExpression(lambda_expr);
            },
            .begin_expression => |expressions| {
                for (expressions) |item| {
                    try self.compileExpression(item);
                }
                try self.lambda_builder.addInstruction(.{ .rip = .{ .drop = @intCast(expressions.len), .keep = 0 } });
            },
            .true_expression => {
                try self.compileSingleton(&self.true_literal_ref, .{ .boolean = true });
            },
            .false_expression => {
                try self.compileSingleton(&self.false_literal_ref, .{ .boolean = false });
            },
            .nil => {
                try self.compileSingleton(&self.false_literal_ref, .nil);
            },
        }

        self.frame_size = before + 1;
    }

    fn compileIntegerLiteral(self: *Self, value: i64) Error!void {
        var ref: ?u8 = null;
        for (self.integer_literals.items) |literal| {
            if (literal.value == value) {
                ref = literal.reference;
                break;
            }
        }

        ref = ref orelse blk: {
            const next = self.lambda_builder.addValue(.{ .integer = value });

            try self.integer_literals.append(.{ .reference = next, .value = value });

            break :blk next;
        };

        try self.lambda_builder.addInstruction(.{ //
            .load = .{ .value = ref.? },
        });
    }

    fn compileVariable(self: *Self, value: []const u8) Error!void {
        const lookup_frame_offset = self.lookupLocal(value);

        const frame_offset = lookup_frame_offset orelse {
            self.issue = .{ .UnkownVariable = value };
            return error.OtherCompilationError;
        };

        const stack_offset = self.computeStackOffset(frame_offset);

        try self.lambda_builder.addInstruction(.{ .pick = .{ .offset = @intCast(stack_offset) } });
    }

    fn computeStackOffset(self: *Self, frame_offset: usize) usize {
        return self.frame_size - frame_offset - 1;
    }

    fn lookupLocal(self: *Self, name: []const u8) ?usize {
        var result: ?usize = 0;

        var i = self.local_bindings.items.len;

        while (i > 0) {
            i -= 1;

            if (std.mem.eql(u8, self.local_bindings.items[i].name, name)) {
                result = self.local_bindings.items[i].frame_offset;
            }
        }

        return result;
    }

    fn compileFunctionCall(self: *Self, call: Expression.Call) Error!void {
        for (call.arguments) |arg| {
            try self.compileExpression(arg);
        }

        try self.compileExpression(call.name);

        try self.lambda_builder.addInstruction(.{ .call = .{ .arg_count = @intCast(call.arguments.len) } });
    }

    fn compileIfExpression(self: *Self, value: Expression.If) Error!void {
        try self.compileExpression(value.condition);

        const jf_index = self.lambda_builder.nextOffset();
        try self.lambda_builder.addInstruction(.{ .jf = .{ .offset = 0 } });

        const before_then = self.lambda_builder.insertedSoFar();
        try self.compileExpression(value.then_branch);
        const after_then = self.lambda_builder.insertedSoFar();

        self.lambda_builder.setInstruction(jf_index, .{ .jf = .{ .offset = @intCast(1 + after_then - before_then) } });

        const jmp_index = self.lambda_builder.nextOffset();
        try self.lambda_builder.addInstruction(.{ .jmp = .{ .offset = 0 } });

        const before_else = self.lambda_builder.insertedSoFar();
        try self.compileExpression(value.else_branch);
        const after_else = self.lambda_builder.insertedSoFar();

        self.lambda_builder.setInstruction(jmp_index, .{ .jmp = .{ .offset = @intCast(1 + after_else - before_else) } });
    }

    fn compileLetExpression(self: *Self, value: Expression.Let) Error!void {
        const size = self.frame_size;
        try self.compileExpression(value.value);

        try self.local_bindings.append(.{ .name = value.name, .frame_offset = size });

        try self.compileExpression(value.body);

        try self.lambda_builder.addInstruction(.{ .rip = .{ .drop = 1, .keep = 1 } });

        _ = self.local_bindings.pop();
    }

    fn compileSingleton(self: *Self, id_ptr: *?u8, value: rt.Value) Error!void {
        const id = blk: {
            if (id_ptr.*) |it| {
                break :blk it;
            } else {
                const it = self.lambda_builder.addValue(value);
                id_ptr.* = it;
                break :blk it;
            }
        };

        try self.lambda_builder.addInstruction(.{ .load = .{ .value = id } });
    }

    fn compileLambdaExpression(self: *Self, expr: Expression.Lambda) Error!void {
        var bindings = ArrayList(Binding).init(self.allocator);
        defer bindings.deinit();

        for (expr.parameters) |parameter| {
            try bindings.append(.{ .name = parameter, .frame_offset = bindings.items.len });
        }

        var bound = ArrayList([]const u8).init(self.allocator);
        defer bound.deinit();

        var free = ArrayList([]const u8).init(self.allocator);
        defer bound.deinit();

        try analysis.findFreeVariables(expr.body, &bound, &free);

        var context_length: usize = 0;

        for (free.items) |item| {
            if (self.lookupLocal(item)) |offset| {
                try bindings.append(.{ .name = item, .frame_offset = bindings.items.len });

                try self.lambda_builder.addInstruction(.{ .pick = .{ .offset = @intCast(self.computeStackOffset(offset)) } });

                context_length += 1;
            }
        }

        var next = Compilation{
            .allocator = self.allocator,
            .lambda_builder = rt.LambdaBuilder.init(self.allocator),
            .frame_size = bindings.items.len,
            .local_bindings = bindings,
            .integer_literals = ArrayList(IntRefPair).init(self.allocator),
            .true_literal_ref = null,
            .false_literal_ref = null,
            .nil_literal_ref = null,
            .issue = null,
        };

        defer next.deinit();

        try next.compileLambdaBody(expr.body);

        const body = try next.lambda_builder.buildOnHeap();

        const id = self.lambda_builder.addBodyReference(body);

        try self.lambda_builder.addInstruction(.{ .loadf = .{ .in_context = @intCast(context_length), .value = id } });
    }

    fn compileLambdaBody(self: *Self, expression: *Expression) Error!void {
        try self.compileExpression(expression);

        try self.lambda_builder.addInstruction(.{ .rip = .{ .drop = @intCast(self.frame_size - 1), .keep = 1 } });
        try self.lambda_builder.addInstruction(.ret);
    }
};

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
        const str = example.@"0";

        const tree = try parsing.parse(str, allocator);
        const result = try translate(tree, allocator);

        const result_type: ExpressionType = result.*;

        try std.testing.expectEqual(example.@"1", result_type);

        var arr = std.ArrayList(u8).init(std.testing.allocator);
        defer arr.deinit();

        try showExpression(result, &arr);

        std.debug.print("{s} -> {s}\n", .{ str, arr.items });
    }
}

test "compilation does not fail" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const tree = try parsing.parse("(let x (begin 10 (if false 1 (lambda (y) y))) x)", allocator);

    const expr = try translate(tree, allocator);

    var compiler = Compilation.init(allocator);

    try compiler.compileExpression(expr);
}
