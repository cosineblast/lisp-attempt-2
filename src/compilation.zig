const std = @import("std");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const parsing = @import("parsing.zig");

const ParseNode = parsing.ParseNode;
const ParseNodeType = parsing.ParseNodeType;

const rt = @import("runtime.zig");

const Tuple = std.meta.Tuple;

pub const translation = @import("compilation/translation.zig");

const LambdaBuilder = @import("LambdaBuilder.zig");

const Immediate = rt.LambdaBody.Immediate;

pub const Expression = union(enum) {
    const Call = struct { name: *Expression, arguments: []*Expression };
    const If = struct { condition: *Expression, then_branch: *Expression, else_branch: *Expression };
    const Let = struct {
        name: []const u8,
        value: *Expression,
        body: *Expression,
    };

    const Lambda =
        struct { self_name: ?[]const u8, parameters: [][]const u8, body: *Expression };

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
    def_expression: struct { name: []const u8, value: *Expression },

    fn onHeap(value: Expression, allocator: Allocator) !*Expression {
        const result = try allocator.create(Expression);
        result.* = value;
        return result;
    }
};

const ExpressionType = std.meta.Tag(Expression);

pub fn showExpression(expression: *Expression, out: *ArrayList(u8)) !void {
    try std.json.stringify(expression.*, .{ .whitespace = .indent_2 }, out.writer());
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

                if (lambda_expr.self_name) |name| {
                    try bound.append(name);
                }

                try findFreeVariables(lambda_expr.body, bound, free);

                if (lambda_expr.self_name) |_| {
                    _ = bound.pop();
                }

                for (0..lambda_expr.parameters.len) |_| {
                    _ = bound.pop();
                }
            },
            .begin_expression => |expressions| {
                for (expressions) |item| {
                    try findFreeVariables(item, bound, free);
                }
            },
            .def_expression => |it| {
                try findFreeVariables(it.value, bound, free);
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
    const Error = error{ OutOfMemory, OtherCompilationError };

    const OtherError = union(enum) {
        OutOfMemory,
    };

    allocator: Allocator,
    lambda_builder: LambdaBuilder,
    frame_size: usize,

    local_bindings: ArrayList(Binding),
    integer_literals: std.AutoHashMap(i64, u16),
    global_ref_table: std.StringHashMap(u16),

    true_literal_id: ?u16,
    false_literal_id: ?u16,
    nil_literal_id: ?u16,
    issue: ?OtherError,
    self_name: ?[]const u8,

    pub fn init(allocator: Allocator) Compilation {
        return .{ //
            .allocator = allocator,
            .lambda_builder = LambdaBuilder.init(allocator),
            .frame_size = 0,
            .local_bindings = ArrayList(Binding).init(allocator),
            .integer_literals = std.AutoHashMap(i64, u16).init(allocator),
            .global_ref_table = std.StringHashMap(u16).init(allocator),
            .true_literal_id = null,
            .false_literal_id = null,
            .nil_literal_id = null,
            .issue = null,
            .self_name = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.local_bindings.deinit();
        self.integer_literals.deinit();
        self.global_ref_table.deinit();
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
                try self.lambda_builder.addInstruction(.{ .rip = .{ .drop = @intCast(expressions.len - 1), .keep = 1 } });
            },
            .def_expression => |it| {
                try self.compileExpression(it.value);
                const id = try self.getGlobal(it.name);
                try self.lambda_builder.addInstruction(.{ .defg = .{ .id = @intCast(id) } });
            },
            .true_expression => {
                try self.compileSingleton(&self.true_literal_id, .{ .boolean = true });
            },
            .false_expression => {
                try self.compileSingleton(&self.false_literal_id, .{ .boolean = false });
            },
            .nil => {
                try self.compileSingleton(&self.false_literal_id, .nil);
            },
        }

        self.frame_size = before + 1;
    }

    fn compileIntegerLiteral(self: *Self, value: i64) Error!void {
        const id_: ?u16 = self.integer_literals.get(value);

        const id = id_ orelse blk: {
            const next = self.lambda_builder.addImmediate(.{ .integer = value });

            try self.integer_literals.put(value, next);

            break :blk next;
        };

        try self.lambda_builder.addInstruction(.{ //
            .load = .{ .id = id },
        });
    }

    fn compileVariable(self: *Self, name: []const u8) Error!void {
        if (self.self_name) |self_name| {
            if (std.mem.eql(u8, name, self_name)) {
                try self.lambda_builder.addInstruction(.load_self);

                return;
            }
        }

        if (self.lookupLocal(name)) |frame_offset| {
            const stack_offset = self.computeStackOffset(frame_offset);

            try self.lambda_builder.addInstruction(.{ .pick = .{ .offset = @intCast(stack_offset) } });
        } else {
            const id = try self.getGlobal(name);

            try self.lambda_builder.addInstruction(.{ .loadg = .{ .id = @intCast(id) } });
        }
    }

    fn getGlobal(self: *Self, name: []const u8) !u16 {
        return self.global_ref_table.get(name) orelse blk: {
            const id = self.lambda_builder.addGlobalReference(name);
            try self.global_ref_table.put(name, id);
            break :blk id;
        };
    }

    fn computeStackOffset(self: *Self, frame_offset: usize) usize {
        return self.frame_size - frame_offset - 1;
    }

    fn lookupLocal(self: *Self, name: []const u8) ?usize {
        var result: ?usize = null;

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

        self.frame_size -= 1; // jf pops one value

        const before_then = self.lambda_builder.insertedSoFar();
        try self.compileExpression(value.then_branch);
        const after_then = self.lambda_builder.insertedSoFar();

        // it would naturally be +1 due to zero-based indexing,
        // but there is also the jmp so it is +2
        self.lambda_builder.setInstruction(jf_index, .{ .jf = .{ .offset = @intCast(2 + after_then - before_then) } });

        const jmp_index = self.lambda_builder.nextOffset();
        try self.lambda_builder.addInstruction(.{ .jmp = .{ .offset = 0 } });

        self.frame_size -= 1; // we're back to the original frame size

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

    fn compileSingleton(self: *Self, id_ptr: *?u16, value: Immediate) Error!void {
        const id = blk: {
            if (id_ptr.*) |it| {
                break :blk it;
            } else {
                const it = self.lambda_builder.addImmediate(value);
                id_ptr.* = it;
                break :blk it;
            }
        };

        try self.lambda_builder.addInstruction(.{ .load = .{ .id = id } });
    }

    fn compileLambdaExpression(self: *Self, expr: Expression.Lambda) Error!void {
        var bindings = ArrayList(Binding).init(self.allocator);

        for (expr.parameters) |parameter| {
            try bindings.append(.{ .name = parameter, .frame_offset = bindings.items.len });
        }

        var bound = ArrayList([]const u8).init(self.allocator);
        defer bound.deinit();

        var free = ArrayList([]const u8).init(self.allocator);
        defer free.deinit();

        try analysis.findFreeVariables(expr.body, &bound, &free);

        var context_length: usize = 0;

        for (free.items) |item| {
            if (self.lookupLocal(item)) |offset| {
                try bindings.append(.{ .name = item, .frame_offset = bindings.items.len });

                try self.lambda_builder.addInstruction(.{ .pick = .{ .offset = @intCast(self.computeStackOffset(offset)) } });

                context_length += 1;
            }
        }

        var next = Compilation{ //
            .allocator = self.allocator,
            .lambda_builder = LambdaBuilder.init(self.allocator),
            .frame_size = bindings.items.len,
            .local_bindings = bindings,
            .integer_literals = std.AutoHashMap(i64, u16).init(self.allocator),
            .true_literal_id = null,
            .false_literal_id = null,
            .nil_literal_id = null,
            .issue = null,
            .global_ref_table = std.StringHashMap(u16).init(self.allocator),
            .self_name = expr.self_name,
        };

        next.lambda_builder.setParameterCount(@intCast(expr.parameters.len));

        defer next.deinit();

        try next.compileLambdaBody(expr.body);

        const body = try next.lambda_builder.buildOnHeap();

        const id = self.lambda_builder.addBodyReference(body);

        try self.lambda_builder.addInstruction(.{ .loadf = .{ .in_context = @intCast(context_length), .id = id } });
    }

    fn compileLambdaBody(self: *Self, expression: *Expression) Error!void {
        try self.compileExpression(expression);

        try self.lambda_builder.addInstruction(.{ .rip = .{ .drop = @intCast(self.frame_size - 1), .keep = 1 } });
        try self.lambda_builder.addInstruction(.ret);
    }
};

pub fn compile(expression: *Expression, allocator: Allocator) Compilation.Error!*rt.LambdaBody {
    var compiler = Compilation.init(allocator);
    defer compiler.deinit();

    try compiler.compileExpression(expression);

    var body = try compiler.lambda_builder.buildOnHeap();
    errdefer body.down(allocator);

    return body;
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
        .{ "(let* x 10 (+ x 10))", .let_expression },
        .{ "(lambda (x y z) (+ x y z))", .lambda },
    };

    for (examples) |example| {
        const str = example.@"0";

        const tree = try parsing.parse(str, allocator);
        const result = try translation.translate(tree, allocator);

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

    const expr = try translation.translate(tree, allocator);

    var compiler = Compilation.init(allocator);

    try compiler.compileExpression(expr);
}
