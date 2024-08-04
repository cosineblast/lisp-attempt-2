const std = @import("std");

const parsing = @import("./parsing.zig");

const Tuple = std.meta.Tuple;

pub const VM = @import("./runtime/VM.zig");

const LambdaBuilder = @import("./LambdaBuilder.zig");

pub const Instruction = union(enum) {
    call: struct { arg_count: u8 },
    tcall: struct { arg_count: u8 },
    pick: struct { offset: u16 },
    jf: struct { offset: u16 },
    jmp: struct { offset: u16 },
    load: struct { id: u16 },
    loadg: struct { id: u16 },
    loadf: struct { id: u16, in_context: u16 },
    rip: struct { drop: u16, keep: u8 },
    ret,
    nop,
};

pub const InstructionType = std.meta.Tag(Instruction);

pub const ListObject = struct { item: Value, next: ?*ListObject };

pub const LambdaBody = struct { //
    code: std.ArrayListUnmanaged(Instruction), //
    immediate_table: [256]Value,
    immediate_count: usize,
    parameter_count: ?u8,
    other_bodies: [256]*LambdaBody,
    other_body_count: usize,
    ref_count: usize = 1,
    global_table: [256][]const u8,
    global_count: usize,

    pub fn down(body: *LambdaBody, allocator: std.mem.Allocator) void {
        std.debug.assert(body.ref_count > 0);

        if (body.ref_count == 1) {
            for (0..body.other_body_count) |i| {
                body.other_bodies[i].down(allocator);
            }
            body.code.deinit(allocator);
            allocator.destroy(body);
        } else {
            body.ref_count -= 1;
        }
    }

    pub fn up(body: *LambdaBody) void {
        std.debug.assert(body.ref_count > 0);

        body.ref_count += 1;
    }
};

pub const BytecodeLambda = struct {
    body: *LambdaBody,
    context: std.ArrayListUnmanaged(Value),
};

pub const Value = union(enum) {
    nil, //
    list: *ListObject,
    integer: i64,
    boolean: bool,
    lambda: *BytecodeLambda,
    real_function: *const fn (state: *VM, count: u8) anyerror!void,

    pub fn format(self: Value, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .integer => |i| {
                try writer.print("{}", .{i});
            },
            .boolean => |x| {
                try writer.print("{}", .{x});
            },
            .lambda => {
                try writer.print("<bfn>", .{});
            },
            .real_function => {
                try writer.print("<rfn>", .{});
            },
            .list => {
                try writer.print("(...)", .{});
            },
            .nil => {
                try writer.print("nil", .{});
            },
        }
    }
};

pub const ValueType = std.meta.Tag(Value);

pub fn dump(lambda: *const LambdaBody) void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var arr = std.ArrayList(u8).init(allocator);
    defer arr.deinit();

    const writer = arr.writer();

    for (lambda.code.items) |item| {
        std.json.stringify(item, .{}, writer) catch unreachable;
        writer.print("\n", .{}) catch unreachable;
    }

    std.debug.print("{s}\n", .{arr.items});

    std.debug.print("immediate table:\n", .{});

    for (0..lambda.immediate_count) |i| {
        std.debug.print("{}: ", .{i});
        print_value(&lambda.immediate_table[i]);
        std.debug.print("\n", .{});
    }

    std.debug.print("\nother bodies {{\n", .{});

    for (0..lambda.other_body_count) |i| {
        std.debug.print("[0] => \n", .{});
        dump(lambda.other_bodies[i]);
    }

    std.debug.print("}}\n", .{});
}

fn print_value(value: *const Value) void {
    switch (value.*) {
        .integer => |i| {
            std.debug.print("{}", .{i});
        },
        .boolean => |x| {
            std.debug.print("{}", .{x});
        },
        .lambda => {
            std.debug.print("<bfn>", .{});
        },
        .real_function => {
            std.debug.print("<cfn>", .{});
        },
        .list => {
            std.debug.print("(...)", .{});
        },
        .nil => {
            std.debug.print("nil", .{});
        },
    }
}

test "basic instruction test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var b = LambdaBuilder.init(allocator);
    const nah = b.addImmediate(.{ .boolean = false });
    const ten = b.addImmediate(.{ .integer = 10 });
    const twenty = b.addImmediate(.{ .integer = 20 });

    try b.addInstruction(Instruction{ .load = .{ .id = ten } });
    try b.addInstruction(Instruction{ .load = .{ .id = twenty } });
    try b.addInstruction(Instruction{ .load = .{ .id = nah } });
    try b.addInstruction(Instruction{ .jf = .{ .offset = 4 } });
    try b.addInstruction(Instruction{ .pick = .{ .offset = 1 } });
    try b.addInstruction(Instruction{ .rip = .{ .keep = 1, .drop = 2 } });
    try b.addInstruction(Instruction{ .jmp = .{ .offset = 1 } });
    try b.addInstruction(Instruction{ .rip = .{
        .drop = 1,
        .keep = 1,
    } });
    try b.addInstruction(Instruction.nop);

    var lambda_body = b.build();
    var lambda: BytecodeLambda = undefined;
    lambda.context = .{ .items = &.{}, .capacity = 0 };
    lambda.body = &lambda_body;
    var frame: VM.Frame = undefined;
    frame.instruction_offset = 0;
    frame.body = lambda.body;

    dump(lambda.body);

    var machine: VM = undefined;
    machine.allocator = allocator;
    machine.active_frame = frame;
    machine.call_stack = std.ArrayList(VM.Frame).init(allocator);
    machine.stack = std.ArrayList(Value).init(allocator);

    try machine.execute();

    try std.testing.expectEqual(20, machine.stack.items[0].integer);
}
