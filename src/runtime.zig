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

pub const LambdaObject = struct {
    body: *LambdaBody,
    context: std.ArrayListUnmanaged(Value),
    // gc tag
};

pub const StringContent = struct {
    items: []const u8,
    // gc tag
};

pub const StringObject = struct { //
    content: *StringContent,
    offset: usize,
    len: usize,

    pub fn items(self: *StringObject) []const u8 {
        return self.content.items[self.offset .. self.offset + self.len];
    }
};

pub const Value = union(enum) {
    nil, //
    list: *ListObject,
    integer: i64,
    boolean: bool,
    lambda: *LambdaObject,
    string: *StringObject,

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
            .string => |str| {
                try writer.print("\"{s}\"", .{str.items()});
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
        std.debug.print("{}", .{lambda.immediate_table[i]});
        std.debug.print("\n", .{});
    }

    std.debug.print("\nother bodies {{\n", .{});

    for (0..lambda.other_body_count) |i| {
        std.debug.print("[0] => \n", .{});
        dump(lambda.other_bodies[i]);
    }

    std.debug.print("}}\n", .{});
}

test "basic instruction test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const example = "(let* x 10 x)";

    const compilation = @import("compilation.zig");
    const tree = try parsing.parse(example, allocator);
    const expr = try compilation.translation.translate(tree, allocator);

    const compiled = try compilation.compile(expr, allocator);

    var vm = try VM.init(allocator);
    defer vm.deinit();

    const result = try vm.eval(compiled);

    try std.testing.expectEqual(result.integer, 10);
}
