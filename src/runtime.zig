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
    defg: struct { id: u8 },
    rip: struct { drop: u16, keep: u8 },
    ret,
    nop,
};

pub const InstructionType = std.meta.Tag(Instruction);

pub const ListObject = struct { item: Value, next: ?*ListObject };

pub const LambdaBody = struct { //
    pub const Immediate = union(enum) { //
        integer: i64,
        boolean: bool,
        symbol: std.ArrayListUnmanaged(u8),
        nil,
    };

    code: std.ArrayListUnmanaged(Instruction),
    immediate_table: [256]Immediate,
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

const GCTag = bool;
const GCTagDefault = false;

pub const LambdaObject = struct { //
    body: *LambdaBody,
    context: std.ArrayListUnmanaged(Value),
    tag: GCTag = GCTagDefault,
};

pub const SymbolObject = struct { //
    content: []const u8,
};

pub const Value = union(enum) {
    nil, //
    list: *ListObject,
    integer: i64,
    boolean: bool,
    lambda: *LambdaObject,
    symbol: *SymbolObject,

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
            .symbol => |symbol| {
                try writer.print("'{s}", .{symbol.content});
            },
            .nil => {
                try writer.print("nil", .{});
            },
        }
    }
};

pub const ValueType = std.meta.Tag(Value);

fn toJsonStream(lambda: *const LambdaBody, json_writer: anytype) anyerror!void {
    try json_writer.beginObject();

    try json_writer.objectField("body");

    try json_writer.beginArray();

    for (lambda.code.items) |item| {
        try json_writer.write(item);
    }

    try json_writer.endArray();

    try json_writer.objectField("immediate_table");

    try json_writer.beginArray();

    for (0..lambda.immediate_count) |i| {
        try json_writer.write(lambda.immediate_table[i]);
    }

    try json_writer.endArray();

    try json_writer.objectField("other_bodies");

    try json_writer.beginArray();
    for (0..lambda.other_body_count) |i| {
        try toJsonStream(lambda.other_bodies[i], json_writer);
    }
    try json_writer.endArray();

    try json_writer.endObject();
}

pub fn dump(lambda: *const LambdaBody) void {
    const stderr = std.io.getStdErr();
    const writer = stderr.writer();
    var json_writer = std.json.writeStream(writer, .{ .whitespace = .indent_2 });

    toJsonStream(lambda, &json_writer) catch unreachable;

    writer.writeByte('\n') catch unreachable;
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
