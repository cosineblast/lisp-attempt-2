const std = @import("std");

const parsing = @import("./parsing.zig");
const rt = @import("./runtime.zig");

const compilation = @import("compilation.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const gpa_allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(gpa_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn();
    var reader = stdin.reader();

    const tree = try parsing.parseFromReader(reader.any(), allocator);

    _ = try compilation.translate(tree, allocator);

    std.debug.print("ok!\n", .{});
}

usingnamespace @import("parsing.zig");

usingnamespace @import("compilation.zig");

test "basic instruction test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var b = rt.LambdaBuilder.init(allocator);
    const nah = b.addValue(.{ .boolean = false });
    const ten = b.addValue(.{ .integer = 10 });
    const twenty = b.addValue(.{ .integer = 20 });

    try b.addInstruction(rt.Instruction{ .load = .{ .value = ten } });
    try b.addInstruction(rt.Instruction{ .load = .{ .value = twenty } });
    try b.addInstruction(rt.Instruction{ .load = .{ .value = nah } });
    try b.addInstruction(rt.Instruction{ .jf = .{ .offset = 4 } });
    try b.addInstruction(rt.Instruction{ .pick = .{ .offset = 1 } });
    try b.addInstruction(rt.Instruction{ .rip = .{ .keep = 1, .drop = 2 } });
    try b.addInstruction(rt.Instruction{ .jmp = .{ .offset = 1 } });
    try b.addInstruction(rt.Instruction{ .rip = .{
        .drop = 1,
        .keep = 1,
    } });
    try b.addInstruction(rt.Instruction.nop);

    var lambda_body = b.build();
    var lambda: rt.BytecodeLambda = undefined;
    lambda.context = &.{};
    lambda.body = &lambda_body;
    var frame: rt.Frame = undefined;
    frame.instruction_offset = 0;
    frame.function = &lambda;

    rt.dump(lambda.body);

    var machine: rt.VM = undefined;
    machine.allocator = allocator;
    machine.active_frame = frame;
    machine.call_stack = std.ArrayList(rt.Frame).init(allocator);
    machine.stack = std.ArrayList(rt.Value).init(allocator);

    try machine.execute();

    try std.testing.expectEqual(20, machine.stack.items[0].integer);
}
