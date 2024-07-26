const std = @import("std");

const parser = @import("./parse.zig");
const vm = @import("./vm.zig");

pub fn main() !void {}

test "basic test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var string = std.ArrayList(u8).init(std.testing.allocator);
    defer string.deinit();

    const result = parser.parse("(123 (456 789) () neat)", allocator) catch unreachable;

    try parser.show(result, &string);

    try std.testing.expectEqualStrings("(123 (456 789) () neat)", string.items);
}

test "vm test" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var b = vm.LambdaBuilder.init(allocator);
    const nah = b.addValue(.{ .boolean = false });
    const ten = b.addValue(.{ .integer = 10 });
    const twenty = b.addValue(.{ .integer = 20 });

    try b.addInstruction(vm.Instruction{ .load = .{ .value = ten } });
    try b.addInstruction(vm.Instruction{ .load = .{ .value = twenty } });
    try b.addInstruction(vm.Instruction{ .load = .{ .value = nah } });
    try b.addInstruction(vm.Instruction{ .jf = .{ .offset = 4 } });
    try b.addInstruction(vm.Instruction{ .pick = .{ .offset = 1 } });
    try b.addInstruction(vm.Instruction{ .rip = .{ .keep = 1, .drop = 2 } });
    try b.addInstruction(vm.Instruction{ .jmp = .{ .offset = 1 } });
    try b.addInstruction(vm.Instruction{ .rip = .{
        .drop = 1,
        .keep = 1,
    } });
    try b.addInstruction(vm.Instruction.nop);

    var lambda = b.build();
    var frame: vm.Frame = undefined;
    frame.instruction_offset = 0;
    frame.function = &lambda;

    vm.dump(&lambda);

    var machine: vm.VM = undefined;
    machine.allocator = allocator;
    machine.active_frame = frame;
    machine.call_stack = std.ArrayList(vm.Frame).init(allocator);
    machine.stack = std.ArrayList(vm.Value).init(allocator);

    try machine.execute();

    try std.testing.expectEqual(20, machine.stack.items[0].integer);
}
