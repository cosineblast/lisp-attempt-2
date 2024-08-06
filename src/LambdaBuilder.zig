const std = @import("std");
const rt = @import("runtime.zig");

const Instruction = rt.Instruction;
const InstructionType = rt.InstructionType;
const LambdaBody = rt.LambdaBody;

// The current implementation of this interpreter uses an array of tagged unions to store the instructions,
// but in the future, that may be replaced by a serialized array of bytes.
// The API of this module tries to be agnostic to which of these is utilized.
const Self = @This();

code: std.ArrayList(Instruction),
immediate_table: [256]LambdaBody.Immediate,
other_bodies: [256]*LambdaBody,
global_table: [256][]const u8,
next_value_index: u8,
next_body_index: u8,
next_global_index: u8,
parameter_count: ?u8,

pub fn init(allocator: std.mem.Allocator) Self {
    return Self{ //
        .code = std.ArrayList(Instruction).init(allocator),
        .immediate_table = undefined,
        .other_bodies = undefined,
        .global_table = undefined,
        .next_value_index = 0,
        .parameter_count = 0,
        .next_body_index = 0,
        .next_global_index = 0,
    };
}

pub fn addInstruction(self: *Self, instruction: Instruction) !void {
    try self.code.append(instruction);
}

// The number of instructions inserted so far in this function
pub fn insertedSoFar(self: *Self) usize {
    return self.code.items.len;
}

/// Returns an unsigned integer that represents the index of beginning of the next instruction to
/// be inserted. As of now, there are no guarantees on the semantics of addition or subtraction
/// with this given offset (the implementation can be based on arrays of tagged unions, or
/// byte arrays), but it is guaranteed that this index can be used with setInstruction
pub fn nextOffset(self: *Self) usize {
    return self.code.items.len;
}

/// Modifies the inserted code, to set the given instruction at the given offset.
/// It is invalid (altough not necessarily detectable) to use this operation with an offset
/// that was not returned by nextOffset, nor to use it to override an existing instruction
/// with another instruction of different serialized-byte-size.
pub fn setInstruction(self: *Self, offset: usize, instruction: Instruction) void {
    std.debug.assert(offset < self.code.items.len);
    std.debug.assert(@as(InstructionType, instruction) == @as(InstructionType, self.code.items[offset]));

    self.code.items[offset] = instruction;
}

pub fn addImmediate(self: *Self, value: LambdaBody.Immediate) u8 {
    const current = self.next_value_index;
    self.immediate_table[current] = value;
    self.next_value_index += 1;
    return current;
}

pub fn addBodyReference(self: *Self, body: *LambdaBody) u8 {
    const current = self.next_body_index;
    self.other_bodies[current] = body;
    self.next_body_index += 1;
    return current;
}

pub fn addGlobalReference(self: *Self, name: []const u8) u8 {
    const current = self.next_global_index;
    self.global_table[current] = name;
    self.next_global_index += 1;
    return current;
}

pub fn setVariadic(self: *Self) void {
    self.parameter_count = null;
}

pub fn setParameterCount(self: *Self, count: u8) void {
    self.parameter_count = count;
}

pub fn build(self: *Self) LambdaBody {
    return LambdaBody{ //
        .parameter_count = self.parameter_count,
        .immediate_table = self.immediate_table,
        .code = self.code.moveToUnmanaged(),
        .immediate_count = self.next_value_index,
        .other_bodies = self.other_bodies,
        .other_body_count = self.next_body_index,
        .global_table = self.global_table,
        .global_count = self.next_global_index,
    };
}

pub fn buildOnHeap(self: *Self) !*LambdaBody {
    const result = try self.code.allocator.create(LambdaBody);
    result.* = self.build();
    return result;
}
