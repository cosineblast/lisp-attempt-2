const std = @import("std");

const parsing = @import("parsing.zig");

pub const InstructionType = enum {
    call,
    tcall,
    pick,
    jf,
    jmp,
    load,
    loadf,
    rip,
    ret,
    nop,
};

// (fn (x y) (if (< x y) x y))

// (x y)
// pick 1
// (x y x)
// pick 1
// (x y x y)
// call </2
// (x y x<y)
// jf else
// (x y)
// pick 1
// (x y x)
// rip 2 1
// (x)
// ret

// (fn (xs k) (map (fn (x) (* x k)) xs))
// (fn (xs k) (let [f (fn (x) (* x k))] (map f xs)))

// (xs k)
// pick 0
// (xs k k)
// loadf f1 1
// (xs k f)
// pick 0
// (xs k f f)
// pick 3
// (xs k f f xs)
// rip 3 2
// (f xs)
// tcall map
//
pub const Instruction = union(InstructionType) {
    // TODO: replace single u8 structs with actual u8s
    call: struct { arg_count: u8 },
    tcall: struct { arg_count: u8 },
    pick: struct { offset: u8 },
    jf: struct { offset: u8 },
    jmp: struct { offset: u8 },
    load: struct { value: u8 },
    loadf: struct { value: u8, in_context: u8 },
    rip: struct { drop: u8, keep: u8 },
    ret,
    nop,
};

pub const ValueType = enum {
    nil,
    list,
    integer,
    boolean,
    lambda,
    real_function,
};

pub const ListObject = struct { item: Value, next: ?*ListObject };

pub const LambdaBody = struct { //
    code: std.ArrayListUnmanaged(Instruction), //
    values: [256]Value,
    value_count: usize,
    parameter_count: ?u8,
    other_bodies: [256]*LambdaBody,
    other_body_count: usize,
    ref_count: usize = 1,

    pub fn down(body: *LambdaBody, allocator: std.mem.Allocator) void {
        std.debug.assert(body.ref_count > 0);

        if (body.ref_count == 1) {
            body.code.deinit(allocator);
            allocator.destroy(body);
        } else {
            body.ref_count -= 1;
        }
    }
};

pub const BytecodeLambda = struct {
    body: *LambdaBody,
    context: []Value,
};

pub const Value = union(ValueType) {
    nil, //
    list: *ListObject,
    integer: i64,
    boolean: bool,
    lambda: *BytecodeLambda,
    real_function: *const fn (state: *VM, count: u8) void,
};

pub const Frame = struct { function: *BytecodeLambda, instruction_offset: usize };

pub const VM = struct {
    stack: std.ArrayList(Value),
    call_stack: std.ArrayList(Frame),
    allocator: std.mem.Allocator,
    active_frame: Frame,

    pub fn execute(self: *VM) !void {
        var instruction_offset = self.active_frame.instruction_offset;
        const limit = self.active_frame.function.body.code.items.len;

        defer self.active_frame.instruction_offset = instruction_offset;

        while (instruction_offset < limit) {
            const instruction = self.active_frame.function.body.code.items[instruction_offset];

            switch (instruction) {
                .pick => |offset| {
                    std.debug.print("> pick {}\n", .{offset.offset});

                    std.debug.assert(offset.offset < self.stack.items.len);
                },
                .jf => |offset| {
                    std.debug.print("> jf {}\n", .{offset.offset});

                    const top = self.stack.pop();

                    if (top == ValueType.boolean and top.boolean == false) {
                        std.debug.print(">  jumped\n", .{});
                        std.debug.assert(instruction_offset + offset.offset < limit);

                        instruction_offset += offset.offset;
                    }

                    continue;
                },
                .jmp => |offset| {
                    std.debug.print("> jmp {}\n", .{offset.offset});
                    std.debug.assert(instruction_offset + offset.offset < limit);

                    instruction_offset += offset.offset;

                    continue;
                },
                .load => |id| {
                    std.debug.print("> load {}\n", .{id.value});
                    try self.stack.append(self.active_frame.function.body.values[id.value]);
                },
                .loadf => {
                    // TODO
                    unreachable;
                },
                .rip => |info| {
                    std.debug.print("> rip {} {}\n", .{ info.drop, info.keep });

                    // x x k k k
                    const keep = info.keep;
                    const drop = info.drop;
                    var i = self.stack.items.len - (keep + drop - 1) - 1;
                    var j = self.stack.items.len - (keep - 1) - 1;
                    var count: usize = 0;

                    while (count < keep) {
                        self.stack.items[i] = self.stack.items[j];
                        i += 1;
                        j += 1;
                        count += 1;
                    }

                    // TODO: check if it is ok to touch this directly,
                    // or if there is a function in the standard library to do this.
                    self.stack.items.len -= drop;
                },
                .call => |info| {
                    std.debug.print("> call\n", .{});

                    const fn_value = self.stack.pop();

                    switch (fn_value) {
                        .real_function => |function| {
                            function(self, info.arg_count);
                        },

                        .lambda => |lambda| {
                            if (lambda.body.parameter_count != null and lambda.body.parameter_count != info.arg_count) {
                                return error.MismatchedCall;
                            }

                            try self.call_stack.append(self.active_frame);

                            self.active_frame.function = lambda;
                            self.active_frame.instruction_offset = 0;

                            for (lambda.context) |value| {
                                try self.stack.append(value);
                            }

                            continue;
                        },

                        else => return error.IllegalCall,
                    }
                },
                .tcall => {
                    // TODO
                    unreachable;
                },
                .ret => {
                    std.debug.print("> ret \n", .{});
                    self.active_frame = self.call_stack.pop();
                    self.active_frame.instruction_offset += 1;

                    continue;
                },
                .nop => {
                    std.debug.print("> nop \n", .{});
                },
            }

            instruction_offset += 1;
        }
    }
};

pub const LambdaBuilder = struct {
    // The current implementation of this interpreter uses an array of tagged unions to store the instructions,
    // but in the future, that may be replaced by a serialized array of bytes.
    // The API of this module tries to be agnostic to which of these is utilized.
    const Self = @This();

    code: std.ArrayList(Instruction),
    values: [256]Value,
    other_bodies: [256]*LambdaBody,
    next_value_index: u8,
    next_body_index: u8,
    parameter_count: ?u8,

    pub fn init(allocator: std.mem.Allocator) Self {
        return LambdaBuilder{ //
            .code = std.ArrayList(Instruction).init(allocator),
            .values = undefined,
            .other_bodies = undefined,
            .next_value_index = 0,
            .parameter_count = 0,
            .next_body_index = 0,
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

    pub fn addValue(self: *Self, value: Value) u8 {
        const current = self.next_value_index;
        self.values[current] = value;
        self.next_value_index += 1;
        return current;
    }

    pub fn addBodyReference(self: *Self, body: *LambdaBody) u8 {
        const current = self.next_body_index;
        self.other_bodies[current] = body;
        self.next_body_index += 1;
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
            .values = self.values,
            .code = self.code.moveToUnmanaged(),
            .value_count = self.next_value_index,
            .other_bodies = self.other_bodies,
            .other_body_count = self.next_body_index,
        };
    }

    pub fn buildOnHeap(self: *Self) !*LambdaBody {
        const result = try self.code.allocator.create(LambdaBody);
        result.* = self.build();
        return result;
    }
};

pub fn dump(lambda: *const LambdaBody) void {
    std.debug.print("(function\n", .{});
    std.debug.print("  (parcount {?})\n", .{lambda.parameter_count});
    std.debug.print("  (code\n", .{});

    for (lambda.code.items) |instruction| {
        switch (instruction) {
            .pick => |offset| {
                std.debug.print("    (pick {})\n", .{offset.offset});
            },
            .jf => |offset| {
                std.debug.print("    (jf {})\n", .{offset.offset});
            },
            .jmp => |offset| {
                std.debug.print("    (jmp {})\n", .{offset.offset});
            },
            .load => |id| {
                std.debug.print("    (load {})\n", .{id.value});
            },
            .loadf => {
                unreachable;
            },
            .rip => |info| {
                std.debug.print("    (rip {} {})\n", .{ info.drop, info.keep });
            },
            .call => |info| {
                std.debug.print("    (call {?})\n", .{info.arg_count});
            },
            .tcall => {
                // TODO
                unreachable;
            },
            .ret => {
                std.debug.print("     (ret)\n", .{});
            },
            .nop => {
                std.debug.print("    (nop)\n", .{});
            },
        }
    }

    std.debug.print("  )\n", .{});

    std.debug.print("  (values\n", .{});

    for (0..lambda.value_count) |i| {
        std.debug.print("    ({} ", .{i});

        print_value(&lambda.values[i]);
        std.debug.print(")\n", .{});
    }

    std.debug.print("  )\n", .{});
    std.debug.print(")\n", .{});
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
