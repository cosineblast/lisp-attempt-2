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
    call: struct { args: u8, target: u8 },
    tcall: struct { args: u8, target: u8 },
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

pub const BytecodeLambda = struct {
    code: []Instruction,
    values: [256]Value,
    value_count: usize,
    parameter_count: ?u8,
};

pub const Value = union(ValueType) { nil, list: *ListObject, integer: i64, boolean: bool, lambda: *BytecodeLambda, real_function: *const fn (state: *VM, count: u8) void };

pub const Frame = struct { function: *BytecodeLambda, instruction_offset: usize };

pub const VM = struct {
    stack: std.ArrayList(Value),
    call_stack: std.ArrayList(Frame),
    allocator: std.mem.Allocator,
    active_frame: Frame,

    pub fn execute(self: *VM) !void {
        var instruction_offset = self.active_frame.instruction_offset;
        const limit = self.active_frame.function.code.len;

        defer self.active_frame.instruction_offset = instruction_offset;

        while (instruction_offset < limit) {
            const instruction = self.active_frame.function.code[instruction_offset];

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
                    try self.stack.append(self.active_frame.function.values[id.value]);
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
                    std.debug.print("> call <>\n", .{});

                    self.active_frame.instruction_offset = instruction_offset;

                    const value = self.active_frame.function.values[info.target];

                    switch (value) {
                        .real_function => |function| {
                            function(self, info.args);
                        },

                        .lambda => |lambda| {
                            if (lambda.parameter_count != null and lambda.parameter_count != info.args) {
                                return error.MismatchedCall;
                            }

                            try self.call_stack.append(self.active_frame);

                            self.active_frame.function = lambda;
                            self.active_frame.instruction_offset = 0;

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
    const Self = @This();

    code: std.ArrayList(Instruction),
    values: [256]Value,
    next_value_index: u8,
    parameter_count: ?u8,

    pub fn init(allocator: std.mem.Allocator) Self {
        return LambdaBuilder{ .code = std.ArrayList(Instruction).init(allocator), .values = undefined, .next_value_index = 0, .parameter_count = 0 };
    }

    pub fn addInstruction(self: *Self, instruction: Instruction) !void {
        try self.code.append(instruction);
    }

    pub fn addValue(self: *Self, value: Value) u8 {
        const current = self.next_value_index;
        self.values[current] = value;
        self.next_value_index += 1;
        return current;
    }

    pub fn setVariadic(self: *Self) void {
        self.parameter_count = null;
    }

    pub fn setParameterCount(self: *Self, count: u8) void {
        self.parameter_count = count;
    }

    pub fn build(self: *Self) BytecodeLambda {
        return BytecodeLambda{ .parameter_count = self.parameter_count, .values = self.values, .code = self.code.items, .value_count = self.next_value_index };
    }
};

pub fn dump(lambda: *const BytecodeLambda) void {
    std.debug.print("(function\n", .{});
    std.debug.print("  (parcount {?})\n", .{lambda.parameter_count});
    std.debug.print("  (code\n", .{});

    for (lambda.code) |instruction| {
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
                std.debug.print("    (call {}/{?})\n", .{ info.target, info.args });
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
