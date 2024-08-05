const std = @import("std");

const rt = @import("../runtime.zig");

const Value = rt.Value;

const Self = @This();

const Tuple = std.meta.Tuple;

const builtins = @import("builtins.zig");

pub const Frame = struct { //
    body: *rt.LambdaBody,
    instruction_offset: usize,
};

stack: std.ArrayList(Value),
call_stack: std.ArrayList(Frame),
allocator: std.mem.Allocator,
active_frame: ?Frame,
globals: std.StringHashMap(Value),

pub fn init(allocator: std.mem.Allocator) !Self {
    var self = Self{ //
        .allocator = allocator,
        .stack = std.ArrayList(Value).init(allocator),
        .call_stack = std.ArrayList(Frame).init(allocator),
        .active_frame = null,
        .globals = std.StringHashMap(Value).init(allocator),
    };

    try self.addBuiltins();

    return self;
}

fn addBuiltins(self: *Self) !void {
    try self.globals.put(
        "+",
        .{ .real_function = builtins.add },
    );
    try self.globals.put("-", .{ .real_function = builtins.subtract });
    try self.globals.put("*", .{ .real_function = builtins.multiply });
    try self.globals.put("/", .{ .real_function = builtins.divide });
    try self.globals.put("int?", .{ .real_function = builtins.isInt });
    try self.globals.put("fn?", .{ .real_function = builtins.isFn });
    try self.globals.put("bool?", .{ .real_function = builtins.isBool });
    try self.globals.put("<", .{ .real_function = builtins.lt });
    try self.globals.put(">", .{ .real_function = builtins.gt });
    try self.globals.put("sample-str", .{ .real_function = builtins.sample_str });
}

pub fn deinit(self: *Self) void {
    self.stack.deinit();
    self.call_stack.deinit();
    self.globals.deinit();
}

pub fn eval(self: *Self, body: *rt.LambdaBody) !Value {
    std.debug.assert(self.active_frame == null);

    self.active_frame = .{ .body = body, .instruction_offset = 0 };

    const before = self.stack.items.len;

    try self.execute();

    std.debug.assert(self.stack.items.len == before + 1);

    self.active_frame = null;

    return self.stack.pop();
}

fn execute(self: *Self) !void {
    while (self.active_frame.?.instruction_offset < self.active_frame.?.body.code.items.len) {
        const instruction = self.active_frame.?.body.code.items[self.active_frame.?.instruction_offset];

        {
            std.debug.print("[{}]s: ", .{self.stack.items.len});
            var i = self.stack.items.len;
            var count: usize = 0;
            while (i != 0 and count < 8) {
                i -= 1;
                std.debug.print("{} ", .{self.stack.items[i]});
                count += 1;
            }
            std.debug.print("\n", .{});
        }

        self.active_frame.?.instruction_offset += 1;

        switch (instruction) {
            .pick => |offset| {
                std.debug.print("> pick {}\n", .{offset.offset});

                std.debug.assert(offset.offset < self.stack.items.len);

                try self.stack.append(self.stack.items[self.stack.items.len - 1 - offset.offset]);
            },
            .jf => |offset| {
                std.debug.print("> jf +{}\n", .{offset.offset});

                const top = self.stack.pop();

                if (top == rt.ValueType.boolean and top.boolean == false) {
                    std.debug.print(">  jumped\n", .{});
                    std.debug.assert(self.active_frame.?.instruction_offset + offset.offset <= self.active_frame.?.body.code.items.len);

                    self.active_frame.?.instruction_offset -= 1;
                    self.active_frame.?.instruction_offset += offset.offset;
                }
            },
            .jmp => |offset| {
                std.debug.print("> jmp +{}\n", .{offset.offset});
                std.debug.assert(self.active_frame.?.instruction_offset + offset.offset <= self.active_frame.?.body.code.items.len);

                self.active_frame.?.instruction_offset -= 1;
                self.active_frame.?.instruction_offset += offset.offset;
            },
            .load => |target| {
                std.debug.print("> load @{}\n", .{target.id});
                try self.stack.append(self.active_frame.?.body.immediate_table[target.id]);
            },
            .loadf => |info| {
                std.debug.print("> loadf @{}\n", .{info.id});

                const body = self.active_frame.?.body.other_bodies[info.id];

                const function = try self.allocator.create(rt.LambdaObject);

                function.body = body;
                var context = std.ArrayList(Value).init(self.allocator);
                function.context = context.moveToUnmanaged();

                for (0..info.in_context) |_| {
                    try function.context.append(self.allocator, self.stack.pop());
                }

                std.mem.reverse(Value, function.context.items);

                try self.stack.append(.{ .lambda = function });
            },
            .loadg => |id| {
                const name = self.active_frame.?.body.global_table[id.id];

                const value_: ?Value = self.globals.get(name);

                if (value_) |value| {
                    try self.stack.append(value);
                } else {
                    return error.UnknownVariable;
                }
            },
            .rip => |info| {
                std.debug.print("> rip {} {}\n", .{ info.drop, info.keep });

                std.debug.assert(self.stack.items.len >= info.drop + info.keep);

                // x x k k k
                const keep = info.keep;
                const drop = info.drop;
                var i = self.stack.items.len - 1 - (keep + drop - 1);
                var j = self.stack.items.len - 1 - (keep - 1);
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
                        try function(self, info.arg_count);
                    },

                    .lambda => |lambda| {
                        if (lambda.body.parameter_count != null and lambda.body.parameter_count != info.arg_count) {
                            return error.MismatchedCall;
                        }

                        try self.call_stack.append(self.active_frame.?);

                        self.active_frame.?.body = lambda.body;
                        self.active_frame.?.instruction_offset = 0;

                        for (lambda.context.items) |value| {
                            try self.stack.append(value);
                        }
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
            },
            .nop => {
                std.debug.print("> nop \n", .{});
            },
        }
    }
}
