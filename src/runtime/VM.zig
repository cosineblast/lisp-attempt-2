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

const GC_LIMIT = 100;

stack: std.ArrayList(Value),
call_stack: std.ArrayList(Frame),
allocator: std.mem.Allocator,
active_frame: ?Frame,
globals: std.StringHashMap(Value),

string_literals: std.StringHashMap(*rt.StringObject),

gc_values: std.ArrayList(GCValue),

// it's hard to remove values from gc_values, so we move
// everything to gc_values_hack, of similar capacity,
// clear gc_values
gc_values_hack: std.ArrayList(GCValue),

gc_counter: u32 = GC_LIMIT,

pub fn init(allocator: std.mem.Allocator) !Self {
    var self = Self{ //
        .allocator = allocator,
        .stack = std.ArrayList(Value).init(allocator),
        .call_stack = std.ArrayList(Frame).init(allocator),
        .active_frame = null,
        .globals = std.StringHashMap(Value).init(allocator),
        .gc_values = std.ArrayList(GCValue).init(allocator),
        .gc_values_hack = std.ArrayList(GCValue).init(allocator),
        .string_literals = std.StringHashMap(*rt.StringObject).init(allocator),
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
    self.destroyGCValues();
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

                    self.active_frame.?.instruction_offset -= 1;

                    std.debug.assert(self.active_frame.?.instruction_offset + offset.offset <= self.active_frame.?.body.code.items.len);

                    self.active_frame.?.instruction_offset += offset.offset;
                }
            },
            .jmp => |offset| {
                std.debug.print("> jmp +{}\n", .{offset.offset});

                self.active_frame.?.instruction_offset -= 1;

                std.debug.assert(self.active_frame.?.instruction_offset + offset.offset <= self.active_frame.?.body.code.items.len);

                self.active_frame.?.instruction_offset += offset.offset;
            },
            .load => |target| {
                std.debug.print("> load @{}\n", .{target.id});

                const immediate = self.active_frame.?.body.immediate_table[target.id];

                const value: Value = switch (immediate) {
                    .integer => |i| .{ .integer = i },
                    .boolean => |b| .{ .boolean = b },
                    .string => |s| blk: {
                        if (self.string_literals.get(s.items)) |object| {
                            break :blk .{ .string = object };
                        } else {
                            const object = try self.registerString(s.items);
                            try self.string_literals.put(s.items, object);
                            break :blk .{ .string = object };
                        }
                    },
                    .nil => .nil,
                };

                try self.stack.append(value);
            },
            .loadf => |info| {
                std.debug.print("> loadf @{}\n", .{info.id});

                const body = self.active_frame.?.body.other_bodies[info.id];

                const function = try self.allocator.create(rt.LambdaObject);

                function.body = body;
                body.up();
                errdefer body.down(self.allocator);

                var context = std.ArrayList(Value).init(self.allocator);
                function.context = context.moveToUnmanaged();

                for (0..info.in_context) |_| {
                    try function.context.append(self.allocator, self.stack.pop());
                }

                std.mem.reverse(Value, function.context.items);

                const value: Value = .{ .lambda = function };

                try self.registerGC(.{ .lambda = function });
                try self.stack.append(value);
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

pub fn registerGC(self: *Self, value: GCValue) !void {
    try self.gc_values.append(value);

    self.gc_counter -= 1;

    if (self.gc_counter == 0) {
        try self.gc();
        self.gc_counter = GC_LIMIT;
    }
}

fn gc(self: *Self) !void {
    for (self.stack.items) |item| {
        tag(item);
    }

    {
        var iterator = self.globals.iterator();

        while (iterator.next()) |entry| {
            tag(entry.value_ptr.*);
        }
    }

    var iterator = self.string_literals.iterator();

    while (iterator.next()) |entry| {
        tag(.{ .string = entry.value_ptr.* });
    }

    // TODO: use data structure with fast append
    // and fast element removal

    for (self.gc_values.items) |item| {
        if (!item.fetchResetTag()) {
            item.die(self.allocator);
        } else {
            try self.gc_values_hack.append(item);
        }
    }

    self.gc_values.items.len = 0;

    std.mem.swap(std.ArrayList(GCValue), &self.gc_values, &self.gc_values_hack);
}

const GCValue = union(enum) {
    string: *rt.StringObject,
    string_content: *rt.StringContent,
    lambda: *rt.LambdaObject,

    fn from(value: Value) ?GCValue {
        switch (value) {
            .string => |str| {
                return .{ .string = str };
            },
            .lambda => |lambda| {
                return .{ .lambda = lambda };
            },
        }
    }

    /// Returns the value of the current tag of the value, and
    /// sets the tag to false
    fn fetchResetTag(self: GCValue) bool {
        switch (self) {
            .string => |s| {
                const it = s.tag;
                s.tag = false;
                return it;
            },
            .string_content => |s| {
                const it = s.tag;
                s.tag = false;
                return it;
            },
            .lambda => |l| {
                const it = l.tag;
                l.tag = false;
                return it;
            },
        }
    }

    fn die(self: GCValue, allocator: std.mem.Allocator) void {
        switch (self) {
            .string => |s| {
                allocator.destroy(s);
            },
            .string_content => |s| {
                allocator.free(s.items);
                allocator.destroy(s);
            },
            .lambda => |l| {
                l.body.down(allocator);
                l.context.deinit(allocator);
                allocator.destroy(l);
            },
        }
    }
};

fn tag(value: Value) void {
    switch (value) {
        .string => |str| {
            str.tag = true;
            str.content.tag = true;
        },
        .lambda => |lambda| {
            lambda.tag = true;
            for (lambda.context.items) |item| {
                tag(item);
            }
        },
        else => {},
    }
}

fn destroyGCValues(self: *Self) void {
    for (self.gc_values.items) |item| {
        item.die(self.allocator);
    }

    self.gc_values_hack.deinit();
    self.gc_values.deinit();
}

pub fn registerString(self: *Self, source: []const u8) !*rt.StringObject {
    const slice = try self.allocator.alloc(u8, source.len);
    errdefer self.allocator.free(slice);

    std.mem.copyForwards(u8, slice, source);

    const content = try self.allocator.create(rt.StringContent);
    errdefer self.allocator.destroy(content);
    content.* = .{ .items = slice };

    const str = rt.StringObject{ .content = content, .len = 4, .offset = 0 };

    const result = try self.allocator.create(rt.StringObject);
    result.* = str;
    errdefer self.allocator.destroy(result);

    // we do this to make sure the second gc doesn't destroy the string object.
    // when we regiter it for the second time
    try self.stack.append(.{ .string = result });

    try self.registerGC(.{ .string = result });
    try self.registerGC(.{ .string_content = content });

    _ = self.stack.pop();

    return result;
}
