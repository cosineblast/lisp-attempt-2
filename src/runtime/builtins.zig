const std = @import("std");

const VM = @import("VM.zig");

const rt = @import("../runtime.zig");

pub fn add(vm: *VM, arg_count: u8) anyerror!void {
    var result: i64 = 0;
    var count: u8 = 0;

    while (count < arg_count) : (count += 1) {
        const value = vm.stack.pop();

        switch (value) {
            .integer => |integer| {
                result += integer;
            },
            else => {
                return error.TypeError;
            },
        }
    }

    try vm.stack.append(.{ .integer = result });
}

pub fn multiply(vm: *VM, arg_count: u8) anyerror!void {
    var result: i64 = 1;
    var count: u8 = 0;

    while (count < arg_count) : (count += 1) {
        const value = vm.stack.pop();

        switch (value) {
            .integer => |integer| {
                result *= integer;
            },
            else => {
                return error.TypeError;
            },
        }
    }

    try vm.stack.append(.{ .integer = result });
}

pub fn subtract(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count < 1) {
        return error.ArityError;
    }

    var result: i64 = switch (vm.stack.items[vm.stack.items.len - arg_count]) {
        .integer => |integer| integer,
        else => return error.TypeError,
    };

    var count: u8 = 1;

    while (count < arg_count) : (count += 1) {
        const value = vm.stack.pop();

        switch (value) {
            .integer => |integer| {
                result -= integer;
            },
            else => {
                return error.TypeError;
            },
        }
    }

    _ = vm.stack.pop();

    try vm.stack.append(.{ .integer = result });
}

pub fn divide(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count < 1) {
        return error.ArityError;
    }

    var result: i64 = switch (vm.stack.items[vm.stack.items.len - arg_count]) {
        .integer => |integer| integer,
        else => return error.TypeError,
    };

    var count: u8 = 1;

    while (count < arg_count) : (count += 1) {
        const value = vm.stack.pop();

        switch (value) {
            .integer => |integer| {
                if (integer == 0) {
                    return error.DivideByZeroError;
                }
                result = @divFloor(result, integer);
            },
            else => {
                return error.TypeError;
            },
        }
    }

    _ = vm.stack.pop();

    try vm.stack.append(.{ .integer = result });
}

pub fn isInt(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count != 1) {
        return error.ArityError;
    }

    const value = vm.stack.pop();

    const result = switch (value) {
        .integer => true,
        else => false,
    };

    try vm.stack.append(.{ .boolean = result });
}

pub fn isFn(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count != 1) {
        return error.ArityError;
    }

    const value = vm.stack.pop();

    const result = switch (value) {
        .lambda => true,
        .real_function => true,
        else => false,
    };

    try vm.stack.append(.{ .boolean = result });
}

pub fn isBool(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count != 1) {
        return error.ArityError;
    }

    const value = vm.stack.pop();

    const result = switch (value) {
        .boolean => true,
        else => false,
    };

    try vm.stack.append(.{ .boolean = result });
}

pub fn lt(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count != 2) {
        return error.ArityError;
    }

    const right = vm.stack.pop();
    const left = vm.stack.pop();

    if (left != .integer or right != .integer) {
        return error.ValueError;
    }

    const result = left.integer < right.integer;

    try vm.stack.append(.{ .boolean = result });
}

pub fn gt(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count != 2) {
        return error.ArityError;
    }

    const right = vm.stack.pop();
    const left = vm.stack.pop();

    if (left != .integer or right != .integer) {
        return error.ValueError;
    }

    const result = left.integer > right.integer;

    try vm.stack.append(.{ .boolean = result });
}

pub fn sample_str(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count != 0) {
        return error.ArityError;
    }

    const slice = try vm.allocator.alloc(u8, 4);
    errdefer vm.allocator.free(slice);

    std.mem.copyForwards(u8, slice, "leak");

    const content = try vm.allocator.create(rt.StringContent);
    errdefer vm.allocator.destroy(content);
    content.* = .{ .items = slice };

    const str = rt.StringObject{ .content = content, .len = 4, .offset = 0 };

    const result = try vm.allocator.create(rt.StringObject);
    result.* = str;
    errdefer vm.allocator.destroy(result);

    // watch out! registering a value in the GC may cause a gc,
    // we must put this in the right order!

    try vm.registerGC(.{ .string = result });
    try vm.registerGC(.{ .string_content = content });

    try vm.stack.append(.{ .string = result });
}
