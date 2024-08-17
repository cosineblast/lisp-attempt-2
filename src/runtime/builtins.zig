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

pub fn sample_symbol(vm: *VM, arg_count: u8) anyerror!void {
    if (arg_count != 0) {
        return error.ArityError;
    }

    const result = try vm.intern("leak");

    try vm.stack.append(result);
}
