const std = @import("std");

const VM = @import("VM.zig");

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
