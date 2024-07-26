const std = @import("std");

const parser = @import("./parse.zig");

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
