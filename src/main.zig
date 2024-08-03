const std = @import("std");

const parsing = @import("./parsing.zig");
const rt = @import("./runtime.zig");

const compilation = @import("compilation.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const base_allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(base_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn();
    var reader = stdin.reader();

    std.debug.print("lisp attempt 2\n", .{});

    while (true) {
        std.debug.print("\n> ", .{});
        const tree = parsing.parseFromReader(reader.any(), allocator) catch |e| {
            if (e == error.EOF) {
                break;
            } else {
                return e;
            }
        };

        std.debug.print("read ok!\n", .{});

        const expr = try compilation.translate(tree, allocator);

        std.debug.print("translate ok!\n", .{});

        var arr = std.ArrayList(u8).init(base_allocator);
        defer arr.deinit();

        try compilation.showExpression(expr, &arr);

        std.debug.print("translation:\n{s}\n", .{arr.items});

        var compiler = compilation.Compilation.init(base_allocator);
        defer compiler.deinit();

        try compiler.compileExpression(expr);

        var body = try compiler.lambda_builder.buildOnHeap();
        defer body.down(base_allocator);

        std.debug.print("compile ok!\n", .{});

        std.debug.print("Lambda Body:\n", .{});

        rt.dump(body);
    }
}

usingnamespace @import("parsing.zig");

usingnamespace @import("compilation.zig");

usingnamespace @import("runtime.zig");
