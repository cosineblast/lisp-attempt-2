const std = @import("std");

const parsing = @import("./parsing.zig");
const rt = @import("./runtime.zig");

const VM = rt.VM;

const compilation = @import("compilation.zig");

const translation = compilation.translation;

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

    var vm = try VM.init(allocator);
    defer vm.deinit();

    while (true) {
        std.debug.print("\n> ", .{});
        const tree = parsing.parseFromReader(reader.any(), allocator) catch |e| {
            if (e == error.EOF) {
                break;
            } else {
                return e;
            }
        };

        std.debug.print("[REPL] read ok!\n", .{});

        const expr = try compilation.translation.translate(tree, allocator);

        std.debug.print("[REPL] translate ok!\n", .{});

        var arr = std.ArrayList(u8).init(base_allocator);
        defer arr.deinit();

        try compilation.showExpression(expr, &arr);

        std.debug.print("[REPL] translation:\n{s}\n", .{arr.items});

        var compiler = compilation.Compilation.init(base_allocator);
        defer compiler.deinit();

        try compiler.compileExpression(expr);

        var body = try compiler.lambda_builder.buildOnHeap();
        defer body.down(base_allocator);

        std.debug.print("[REPL] compile ok!\n", .{});

        std.debug.print("[REPL] lambda Body:\n", .{});

        rt.dump(body);

        std.debug.print("[REPL] executing... \n", .{});

        const result = try vm.eval(body);

        std.debug.print("{}", .{result});
    }
}

usingnamespace @import("parsing.zig");

usingnamespace @import("compilation.zig");

usingnamespace @import("runtime.zig");
