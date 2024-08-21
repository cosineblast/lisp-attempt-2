const std = @import("std");

const parsing = @import("./parsing.zig");
const rt = @import("./runtime.zig");

const VM = rt.VM;

const compilation = @import("compilation.zig");

const translation = compilation.translation;

const verbose = true;

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

    var vm_diagnostic: VM.Diagnostic = undefined;
    var vm = try VM.initWithSettings(base_allocator, .{ .verbose = verbose, .diagnostic = &vm_diagnostic });
    defer vm.deinit();

    while (true) {
        std.debug.print("\n> ", .{});

        var parse_issue: parsing.Diagnostic = undefined;

        const tree = parsing.parseFromReader(reader.any(), allocator, .{ .diagnostic = &parse_issue }) catch |err| {
            if (err == error.ParseError) {
                if (parse_issue == .eof) {
                    break;
                }

                std.debug.print("parser error: {}", .{parse_issue});
                continue;
            }
            return err;
        };

        if (verbose) {
            std.debug.print("[REPL] read ok!\n", .{});
        }

        var diagnostic: translation.Diagnostic = undefined;

        const expr = compilation.translation.translate(tree, allocator, &diagnostic) catch |err| {
            if (err == error.TranslationError) {
                std.debug.print("translation error: {}", .{diagnostic});
                continue;
            }
            return err;
        };

        if (verbose) {
            std.debug.print("[REPL] translate ok!\n", .{});
        }

        var arr = std.ArrayList(u8).init(base_allocator);
        defer arr.deinit();

        try compilation.showExpression(expr, &arr);

        if (verbose) {
            std.debug.print("[REPL] translation:\n{s}\n", .{arr.items});
        }

        var body = try compilation.compile(expr, base_allocator);
        defer body.down(base_allocator);

        if (verbose) {
            std.debug.print("[REPL] compile ok!\n", .{});

            std.debug.print("[REPL] lambda Body:\n", .{});

            rt.dump(body);

            std.debug.print("[REPL] executing... \n", .{});
        }

        const result = vm.eval(body) catch |err| {
            if (err == error.VMError) {
                std.debug.print("vmerror: {}", .{vm_diagnostic});
                continue;
            }

            return err;
        };

        std.debug.print("{}", .{result});
    }
}

usingnamespace @import("parsing.zig");

usingnamespace @import("compilation.zig");

usingnamespace @import("runtime.zig");
