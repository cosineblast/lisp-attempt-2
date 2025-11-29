const std = @import("std");

const parsing = @import("./parsing.zig");
const rt = @import("./runtime.zig");

const VM = rt.VM;

const compilation = @import("compilation.zig");

const translation = compilation.translation;

fn parse(reader: *std.Io.Reader, allocator: std.mem.Allocator) !?*parsing.ParseNode {
    var diagnostic: parsing.Diagnostic = undefined;

    return parsing.parseFromReader(reader, allocator, .{ .diagnostic = &diagnostic }) catch |err| {
        if (err == error.ParseError) {
            if (diagnostic == .eof) {
                std.process.exit(0);
            }

            std.debug.print("parser error: {}", .{diagnostic});

            return null;
        }
        return err;
    };
}

fn translate(tree: *parsing.ParseNode, allocator: std.mem.Allocator) !?*compilation.Expression {
    var diagnostic: translation.Diagnostic = undefined;

    return compilation.translation.translate(tree, allocator, &diagnostic) catch |err| {
        if (err == error.TranslationError) {
            std.debug.print("translation error: {}", .{diagnostic});
            return null;
        }
        return err;
    };
}

fn isVerbose() bool {
    var args = std.process.args();

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-v") or
            std.mem.eql(u8, arg, "--verbose"))
        {
            return true;
        }
    }

    return false;
}

pub fn main() !void {
    const verbose = isVerbose();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    defer _ = gpa.deinit();

    const base_allocator = gpa.allocator();

    var arena = std.heap.ArenaAllocator.init(base_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var stdin_buffer: [4096]u8 = undefined;
    
    const stdin = std.fs.File.stdin();
    var stdin_reader = stdin.reader(&stdin_buffer);
    const reader = &stdin_reader.interface;

    std.debug.print("lisp attempt 2\n", .{});

    var vm_diagnostic: VM.Diagnostic = undefined;
    var vm = try VM.initWithSettings(base_allocator, .{ .verbose = verbose, .diagnostic = &vm_diagnostic });
    defer vm.deinit();

    while (true) {
        std.debug.print("\n> ", .{});

        const tree = try parse(reader, allocator) orelse continue;

        if (verbose) {
            std.debug.print("[REPL] read ok!\n", .{});
        }

        const expr = try translate(tree, allocator) orelse continue;

        if (verbose) {
            std.debug.print("[REPL] translate ok!\n", .{});

            std.debug.print("[REPL] translation:\n", .{});

            var buffer: [4096]u8 = undefined;
            var stderr_fwriter = std.fs.File.stderr().writer(&buffer);
            const writer = &stderr_fwriter.interface;

            try compilation.showExpression(expr, writer);
            try writer.print("\n", .{});
            
            try writer.flush();
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

        std.debug.print("{f}", .{result});
    }
}

