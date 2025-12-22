const std = @import("std");
const spv = @import("spv");

const shader_source = @embedFile("shader");

pub fn main() !void {
    {
        var gpa: std.heap.DebugAllocator(.{}) = .init;
        defer _ = gpa.deinit();

        const allocator = gpa.allocator();

        const ctx = try spv.Interpreter.init();
        defer ctx.deinit();

        const module = try spv.Module.init(allocator, &ctx, @ptrCast(@alignCast(shader_source)));
        defer module.deinit(allocator);
    }
    std.log.info("Successfully executed", .{});
}
