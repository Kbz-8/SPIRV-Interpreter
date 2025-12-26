const std = @import("std");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

pub fn main() !void {
    {
        var gpa: std.heap.DebugAllocator(.{}) = .init;
        defer _ = gpa.deinit();

        const allocator = gpa.allocator();

        var module = try spv.Module.init(allocator, @ptrCast(@alignCast(shader_source)));
        defer module.deinit(allocator);

        var rt = spv.Runtime.init(&module);
        defer rt.deinit();

        try rt.callEntryPoint(0);
    }
    std.log.info("Successfully executed", .{});
}
