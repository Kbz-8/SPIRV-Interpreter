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

        var rt = try spv.Runtime.init(allocator, &module);
        defer rt.deinit(allocator);

        try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));
        var value: f32 = undefined;
        var value2: f32 = undefined;
        try rt.readOutput(f32, @as([*]f32, @ptrCast(&value))[0..1], try rt.getResultByName("value"));
        try rt.readOutput(f32, @as([*]f32, @ptrCast(&value2))[0..1], try rt.getResultByName("value2"));
        std.log.info("Output: {d} {d}", .{ value, value2 });
    }
    std.log.info("Successfully executed", .{});
}
