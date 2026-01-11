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
        var output: [4]f32 = undefined;
        try rt.readOutput(f32, output[0..output.len], try rt.getResultByName("color"));
        std.log.info("Result: Vec4[{d}, {d}, {d}, {d}]", .{ output[0], output[1], output[2], output[3] });
    }
    std.log.info("Successfully executed", .{});
}
