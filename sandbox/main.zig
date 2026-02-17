const std = @import("std");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

const SSBO = struct {
    value: [256]i32 = [_]i32{0} ** 256,
};

pub fn main() !void {
    {
        var gpa: std.heap.DebugAllocator(.{
            .enable_memory_limit = true,
        }) = .init;
        defer _ = gpa.deinit();

        const allocator = gpa.allocator();

        var module = try spv.Module.init(allocator, @ptrCast(@alignCast(shader_source)), .{});
        defer module.deinit(allocator);

        var rt = try spv.Runtime.init(allocator, &module);
        defer rt.deinit(allocator);

        const entry = try rt.getEntryPointByName("main");

        var ssbo: SSBO = .{};

        for (0..16) |i| {
            for (0..16) |x| {
                for (0..16) |y| {
                    const global_invocation_indices = [3]i32{
                        @as(i32, @intCast(i * 16 + x)),
                        @as(i32, @intCast(y)),
                        1,
                    };

                    try rt.writeBuiltIn(std.mem.asBytes(&global_invocation_indices), .GlobalInvocationId);
                    try rt.writeDescriptorSet(allocator, std.mem.asBytes(&ssbo), 0, 0);
                    rt.callEntryPoint(allocator, entry) catch |err| switch (err) {
                        spv.Runtime.RuntimeError.OutOfBounds => continue,
                        else => return err,
                    };
                    try rt.readDescriptorSet(std.mem.asBytes(&ssbo), 0, 0);
                }
            }
        }

        std.log.info("Output: {any}", .{ssbo});

        std.log.info("Total memory used: {d:.3} KB\n", .{@as(f32, @floatFromInt(gpa.total_requested_bytes)) / 1000.0});
    }
    std.log.info("Successfully executed", .{});
}
