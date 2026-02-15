const std = @import("std");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

const Input = struct {
    value: [4]i32 = [4]i32{ 0, 0, 0, 0 },
};

const Output = struct {
    value: [4]i32 = [4]i32{ 0, 0, 0, 0 },
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

        var input: Input = .{};
        var output: Output = .{};

        try rt.writeDescriptorSet(allocator, std.mem.asBytes(&input), 0, 0);
        try rt.writeDescriptorSet(allocator, std.mem.asBytes(&output), 0, 1);

        try rt.callEntryPoint(allocator, entry);

        try rt.readDescriptorSet(std.mem.asBytes(&output), 0, 1);

        std.log.info("Output: {any}", .{output});

        std.log.info("\nTotal memory used: {d:.3} KB\n", .{@as(f32, @floatFromInt(gpa.total_requested_bytes)) / 1000.0});
    }
    std.log.info("Successfully executed", .{});
}
