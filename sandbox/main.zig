const std = @import("std");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

pub fn main() !void {
    {
        var gpa: std.heap.DebugAllocator(.{
            .enable_memory_limit = true,
        }) = .init;
        defer _ = gpa.deinit();

        const allocator = gpa.allocator();

        var module = try spv.Module.init(allocator, @ptrCast(@alignCast(shader_source)), .{});
        defer module.deinit(allocator);

        //var rt = try spv.Runtime.init(allocator, &module);
        //defer rt.deinit(allocator);

        //const entry = try rt.getEntryPointByName("main");
        //const color = try rt.getResultByName("color");
        //const time = try rt.getResultByName("time");
        //const pos = try rt.getResultByName("pos");
        //const res = try rt.getResultByName("res");

        //var output: [4]f32 = undefined;

        //try rt.writeInput(f32, &.{@as(f32, @floatFromInt(std.time.milliTimestamp()))}, time);
        //try rt.writeInput(f32, &.{ 1250.0, 720.0 }, res);
        //try rt.writeInput(f32, &.{ 0.0, 0.0 }, pos);

        //try rt.callEntryPoint(allocator, entry);

        //try rt.readOutput(f32, output[0..output.len], color);
        //std.log.info("Output: Vec4{any}", .{output});

        std.log.info("Total memory used: {d:.3} MB\n", .{@as(f32, @floatFromInt(gpa.total_requested_bytes)) / 1000.0});
    }
    std.log.info("Successfully executed", .{});
}
