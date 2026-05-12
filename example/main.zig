const std = @import("std");
const sdl3 = @import("sdl3");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

const screen_width = 400;
const screen_height = 240;

pub fn main() !void {
    {
        //var gpa: std.heap.DebugAllocator(.{}) = .init;
        //defer _ = gpa.deinit();

        const allocator = std.heap.smp_allocator;

        var threaded: std.Io.Threaded = .init(allocator, .{
            .async_limit = @enumFromInt(screen_height),
        });
        defer threaded.deinit();
        const io = threaded.io();

        defer sdl3.shutdown();
        const init_flags = sdl3.InitFlags{ .video = true, .events = true };
        try sdl3.init(init_flags);
        defer sdl3.quit(init_flags);

        const window = try sdl3.video.Window.init("Hello triangle", screen_width, screen_height, .{});
        defer window.deinit();

        const surface = try window.getSurface();

        var module = try spv.Module.init(allocator, @ptrCast(@alignCast(shader_source)), .{});
        defer module.deinit(allocator);

        const fragment_count = screen_height * screen_width;

        const batch_size = switch (threaded.async_limit) {
            .nothing => 1,
            .unlimited => std.Thread.getCpuCount() catch 1, // If we cannot get the CPU count, fallback on single runtime
            else => |count| @intFromEnum(count),
        };

        const runners: []Runner = try allocator.alloc(Runner, batch_size);
        defer {
            for (runners) |*runner| {
                runner.rt.deinit(allocator);
            }
            allocator.free(runners);
        }

        for (runners) |*runner| {
            var rt = try spv.Runtime.init(allocator, &module, undefined);
            runner.* = .{
                .allocator = allocator,
                .surface = surface,
                .rt = rt,
                .entry = try rt.getEntryPointByName("main"),
                .color = try rt.getResultByName("color"),
                .time = try rt.getResultByName("time"),
                .pos = try rt.getResultByName("pos"),
                .res = try rt.getResultByName("res"),
                .invocation_count = fragment_count,
                .batch_size = batch_size,
            };
        }
        const timer = std.Io.Timestamp.now(io, .real);

        var wg: std.Io.Group = .init;
        var quit = false;
        while (!quit) {
            try surface.clear(.{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 1.0 });

            while (sdl3.events.poll()) |event|
                switch (event) {
                    .quit => quit = true,
                    .terminating => quit = true,
                    else => {},
                };

            {
                try surface.lock();
                defer surface.unlock();

                const pixel_map: [*]u32 = @as([*]u32, @ptrCast(@alignCast((surface.getPixels() orelse return).ptr)));

                const duration = timer.untilNow(io, .real);
                const delta: f32 = @as(f32, @floatFromInt(duration.toNanoseconds())) / std.time.ns_per_s;

                for (0..@min(batch_size, fragment_count)) |batch_id| {
                    wg.async(io, Runner.runWrapper, .{ &runners[batch_id], batch_id, pixel_map, delta });
                }
                try wg.await(io);
            }

            try window.updateSurface();
        }
    }
    std.log.info("Successfully executed", .{});
}

const Runner = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    surface: sdl3.surface.Surface,
    rt: spv.Runtime,
    entry: spv.SpvWord,
    color: spv.SpvWord,
    time: spv.SpvWord,
    pos: spv.SpvWord,
    res: spv.SpvWord,
    invocation_count: usize,
    batch_size: usize,

    fn runWrapper(self: *Self, batch_id: usize, pixel_map: [*]u32, timer: f32) void {
        @call(.always_inline, Self.run, .{ self, batch_id, pixel_map, timer }) catch |err| {
            std.log.err("{s}", .{@errorName(err)});
            if (@errorReturnTrace()) |trace| {
                std.debug.dumpErrorReturnTrace(trace);
            }
            std.process.abort();
        };
    }

    fn run(self: *Self, batch_id: usize, pixel_map: [*]u32, timer: f32) !void {
        var rt = self.rt; // Copy to avoid pointer access of `self` at runtime. Okay as Runtime contains only pointers and trivially copyable fields

        var output: [4]f32 = undefined;

        var invocation_index: usize = batch_id;
        while (invocation_index < self.invocation_count) : (invocation_index += self.batch_size) {
            const y = @divTrunc(invocation_index, screen_width);
            const x = @mod(invocation_index, screen_width);

            try rt.writeInput(std.mem.asBytes(&timer), self.time);
            try rt.writeInput(std.mem.asBytes(&[_]f32{ @floatFromInt(screen_width), @floatFromInt(screen_height) }), self.res);
            try rt.writeInput(std.mem.asBytes(&[_]f32{ @floatFromInt(x), @floatFromInt(y) }), self.pos);
            try rt.callEntryPoint(self.allocator, self.entry);
            try rt.readOutput(std.mem.asBytes(output[0..]), self.color);

            const rgba = self.surface.mapRgba(
                @intCast(std.math.clamp(@as(i32, @intFromFloat(output[0] * 255.0)), 0, 255)),
                @intCast(std.math.clamp(@as(i32, @intFromFloat(output[1] * 255.0)), 0, 255)),
                @intCast(std.math.clamp(@as(i32, @intFromFloat(output[2] * 255.0)), 0, 255)),
                @intCast(std.math.clamp(@as(i32, @intFromFloat(output[3] * 255.0)), 0, 255)),
            );

            pixel_map[invocation_index] = rgba.value;
        }
    }
};
