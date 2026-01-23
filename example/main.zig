const std = @import("std");
const sdl3 = @import("sdl3");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

const screen_width = 1250;
const screen_height = 720;

pub fn main() !void {
    {
        //var gpa: std.heap.DebugAllocator(.{}) = .init;
        //defer _ = gpa.deinit();

        defer sdl3.shutdown();
        const init_flags = sdl3.InitFlags{ .video = true, .events = true };
        try sdl3.init(init_flags);
        defer sdl3.quit(init_flags);

        const window = try sdl3.video.Window.init("Hello triangle", screen_width, screen_height, .{});
        defer window.deinit();

        const surface = try window.getSurface();

        const allocator = std.heap.smp_allocator;

        var module = try spv.Module.init(allocator, @ptrCast(@alignCast(shader_source)), .{});
        defer module.deinit(allocator);

        var runner_cache: std.ArrayList(Runner) = try .initCapacity(allocator, screen_height);
        defer {
            for (runner_cache.items) |*runner| {
                runner.rt.deinit(allocator);
            }
            runner_cache.deinit(allocator);
        }

        for (0..screen_height) |_| {
            (try runner_cache.addOne(allocator)).* = .{
                .allocator = allocator,
                .surface = surface,
                .rt = try spv.Runtime.init(allocator, &module),
            };
        }

        var thread_pool: std.Thread.Pool = undefined;
        try thread_pool.init(.{
            .allocator = allocator,
        });

        var quit = false;
        while (!quit) {
            try surface.clear(.{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0 });

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

                var timer = try std.time.Timer.start();
                defer {
                    const ns = timer.lap();
                    const ms = @as(f32, @floatFromInt(ns)) / std.time.ns_per_s;
                    std.log.info("Took {d:.3}s - {d:.3}fps to render", .{ ms, 1.0 / ms });
                }

                var wait_group: std.Thread.WaitGroup = .{};
                for (0..screen_height) |y| {
                    const runner = &runner_cache.items[y];
                    thread_pool.spawnWg(&wait_group, Runner.run, .{ runner, y, pixel_map });
                }
                thread_pool.waitAndWork(&wait_group);
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

    fn run(self: *Self, y: usize, pixel_map: [*]u32) void {
        var rt = self.rt; // Copy to avoid pointer access of `self` at runtime. Okay as Runtime contains only pointers and trivially copyable fields

        const entry = rt.getEntryPointByName("main") catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
        const color = rt.getResultByName("color") catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
        const time = rt.getResultByName("time") catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
        const pos = rt.getResultByName("pos") catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
        const res = rt.getResultByName("res") catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
        var output: [4]f32 = undefined;

        for (0..screen_width) |x| {
            rt.writeInput(f32, &.{@as(f32, @floatFromInt(std.time.milliTimestamp()))}, time) catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
            rt.writeInput(f32, &.{ @floatFromInt(screen_width), @floatFromInt(screen_height) }, res) catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
            rt.writeInput(f32, &.{ @floatFromInt(x), @floatFromInt(y) }, pos) catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
            rt.callEntryPoint(self.allocator, entry) catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});
            rt.readOutput(f32, output[0..], color) catch |err| std.debug.panic("Catch error {s}", .{@errorName(err)});

            const rgba = self.surface.mapRgba(
                @truncate(@as(u32, @intFromFloat(output[0] * 255.0))),
                @truncate(@as(u32, @intFromFloat(output[1] * 255.0))),
                @truncate(@as(u32, @intFromFloat(output[2] * 255.0))),
                @truncate(@as(u32, @intFromFloat(output[3] * 255.0))),
            );

            pixel_map[(y * self.surface.getWidth()) + x] = rgba.value;
        }
    }
};
