const std = @import("std");
const sdl3 = @import("sdl3");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

const screen_width = 640;
const screen_height = 480;

pub fn main() !void {
    {
        var gpa: std.heap.DebugAllocator(.{}) = .init;
        defer _ = gpa.deinit();

        defer sdl3.shutdown();
        const init_flags = sdl3.InitFlags{ .video = true };
        try sdl3.init(init_flags);
        defer sdl3.quit(init_flags);

        const window = try sdl3.video.Window.init("Hello SDL3", screen_width, screen_height, .{});
        defer window.deinit();

        const allocator = gpa.allocator();

        var module = try spv.Module.init(allocator, @ptrCast(@alignCast(shader_source)));
        defer module.deinit(allocator);

        const surface = try window.getSurface();
        try surface.clear(.{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0 });

        {
            try surface.lock();
            defer surface.unlock();

            const map = surface.getPixels() orelse return;

            var pool: std.Thread.Pool = undefined;
            try pool.init(.{
                .allocator = allocator,
                .n_jobs = 16384,
            });
            defer pool.deinit();

            var wait_group: std.Thread.WaitGroup = .{};

            const margin_x = @divTrunc(screen_width, 5);
            const margin_y = @divTrunc(screen_height, 5);
            const top_y = margin_y;
            const bottom_y = (screen_height - 1) - margin_y;
            const center_x = @divTrunc(screen_width, 2);
            const tri_h = bottom_y - top_y;
            const max_half_w = @divTrunc(screen_width, 2) - margin_x;

            for (top_y..bottom_y) |y| {
                const t: f32 = @as(f32, @floatFromInt(y - top_y)) / @as(f32, @floatFromInt(tri_h));
                const half_w: usize = @intFromFloat((t * @as(f32, @floatFromInt(max_half_w))) + 0.5);
                const x0 = std.math.clamp(center_x - half_w, 0, screen_width - 1);
                const x1 = std.math.clamp(center_x + half_w, 0, screen_width - 1);

                for (x0..x1) |x| {
                    pool.spawnWg(&wait_group, run, .{ allocator, &module, map, surface, x, y });
                }
            }
            pool.waitAndWork(&wait_group);
        }

        try window.updateSurface();

        std.Thread.sleep(5_000_000_000);
    }
    std.log.info("Successfully executed", .{});
}

fn run(allocator: std.mem.Allocator, module: *spv.Module, map: []u8, surface: sdl3.surface.Surface, x: usize, y: usize) void {
    var rt = spv.Runtime.init(allocator, module) catch |err| std.debug.panic("Failed with error {s}", .{@errorName(err)});
    defer rt.deinit(allocator);

    var output: [4]f32 = undefined;

    const entry = rt.getEntryPointByName("main") catch |err| std.debug.panic("Failed with error {s}", .{@errorName(err)});
    const color = rt.getResultByName("color") catch |err| std.debug.panic("Failed with error {s}", .{@errorName(err)});

    rt.callEntryPoint(allocator, entry) catch |err| std.debug.panic("Failed with error {s}", .{@errorName(err)});
    rt.readOutput(f32, output[0..], color) catch |err| std.debug.panic("Failed with error {s}", .{@errorName(err)});

    const rgba = surface.mapRgba(
        @intFromFloat(output[0] * 255.0),
        @intFromFloat(output[1] * 255.0),
        @intFromFloat(output[2] * 255.0),
        @intFromFloat(output[3] * 255.0),
    );

    var pixel_map: [*]u32 = @as([*]u32, @ptrCast(@alignCast(map.ptr)));
    pixel_map[(y * surface.getWidth()) + x] = rgba.value;
}
