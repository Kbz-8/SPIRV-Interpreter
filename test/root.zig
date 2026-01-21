const std = @import("std");
const spv = @import("spv");
const nzsl = @import("nzsl");

pub fn compileNzsl(allocator: std.mem.Allocator, source: []const u8) ![]const u32 {
    const module = try nzsl.parser.parseSource(source);
    defer module.deinit();

    const params = try nzsl.BackendParameters.init();
    defer params.deinit();
    params.setDebugLevel(.full);

    const writer = try nzsl.SpirvWriter.init();
    defer writer.deinit();

    const output = try writer.generate(module, params);
    defer output.deinit();

    return allocator.dupe(u32, output.getCode());
}

pub const case = struct {
    pub fn expectOutput(comptime T: type, comptime len: usize, source: []const u32, output_name: []const u8, expected: []const T) !void {
        const allocator = std.testing.allocator;

        const module_options = [_]spv.Module.ModuleOptions{
            .{
                .use_simd_vectors_specializations = true,
            },
            .{
                .use_simd_vectors_specializations = false,
            },
        };

        for (module_options) |opt| {
            var module = try spv.Module.init(allocator, source, opt);
            defer module.deinit(allocator);

            var rt = try spv.Runtime.init(allocator, &module);
            defer rt.deinit(allocator);

            try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));
            var output: [len]T = undefined;
            try rt.readOutput(T, output[0..len], try rt.getResultByName(output_name));

            try std.testing.expectEqualSlices(T, expected, &output);
        }
    }

    pub fn expectOutputWithInput(comptime T: type, comptime len: usize, source: []const u32, output_name: []const u8, expected: []const T, input_name: []const u8, input: []const T) !void {
        const allocator = std.testing.allocator;

        const module_options = [_]spv.Module.ModuleOptions{
            .{
                .use_simd_vectors_specializations = true,
            },
            .{
                .use_simd_vectors_specializations = false,
            },
        };

        for (module_options) |opt| {
            var module = try spv.Module.init(allocator, source, opt);
            defer module.deinit(allocator);

            var rt = try spv.Runtime.init(allocator, &module);
            defer rt.deinit(allocator);

            try rt.writeInput(T, input[0..len], try rt.getResultByName(input_name));

            try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));
            var output: [len]T = undefined;
            try rt.readOutput(T, output[0..len], try rt.getResultByName(output_name));

            try std.testing.expectEqualSlices(T, expected, &output);
        }
    }

    pub fn random(comptime T: type) T {
        var prng: std.Random.DefaultPrng = .init(@intCast(std.time.microTimestamp()));
        const rand = prng.random();

        return switch (@typeInfo(T)) {
            .int => rand.int(T),
            .float => rand.float(T),
            .vector => |v| blk: {
                var vec: @Vector(v.len, v.child) = undefined;
                for (0..v.len) |i| {
                    vec[i] = random(v.child);
                }
                break :blk vec;
            },
            inline else => unreachable,
        };
    }

    pub fn Vec(comptime len: usize, comptime T: type) type {
        return struct {
            const Self = @This();
            val: @Vector(len, T),
            pub fn format(self: *const Self, w: *std.Io.Writer) std.Io.Writer.Error!void {
                inline for (0..len) |i| {
                    try w.print("{d}", .{self.val[i]});
                    if (i < len - 1) try w.writeAll(", ");
                }
            }
        };
    }
};

test {
    std.testing.refAllDecls(@import("arrays.zig"));
    std.testing.refAllDecls(@import("basics.zig"));
    std.testing.refAllDecls(@import("bitwise.zig"));
    std.testing.refAllDecls(@import("branching.zig"));
    std.testing.refAllDecls(@import("casts.zig"));
    std.testing.refAllDecls(@import("functions.zig"));
    std.testing.refAllDecls(@import("inputs.zig"));
    std.testing.refAllDecls(@import("loops.zig"));
    std.testing.refAllDecls(@import("maths.zig"));
}
