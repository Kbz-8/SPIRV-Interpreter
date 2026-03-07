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
    pub const Config = struct {
        source: []const u32,
        inputs: []const []const u8 = &.{},
        expected_outputs: []const []const u8 = &.{},
        descriptor_sets: []const []const []const u8 = &.{},
        expected_descriptor_sets: []const []const []const u8 = &.{},
    };

    pub fn expect(config: Config) !void {
        const allocator = std.testing.allocator;

        // To test with all important module options
        const module_options = [_]spv.Module.ModuleOptions{
            .{
                .use_simd_vectors_specializations = true,
            },
            .{
                .use_simd_vectors_specializations = false,
            },
        };

        for (module_options) |opt| {
            var module = try spv.Module.init(allocator, config.source, opt);
            defer module.deinit(allocator);

            var rt = try spv.Runtime.init(allocator, &module);
            defer rt.deinit(allocator);

            for (config.inputs, 0..) |input, n| {
                try rt.writeInput(input[0..], module.input_locations[n]);
            }

            for (config.descriptor_sets, 0..) |descriptor_set, set_index| {
                for (descriptor_set, 0..) |descriptor_binding, binding_index| {
                    try rt.writeDescriptorSet(allocator, descriptor_binding, @intCast(set_index), @intCast(binding_index));
                }
            }

            try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));

            for (config.expected_outputs, 0..) |expected, n| {
                const output = try allocator.alloc(u8, expected.len);
                defer allocator.free(output);

                try rt.readOutput(output[0..], module.output_locations[n]);
                try std.testing.expectEqualSlices(u8, expected, output);
            }

            for (config.expected_descriptor_sets, 0..) |expected_descriptor_set, set_index| {
                for (expected_descriptor_set, 0..) |expected_descriptor_binding, binding_index| {
                    const data = try allocator.alloc(u8, expected_descriptor_binding.len);
                    defer allocator.free(data);

                    try rt.readDescriptorSet(data, @intCast(set_index), @intCast(binding_index));
                    try std.testing.expectEqualSlices(u8, expected_descriptor_binding, data);
                }
            }
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
    std.testing.refAllDecls(@import("ssbo.zig"));
}
