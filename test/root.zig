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
    pub fn expectOutput(comptime T: type, source: []const u32, output_name: []const u8, comptime expected: []const T) !void {
        const allocator = std.testing.allocator;

        var module = try spv.Module.init(allocator, source);
        defer module.deinit(allocator);

        var rt = try spv.Runtime.init(allocator, &module);
        defer rt.deinit(allocator);

        try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));
        var output: [expected.len]T = undefined;
        try rt.readOutput(T, output[0..output.len], try rt.getResultByName(output_name));

        try std.testing.expectEqualSlices(T, expected, &output);
    }
};

test {
    std.testing.refAllDecls(@import("basics.zig"));
    std.testing.refAllDecls(@import("maths.zig"));
}
