# SPIR-V Interpreter <a href="https://git.kbz8.me/kbz_8/SPIRV-Interpreter/actions?workflows=build.yml"><img src="https://git.kbz8.me/kbz_8/SPIRV-Interpreter/actions/workflows/build.yml/badge.svg"></a> <a href="https://git.kbz8.me/kbz_8/SPIRV-Interpreter/actions?workflows=test.yml"><img src="https://git.kbz8.me/kbz_8/SPIRV-Interpreter/actions/workflows/test.yml/badge.svg"></a>

A small footprint SPIR-V interpreter to execute SPIR-V shaders on the CPU. It is designed to be used with multiple runtimes concurrently.

```zig
const std = @import("std");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

pub fn main() !void {
    {
        var gpa: std.heap.DebugAllocator(.{}) = .init;
        defer _ = gpa.deinit();

        const allocator = gpa.allocator();

        var module = try spv.Module.init(allocator, @ptrCast(@alignCast(shader_source)), .{});
        defer module.deinit(allocator);

        var rt = try spv.Runtime.init(allocator, &module);
        defer rt.deinit(allocator);

        try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));
        var output: [4]f32 = undefined;
        try rt.readOutput(f32, output[0..output.len], try rt.getResultByName("color"));
        std.log.info("Output: Vec4{any}", .{output});
    }
    std.log.info("Successfully executed", .{});
}
```
