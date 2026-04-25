# SPIR-V Interpreter <a href="https://git.kbz8.me/kbz_8/SPIRV-Interpreter/actions?workflows=build.yml"><img src="https://git.kbz8.me/kbz_8/SPIRV-Interpreter/actions/workflows/build.yml/badge.svg"></a> <a href="https://git.kbz8.me/kbz_8/SPIRV-Interpreter/actions?workflows=test.yml"><img src="https://git.kbz8.me/kbz_8/SPIRV-Interpreter/actions/workflows/test.yml/badge.svg"></a>

A small footprint SPIR-V interpreter to execute SPIR-V shaders on the CPU. It is designed to be used with multiple runtimes concurrently and can be SIMD accelerated.

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
        try rt.readOutput(std.mem.asBytes(output[0..output.len]), try rt.getResultByName("color"));
        std.log.info("Output: Vec4{any}", .{output});
    }
    std.log.info("Successfully executed", .{});
}
```

## C bindings

### Build
To build the FFI bindings just
```
zig build ffi-c --release=[fast, small, safe]
```

You can also build a shared lib using
```
zig build ffi-c --release=[fast, small, sage] -Dffi-build-static=false
```

You'll find the library in `./zig-out/lib/` and the header file in `./zig-out/include/` or in `./ffi/`.

### Example

```c
#include <stdio.h>
#include <SpirvInterpreter.h>

static const unsigned char shader_source[]  = {
    /* Shader bytecode */
}

int main(void)
{
    SpvModule module;
    SpvModuleOptions options;
    options.use_simd_vectors_specializations = 1;

    if(SpvInitModule(&module, (SpvWord*)shader_source, sizeof(shader_source) / 4, options) != SPV_RESULT_SUCCESS)
        return -1;

    SpvRuntime runtime;
    if(SpvInitRuntime(&runtime, module) != SPV_RESULT_SUCCESS)
        return -1;

    SpvWord main_entry_index;
    SpvGetEntryPointByName(runtime, "main", &main_entry_index);
    SpvCallEntryPoint(runtime, main_entry_index);

    float output[4];
    SpvWord output_result;
    SpvGetResultByName(runtime, "color", &output_result);
    SpvReadOutput(runtime, (SpvByte*)output, sizeof(output), output_result);

    printf("Output: Vec4[%f, %f, %f, %f]\n", output[0], output[1], output[2], output[3]);

    SpvDeinitRuntime(runtime);
    SpvDeinitModule(module);
    return 0;
}
```
