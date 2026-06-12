# SPIR-V Interpreter Usage

## Concepts

A typical invocation has this lifecycle:

1. Load SPIR-V bytecode.
2. Create a `Module` from the bytecode.
3. Create one or more `Runtime` instances from the module.
4. Write inputs, built-ins, descriptors, push constants, or specialization constants.
5. Execute an entry point.
6. Read outputs or built-ins.
7. Destroy runtimes, then destroy the module.

A `Module` represents parsed SPIR-V bytecode. A `Runtime` represents one executable invocation state. Use separate runtimes when running invocations concurrently.

---

# Zig usage

## Add the dependency

With Zig `0.16.0` or newer:

```sh
zig fetch --save git+https://git.kbz8.me/kbz_8/SPIRV-Interpreter
```

Then import the package module from your `build.zig`:

```zig
const spv_dep = b.dependency("SPIRV_Interpreter", .{
    .target = target,
    .optimize = optimize,
});

exe.root_module.addImport("spv", spv_dep.module("spv"));
```

In your code:

```zig
const spv = @import("spv");
```

## Minimal Zig example

This example loads an embedded `.spv` file, calls the `main` entry point, and reads a `vec4<f32>` output named `color`.

```zig
const std = @import("std");
const spv = @import("spv");

const shader_source = @embedFile("shader.spv");

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    var module = try spv.Module.init(
        allocator,
        @ptrCast(@alignCast(shader_source)),
        .{},
    );
    defer module.deinit(allocator);

    // If the shader does not execute image operations, the image API is unused.
    // For image load/store/sampling shaders, provide a real spv.Runtime.ImageAPI.
    var rt = try spv.Runtime.init(allocator, &module, undefined);
    defer rt.deinit(allocator);

    const entry = try rt.getEntryPointByName("main");
    const color_result = try rt.getResultByName("color");

    try rt.callEntryPoint(allocator, entry);

    var color: [4]f32 = undefined;
    try rt.readOutput(std.mem.sliceAsBytes(color[0..]), color_result);

    std.debug.print("color = {any}\n", .{color});
}
```

## Writing inputs

You can write an input by result id:

```zig
const pos_result = try rt.getResultByName("pos");

const pos = [_]f32{ 10.0, 20.0 };
try rt.writeInput(std.mem.sliceAsBytes(pos[0..]), pos_result);
```

Or by location:

```zig
const uv = [_]f32{ 0.25, 0.75 };
try rt.writeInputLocation(std.mem.sliceAsBytes(uv[0..]), 0);
```

For scalar or struct data, use `std.mem.asBytes`:

```zig
const time: f32 = 1.5;
const time_result = try rt.getResultByName("time");

try rt.writeInput(std.mem.asBytes(&time), time_result);
```

## Reading outputs

You can read an output by result id:

```zig
const color_result = try rt.getResultByName("color");

var color: [4]f32 = undefined;
try rt.readOutput(std.mem.sliceAsBytes(color[0..]), color_result);
```

Or locate an output by `Location` decoration:

```zig
const color_result = try rt.getResultByLocation(0, .output);

var color: [4]f32 = undefined;
try rt.readOutput(std.mem.sliceAsBytes(color[0..]), color_result);
```

## Push constants

Push constants are passed as raw bytes:

```zig
const PushConstants = extern struct {
    time: f32,
    scale: f32,
};

const push_constants = PushConstants{
    .time = 1.0,
    .scale = 2.0,
};

try rt.populatePushConstants(std.mem.asBytes(&push_constants));
```

## Descriptor sets

Descriptor writes are passed as raw bytes and selected by set, binding, and descriptor index:

```zig
try rt.writeDescriptorSet(
    std.mem.sliceAsBytes(buffer[0..]),
    0, // set
    1, // binding
    0, // descriptor index
);
```

For non-array descriptors, use descriptor index `0`.

After running a shader that writes through descriptor-backed memory, flush descriptor sets before reading the backing data:

```zig
try rt.callEntryPoint(allocator, entry);
try rt.flushDescriptorSets(allocator);
```

## Specialization constants

Specialization constants are selected by specialization id:

```zig
const value: u32 = 64;

try rt.addSpecializationInfo(
    allocator,
    .{
        .id = 0,
        .offset = 0,
        .size = @sizeOf(u32),
    },
    std.mem.asBytes(&value),
);
```

Add specialization constants before calling the entry point.

To copy specialization constants between runtimes:

```zig
try rt.copySpecializationConstantsFrom(allocator, &source_rt);
```

## Entry points and barriers

For most shaders, `callEntryPoint` is enough:

```zig
try rt.callEntryPoint(allocator, entry);
```

For shaders that may hit barriers, use `beginEntryPoint` and `continueEntryPoint`:

```zig
var status = try rt.beginEntryPoint(allocator, entry);

while (status == .barrier) {
    // Synchronize other invocations here if needed.
    status = try rt.continueEntryPoint(allocator);
}
```

## Multiple runtimes

A module can be shared by multiple runtimes. This is the preferred model for parallel execution:

```zig
var rt_a = try spv.Runtime.init(allocator, &module, undefined);
defer rt_a.deinit(allocator);

var rt_b = try spv.Runtime.init(allocator, &module, undefined);
defer rt_b.deinit(allocator);
```

Do not mutate the same runtime concurrently from multiple threads. Use one runtime per worker or invocation stream.

## Image operations

Shaders that use image load, image store, image sampling, or image-size queries need a real image API:

```zig
const image_api = spv.Runtime.ImageAPI{
    .readImageFloat4 = readImageFloat4,
    .readImageInt4 = readImageInt4,
    .writeImageFloat4 = writeImageFloat4,
    .writeImageInt4 = writeImageInt4,
    .sampleImageFloat4 = sampleImageFloat4,
    .sampleImageInt4 = sampleImageInt4,
    .sampleImageDref = sampleImageDref,
    .queryImageSize = queryImageSize,
};

var rt = try spv.Runtime.init(allocator, &module, image_api);
```

Sample callbacks receive optional explicit LOD and an integer texel offset:

```zig
fn sampleImageFloat4(
    driver_image: *anyopaque,
    driver_sampler: *anyopaque,
    dim: spv.SpvDim,
    x: f32,
    y: f32,
    z: f32,
    lod: ?f32,
    offset: spv.Runtime.ImageOffset,
) spv.Runtime.RuntimeError!spv.Runtime.Vec4(f32) {
    _ = .{ driver_image, driver_sampler, dim, x, y, z, lod, offset };
    return .{ .x = 0, .y = 0, .z = 0, .w = 1 };
}
```

Depth-comparison samplers call `sampleImageDref` and return a scalar `f32`.

## Derivatives

Fragment shaders using derivative operations need derivative data on the source result:

```zig
try rt.setDerivativeFromMemory(
    allocator,
    input_result,
    std.mem.asBytes(&dx),
    std.mem.asBytes(&dy),
);

try rt.copyDerivative(allocator, dst_result, input_result);
rt.clearDerivative(allocator, dst_result);
```

For low-level integrations, `setDerivative` accepts interpreter `Value` objects directly.

---

# C usage

## Build the C FFI

Build the static C FFI library:

```sh
zig build ffi-c --release=fast
```

Other release modes are also supported:

```sh
zig build ffi-c --release=safe
zig build ffi-c --release=small
```

To build a shared library instead of a static library:

```sh
zig build ffi-c --release=fast -Dffi-build-static=false
```

The library is installed into:

```text
zig-out/lib/
```

The public header is installed into:

```text
zig-out/include/SpirvInterpreter.h
```

The source header is also available in:

```text
ffi/SpirvInterpreter.h
```

## Minimal C example

```c
#include <stdio.h>
#include <SpirvInterpreter.h>

static const unsigned char shader_source[]  = {
    /* Shader bytecode */
};

int main(void)
{
    SpvModule module;
    SpvModuleOptions options;
    options.use_simd_vectors_specializations = 1;

    if(SpvInitModule(&module, (SpvWord*)shader_source, sizeof(shader_source) / 4, options) != SPV_RESULT_SUCCESS)
        return -1;

    SpvRuntime runtime;
    /**
     * A zeroed image API is only safe when the shader does not execute image
     * load/store/sample/query operations.
     */
    if(SpvInitRuntime(&runtime, module, (SpvImageAPI){0}) != SPV_RESULT_SUCCESS)
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

## Writing inputs from C

Write by result id:

```c
SpvWord pos_result = 0;

if (SpvGetResultByName(runtime, "pos", &pos_result) != SPV_RESULT_SUCCESS)
    return 1;

float pos[2] = {10.0f, 20.0f};

if (SpvWriteInput(runtime, (const SpvByte*)pos, sizeof(pos), pos_result) != SPV_RESULT_SUCCESS)
    return 1;
```

Write by input location:

```c
float uv[2] = {0.25f, 0.75f};

if (SpvWriteInputLocation(runtime, (const SpvByte*)uv, sizeof(uv), 0) != SPV_RESULT_SUCCESS)
    return 1;
```

## Reading outputs from C

Read by result id:

```c
SpvWord color_result = 0;
float color[4] = {0};

SpvGetResultByName(runtime, "color", &color_result);
SpvReadOutput(runtime, (SpvByte*)color, sizeof(color), color_result);
```

Read by output location:

```c
SpvWord color_result = 0;

SpvGetResultByLocation(runtime, 0, SPV_LOCATION_OUTPUT, &color_result);
```

For component-qualified locations:

```c
SpvWord result = 0;

SpvGetResultByLocationComponent(runtime, 0, 1, SPV_LOCATION_OUTPUT, &result);
```

## Push constants from C

```c
typedef struct PushConstants
{
    float time;
    float scale;
} PushConstants;

PushConstants push_constants = {
    .time = 1.0f,
    .scale = 2.0f,
};

SpvPopulatePushConstants(runtime, (const SpvByte*)&push_constants, sizeof(push_constants));
```

## Descriptor sets from C

```c
SpvWriteDescriptorSet(runtime, (const SpvByte*)buffer, buffer_size,
    0, /* set */
    1, /* binding */
    0  /* descriptor index */
);
```

For non-array descriptors, use descriptor index `0`.

You can query a descriptor binding from a module:

```c
SpvWord result = 0;
SpvResult status = SpvModuleGetBindingResult(module, 0, 1, &result);
```

After a shader writes through descriptor-backed memory, flush descriptor sets before reading the backing data:

```c
SpvCallEntryPoint(runtime, entry);
SpvFlushDescriptorSets(runtime);
```

## Specialization constants from C

```c
unsigned int value = 64;

SpvRuntimeSpecializationEntry entry = {
    .id = 0,
    .offset = 0,
    .size = sizeof(value),
};

SpvAddSpecializationInfo(runtime, entry, (const SpvByte*)&value, sizeof(value));
```

Add specialization constants before calling the entry point.

To duplicate specialization constants from another runtime:

```c
SpvCopySpecializationConstantsFrom(runtime, source_runtime);
```

## Derivatives from C

```c
float dx[4] = {1.0f, 0.0f, 0.0f, 0.0f};
float dy[4] = {0.0f, 1.0f, 0.0f, 0.0f};

SpvSetDerivativeFromMemory(
    runtime,
    result,
    (const SpvByte*)dx,
    sizeof(dx),
    (const SpvByte*)dy,
    sizeof(dy));

SpvCopyDerivative(runtime, dst_result, result);
SpvClearDerivative(runtime, dst_result);
```

## Barriers from C

For most shaders:

```c
SpvCallEntryPoint(runtime, entry);
```

For shaders that may hit barriers:

```c
SpvEntryPointStatus status = SPV_ENTRY_POINT_COMPLETED;

SpvBeginEntryPoint(runtime, entry, &status);

while (status == SPV_ENTRY_POINT_BARRIER)
{
    /* Synchronize other invocations here if needed. */
    SpvContinueEntryPoint(runtime, &status);
}
```

## Image API from C

Shaders that execute image operations must provide callbacks in `SpvImageAPI`.

```c
static SpvResult ReadImageFloat4(
    void* driver_image,
    SpvDim dim,
    int x,
    int y,
    int z,
    SpvVec4f* dst)
{
    (void)driver_image;
    (void)dim;
    (void)x;
    (void)y;
    (void)z;

    dst->x = 0.0f;
    dst->y = 0.0f;
    dst->z = 0.0f;
    dst->w = 1.0f;

    return SPV_RESULT_SUCCESS;
}
```

Sampling callbacks include an explicit-LOD flag/value and an offset:

```c
static SpvResult SampleImageFloat4(
    void* driver_image,
    void* driver_sampler,
    SpvDim dim,
    float x,
    float y,
    float z,
    SpvBool has_lod,
    float lod,
    SpvImageOffset offset,
    SpvVec4f* dst)
{
    (void)driver_image;
    (void)driver_sampler;
    (void)dim;
    (void)has_lod;
    (void)lod;
    (void)offset;

    dst->x = x;
    dst->y = y;
    dst->z = z;
    dst->w = 1.0f;
    return SPV_RESULT_SUCCESS;
}
```

Depth-comparison samplers add `dref` and write one `float`:

```c
static SpvResult SampleImageDref(
    void* driver_image,
    void* driver_sampler,
    SpvDim dim,
    float x,
    float y,
    float z,
    float dref,
    SpvBool has_lod,
    float lod,
    SpvImageOffset offset,
    float* dst)
{
    (void)driver_image;
    (void)driver_sampler;
    (void)dim;
    (void)x;
    (void)y;
    (void)z;
    (void)has_lod;
    (void)lod;
    (void)offset;

    *dst = dref;
    return SPV_RESULT_SUCCESS;
}
```

The image API table contains these callbacks:

```c
SpvImageAPI image_api = {
    .SpvReadImageFloat4 = ReadImageFloat4,
    .SpvReadImageInt4 = ReadImageInt4,
    .SpvWriteImageFloat4 = WriteImageFloat4,
    .SpvWriteImageInt4 = WriteImageInt4,
    .SpvSampleImageFloat4 = SampleImageFloat4,
    .SpvSampleImageInt4 = SampleImageInt4,
    .SpvSampleImageDref = SampleImageDref,
    .SpvQueryImageSize = QueryImageSize,
};
```

Pass the table when creating the runtime:

```c
SpvInitRuntime(&runtime, module, image_api);
```

## Cleanup order

Always destroy runtimes before destroying the module they were created from:

```c
SpvDeinitRuntime(runtime);
SpvDeinitModule(module);
```
