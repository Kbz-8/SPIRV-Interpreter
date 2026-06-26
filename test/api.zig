const std = @import("std");
const spv = @import("spv");
const root = @import("root.zig");

const compileNzsl = root.compileNzsl;

const ImageState = struct {
    expected_sampler: *anyopaque,
    sample_calls: usize = 0,
    dref_calls: usize = 0,
    last_x: f32 = 0,
    last_y: f32 = 0,
    last_z: f32 = 0,
    last_dref: f32 = 0,
    last_lod: ?f32 = null,
    last_offset: spv.Runtime.ImageOffset = .{},
};

fn readImageFloat4(_: *anyopaque, _: spv.spv.SpvDim, _: i32, _: i32, _: i32, _: ?i32) spv.Runtime.RuntimeError!spv.Runtime.Vec4(f32) {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

fn readImageInt4(_: *anyopaque, _: spv.spv.SpvDim, _: i32, _: i32, _: i32, _: ?i32) spv.Runtime.RuntimeError!spv.Runtime.Vec4(u32) {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

fn writeImageFloat4(_: *anyopaque, _: spv.spv.SpvDim, _: i32, _: i32, _: i32, _: spv.Runtime.Vec4(f32)) spv.Runtime.RuntimeError!void {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

fn writeImageInt4(_: *anyopaque, _: spv.spv.SpvDim, _: i32, _: i32, _: i32, _: spv.Runtime.Vec4(u32)) spv.Runtime.RuntimeError!void {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

fn sampleImageFloat4(driver_image: *anyopaque, driver_sampler: *anyopaque, _: spv.spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32, offset: spv.Runtime.ImageOffset) spv.Runtime.RuntimeError!spv.Runtime.Vec4(f32) {
    const state: *ImageState = @ptrCast(@alignCast(driver_image));
    if (state.expected_sampler != driver_sampler) return spv.Runtime.RuntimeError.InvalidSpirV;
    state.sample_calls += 1;
    state.last_x = x;
    state.last_y = y;
    state.last_z = z;
    state.last_lod = lod;
    state.last_offset = offset;
    return .{ .x = x, .y = y, .z = 9.0, .w = 1.0 };
}

fn sampleImageInt4(_: *anyopaque, _: *anyopaque, _: spv.spv.SpvDim, _: f32, _: f32, _: f32, _: ?f32, _: spv.Runtime.ImageOffset) spv.Runtime.RuntimeError!spv.Runtime.Vec4(u32) {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

fn sampleImageDref(driver_image: *anyopaque, driver_sampler: *anyopaque, _: spv.spv.SpvDim, x: f32, y: f32, z: f32, dref: f32, lod: ?f32, offset: spv.Runtime.ImageOffset) spv.Runtime.RuntimeError!f32 {
    const state: *ImageState = @ptrCast(@alignCast(driver_image));
    if (state.expected_sampler != driver_sampler) return spv.Runtime.RuntimeError.InvalidSpirV;
    state.dref_calls += 1;
    state.last_x = x;
    state.last_y = y;
    state.last_z = z;
    state.last_dref = dref;
    state.last_lod = lod;
    state.last_offset = offset;
    return dref + x + y;
}

fn queryImageSize(_: *anyopaque, _: spv.spv.SpvDim, _: bool, _: ?i32) spv.Runtime.RuntimeError!spv.Runtime.Vec4(u32) {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

fn queryImageLevels(_: *anyopaque) spv.Runtime.RuntimeError!u32 {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

fn queryImageSamples(_: *anyopaque) spv.Runtime.RuntimeError!u32 {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

fn queryImageLod(_: *anyopaque, _: *anyopaque, _: spv.spv.SpvDim, _: spv.Runtime.ImageDerivatives) spv.Runtime.RuntimeError!spv.Runtime.Vec4(f32) {
    return spv.Runtime.RuntimeError.UnsupportedSpirV;
}

const image_api: spv.Runtime.ImageAPI = .{
    .readImageFloat4 = readImageFloat4,
    .readImageInt4 = readImageInt4,
    .writeImageFloat4 = writeImageFloat4,
    .writeImageInt4 = writeImageInt4,
    .sampleImageFloat4 = sampleImageFloat4,
    .sampleImageInt4 = sampleImageInt4,
    .sampleImageDref = sampleImageDref,
    .queryImageSize = queryImageSize,
    .queryImageLevels = queryImageLevels,
    .queryImageSamples = queryImageSamples,
    .queryImageLod = queryImageLod,
};

fn initModule(allocator: std.mem.Allocator, shader: []const u8) !struct { code: []const u32, module: spv.Module } {
    const code = try compileNzsl(allocator, shader);
    errdefer allocator.free(code);

    const module = try spv.Module.init(allocator, code, .{
        .use_simd_vectors_specializations = false,
    });
    return .{ .code = code, .module = module };
}

test "Runtime API lifecycle" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct FragIn
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main(input: FragIn) -> FragOut
        \\ {
        \\     let output: FragOut;
        \\     output.color = input.color;
        \\     return output;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    const input_result = try rt.getResultByLocationComponent(0, 0, .input);
    const output_result = try rt.getResultByName("color");
    const output_location_result = try rt.getResultByLocationComponent(0, 0, .output);
    try std.testing.expectEqual(input_result, try rt.getResultByLocation(0, .input));
    try std.testing.expectEqual(output_location_result, try rt.getResultByLocation(0, .output));
    try std.testing.expectEqual(@as(usize, 16), try rt.getInputLocationMemorySize(0));
    try std.testing.expectEqual(@as(usize, 16), try rt.getResultMemorySize(output_result));
    try std.testing.expectEqual(@as(usize, 16), try rt.getResultMemorySize(output_location_result));
    try std.testing.expectEqual(.Float, try rt.getResultPrimitiveType(output_result));
    try std.testing.expect(rt.hasResultDecoration(output_result, .Location));

    const input = [_]f32{ 10.0, 20.0, 30.0, 40.0 };
    try rt.writeInputLocation(std.mem.asBytes(&input), 0);

    const entry = try rt.getEntryPointByName("main");
    try std.testing.expectEqual(.completed, try rt.beginEntryPoint(allocator, entry));

    var output: [4]f32 = undefined;
    try rt.readOutput(std.mem.asBytes(&output), output_location_result);
    try std.testing.expectEqualSlices(f32, &input, &output);

    rt.resetInvocation(allocator);
}

test "Module binding writes" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ [layout(std430)]
        \\ struct SSBO
        \\ {
        \\     value: u32
        \\ }
        \\
        \\ external
        \\ {
        \\     [set(2), binding(3)] ssbo: storage[SSBO],
        \\ }
        \\
        \\ [entry(compute)]
        \\ [workgroup(1, 1, 1)]
        \\ fn main()
        \\ {
        \\     ssbo.value = ssbo.value + 7;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    const binding_result = compiled.module.getBindingResult(2, 3) orelse return error.TestExpectedEqual;
    try std.testing.expectEqual(@as(?spv.SpvWord, null), compiled.module.getBindingResult(0, 0));

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    var storage: u32 = 35;
    try rt.writeDescriptorSet(std.mem.asBytes(&storage), 2, 3, 0);
    try std.testing.expectEqual(error.NotFound, rt.writeDescriptorSet(std.mem.asBytes(&storage), 2, 4, 0));

    try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));
    try rt.flushDescriptorSets(allocator);

    _ = binding_result;
    try std.testing.expectEqual(@as(u32, 42), storage);
}

test "Push constants" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct Data
        \\ {
        \\     color: vec4[f32]
        \\ }
        \\
        \\ external
        \\ {
        \\     data: push_constant[Data]
        \\ }
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let output: FragOut;
        \\     output.color = data.color;
        \\     return output;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    const push_constants = [_]f32{ 0.125, 0.25, 0.5, 1.0 };
    try rt.populatePushConstants(std.mem.asBytes(&push_constants));
    try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));

    var output: [4]f32 = undefined;
    try rt.readOutput(std.mem.asBytes(&output), try rt.getResultByLocation(0, .output));
    try std.testing.expectEqualSlices(f32, &push_constants, &output);
}

test "Built-in inputs and outputs" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct VertIn
        \\ {
        \\     [builtin(vertex_index)] vertex_index: i32
        \\ }
        \\
        \\ struct VertOut
        \\ {
        \\     [builtin(position)] position: vec4[f32]
        \\ }
        \\
        \\ [entry(vert)]
        \\ fn main(input: VertIn) -> VertOut
        \\ {
        \\     let value = f32(input.vertex_index);
        \\     let output: VertOut;
        \\     output.position = vec4[f32](value, value + 1.0, value + 2.0, 1.0);
        \\     return output;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    const vertex_index: i32 = 7;
    try rt.writeBuiltIn(allocator, std.mem.asBytes(&vertex_index), .VertexIndex);
    try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));

    var position: [4]f32 = undefined;
    try rt.readBuiltIn(std.mem.asBytes(&position), .Position);
    try std.testing.expectEqualSlices(f32, &.{ 7.0, 8.0, 9.0, 1.0 }, &position);
}

test "Integer output metadata" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] value: u32
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let output: FragOut;
        \\     output.value = 0xA5A5_A5A5;
        \\     return output;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    const output_result = try rt.getResultByLocation(0, .output);
    try std.testing.expectEqual(.UInt, try rt.getResultPrimitiveType(output_result));
    try std.testing.expectEqual(@as(usize, 4), try rt.getResultMemorySize(output_result));

    try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));

    var output: u32 = 0;
    try rt.readOutput(std.mem.asBytes(&output), output_result);
    try std.testing.expectEqual(@as(u32, 0xA5A5_A5A5), output);
}

test "Runtime API error paths" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let output: FragOut;
        \\     output.color = vec4[f32](1.0, 2.0, 3.0, 4.0);
        \\     return output;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    try std.testing.expectEqual(error.NotFound, rt.getEntryPointByName("missing"));
    try std.testing.expectEqual(error.NotFound, rt.getResultByName("missing"));
    try std.testing.expectEqual(error.NotFound, rt.getResultByLocation(31, .input));
    try std.testing.expectEqual(error.NotFound, rt.getInputLocationMemorySize(31));

    try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));

    var too_small: [3]u8 = undefined;
    try std.testing.expectEqual(error.OutOfBounds, rt.readOutput(&too_small, try rt.getResultByLocation(0, .output)));
}

test "Derivative memory buffers" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct FragIn
        \\ {
        \\     [location(0)] normal: vec3[f32]
        \\ }
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main(input: FragIn) -> FragOut
        \\ {
        \\     let output: FragOut;
        \\     output.color = vec4[f32](input.normal.x, input.normal.y, input.normal.z, 1.0);
        \\     return output;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    const input_result = try rt.getResultByLocation(0, .input);
    const dx = [_]f32{ -1.0, 2.0, -3.0 };
    const dy = [_]f32{ 4.0, -5.0, 6.0 };
    const short_dx = [_]f32{ -1.0, 2.0 };
    try std.testing.expectEqual(error.OutOfBounds, rt.setDerivativeFromMemory(allocator, input_result, std.mem.asBytes(&short_dx), std.mem.asBytes(&dy)));

    const output_result = try rt.getResultByName("color");
    try rt.setDerivativeFromMemory(allocator, input_result, std.mem.asBytes(&dx), std.mem.asBytes(&dy));
    try rt.copyDerivative(allocator, output_result, input_result);
    rt.clearDerivative(allocator, input_result);
    rt.clearDerivative(allocator, output_result);

    const input = [_]f32{ 1.0, 2.0, 3.0 };
    try rt.writeInput(allocator, std.mem.asBytes(&input), input_result);
    try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));

    var output: [4]f32 = undefined;
    try rt.readOutput(std.mem.asBytes(&output), try rt.getResultByLocation(0, .output));
    try std.testing.expectEqualSlices(f32, &.{ 1.0, 2.0, 3.0, 1.0 }, &output);

    try rt.setDerivativeFromMemory(allocator, input_result, std.mem.asBytes(&dx), std.mem.asBytes(&dy));
    try rt.copyDerivative(allocator, output_result, input_result);
    rt.clearDerivative(allocator, output_result);
}

test "Image sampling callback" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ external
        \\ {
        \\     [set(0), binding(0)] tex: sampler2D[f32],
        \\ }
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let output: FragOut;
        \\     output.color = tex.Sample(vec2[f32](0.25, 0.75));
        \\     return output;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    var sampler: u8 = 0;
    var image_state: ImageState = .{ .expected_sampler = &sampler };
    var descriptor = [_]usize{
        @intFromPtr(&image_state),
        @intFromPtr(&sampler),
    };

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    try rt.writeDescriptorSet(std.mem.asBytes(&descriptor), 0, 0, 0);
    try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));

    var output: [4]f32 = undefined;
    try rt.readOutput(std.mem.asBytes(&output), try rt.getResultByName("color"));
    try std.testing.expectEqualSlices(f32, &.{ 0.25, 0.75, 9.0, 1.0 }, &output);
    try std.testing.expectEqual(@as(usize, 1), image_state.sample_calls);
    try std.testing.expectEqual(@as(?f32, null), image_state.last_lod);
    try std.testing.expectEqual(spv.Runtime.ImageOffset{}, image_state.last_offset);
}

test "Depth sampling dref callback" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ external
        \\ {
        \\     [set(0), binding(0)] tex: depth_sampler2D[f32],
        \\ }
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let value = tex.SampleDepthComp(vec2[f32](0.25, 0.75), 0.5);
        \\     let output: FragOut;
        \\     output.color = vec4[f32](value, 0.0, 0.0, 1.0);
        \\     return output;
        \\ }
    ;

    var compiled = try initModule(allocator, shader);
    defer allocator.free(compiled.code);
    defer compiled.module.deinit(allocator);

    var sampler: u8 = 0;
    var image_state: ImageState = .{ .expected_sampler = &sampler };
    var descriptor = [_]usize{
        @intFromPtr(&image_state),
        @intFromPtr(&sampler),
    };

    var rt = try spv.Runtime.init(allocator, &compiled.module, image_api);
    defer rt.deinit(allocator);

    try rt.writeDescriptorSet(std.mem.asBytes(&descriptor), 0, 0, 0);
    try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));

    var output: [4]f32 = undefined;
    try rt.readOutput(std.mem.asBytes(&output), try rt.getResultByName("color"));
    try std.testing.expectEqualSlices(f32, &.{ 1.5, 0.0, 0.0, 1.0 }, &output);
    try std.testing.expectEqual(@as(usize, 1), image_state.dref_calls);
    try std.testing.expectEqual(@as(f32, 0.5), image_state.last_dref);
}
