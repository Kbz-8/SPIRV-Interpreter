const std = @import("std");
const ffi = @import("ffi.zig");
const spv = ffi.spv;

const CSpecializationEntry = extern struct {
    id: spv.SpvWord,
    offset: c_ulong,
    size: c_ulong,
};

const LocationType = enum(c_int) {
    input = 0,
    output = 1,
};

const Vec4f = extern struct {
    x: f32,
    y: f32,
    z: f32,
    w: f32,
};

const Vec4u = extern struct {
    x: c_uint,
    y: c_uint,
    z: c_uint,
    w: c_uint,
};

const readImageFloat4_PFN = *const fn (driver_image: ?*anyopaque, x: c_int, y: c_int, z: c_int, dst: *Vec4f) callconv(.c) ffi.Result;
const readImageInt4_PFN = *const fn (driver_image: ?*anyopaque, x: c_int, y: c_int, z: c_int, dst: *Vec4u) callconv(.c) ffi.Result;
const writeImageFloat4_PFN = *const fn (driver_image: ?*anyopaque, x: c_int, y: c_int, z: c_int, src: Vec4f) callconv(.c) ffi.Result;
const writeImageInt4_PFN = *const fn (driver_image: ?*anyopaque, x: c_int, y: c_int, z: c_int, src: Vec4u) callconv(.c) ffi.Result;

const ImageAPI = extern struct {
    readImageFloat4: readImageFloat4_PFN,
    readImageInt4: readImageInt4_PFN,
    writeImageFloat4: writeImageFloat4_PFN,
    writeImageInt4: writeImageInt4_PFN,
};

fn toCResult(err: spv.Runtime.RuntimeError) ffi.Result {
    return switch (err) {
        spv.Runtime.RuntimeError.DivisionByZero => ffi.Result.DivisionByZero,
        spv.Runtime.RuntimeError.InvalidEntryPoint => ffi.Result.InvalidEntryPoint,
        spv.Runtime.RuntimeError.InvalidSpirV => ffi.Result.InvalidSpirV,
        spv.Runtime.RuntimeError.InvalidValueType => ffi.Result.InvalidValueType,
        spv.Runtime.RuntimeError.Killed => ffi.Result.Killed,
        spv.Runtime.RuntimeError.NotFound => ffi.Result.NotFound,
        spv.Runtime.RuntimeError.OutOfMemory => ffi.Result.OutOfMemory,
        spv.Runtime.RuntimeError.OutOfBounds => ffi.Result.OutOfBounds,
        spv.Runtime.RuntimeError.ToDo => ffi.Result.ToDo,
        spv.Runtime.RuntimeError.Unreachable => ffi.Result.Unreachable,
        spv.Runtime.RuntimeError.UnsupportedSpirV => ffi.Result.UnsupportedSpirV,
        spv.Runtime.RuntimeError.UnsupportedExtension => ffi.Result.UnsupportedExtension,
        spv.Runtime.RuntimeError.Unknown => ffi.Result.Unknown,
    };
}

fn fromCResult(res: ffi.Result) spv.Runtime.RuntimeError!void {
    return switch (res) {
        ffi.Result.DivisionByZero => spv.Runtime.RuntimeError.DivisionByZero,
        ffi.Result.InvalidEntryPoint => spv.Runtime.RuntimeError.InvalidEntryPoint,
        ffi.Result.InvalidSpirV => spv.Runtime.RuntimeError.InvalidSpirV,
        ffi.Result.InvalidValueType => spv.Runtime.RuntimeError.InvalidValueType,
        ffi.Result.Killed => spv.Runtime.RuntimeError.Killed,
        ffi.Result.NotFound => spv.Runtime.RuntimeError.NotFound,
        ffi.Result.OutOfMemory => spv.Runtime.RuntimeError.OutOfMemory,
        ffi.Result.OutOfBounds => spv.Runtime.RuntimeError.OutOfBounds,
        ffi.Result.ToDo => spv.Runtime.RuntimeError.ToDo,
        ffi.Result.Unreachable => spv.Runtime.RuntimeError.Unreachable,
        ffi.Result.UnsupportedSpirV => spv.Runtime.RuntimeError.UnsupportedSpirV,
        ffi.Result.UnsupportedExtension => spv.Runtime.RuntimeError.UnsupportedExtension,
        ffi.Result.Unknown => spv.Runtime.RuntimeError.Unknown,
        else => {},
    };
}

const ImageAPIBridge = struct {
    threadlocal var current_image_api: ?*const ImageAPI = null; // Hacky

    fn getImageAPI() spv.Runtime.RuntimeError!*const ImageAPI {
        return current_image_api orelse spv.Runtime.RuntimeError.Unknown;
    }

    fn readImageFloat4(driver_image: *anyopaque, x: i32, y: i32, z: i32) spv.Runtime.RuntimeError!spv.Runtime.Vec4(f32) {
        const image_api = try getImageAPI();

        var dst: Vec4f = undefined;
        const result = image_api.readImageFloat4(driver_image, @intCast(x), @intCast(y), @intCast(z), &dst);

        try fromCResult(result);

        return .{
            .x = dst.x,
            .y = dst.y,
            .z = dst.z,
            .w = dst.w,
        };
    }

    fn readImageInt4(driver_image: *anyopaque, x: i32, y: i32, z: i32) spv.Runtime.RuntimeError!spv.Runtime.Vec4(u32) {
        const image_api = try getImageAPI();

        var dst: Vec4u = undefined;
        const result = image_api.readImageInt4(driver_image, @intCast(x), @intCast(y), @intCast(z), &dst);

        try fromCResult(result);

        return .{
            .x = dst.x,
            .y = dst.y,
            .z = dst.z,
            .w = dst.w,
        };
    }

    fn writeImageFloat4(driver_image: *anyopaque, x: i32, y: i32, z: i32, pixel: spv.Runtime.Vec4(f32)) spv.Runtime.RuntimeError!void {
        const image_api = try getImageAPI();

        const result = image_api.writeImageFloat4(driver_image, @intCast(x), @intCast(y), @intCast(z), .{ .x = pixel.x, .y = pixel.y, .z = pixel.z, .w = pixel.w });

        try fromCResult(result);
    }

    fn writeImageInt4(driver_image: *anyopaque, x: i32, y: i32, z: i32, pixel: spv.Runtime.Vec4(u32)) spv.Runtime.RuntimeError!void {
        const image_api = try getImageAPI();

        const result = image_api.writeImageInt4(driver_image, @intCast(x), @intCast(y), @intCast(z), .{ .x = pixel.x, .y = pixel.y, .z = pixel.z, .w = pixel.w });

        try fromCResult(result);
    }
};

const RuntimeWrapper = struct {
    rt: spv.Runtime,
    image_api: ImageAPI,
};

export fn SpvInitRuntime(rt: **RuntimeWrapper, module: *spv.Module, image_api: ImageAPI) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;

    rt.* = allocator.create(RuntimeWrapper) catch return .OutOfMemory;
    rt.*.image_api = image_api;

    rt.*.rt = spv.Runtime.init(
        allocator,
        module,
        .{
            .readImageFloat4 = ImageAPIBridge.readImageFloat4,
            .readImageInt4 = ImageAPIBridge.readImageInt4,
            .writeImageFloat4 = ImageAPIBridge.writeImageFloat4,
            .writeImageInt4 = ImageAPIBridge.writeImageInt4,
        },
    ) catch |err| {
        allocator.destroy(rt.*);
        return toCResult(err);
    };
    return .Success;
}

export fn SpvDeinitRuntime(rt: *RuntimeWrapper) callconv(.c) void {
    const allocator = std.heap.c_allocator;
    rt.rt.deinit(allocator);
    allocator.destroy(rt);
}

export fn SpvAddSpecializationInfo(rt: *RuntimeWrapper, entry: CSpecializationEntry, data: [*]const u8, data_size: c_ulong) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;
    rt.rt.addSpecializationInfo(
        allocator,
        .{
            .id = entry.id,
            .offset = @intCast(entry.offset),
            .size = @intCast(entry.size),
        },
        data[0..data_size],
    ) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvGetEntryPointByName(rt: *RuntimeWrapper, name: [*:0]const u8, result: *spv.SpvWord) callconv(.c) ffi.Result {
    result.* = rt.rt.getEntryPointByName(std.mem.span(name)) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvGetResultByLocation(rt: *RuntimeWrapper, location: spv.SpvWord, kind: LocationType, result: *spv.SpvWord) callconv(.c) ffi.Result {
    result.* = rt.rt.getResultByLocation(location, switch (kind) {
        .input => .input,
        .output => .output,
    }) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvGetResultByName(rt: *RuntimeWrapper, name: [*:0]const u8, result: *spv.SpvWord) callconv(.c) ffi.Result {
    result.* = rt.rt.getResultByName(std.mem.span(name)) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvGetResultMemorySize(rt: *RuntimeWrapper, result: spv.SpvWord, size: *c_ulong) callconv(.c) ffi.Result {
    size.* = rt.rt.getResultMemorySize(result) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvHasResultDecoration(rt: *RuntimeWrapper, result: spv.SpvWord, decoration: spv.spv.SpvDecoration) callconv(.c) c_int {
    return if (rt.rt.hasResultDecoration(result, decoration)) 1 else 0;
}

export fn SpvCallEntryPoint(rt: *RuntimeWrapper, entry_point: spv.SpvWord) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;

    // Ultra hacky
    const previous_image_api = ImageAPIBridge.current_image_api;
    ImageAPIBridge.current_image_api = &rt.image_api;
    defer ImageAPIBridge.current_image_api = previous_image_api;

    rt.rt.callEntryPoint(allocator, entry_point) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvReadOutput(rt: *RuntimeWrapper, output: [*]u8, output_size: c_ulong, result: spv.SpvWord) callconv(.c) ffi.Result {
    rt.rt.readOutput(output[0..output_size], result) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvReadBuiltIn(rt: *RuntimeWrapper, output: [*]u8, output_size: c_ulong, builtin: spv.spv.SpvBuiltIn) callconv(.c) ffi.Result {
    rt.rt.readBuiltIn(output[0..output_size], builtin) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteInput(rt: *RuntimeWrapper, input: [*]const u8, input_size: c_ulong, result: spv.SpvWord) callconv(.c) ffi.Result {
    rt.rt.writeInput(input[0..input_size], result) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteBuiltIn(rt: *RuntimeWrapper, input: [*]const u8, input_size: c_ulong, builtin: spv.spv.SpvBuiltIn) callconv(.c) ffi.Result {
    rt.rt.writeBuiltIn(input[0..input_size], builtin) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteDescriptorSet(rt: *RuntimeWrapper, input: [*]const u8, input_size: c_ulong, set: spv.SpvWord, binding: spv.SpvWord, descriptor_index: spv.SpvWord) callconv(.c) ffi.Result {
    rt.rt.writeDescriptorSet(input[0..input_size], set, binding, descriptor_index) catch |err| return toCResult(err);
    return .Success;
}
