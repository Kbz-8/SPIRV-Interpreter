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

const EntryPointStatus = enum(c_int) {
    completed = 0,
    barrier = 1,
};

const PrimitiveType = enum(c_int) {
    bool = 0,
    float = 1,
    sint = 2,
    uint = 3,
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

const ImageOffset = extern struct {
    x: c_int,
    y: c_int,
    z: c_int,
};

const ImageDerivatives = extern struct {
    dx: Vec4f,
    dy: Vec4f,
};

const ReadImageInfo = extern struct {
    driver_image: ?*anyopaque,
    dim: spv.spv.SpvDim,
    x: c_int,
    y: c_int,
    z: c_int,
    lod: c_int,
    has_lod: ffi.SpvCBool,
};

const WriteImageInfo = extern struct {
    driver_image: ?*anyopaque,
    dim: spv.spv.SpvDim,
    x: c_int,
    y: c_int,
    z: c_int,
};

const SampleImageInfo = extern struct {
    driver_image: ?*anyopaque,
    driver_sampler: ?*anyopaque,
    dim: spv.spv.SpvDim,
    x: f32,
    y: f32,
    z: f32,
    w: f32,
    lod: f32,
    has_lod: ffi.SpvCBool,
    offset: ImageOffset,
};

const QueryImageInfo = extern struct {
    driver_image: ?*anyopaque,
    dim: spv.spv.SpvDim,
    arrayed: ffi.SpvCBool,
    lod: c_int,
    has_lod: ffi.SpvCBool,
};

const QueryImageLodInfo = extern struct {
    driver_image: ?*anyopaque,
    driver_sampler: ?*anyopaque,
    dim: spv.spv.SpvDim,
    derivatives: ImageDerivatives,
};

const readImageFloat4_PFN = *const fn (info: ReadImageInfo, dst: *Vec4f) callconv(.c) ffi.Result;
const readImageInt4_PFN = *const fn (info: ReadImageInfo, dst: *Vec4u) callconv(.c) ffi.Result;
const writeImageFloat4_PFN = *const fn (info: WriteImageInfo, src: Vec4f) callconv(.c) ffi.Result;
const writeImageInt4_PFN = *const fn (info: WriteImageInfo, src: Vec4u) callconv(.c) ffi.Result;
const sampleImageFloat4_PFN = *const fn (info: SampleImageInfo, dst: *Vec4f) callconv(.c) ffi.Result;
const sampleImageInt4_PFN = *const fn (info: SampleImageInfo, dst: *Vec4u) callconv(.c) ffi.Result;
const sampleImageDref_PFN = *const fn (info: SampleImageInfo, dref: f32, dst: *f32) callconv(.c) ffi.Result;
const queryImageSize_PFN = *const fn (info: QueryImageInfo, dst: *Vec4u) callconv(.c) ffi.Result;
const queryImageLevels_PFN = *const fn (driver_image: ?*anyopaque, dst: *ffi.SpvCSize) callconv(.c) ffi.Result;
const queryImageSamples_PFN = *const fn (driver_image: ?*anyopaque, dst: *ffi.SpvCSize) callconv(.c) ffi.Result;
const queryImageLod_PFN = *const fn (info: QueryImageLodInfo, dst: *Vec4f) callconv(.c) ffi.Result;

const ImageAPI = extern struct {
    readImageFloat4: readImageFloat4_PFN,
    readImageInt4: readImageInt4_PFN,
    writeImageFloat4: writeImageFloat4_PFN,
    writeImageInt4: writeImageInt4_PFN,
    sampleImageFloat4: sampleImageFloat4_PFN,
    sampleImageInt4: sampleImageInt4_PFN,
    sampleImageDref: sampleImageDref_PFN,
    queryImageSize: queryImageSize_PFN,
    queryImageLevels: queryImageLevels_PFN,
    queryImageSamples: queryImageSamples_PFN,
    queryImageLod: queryImageLod_PFN,
};

fn toCResult(err: spv.Runtime.RuntimeError) ffi.Result {
    return switch (err) {
        spv.Runtime.RuntimeError.Barrier => ffi.Result.Barrier,
        spv.Runtime.RuntimeError.InvalidEntryPoint => ffi.Result.InvalidEntryPoint,
        spv.Runtime.RuntimeError.InvalidSpirV => ffi.Result.InvalidSpirV,
        spv.Runtime.RuntimeError.InvalidValueType => ffi.Result.InvalidValueType,
        spv.Runtime.RuntimeError.Killed => ffi.Result.Killed,
        spv.Runtime.RuntimeError.NotFound => ffi.Result.NotFound,
        spv.Runtime.RuntimeError.OutOfBounds => ffi.Result.OutOfBounds,
        spv.Runtime.RuntimeError.OutOfMemory => ffi.Result.OutOfMemory,
        spv.Runtime.RuntimeError.ToDo => ffi.Result.ToDo,
        spv.Runtime.RuntimeError.Unknown => ffi.Result.Unknown,
        spv.Runtime.RuntimeError.Unreachable => ffi.Result.Unreachable,
        spv.Runtime.RuntimeError.UnsupportedExtension => ffi.Result.UnsupportedExtension,
        spv.Runtime.RuntimeError.UnsupportedSpirV => ffi.Result.UnsupportedSpirV,
    };
}

fn fromCResult(res: ffi.Result) spv.Runtime.RuntimeError!void {
    return switch (res) {
        ffi.Result.Barrier => spv.Runtime.RuntimeError.Barrier,
        ffi.Result.InvalidEntryPoint => spv.Runtime.RuntimeError.InvalidEntryPoint,
        ffi.Result.InvalidSpirV => spv.Runtime.RuntimeError.InvalidSpirV,
        ffi.Result.InvalidValueType => spv.Runtime.RuntimeError.InvalidValueType,
        ffi.Result.Killed => spv.Runtime.RuntimeError.Killed,
        ffi.Result.NotFound => spv.Runtime.RuntimeError.NotFound,
        ffi.Result.OutOfBounds => spv.Runtime.RuntimeError.OutOfBounds,
        ffi.Result.OutOfMemory => spv.Runtime.RuntimeError.OutOfMemory,
        ffi.Result.ToDo => spv.Runtime.RuntimeError.ToDo,
        ffi.Result.Unknown => spv.Runtime.RuntimeError.Unknown,
        ffi.Result.Unreachable => spv.Runtime.RuntimeError.Unreachable,
        ffi.Result.UnsupportedExtension => spv.Runtime.RuntimeError.UnsupportedExtension,
        ffi.Result.UnsupportedSpirV => spv.Runtime.RuntimeError.UnsupportedSpirV,
        else => {},
    };
}

fn toCEntryPointStatus(status: spv.Runtime.EntryPointStatus) EntryPointStatus {
    return switch (status) {
        .completed => .completed,
        .barrier => .barrier,
    };
}

/// Hacky wrapper
const ImageAPIBridge = struct {
    threadlocal var current_image_api: ?*const ImageAPI = null;

    fn getImageAPI() spv.Runtime.RuntimeError!*const ImageAPI {
        return current_image_api orelse spv.Runtime.RuntimeError.Unknown;
    }

    fn toCVec4f(value: spv.Runtime.Vec4(f32)) Vec4f {
        return .{ .x = value.x, .y = value.y, .z = value.z, .w = value.w };
    }

    fn toCVec4u(value: spv.Runtime.Vec4(u32)) Vec4u {
        return .{ .x = value.x, .y = value.y, .z = value.z, .w = value.w };
    }

    fn readImageInfo(driver_image: *anyopaque, dim: spv.spv.SpvDim, x: i32, y: i32, z: i32, lod: ?i32) ReadImageInfo {
        return .{
            .driver_image = driver_image,
            .dim = dim,
            .x = @intCast(x),
            .y = @intCast(y),
            .z = @intCast(z),
            .lod = @intCast(lod orelse 0),
            .has_lod = if (lod == null) 0 else 1,
        };
    }

    fn writeImageInfo(driver_image: *anyopaque, dim: spv.spv.SpvDim, x: i32, y: i32, z: i32) WriteImageInfo {
        return .{
            .driver_image = driver_image,
            .dim = dim,
            .x = @intCast(x),
            .y = @intCast(y),
            .z = @intCast(z),
        };
    }

    fn sampleImageInfo(driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.spv.SpvDim, x: f32, y: f32, z: f32, w: f32, lod: ?f32, offset: spv.Runtime.ImageOffset) SampleImageInfo {
        return .{
            .driver_image = driver_image,
            .driver_sampler = driver_sampler,
            .dim = dim,
            .x = x,
            .y = y,
            .z = z,
            .w = w,
            .lod = lod orelse 0.0,
            .has_lod = if (lod == null) 0 else 1,
            .offset = toCImageOffset(offset),
        };
    }

    fn readImageFloat4(driver_image: *anyopaque, dim: spv.spv.SpvDim, x: i32, y: i32, z: i32, lod: ?i32) spv.Runtime.RuntimeError!spv.Runtime.Vec4(f32) {
        const image_api = try getImageAPI();

        var dst: Vec4f = undefined;
        const result = image_api.readImageFloat4(readImageInfo(driver_image, dim, x, y, z, lod), &dst);

        try fromCResult(result);

        return .{
            .x = dst.x,
            .y = dst.y,
            .z = dst.z,
            .w = dst.w,
        };
    }

    fn readImageInt4(driver_image: *anyopaque, dim: spv.spv.SpvDim, x: i32, y: i32, z: i32, lod: ?i32) spv.Runtime.RuntimeError!spv.Runtime.Vec4(u32) {
        const image_api = try getImageAPI();

        var dst: Vec4u = undefined;
        const result = image_api.readImageInt4(readImageInfo(driver_image, dim, x, y, z, lod), &dst);

        try fromCResult(result);

        return .{
            .x = dst.x,
            .y = dst.y,
            .z = dst.z,
            .w = dst.w,
        };
    }

    fn writeImageFloat4(driver_image: *anyopaque, dim: spv.spv.SpvDim, x: i32, y: i32, z: i32, pixel: spv.Runtime.Vec4(f32)) spv.Runtime.RuntimeError!void {
        const image_api = try getImageAPI();

        const result = image_api.writeImageFloat4(writeImageInfo(driver_image, dim, x, y, z), toCVec4f(pixel));

        try fromCResult(result);
    }

    fn writeImageInt4(driver_image: *anyopaque, dim: spv.spv.SpvDim, x: i32, y: i32, z: i32, pixel: spv.Runtime.Vec4(u32)) spv.Runtime.RuntimeError!void {
        const image_api = try getImageAPI();

        const result = image_api.writeImageInt4(writeImageInfo(driver_image, dim, x, y, z), toCVec4u(pixel));

        try fromCResult(result);
    }

    fn toCImageOffset(offset: spv.Runtime.ImageOffset) ImageOffset {
        return .{
            .x = @intCast(offset.x),
            .y = @intCast(offset.y),
            .z = @intCast(offset.z),
        };
    }

    fn sampleImageFloat4(driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32, offset: spv.Runtime.ImageOffset) spv.Runtime.RuntimeError!spv.Runtime.Vec4(f32) {
        const image_api = try getImageAPI();

        var dst: Vec4f = undefined;
        const result = image_api.sampleImageFloat4(sampleImageInfo(driver_image, driver_sampler, dim, x, y, z, 1.0, lod, offset), &dst);

        try fromCResult(result);

        return .{
            .x = dst.x,
            .y = dst.y,
            .z = dst.z,
            .w = dst.w,
        };
    }

    fn sampleImageInt4(driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32, offset: spv.Runtime.ImageOffset) spv.Runtime.RuntimeError!spv.Runtime.Vec4(u32) {
        const image_api = try getImageAPI();

        var dst: Vec4u = undefined;
        const result = image_api.sampleImageInt4(sampleImageInfo(driver_image, driver_sampler, dim, x, y, z, 1.0, lod, offset), &dst);

        try fromCResult(result);

        return .{
            .x = dst.x,
            .y = dst.y,
            .z = dst.z,
            .w = dst.w,
        };
    }

    fn sampleImageDref(driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.spv.SpvDim, x: f32, y: f32, z: f32, w: f32, dref: f32, lod: ?f32, offset: spv.Runtime.ImageOffset) spv.Runtime.RuntimeError!f32 {
        const image_api = try getImageAPI();

        var dst: f32 = undefined;
        const result = image_api.sampleImageDref(sampleImageInfo(driver_image, driver_sampler, dim, x, y, z, w, lod, offset), dref, &dst);

        try fromCResult(result);

        return dst;
    }

    fn queryImageSize(driver_image: *anyopaque, dim: spv.spv.SpvDim, arrayed: bool, lod: ?i32) spv.Runtime.RuntimeError!spv.Runtime.Vec4(u32) {
        const image_api = try getImageAPI();

        var dst: Vec4u = undefined;
        const result = image_api.queryImageSize(.{
            .driver_image = driver_image,
            .dim = dim,
            .arrayed = if (arrayed) 1 else 0,
            .lod = @intCast(lod orelse 0),
            .has_lod = if (lod == null) 0 else 1,
        }, &dst);

        try fromCResult(result);

        return .{
            .x = dst.x,
            .y = dst.y,
            .z = dst.z,
            .w = dst.w,
        };
    }

    fn queryImageLevels(driver_image: *anyopaque) spv.Runtime.RuntimeError!u32 {
        const image_api = try getImageAPI();

        var dst: ffi.SpvCSize = undefined;
        const result = image_api.queryImageLevels(driver_image, &dst);

        try fromCResult(result);

        return @intCast(dst);
    }

    fn queryImageSamples(driver_image: *anyopaque) spv.Runtime.RuntimeError!u32 {
        const image_api = try getImageAPI();

        var dst: ffi.SpvCSize = undefined;
        const result = image_api.queryImageSamples(driver_image, &dst);

        try fromCResult(result);

        return @intCast(dst);
    }

    fn queryImageLod(driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.spv.SpvDim, derivatives: spv.Runtime.ImageDerivatives) spv.Runtime.RuntimeError!spv.Runtime.Vec4(f32) {
        const image_api = try getImageAPI();

        var dst: Vec4f = undefined;
        const result = image_api.queryImageLod(.{
            .driver_image = driver_image,
            .driver_sampler = driver_sampler,
            .dim = dim,
            .derivatives = .{
                .dx = toCVec4f(derivatives.dx),
                .dy = toCVec4f(derivatives.dy),
            },
        }, &dst);

        try fromCResult(result);

        return .{
            .x = dst.x,
            .y = dst.y,
            .z = dst.z,
            .w = dst.w,
        };
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
            .sampleImageFloat4 = ImageAPIBridge.sampleImageFloat4,
            .sampleImageInt4 = ImageAPIBridge.sampleImageInt4,
            .sampleImageDref = ImageAPIBridge.sampleImageDref,
            .queryImageSize = ImageAPIBridge.queryImageSize,
            .queryImageLevels = ImageAPIBridge.queryImageLevels,
            .queryImageSamples = ImageAPIBridge.queryImageSamples,
            .queryImageLod = ImageAPIBridge.queryImageLod,
        },
    ) catch |err| {
        allocator.destroy(rt.*);
        return toCResult(err);
    };
    return .Success;
}

export fn SpvInitRuntimeFrom(rt: **RuntimeWrapper, other: *RuntimeWrapper, image_api: ImageAPI) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;

    rt.* = allocator.create(RuntimeWrapper) catch return .OutOfMemory;
    rt.*.image_api = image_api;

    rt.*.rt = spv.Runtime.initFrom(
        allocator,
        &other.rt,
        .{
            .readImageFloat4 = ImageAPIBridge.readImageFloat4,
            .readImageInt4 = ImageAPIBridge.readImageInt4,
            .writeImageFloat4 = ImageAPIBridge.writeImageFloat4,
            .writeImageInt4 = ImageAPIBridge.writeImageInt4,
            .sampleImageFloat4 = ImageAPIBridge.sampleImageFloat4,
            .sampleImageInt4 = ImageAPIBridge.sampleImageInt4,
            .sampleImageDref = ImageAPIBridge.sampleImageDref,
            .queryImageSize = ImageAPIBridge.queryImageSize,
            .queryImageLevels = ImageAPIBridge.queryImageLevels,
            .queryImageSamples = ImageAPIBridge.queryImageSamples,
            .queryImageLod = ImageAPIBridge.queryImageLod,
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

export fn SpvFlushDescriptorSets(rt: *RuntimeWrapper) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;
    rt.rt.flushDescriptorSets(allocator) catch |err| return toCResult(err);
    return .Success;
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

export fn SpvCopySpecializationConstantsFrom(rt: *RuntimeWrapper, other: *const RuntimeWrapper) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;
    rt.rt.copySpecializationConstantsFrom(allocator, &other.rt) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvSetDerivativeFromMemory(rt: *RuntimeWrapper, result: spv.SpvWord, dx: [*]const u8, dx_size: c_ulong, dy: [*]const u8, dy_size: c_ulong) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;
    rt.rt.setDerivativeFromMemory(allocator, result, dx[0..dx_size], dy[0..dy_size]) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvClearDerivative(rt: *RuntimeWrapper, result: spv.SpvWord) callconv(.c) void {
    const allocator = std.heap.c_allocator;
    rt.rt.clearDerivative(allocator, result);
}

export fn SpvCopyDerivative(rt: *RuntimeWrapper, dst: spv.SpvWord, src: spv.SpvWord) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;
    rt.rt.copyDerivative(allocator, dst, src) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvPopulatePushConstants(rt: *RuntimeWrapper, data: [*]const u8, data_size: c_ulong) callconv(.c) ffi.Result {
    rt.rt.populatePushConstants(data[0..data_size]) catch |err| return toCResult(err);
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

export fn SpvGetResultLocation(rt: *RuntimeWrapper, location: spv.SpvWord, kind: LocationType, result: *spv.SpvWord) callconv(.c) ffi.Result {
    return SpvGetResultByLocation(rt, location, kind, result);
}

export fn SpvGetResultByLocationComponent(rt: *RuntimeWrapper, location: spv.SpvWord, component: spv.SpvWord, kind: LocationType, result: *spv.SpvWord) callconv(.c) ffi.Result {
    result.* = rt.rt.getResultByLocationComponent(location, component, switch (kind) {
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

export fn SpvGetInputLocationMemorySize(rt: *RuntimeWrapper, location: spv.SpvWord, size: *c_ulong) callconv(.c) ffi.Result {
    size.* = rt.rt.getInputLocationMemorySize(location) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvGetResultPrimitiveType(rt: *RuntimeWrapper, result: spv.SpvWord, primitive_type: *PrimitiveType) callconv(.c) ffi.Result {
    primitive_type.* = switch (rt.rt.getResultPrimitiveType(result) catch |err| return toCResult(err)) {
        .Bool => .bool,
        .Float => .float,
        .SInt => .sint,
        .UInt => .uint,
    };
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

export fn SpvBeginEntryPoint(rt: *RuntimeWrapper, entry_point: spv.SpvWord, status: *EntryPointStatus) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;

    const previous_image_api = ImageAPIBridge.current_image_api;
    ImageAPIBridge.current_image_api = &rt.image_api;
    defer ImageAPIBridge.current_image_api = previous_image_api;

    status.* = toCEntryPointStatus(rt.rt.beginEntryPoint(allocator, entry_point) catch |err| return toCResult(err));
    return .Success;
}

export fn SpvContinueEntryPoint(rt: *RuntimeWrapper, status: *EntryPointStatus) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;

    const previous_image_api = ImageAPIBridge.current_image_api;
    ImageAPIBridge.current_image_api = &rt.image_api;
    defer ImageAPIBridge.current_image_api = previous_image_api;

    status.* = toCEntryPointStatus(rt.rt.continueEntryPoint(allocator) catch |err| return toCResult(err));
    return .Success;
}

export fn SpvResetInvocation(rt: *RuntimeWrapper) callconv(.c) void {
    const allocator = std.heap.c_allocator;
    rt.rt.resetInvocation(allocator);
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
    const allocator = std.heap.c_allocator;
    rt.rt.writeInput(allocator, input[0..input_size], result) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteInputLocation(rt: *RuntimeWrapper, input: [*]const u8, input_size: c_ulong, location: spv.SpvWord) callconv(.c) ffi.Result {
    rt.rt.writeInputLocation(input[0..input_size], location) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteBuiltIn(rt: *RuntimeWrapper, input: [*]const u8, input_size: c_ulong, builtin: spv.spv.SpvBuiltIn) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;
    rt.rt.writeBuiltIn(allocator, input[0..input_size], builtin) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteDescriptorSet(rt: *RuntimeWrapper, input: [*]const u8, input_size: c_ulong, set: spv.SpvWord, binding: spv.SpvWord, descriptor_index: spv.SpvWord) callconv(.c) ffi.Result {
    rt.rt.writeDescriptorSet(input[0..input_size], set, binding, descriptor_index) catch |err| return toCResult(err);
    return .Success;
}
