const std = @import("std");
const ffi = @import("ffi.zig");
const spv = ffi.spv;

const CSpecializationEntry = extern struct {
    id: spv.SpvWord,
    offset: u32,
    size: u32,
};

const LocationType = enum(c_int) {
    input = 0,
    output = 1,
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

export fn SpvInitRuntime(rt: **spv.Runtime, module: *spv.Module) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;

    rt.* = allocator.create(spv.Runtime) catch return .OutOfMemory;

    rt.*.* = spv.Runtime.init(allocator, module) catch |err| {
        allocator.destroy(rt.*);
        return toCResult(err);
    };
    return .Success;
}

export fn SpvDeinitRuntime(rt: *spv.Runtime) callconv(.c) void {
    const allocator = std.heap.c_allocator;
    rt.deinit(allocator);
    allocator.destroy(rt);
}

export fn SpvAddSpecializationInfo(rt: *spv.Runtime, entry: CSpecializationEntry, data: [*]const u8, data_size: u32) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;
    rt.addSpecializationInfo(
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

export fn SpvGetEntryPointByName(rt: *spv.Runtime, name: [*:0]const u8, result: *spv.SpvWord) callconv(.c) ffi.Result {
    result.* = rt.getEntryPointByName(std.mem.span(name)) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvGetResultByLocation(rt: *spv.Runtime, location: spv.SpvWord, kind: LocationType, result: *spv.SpvWord) callconv(.c) ffi.Result {
    result.* = rt.getResultByLocation(location, switch (kind) {
        .input => .input,
        .output => .output,
    }) catch |err| return toCResult(err);
    return .Success;
}
export fn SpvGetResultByName(rt: *spv.Runtime, name: [*:0]const u8, result: *spv.SpvWord) callconv(.c) ffi.Result {
    result.* = rt.getResultByName(std.mem.span(name)) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvCallEntryPoint(rt: *spv.Runtime, entry_point: spv.SpvWord) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;
    rt.callEntryPoint(allocator, entry_point) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvReadOutput(rt: *spv.Runtime, output: [*]u8, output_size: u32, result: spv.SpvWord) callconv(.c) ffi.Result {
    rt.readOutput(output[0..output_size], result) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvReadBuiltIn(rt: *spv.Runtime, output: [*]u8, output_size: u32, builtin: spv.spv.SpvBuiltIn) callconv(.c) ffi.Result {
    rt.readBuiltIn(output[0..output_size], builtin) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteInput(rt: *spv.Runtime, input: [*]const u8, input_size: u32, result: spv.SpvWord) callconv(.c) ffi.Result {
    rt.writeInput(input[0..input_size], result) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteBuiltIn(rt: *spv.Runtime, input: [*]const u8, input_size: u32, builtin: spv.spv.SpvBuiltIn) callconv(.c) ffi.Result {
    rt.writeBuiltIn(input[0..input_size], builtin) catch |err| return toCResult(err);
    return .Success;
}

export fn SpvWriteDescriptorSet(rt: *spv.Runtime, input: [*]const u8, input_size: u32, set: spv.SpvWord, binding: spv.SpvWord, descriptor_index: spv.SpvWord) callconv(.c) ffi.Result {
    rt.writeDescriptorSet(input[0..input_size], set, binding, descriptor_index) catch |err| return toCResult(err);
    return .Success;
}
