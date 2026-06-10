const std = @import("std");
const ffi = @import("ffi.zig");
const spv = ffi.spv;

const Options = extern struct {
    use_simd_vectors_specializations: ffi.SpvCBool,
};

const ReflectionInfos = extern struct {
    local_size_x: ffi.SpvCWord,
    local_size_y: ffi.SpvCWord,
    local_size_z: ffi.SpvCWord,

    geometry_invocations: ffi.SpvCWord,
    geometry_output_count: ffi.SpvCWord,
    geometry_input: ffi.SpvCWord,
    geometry_output: ffi.SpvCWord,

    needs_derivatives: ffi.SpvCBool,
    has_control_barriers: ffi.SpvCBool,
};

fn toCResult(err: spv.Module.ModuleError) ffi.Result {
    return switch (err) {
        spv.Module.ModuleError.InvalidSpirV => ffi.Result.InvalidSpirV,
        spv.Module.ModuleError.InvalidMagic => ffi.Result.InvalidMagic,
        spv.Module.ModuleError.UnsupportedEndianness => ffi.Result.UnsupportedEndianness,
        spv.Module.ModuleError.UnsupportedExtension => ffi.Result.UnsupportedExtension,
        spv.Module.ModuleError.OutOfMemory => ffi.Result.OutOfMemory,
    };
}

export fn SpvInitModule(module: **spv.Module, source: [*]const ffi.SpvCWord, source_len: ffi.SpvCSize, options: Options) callconv(.c) ffi.Result {
    const allocator = std.heap.c_allocator;

    module.* = allocator.create(spv.Module) catch return .OutOfMemory;

    const cast_source: []const u32 = @as([*]const u32, @ptrCast(source[0..source_len]))[0..source_len];
    module.*.* = spv.Module.init(
        allocator,
        cast_source[0..source_len],
        .{
            .use_simd_vectors_specializations = if (options.use_simd_vectors_specializations == 0) false else true,
        },
    ) catch |err| {
        allocator.destroy(module.*);
        return toCResult(err);
    };
    return .Success;
}

export fn SpvDeinitModule(module: *spv.Module) callconv(.c) void {
    const allocator = std.heap.c_allocator;
    module.deinit(allocator);
    allocator.destroy(module);
}

export fn SpvModuleGetReflectionInfos(module: *spv.Module) callconv(.c) ReflectionInfos {
    return .{
        .local_size_x = module.reflection_infos.local_size_x,
        .local_size_y = module.reflection_infos.local_size_y,
        .local_size_z = module.reflection_infos.local_size_z,
        .geometry_invocations = module.reflection_infos.geometry_invocations,
        .geometry_output_count = module.reflection_infos.geometry_output_count,
        .geometry_input = module.reflection_infos.geometry_input,
        .geometry_output = module.reflection_infos.geometry_output,
        .needs_derivatives = if (module.reflection_infos.needs_derivatives) 1 else 0,
        .has_control_barriers = if (module.reflection_infos.has_control_barriers) 1 else 0,
    };
}
