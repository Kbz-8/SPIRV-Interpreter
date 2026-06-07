//! A runtime meant for actual shader invocations.

const std = @import("std");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");
const lib = @import("lib.zig");
const pretty = @import("pretty");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Module = @import("Module.zig");
const PrimitiveType = @import("Value.zig").PrimitiveType;
const Result = @import("Result.zig");
const Value = @import("Value.zig").Value;
const WordIterator = @import("WordIterator.zig");

const Self = @This();

pub const RuntimeError = error{
    Barrier,
    DivisionByZero,
    InvalidEntryPoint,
    InvalidSpirV,
    InvalidValueType,
    Killed,
    NotFound,
    OutOfMemory,
    OutOfBounds,
    ToDo,
    Unreachable,
    UnsupportedSpirV,
    UnsupportedExtension,
    Unknown,
};

pub const EntryPointStatus = enum {
    completed,
    barrier,
};

pub const SpecializationEntry = struct {
    id: SpvWord,
    offset: usize,
    size: usize,
};

pub const Derivative = struct {
    dx: Value,
    dy: Value,

    pub fn dupe(self: *const @This(), allocator: std.mem.Allocator) RuntimeError!@This() {
        return .{
            .dx = try self.dx.dupe(allocator),
            .dy = try self.dy.dupe(allocator),
        };
    }

    pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.dx.deinit(allocator);
        self.dy.deinit(allocator);
    }
};

pub const Function = struct {
    source_location: usize,
    result: *Result,
    ret: *Result,
};

pub fn Vec4(comptime T: type) type {
    return struct {
        x: T,
        y: T,
        z: T,
        w: T,
    };
}

pub const ImageAPI = struct {
    readImageFloat4: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32) RuntimeError!Vec4(f32),
    readImageInt4: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32) RuntimeError!Vec4(u32),
    writeImageFloat4: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32, pixel: Vec4(f32)) RuntimeError!void,
    writeImageInt4: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32, pixel: Vec4(u32)) RuntimeError!void,
    sampleImageFloat4: *const fn (driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32) RuntimeError!Vec4(f32),
    sampleImageInt4: *const fn (driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32) RuntimeError!Vec4(u32),
    queryImageSize: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, arrayed: bool) RuntimeError!Vec4(u32),
};

mod: *Module,
it: WordIterator,

/// Local deep copy of module's results to be able to run multiple runtimes concurrently
results: []Result,

current_parameter_index: SpvWord,
current_function: ?*Result,
function_stack: std.ArrayList(Function),

current_label: ?SpvWord,
previous_label: ?SpvWord,

specialization_constants: std.AutoHashMapUnmanaged(u32, []const u8),
derivatives: std.AutoHashMapUnmanaged(SpvWord, Derivative),

image_api: ImageAPI,

pub fn init(allocator: std.mem.Allocator, module: *Module, image_api: ImageAPI) RuntimeError!Self {
    return .{
        .mod = module,
        .it = module.it,
        .results = blk: {
            const results = allocator.dupe(Result, module.results) catch return RuntimeError.OutOfMemory;
            for (results, module.results) |*new_result, result| {
                new_result.* = result.dupe(allocator) catch return RuntimeError.OutOfMemory;
                if (new_result.variant) |*variant| {
                    switch (variant.*) {
                        .AccessChain => |*access_chain| {
                            allocator.free(access_chain.indexes);
                            access_chain.value.deinit(allocator);
                            new_result.variant = null;
                        },
                        else => {},
                    }
                }
            }
            break :blk results;
        },
        .current_parameter_index = 0,
        .current_function = null,
        .function_stack = .empty,
        .current_label = null,
        .previous_label = null,
        .specialization_constants = .empty,
        .derivatives = .empty,
        .image_api = image_api,
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    for (self.results) |*result| {
        result.deinit(allocator);
    }
    allocator.free(self.results);
    self.function_stack.deinit(allocator);
    var it = self.specialization_constants.iterator();
    while (it.next()) |entry| {
        allocator.free(entry.value_ptr.*);
    }
    self.specialization_constants.deinit(allocator);

    var derivatives = self.derivatives.iterator();
    while (derivatives.next()) |entry| {
        entry.value_ptr.deinit(allocator);
    }
    self.derivatives.deinit(allocator);
}

pub fn addSpecializationInfo(self: *Self, allocator: std.mem.Allocator, entry: SpecializationEntry, data: []const u8) RuntimeError!void {
    const slice = allocator.dupe(u8, data[entry.offset .. entry.offset + entry.size]) catch return RuntimeError.OutOfMemory;
    self.specialization_constants.put(allocator, entry.id, slice) catch return RuntimeError.OutOfMemory;
}

pub fn copySpecializationConstantsFrom(self: *Self, allocator: std.mem.Allocator, other: *const Self) RuntimeError!void {
    var it = other.specialization_constants.iterator();
    while (it.next()) |entry| {
        const slice = allocator.dupe(u8, entry.value_ptr.*) catch return RuntimeError.OutOfMemory;
        self.specialization_constants.put(allocator, entry.key_ptr.*, slice) catch {
            allocator.free(slice);
            return RuntimeError.OutOfMemory;
        };
    }
}

pub fn setDerivative(self: *Self, allocator: std.mem.Allocator, result: SpvWord, dx: *const Value, dy: *const Value) RuntimeError!void {
    const derivative: Derivative = .{
        .dx = try dx.dupe(allocator),
        .dy = try dy.dupe(allocator),
    };
    errdefer {
        var tmp = derivative;
        tmp.deinit(allocator);
    }

    const gop = self.derivatives.getOrPut(allocator, result) catch return RuntimeError.OutOfMemory;
    if (gop.found_existing) {
        gop.value_ptr.deinit(allocator);
    }
    gop.value_ptr.* = derivative;
}

pub fn setDerivativeFromMemory(self: *Self, allocator: std.mem.Allocator, result: SpvWord, dx: []const u8, dy: []const u8) RuntimeError!void {
    const target_type = try self.getResultTargetTypeWord(result);

    var dx_value = try Value.init(allocator, self.results, target_type, false);
    defer dx_value.deinit(allocator);
    _ = try dx_value.write(dx);

    var dy_value = try Value.init(allocator, self.results, target_type, false);
    defer dy_value.deinit(allocator);
    _ = try dy_value.write(dy);

    try self.setDerivative(allocator, result, &dx_value, &dy_value);
}

fn getResultTargetTypeWord(self: *const Self, result: SpvWord) RuntimeError!SpvWord {
    return switch ((try self.results[result].getConstVariant()).*) {
        .Variable => |v| v.type_word,
        .Constant => |c| c.type_word,
        .FunctionParameter => |p| p.type_word,
        .AccessChain => |a| a.target,
        else => return RuntimeError.InvalidSpirV,
    };
}

pub fn clearDerivative(self: *Self, allocator: std.mem.Allocator, result: SpvWord) void {
    if (self.derivatives.fetchRemove(result)) |kv| {
        var derivative = kv.value;
        derivative.deinit(allocator);
    }
}

pub fn copyDerivative(self: *Self, allocator: std.mem.Allocator, dst: SpvWord, src: SpvWord) RuntimeError!void {
    if (self.derivatives.get(src)) |derivative| {
        try self.setDerivative(allocator, dst, &derivative.dx, &derivative.dy);
    } else {
        self.clearDerivative(allocator, dst);
    }
}

pub fn getEntryPointByName(self: *const Self, name: []const u8) RuntimeError!SpvWord {
    for (self.mod.entry_points.items, 0..) |entry_point, i| {
        if (blk: {
            // Not using std.mem.eql as entry point names may have longer size than their content
            for (0..@min(name.len, entry_point.name.len)) |j| {
                if (name[j] != entry_point.name[j])
                    break :blk false;
            }
            if (entry_point.name.len != name.len and entry_point.name[name.len] != 0)
                break :blk false;
            break :blk true;
        }) return @intCast(i);
    }
    return RuntimeError.NotFound;
}

pub fn getResultByName(self: *const Self, name: []const u8) RuntimeError!SpvWord {
    for (self.results, 0..) |result, i| {
        if (result.name) |result_name| {
            if (blk: {
                // Same as entry points
                for (0..@min(name.len, result_name.len)) |j| {
                    if (name[j] != result_name[j]) break :blk false;
                }
                break :blk true;
            }) return @intCast(i);
        }
    }
    return RuntimeError.NotFound;
}

pub const LocationKind = enum { input, output };

pub inline fn getResultByLocation(self: *const Self, location: SpvWord, kind: LocationKind) RuntimeError!SpvWord {
    return self.getResultByLocationComponent(location, 0, kind);
}

pub fn getResultByLocationComponent(self: *const Self, location: SpvWord, component: SpvWord, kind: LocationKind) RuntimeError!SpvWord {
    switch (kind) {
        .input => if (location < self.mod.input_locations.len and component < 4 and self.mod.input_locations[location][component] != 0) {
            return self.mod.input_locations[location][component];
        },
        .output => if (location < self.mod.output_locations.len and component < 4 and self.mod.output_locations[location][component] != 0) {
            return self.mod.output_locations[location][component];
        },
    }
    return RuntimeError.NotFound;
}

pub fn getResultPrimitiveType(self: *const Self, result: SpvWord) RuntimeError!PrimitiveType {
    if (result >= self.results.len)
        return RuntimeError.OutOfBounds;
    return (try self.results[result].getConstValue()).resolvePrimitiveType();
}

pub fn resultIsInteger(self: *const Self, result: SpvWord) bool {
    return switch (self.getResultPrimitiveType(result) catch return false) {
        .SInt, .UInt => true,
        else => false,
    };
}

pub fn dumpResultsTable(self: *Self, allocator: std.mem.Allocator, writer: *std.Io.Writer) RuntimeError!void {
    const dump = pretty.dump(allocator, self.results, .{
        .tab_size = 4,
        .max_depth = 0,
        .struct_max_len = 0,
        .array_max_len = 0,
    }) catch return RuntimeError.OutOfMemory;
    defer allocator.free(dump);
    writer.print("{s}", .{dump}) catch return RuntimeError.Unknown;
    writer.flush() catch return RuntimeError.Unknown;
}

/// Calls an entry point, `entry_point_index` being the index of the entry point ordered by declaration in the bytecode
pub inline fn callEntryPoint(self: *Self, allocator: std.mem.Allocator, entry_point_index: SpvWord) RuntimeError!void {
    _ = try self.beginEntryPoint(allocator, entry_point_index);
}

pub fn beginEntryPoint(self: *Self, allocator: std.mem.Allocator, entry_point_index: SpvWord) RuntimeError!EntryPointStatus {
    self.reset();
    if (entry_point_index >= self.mod.entry_points.items.len)
        return RuntimeError.InvalidEntryPoint;

    // Spec constants pass
    try self.pass(allocator, .initMany(&.{
        .SpecConstantTrue,
        .SpecConstantFalse,
        .SpecConstantComposite,
        .SpecConstant,
        .SpecConstantOp,
    }));

    {
        const entry_point_desc = &self.mod.entry_points.items[entry_point_index];
        const entry_point_result = &self.mod.results[entry_point_desc.id];
        if (entry_point_result.variant) |variant| {
            switch (variant) {
                .Function => |f| {
                    if (!self.it.jumpToSourceLocation(f.source_location))
                        return RuntimeError.InvalidEntryPoint;
                    self.function_stack.append(allocator, .{
                        .source_location = f.source_location,
                        .result = entry_point_result,
                        .ret = &self.results[f.return_type],
                    }) catch return RuntimeError.OutOfMemory;
                },
                else => return RuntimeError.InvalidEntryPoint,
            }
        } else {
            return RuntimeError.InvalidEntryPoint;
        }
    }

    // Execution pass
    return self.continueEntryPoint(allocator);
}

pub fn continueEntryPoint(self: *Self, allocator: std.mem.Allocator) RuntimeError!EntryPointStatus {
    self.pass(allocator, null) catch |err| switch (err) {
        RuntimeError.Barrier => return .barrier,
        else => return err,
    };
    return .completed;
}

fn pass(self: *Self, allocator: std.mem.Allocator, op_set: ?std.EnumSet(spv.SpvOp)) RuntimeError!void {
    self.it.did_jump = false; // To reset function jump
    while (self.it.nextOrNull()) |opcode_data| {
        const word_count_with_header = (opcode_data & (~spv.SpvOpCodeMask)) >> spv.SpvWordCountShift;
        if (word_count_with_header == 0) return RuntimeError.InvalidSpirV;
        const word_count = word_count_with_header - 1;
        const opcode = (opcode_data & spv.SpvOpCodeMask);

        if (op_set) |set| {
            @branchHint(.unlikely);
            if (!set.contains(@enumFromInt(opcode))) {
                _ = self.it.skipN(word_count);
                continue;
            }
        }

        var it_tmp = self.it; // Save because operations may iter on this iterator
        if (op.runtime_dispatcher[opcode]) |pfn| {
            try pfn(allocator, word_count, self);
        }
        if (!self.it.did_jump) {
            _ = it_tmp.skipN(word_count);
            self.it = it_tmp;
        } else {
            self.it.did_jump = false;
        }
    }
}

pub fn populatePushConstants(self: *Self, blob: []const u8) RuntimeError!void {
    for (self.results) |*result| {
        if (result.variant == null or std.meta.activeTag(result.variant.?) != .Variable)
            continue;
        const variable = &result.variant.?.Variable;
        if (variable.storage_class != .PushConstant)
            continue;
        _ = try variable.value.write(blob);
    }
}

pub fn writeDescriptorSet(self: *const Self, input: []const u8, set: SpvWord, binding: SpvWord, descriptor_index: SpvWord) RuntimeError!void {
    const result = self.mod.getBindingResult(set, binding) orelse return RuntimeError.NotFound;
    const value = &(self.results[result].variant orelse return).Variable.value;
    switch (value.*) {
        .Array => |arr| {
            if (descriptor_index >= arr.values.len)
                return RuntimeError.NotFound;
            _ = try arr.values[descriptor_index].write(input);
        },
        else => {
            if (descriptor_index != 0)
                return RuntimeError.NotFound;
            _ = try value.write(input);
        },
    }
}

fn readResultValue(self: *const Self, output: []u8, result: SpvWord) RuntimeError!void {
    const variant = self.results[result].variant orelse return RuntimeError.InvalidSpirV;
    switch (variant) {
        .Variable => |v| _ = try v.value.read(output),
        .AccessChain => |a| switch (a.value) {
            .Pointer => |ptr| switch (ptr.ptr) {
                .common => |value_ptr| _ = try value_ptr.read(output),
                .f32_ptr => |value_ptr| {
                    if (output.len < @sizeOf(f32)) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, output[0..@sizeOf(f32)], std.mem.asBytes(value_ptr));
                },
                .i32_ptr => |value_ptr| {
                    if (output.len < @sizeOf(i32)) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, output[0..@sizeOf(i32)], std.mem.asBytes(value_ptr));
                },
                .u32_ptr => |value_ptr| {
                    if (output.len < @sizeOf(u32)) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, output[0..@sizeOf(u32)], std.mem.asBytes(value_ptr));
                },
            },
            else => _ = try a.value.read(output),
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn writeResultValue(self: *const Self, input: []const u8, result: SpvWord) RuntimeError!void {
    if (self.results[result].variant) |*variant| {
        switch (variant.*) {
            .Variable => |*v| _ = try v.value.write(input),
            .AccessChain => |*a| switch (a.value) {
                .Pointer => |ptr| switch (ptr.ptr) {
                    .common => |value_ptr| _ = try value_ptr.write(input),
                    .f32_ptr => |value_ptr| {
                        if (input.len < @sizeOf(f32)) return RuntimeError.OutOfBounds;
                        std.mem.copyForwards(u8, std.mem.asBytes(value_ptr), input[0..@sizeOf(f32)]);
                    },
                    .i32_ptr => |value_ptr| {
                        if (input.len < @sizeOf(i32)) return RuntimeError.OutOfBounds;
                        std.mem.copyForwards(u8, std.mem.asBytes(value_ptr), input[0..@sizeOf(i32)]);
                    },
                    .u32_ptr => |value_ptr| {
                        if (input.len < @sizeOf(u32)) return RuntimeError.OutOfBounds;
                        std.mem.copyForwards(u8, std.mem.asBytes(value_ptr), input[0..@sizeOf(u32)]);
                    },
                },
                else => _ = try a.value.write(input),
            },
            else => return RuntimeError.InvalidSpirV,
        }
    } else {
        return RuntimeError.InvalidSpirV;
    }
}

const InputLocationTarget = struct {
    result: SpvWord,
    matrix_column: ?usize = null,
};

fn resolveInputLocationTarget(self: *const Self, location: SpvWord) RuntimeError!InputLocationTarget {
    if (location < self.mod.input_locations.len and self.mod.input_locations[location][0] != 0) {
        const result = self.mod.input_locations[location][0];
        const value = try self.results[result].getConstValue();
        switch (value.*) {
            .Matrix => return .{ .result = result, .matrix_column = 0 },
            else => return .{ .result = result },
        }
    }

    var base_location = location;
    while (base_location > 0) {
        base_location -= 1;

        const result = if (base_location < self.mod.input_locations.len)
            self.mod.input_locations[base_location][0]
        else
            0;
        if (result == 0) continue;

        const location_offset: usize = @intCast(location - base_location);
        const value = try self.results[result].getConstValue();
        switch (value.*) {
            .Matrix => |columns| {
                if (location_offset < columns.len) {
                    return .{
                        .result = result,
                        .matrix_column = location_offset,
                    };
                }
            },
            else => {},
        }
    }

    return RuntimeError.NotFound;
}

fn getInputLocationTargetValue(self: *const Self, target: InputLocationTarget) RuntimeError!*Value {
    const value = switch ((try self.results[target.result].getVariant()).*) {
        .Variable => |*v| &v.value,
        .AccessChain => |*a| &a.value,
        else => return RuntimeError.InvalidSpirV,
    };

    if (target.matrix_column) |column| {
        switch (value.*) {
            .Matrix => |columns| {
                if (column >= columns.len) return RuntimeError.OutOfBounds;
                return &columns[column];
            },
            else => return RuntimeError.InvalidValueType,
        }
    }

    return value;
}

pub fn readOutput(self: *const Self, output: []u8, result: SpvWord) RuntimeError!void {
    for (&self.mod.output_locations) |*location| {
        if (std.mem.indexOfScalar(SpvWord, location, result)) |_| {
            try self.readResultValue(output, result);
            return;
        }
    }
    return RuntimeError.NotFound;
}

pub fn readBuiltIn(self: *const Self, output: []u8, builtin: spv.SpvBuiltIn) RuntimeError!void {
    if (self.mod.builtins.get(builtin)) |result| {
        try self.readResultValue(output, result);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeInput(self: *const Self, input: []const u8, result: SpvWord) RuntimeError!void {
    for (&self.mod.input_locations) |*location| {
        if (std.mem.indexOfScalar(SpvWord, location, result)) |_| {
            try self.writeResultValue(input, result);
            if (self.results[result].variant) |*variant| switch (variant.*) {
                .Variable => |*v| v.value.clearExternalData(),
                .AccessChain => |*a| a.value.clearExternalData(),
                else => {},
            };
            return;
        }
    }
    return RuntimeError.NotFound;
}

pub fn getInputLocationMemorySize(self: *const Self, location: SpvWord) RuntimeError!usize {
    const target = try self.resolveInputLocationTarget(location);
    return (try self.getInputLocationTargetValue(target)).getPlainMemorySize();
}

pub fn writeInputLocation(self: *const Self, input: []const u8, location: SpvWord) RuntimeError!void {
    const target = try self.resolveInputLocationTarget(location);
    const value = try self.getInputLocationTargetValue(target);
    _ = try value.write(input);
    value.clearExternalData();
}

pub fn writeBuiltIn(self: *const Self, input: []const u8, builtin: spv.SpvBuiltIn) RuntimeError!void {
    if (self.mod.builtins.get(builtin)) |result| {
        try self.writeResultValue(input, result);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn flushDescriptorSets(self: *const Self, allocator: std.mem.Allocator) RuntimeError!void {
    for (self.results) |*result| {
        try result.flushPtr(allocator);
    }
}

pub fn getResultMemorySize(self: *const Self, result: SpvWord) RuntimeError!usize {
    const value = try self.results[result].getConstValue();
    return value.getPlainMemorySize();
}

pub fn hasResultDecoration(self: *const Self, result: SpvWord, decoration: spv.SpvDecoration) bool {
    for (self.results[result].decorations.items) |result_decoration| {
        if (result_decoration.rtype == decoration)
            return true;
    }
    return false;
}

pub fn resetInvocation(self: *Self, allocator: std.mem.Allocator) void {
    var derivatives = self.derivatives.iterator();
    while (derivatives.next()) |entry| {
        entry.value_ptr.deinit(allocator);
    }
    self.derivatives.clearRetainingCapacity();

    for (self.results) |*result| {
        if (result.variant) |*variant| {
            switch (variant.*) {
                .AccessChain => |*access_chain| {
                    access_chain.value.flushPtr(allocator) catch {};
                },
                else => {},
            }
        }
    }

    for (self.results) |*result| {
        if (result.variant) |*variant| {
            switch (variant.*) {
                .AccessChain => |*access_chain| {
                    access_chain.value.deinit(allocator);
                    allocator.free(access_chain.indexes);
                    result.variant = null;
                },
                .FunctionParameter => |*parameter| {
                    parameter.value_ptr = null;
                },
                else => {},
            }
        }
    }

    self.reset();
}

fn reset(self: *Self) void {
    self.function_stack.clearRetainingCapacity();
    self.current_parameter_index = 0;
    self.current_function = null;
    self.current_label = null;
    self.previous_label = null;
}
