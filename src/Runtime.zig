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

pub const WorkgroupMemory = struct {
    result: SpvWord,
    bytes: []u8,
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
    current_label: ?SpvWord,
    previous_label: ?SpvWord,
};

pub fn Vec4(comptime T: type) type {
    return struct {
        x: T,
        y: T,
        z: T,
        w: T,
    };
}

pub const ImageOffset = struct {
    x: i32 = 0,
    y: i32 = 0,
    z: i32 = 0,
};

pub const ImageDerivatives = struct {
    dx: Vec4(f32),
    dy: Vec4(f32),
};

pub const ImageAPI = struct {
    readImageFloat4: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32, lod: ?i32) RuntimeError!Vec4(f32),
    readImageInt4: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32, lod: ?i32) RuntimeError!Vec4(u32),
    writeImageFloat4: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32, pixel: Vec4(f32)) RuntimeError!void,
    writeImageInt4: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32, pixel: Vec4(u32)) RuntimeError!void,
    sampleImageFloat4: *const fn (driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32, offset: ImageOffset) RuntimeError!Vec4(f32),
    sampleImageInt4: *const fn (driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32, offset: ImageOffset) RuntimeError!Vec4(u32),
    sampleImageDref: *const fn (driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, w: f32, dref: f32, lod: ?f32, offset: ImageOffset) RuntimeError!f32,
    queryImageSize: *const fn (driver_image: *anyopaque, dim: spv.SpvDim, arrayed: bool, lod: ?i32) RuntimeError!Vec4(u32),
    queryImageLevels: *const fn (driver_image: *anyopaque) RuntimeError!u32,
    queryImageSamples: *const fn (driver_image: *anyopaque) RuntimeError!u32,
    queryImageLod: *const fn (driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, derivatives: ImageDerivatives) RuntimeError!Vec4(f32),
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
active_entry_point: ?SpvWord,

specialization_constants: std.AutoHashMapUnmanaged(u32, []const u8),
derivatives: std.AutoHashMapUnmanaged(SpvWord, Derivative),
phi_values: std.AutoHashMapUnmanaged(SpvWord, Value),
helper_invocation: bool,

image_api: ImageAPI,

pub fn init(allocator: std.mem.Allocator, module: *Module, image_api: ImageAPI) RuntimeError!Self {
    var self: Self = .{
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
        .active_entry_point = null,
        .specialization_constants = .empty,
        .derivatives = .empty,
        .phi_values = .empty,
        .helper_invocation = false,
        .image_api = image_api,
    };
    errdefer self.deinit(allocator);
    try self.refreshValueLayouts();
    return self;
}

pub fn initFrom(allocator: std.mem.Allocator, other: *const Self, image_api: ImageAPI) RuntimeError!Self {
    const results = allocator.alloc(Result, other.results.len) catch return RuntimeError.OutOfMemory;
    var initialized: usize = 0;
    errdefer {
        for (results[0..initialized]) |*result| {
            result.deinit(allocator);
        }
        allocator.free(results);
    }

    for (results, other.results) |*new_result, result| {
        new_result.* = result.dupe(allocator) catch return RuntimeError.OutOfMemory;
        initialized += 1;
    }

    var self: Self = .{
        .mod = other.mod,
        .it = other.mod.it,
        .results = results,
        .current_parameter_index = 0,
        .current_function = null,
        .function_stack = .empty,
        .current_label = null,
        .previous_label = null,
        .active_entry_point = other.active_entry_point,
        .specialization_constants = .empty,
        .derivatives = .empty,
        .phi_values = .empty,
        .helper_invocation = other.helper_invocation,
        .image_api = image_api,
    };
    errdefer self.deinit(allocator);

    try self.copySpecializationConstantsFrom(allocator, other);
    try self.refreshValueLayouts();
    return self;
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

    self.clearPhiValues(allocator);
    self.phi_values.deinit(allocator);
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

fn applyValueLayout(results: []Result, value: *Value, type_word: SpvWord) RuntimeError!void {
    const resolved_type_word = results[type_word].resolveTypeWordOrNull() orelse type_word;
    const type_data = (try results[resolved_type_word].getConstVariant()).Type;

    switch (value.*) {
        .Structure => |*structure| switch (type_data) {
            .Structure => |type_structure| {
                @memcpy(@constCast(structure.offsets), type_structure.members_offsets);
                @memcpy(@constCast(structure.matrix_strides), type_structure.members_matrix_strides);
                @memcpy(@constCast(structure.row_major), type_structure.members_row_major);
                for (structure.values, type_structure.members_type_word, 0..) |*member_value, member_type_word, member_index| {
                    if (member_value.* == .RuntimeArray) {
                        member_value.RuntimeArray.matrix_stride = type_structure.members_matrix_strides[member_index];
                        member_value.RuntimeArray.row_major = type_structure.members_row_major[member_index];
                    }
                    try applyValueLayout(results, member_value, member_type_word);
                }
            },
            else => {},
        },
        .Array => |array| switch (type_data) {
            .Array => |type_array| for (array.values) |*element| {
                try applyValueLayout(results, element, type_array.components_type_word);
            },
            else => {},
        },
        .Matrix => |columns| switch (type_data) {
            .Matrix => |type_matrix| for (columns) |*column| {
                try applyValueLayout(results, column, type_matrix.column_type_word);
            },
            else => {},
        },
        .Vector => |elements| switch (type_data) {
            .Vector => |type_vector| for (elements) |*element| {
                try applyValueLayout(results, element, type_vector.components_type_word);
            },
            else => {},
        },
        else => {},
    }
}

fn refreshValueLayouts(self: *Self) RuntimeError!void {
    for (self.results) |*result| {
        if (result.variant) |*variant| switch (variant.*) {
            .Variable => |*v| switch (v.storage_class) {
                .StorageBuffer,
                .Uniform,
                .PushConstant,
                .Workgroup,
                => try applyValueLayout(self.results, &v.value, v.type_word),
                else => {},
            },
            else => {},
        };
    }
}

pub fn refreshResultValueLayout(self: *Self, result: SpvWord) RuntimeError!void {
    const type_word = try self.results[result].getValueTypeWord();
    try applyValueLayout(self.results, try self.results[result].getValue(), type_word);
}

fn typePlainMemorySize(self: *const Self, type_word: SpvWord) RuntimeError!usize {
    const resolved_word = self.results[type_word].resolveTypeWordOrNull() orelse type_word;
    const target_type = (try self.results[resolved_word].getConstVariant()).Type;

    return switch (target_type) {
        .Array => |a| blk: {
            const stride: usize = if (a.stride != 0)
                @intCast(a.stride)
            else
                try self.typePlainMemorySize(a.components_type_word);
            break :blk stride * @as(usize, @intCast(a.member_count));
        },
        .RuntimeArray => return RuntimeError.InvalidValueType,
        .Structure => |s| blk: {
            var size: usize = 0;
            for (s.members_type_word, 0..) |member_type_word, i| {
                const member_offset: usize = @intCast(s.members_offsets[i] orelse size);
                size = @max(size, member_offset + try self.typePlainMemorySize(member_type_word));
            }
            break :blk size;
        },
        else => target_type.getSize(self.results),
    };
}

pub fn createWorkgroupMemory(self: *Self, allocator: std.mem.Allocator) RuntimeError![]WorkgroupMemory {
    var count: usize = 0;
    for (self.results) |result| {
        if (result.variant) |variant| switch (variant) {
            .Variable => |v| {
                if (v.storage_class == .Workgroup) count += 1;
            },
            else => {},
        };
    }

    const memories = allocator.alloc(WorkgroupMemory, count) catch return RuntimeError.OutOfMemory;
    errdefer allocator.free(memories);

    var index: usize = 0;
    for (self.results, 0..) |result, result_id| {
        if (result.variant) |variant| switch (variant) {
            .Variable => |v| {
                if (v.storage_class == .Workgroup) {
                    const size = try self.typePlainMemorySize(v.type_word);
                    const bytes = allocator.alloc(u8, size) catch return RuntimeError.OutOfMemory;
                    @memset(bytes, 0);
                    memories[index] = .{
                        .result = @intCast(result_id),
                        .bytes = bytes,
                    };
                    index += 1;
                }
            },
            else => {},
        };
    }

    return memories;
}

pub fn destroyWorkgroupMemory(_: *Self, allocator: std.mem.Allocator, memories: []WorkgroupMemory) void {
    for (memories) |memory| allocator.free(memory.bytes);
    allocator.free(memories);
}

pub fn bindWorkgroupMemory(self: *Self, memories: []const WorkgroupMemory) RuntimeError!void {
    for (memories) |memory| {
        _ = try (try self.results[memory.result].getValue()).write(memory.bytes);
    }
}

fn clearPhiValues(self: *Self, allocator: std.mem.Allocator) void {
    var it = self.phi_values.iterator();
    while (it.next()) |entry| {
        entry.value_ptr.deinit(allocator);
    }
    self.phi_values.clearRetainingCapacity();
}

pub fn snapshotPhiValues(self: *Self, allocator: std.mem.Allocator) RuntimeError!void {
    self.clearPhiValues(allocator);

    for (self.results, 0..) |*result, result_id| {
        const value = switch (result.variant orelse continue) {
            .Constant => |*constant| &constant.value,
            .FunctionParameter => |*parameter| parameter.value_ptr orelse continue,
            else => continue,
        };
        if (std.meta.activeTag(value.*) == .Pointer) continue;
        const snapshot = try value.dupe(allocator);
        const gop = self.phi_values.getOrPut(allocator, @intCast(result_id)) catch {
            var tmp = snapshot;
            tmp.deinit(allocator);
            return RuntimeError.OutOfMemory;
        };
        if (gop.found_existing) gop.value_ptr.deinit(allocator);
        gop.value_ptr.* = snapshot;
    }
}

pub fn getPhiValueSnapshot(self: *Self, id: SpvWord) ?*const Value {
    return self.phi_values.getPtr(id);
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
    const memory_size = try dx_value.getPlainMemorySize();
    if (dx.len < memory_size or dy.len < memory_size) return RuntimeError.OutOfBounds;
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

pub fn applySpecializationLayout(self: *Self, allocator: std.mem.Allocator) RuntimeError!void {
    self.reset();
    try self.applySpecializationConstants(allocator);
    try self.applySpecializationDependentLayout(allocator);
}

pub fn applySpecializationInvocationLayout(self: *Self, allocator: std.mem.Allocator) RuntimeError!void {
    self.reset();
    if (self.specialization_constants.count() != 0)
        try self.applySpecializationConstants(allocator);
    try self.applySpecializationDependentLayout(allocator);
}

fn applySpecializationConstants(self: *Self, allocator: std.mem.Allocator) RuntimeError!void {
    try self.pass(allocator, .initMany(&.{
        .SpecConstantTrue,
        .SpecConstantFalse,
        .SpecConstantComposite,
        .ConstantNull,
        .SpecConstant,
        .SpecConstantOp,
    }));
}

fn applySpecializationDependentLayout(self: *Self, allocator: std.mem.Allocator) RuntimeError!void {
    try self.pass(allocator, .initMany(&.{
        .TypeArray,
        .Variable,
    }));
}

pub fn getEntryPointByName(self: *const Self, name: []const u8) RuntimeError!SpvWord {
    if (self.active_entry_point) |entry_point| {
        if (entryPointNameMatches(self.mod.entry_points.items[entry_point].name, name))
            return entry_point;
    }

    for (self.mod.entry_points.items, 0..) |entry_point, i| {
        if (entryPointNameMatches(entry_point.name, name)) return @intCast(i);
    }
    return RuntimeError.NotFound;
}

pub fn getEntryPointByNameAndExecutionModel(self: *const Self, name: []const u8, execution_model: spv.SpvExecutionModel) RuntimeError!SpvWord {
    for (self.mod.entry_points.items, 0..) |entry_point, i| {
        if (entry_point.exec_model == execution_model and entryPointNameMatches(entry_point.name, name))
            return @intCast(i);
    }
    return RuntimeError.NotFound;
}

fn entryPointNameMatches(entry_point_name: []const u8, name: []const u8) bool {
    // Not using std.mem.eql as entry point names may have longer size than their content.
    for (0..@min(name.len, entry_point_name.len)) |j| {
        if (name[j] != entry_point_name[j])
            return false;
    }
    return entry_point_name.len == name.len or entry_point_name[name.len] == 0;
}

pub fn selectEntryPoint(self: *Self, entry_point_index: SpvWord) RuntimeError!void {
    if (entry_point_index >= self.mod.entry_points.items.len)
        return RuntimeError.InvalidEntryPoint;
    self.active_entry_point = entry_point_index;
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
    const locations = switch (kind) {
        .input => &self.mod.input_locations,
        .output => &self.mod.output_locations,
    };
    if (location >= locations.len or component >= 4)
        return RuntimeError.NotFound;

    const result = locations[location][component];
    if (result != 0 and self.resultIsInActiveInterface(result))
        return result;

    if (self.active_entry_point) |entry_point_index| {
        const entry_point = self.mod.entry_points.items[entry_point_index];
        for (entry_point.globals) |global| {
            if (global >= self.results.len)
                continue;

            const variant = self.results[global].variant orelse continue;
            const variable = switch (variant) {
                .Variable => |v| v,
                else => continue,
            };
            const storage_class_matches = switch (kind) {
                .input => variable.storage_class == .Input,
                .output => variable.storage_class == .Output,
            };
            if (!storage_class_matches)
                continue;

            for (self.results[global].decorations.items) |decoration| {
                if (decoration.rtype == .Location and
                    decoration.literal_1 == location and
                    self.resultComponent(global) == component)
                {
                    return global;
                }
            }
        }
    }
    return RuntimeError.NotFound;
}

fn resultIsInActiveInterface(self: *const Self, result: SpvWord) bool {
    const entry_point_index = self.active_entry_point orelse return true;
    const global = self.resultGlobal(result) orelse return false;
    return std.mem.indexOfScalar(SpvWord, self.mod.entry_points.items[entry_point_index].globals, global) != null;
}

fn resultGlobal(self: *const Self, result: SpvWord) ?SpvWord {
    if (result >= self.results.len)
        return null;

    const variant = self.results[result].variant orelse return result;
    return switch (variant) {
        .AccessChain => |access_chain| access_chain.base,
        else => result,
    };
}

fn resultComponent(self: *const Self, result: SpvWord) SpvWord {
    if (result >= self.results.len)
        return 0;

    for (self.results[result].decorations.items) |decoration| {
        if (decoration.rtype == .Component)
            return decoration.literal_1;
    }
    return 0;
}

pub fn getResultPrimitiveType(self: *const Self, result: SpvWord) RuntimeError!PrimitiveType {
    if (result >= self.results.len)
        return RuntimeError.OutOfBounds;
    return (try self.results[result].getConstValue()).resolvePrimitiveType();
}

pub fn getWorkgroupSize(self: *Self, allocator: std.mem.Allocator) RuntimeError!?@Vector(3, u32) {
    try self.pass(allocator, .initMany(&.{
        .SpecConstantTrue,
        .SpecConstantFalse,
        .SpecConstantComposite,
        .ConstantNull,
        .SpecConstant,
        .SpecConstantOp,
    }));

    for (self.results) |*result| {
        for (result.decorations.items) |decoration| {
            if (decoration.rtype != .BuiltIn or decoration.literal_1 != @intFromEnum(spv.SpvBuiltIn.WorkgroupSize))
                continue;

            const value = try result.getValue();
            return switch (value.*) {
                .Vector3u32 => |v| v,
                .Vector => |values| blk: {
                    if (values.len != 3)
                        return RuntimeError.InvalidValueType;

                    var result_value = @Vector(3, u32){ 0, 0, 0 };
                    inline for (0..3) |i| {
                        result_value[i] = switch (values[i]) {
                            .Int => |int| int.value.uint32,
                            else => return RuntimeError.InvalidValueType,
                        };
                    }
                    break :blk result_value;
                },
                else => return RuntimeError.InvalidValueType,
            };
        }
    }

    return null;
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
    if (self.specialization_constants.count() != 0)
        try self.applySpecializationConstants(allocator);

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
                        .current_label = null,
                        .previous_label = null,
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
    const value = if (self.results[result].variant) |*variant| switch (variant.*) {
        .Variable => |*variable| &variable.value,
        else => return RuntimeError.InvalidSpirV,
    } else return;
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

fn readConstantIndex(self: *const Self, result: SpvWord) RuntimeError!usize {
    const value = try self.results[result].getConstValue();
    return switch (value.*) {
        .Int => |i| switch (i.bit_count) {
            8 => if (i.is_signed) std.math.cast(usize, i.value.sint8) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint8),
            16 => if (i.is_signed) std.math.cast(usize, i.value.sint16) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint16),
            32 => if (i.is_signed) std.math.cast(usize, i.value.sint32) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint32),
            64 => if (i.is_signed) std.math.cast(usize, i.value.sint64) orelse RuntimeError.OutOfBounds else std.math.cast(usize, i.value.uint64) orelse RuntimeError.OutOfBounds,
            else => RuntimeError.InvalidSpirV,
        },
        else => RuntimeError.InvalidSpirV,
    };
}

fn accessChainPrefixValue(self: *const Self, result: SpvWord, prefix_len: usize) RuntimeError!*Value {
    const access_chain = switch ((self.results[result].variant orelse return RuntimeError.InvalidSpirV)) {
        .AccessChain => |*a| a,
        else => return RuntimeError.InvalidSpirV,
    };
    if (prefix_len > access_chain.indexes.len) return RuntimeError.OutOfBounds;

    var value = switch ((self.results[access_chain.base].variant orelse return RuntimeError.InvalidSpirV)) {
        .Variable => |*v| &v.value,
        .AccessChain => |*a| switch (a.value) {
            .Pointer => |ptr| switch (ptr.ptr) {
                .common => |value_ptr| value_ptr,
                else => return RuntimeError.InvalidSpirV,
            },
            else => &a.value,
        },
        else => return RuntimeError.InvalidSpirV,
    };

    for (access_chain.indexes[0..prefix_len]) |index_id| {
        const index = try self.readConstantIndex(index_id);
        value = switch (value.*) {
            .Vector, .Matrix => |values| blk: {
                if (index >= values.len) return RuntimeError.OutOfBounds;
                break :blk &values[index];
            },
            .Array => |arr| blk: {
                if (index >= arr.values.len) return RuntimeError.OutOfBounds;
                break :blk &arr.values[index];
            },
            .Structure => |structure| blk: {
                if (index >= structure.values.len) return RuntimeError.OutOfBounds;
                break :blk &structure.values[index];
            },
            else => return RuntimeError.InvalidValueType,
        };
    }

    return value;
}

fn writeResultValue(self: *const Self, allocator: std.mem.Allocator, input: []const u8, result: SpvWord) RuntimeError!void {
    if (self.results[result].variant) |*variant| {
        switch (variant.*) {
            .Variable => |*v| switch (v.value) {
                .Pointer => |*ptr| {
                    if (ptr.owns_uniform_backing_value) {
                        if (ptr.uniform_backing_value) |value_ptr| {
                            _ = try value_ptr.write(input);
                            return;
                        }
                    }

                    const target_type = switch ((try self.results[v.type_word].getConstVariant()).*) {
                        .Type => |t| switch (t) {
                            .Pointer => |p| p.target,
                            else => return RuntimeError.InvalidSpirV,
                        },
                        else => return RuntimeError.InvalidSpirV,
                    };
                    const value_ptr = allocator.create(Value) catch return RuntimeError.OutOfMemory;
                    errdefer allocator.destroy(value_ptr);

                    value_ptr.* = try Value.init(allocator, self.results, target_type, false);
                    errdefer value_ptr.deinit(allocator);

                    _ = try value_ptr.write(input);
                    ptr.* = .{
                        .ptr = .{ .common = value_ptr },
                        .uniform_backing_value = value_ptr,
                        .owns_uniform_backing_value = true,
                    };
                },
                else => _ = try v.value.write(input),
            },
            .AccessChain => |*a| switch (a.value) {
                .Pointer => |ptr| switch (ptr.ptr) {
                    .common => |value_ptr| {
                        const value_size = try value_ptr.getPlainMemorySize();
                        if (a.indexes.len > 1 and input.len > value_size) {
                            const parent = try self.accessChainPrefixValue(result, a.indexes.len - 1);
                            _ = try parent.write(input);
                        } else {
                            _ = try value_ptr.write(input);
                        }
                    },
                    .f32_ptr => |value_ptr| {
                        if (input.len < @sizeOf(f32)) return RuntimeError.OutOfBounds;
                        if (a.indexes.len > 1 and input.len > @sizeOf(f32)) {
                            const parent = try self.accessChainPrefixValue(result, a.indexes.len - 1);
                            _ = try parent.write(input);
                        } else {
                            std.mem.copyForwards(u8, std.mem.asBytes(value_ptr), input[0..@sizeOf(f32)]);
                        }
                    },
                    .i32_ptr => |value_ptr| {
                        if (input.len < @sizeOf(i32)) return RuntimeError.OutOfBounds;
                        if (a.indexes.len > 1 and input.len > @sizeOf(i32)) {
                            const parent = try self.accessChainPrefixValue(result, a.indexes.len - 1);
                            _ = try parent.write(input);
                        } else {
                            std.mem.copyForwards(u8, std.mem.asBytes(value_ptr), input[0..@sizeOf(i32)]);
                        }
                    },
                    .u32_ptr => |value_ptr| {
                        if (input.len < @sizeOf(u32)) return RuntimeError.OutOfBounds;
                        if (a.indexes.len > 1 and input.len > @sizeOf(u32)) {
                            const parent = try self.accessChainPrefixValue(result, a.indexes.len - 1);
                            _ = try parent.write(input);
                        } else {
                            std.mem.copyForwards(u8, std.mem.asBytes(value_ptr), input[0..@sizeOf(u32)]);
                        }
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
    struct_member: ?usize = null,
    array_element: ?usize = null,
    matrix_column: ?usize = null,
};

fn resolveInputLocationTarget(self: *const Self, location: SpvWord) RuntimeError!InputLocationTarget {
    if (self.getResultByLocationComponent(location, 0, .input)) |result| {
        if (self.results[result].variant == null)
            return RuntimeError.NotFound;

        const value = try self.results[result].getConstValue();
        switch (value.*) {
            .Array => |arr| {
                if (arr.values.len == 0) return RuntimeError.OutOfBounds;
                return .{ .result = result };
            },
            .Matrix => return .{ .result = result, .matrix_column = 0 },
            else => return .{ .result = result },
        }
    } else |err| switch (err) {
        RuntimeError.NotFound => {},
        else => return err,
    }

    for (self.results, 0..) |*result, id| {
        const variant = result.variant orelse continue;
        const variable = switch (variant) {
            .Variable => |v| v,
            else => continue,
        };
        if (variable.storage_class != .Input) continue;

        const type_word = switch ((self.results[variable.type_word].variant orelse continue)) {
            .Type => |t| switch (t) {
                .Pointer => |ptr| ptr.target,
                else => variable.type_word,
            },
            else => continue,
        };
        const type_result = &self.results[type_word];
        const type_variant = type_result.variant orelse continue;
        switch (type_variant) {
            .Type => |t| switch (t) {
                .Structure => {
                    for (type_result.decorations.items) |decoration| {
                        if (decoration.rtype == .Location and decoration.literal_1 == location) {
                            return .{
                                .result = @intCast(id),
                                .struct_member = @intCast(decoration.index),
                            };
                        }
                    }
                },
                else => {},
            },
            else => {},
        }
    }

    var base_location = location;
    while (base_location > 0) {
        base_location -= 1;

        const result = self.getResultByLocationComponent(base_location, 0, .input) catch |err| switch (err) {
            RuntimeError.NotFound => continue,
            else => return err,
        };

        const location_offset: usize = @intCast(location - base_location);
        const value = try self.results[result].getConstValue();
        switch (value.*) {
            .Array => |arr| {
                if (arr.values.len == 0) continue;

                const element_locations = switch (arr.values[0]) {
                    .Matrix => |columns| columns.len,
                    else => 1,
                };
                if (element_locations == 0) continue;

                const element_index = location_offset / element_locations;
                if (element_index >= arr.values.len) continue;

                const element_location = location_offset % element_locations;
                return .{
                    .result = result,
                    .array_element = element_index,
                    .matrix_column = if (std.meta.activeTag(arr.values[element_index]) == .Matrix) element_location else null,
                };
            },
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

    const element_value = if (target.array_element) |element| blk: {
        switch (value.*) {
            .Array => |arr| {
                if (element >= arr.values.len) return RuntimeError.OutOfBounds;
                break :blk &arr.values[element];
            },
            else => return RuntimeError.InvalidValueType,
        }
    } else value;

    const member_value = if (target.struct_member) |member| blk: {
        switch (element_value.*) {
            .Structure => |structure| {
                if (member >= structure.values.len) return RuntimeError.OutOfBounds;
                break :blk &structure.values[member];
            },
            else => return RuntimeError.InvalidValueType,
        }
    } else element_value;

    if (target.matrix_column) |column| {
        switch (member_value.*) {
            .Matrix => |columns| {
                if (column >= columns.len) return RuntimeError.OutOfBounds;
                return &columns[column];
            },
            else => return RuntimeError.InvalidValueType,
        }
    }

    return member_value;
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
    if (self.getBuiltinResult(builtin)) |result| {
        try self.readResultValue(output, result);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeInput(self: *const Self, allocator: std.mem.Allocator, input: []const u8, result: SpvWord) RuntimeError!void {
    for (&self.mod.input_locations) |*location| {
        if (std.mem.indexOfScalar(SpvWord, location, result)) |_| {
            try self.writeResultValue(allocator, input, result);
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

pub fn writeBuiltIn(self: *const Self, allocator: std.mem.Allocator, input: []const u8, builtin: spv.SpvBuiltIn) RuntimeError!void {
    if (self.getBuiltinResult(builtin)) |result| {
        try self.writeResultValue(allocator, input, result);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn getBuiltinResult(self: *const Self, builtin: spv.SpvBuiltIn) ?SpvWord {
    if (self.mod.builtins.get(builtin)) |result| {
        if (self.resultIsInActiveInterface(result))
            return result;
    }

    const entry_point_index = self.active_entry_point orelse return null;
    const entry_point = self.mod.entry_points.items[entry_point_index];
    for (entry_point.globals) |global| {
        if (global >= self.results.len)
            continue;

        for (self.results[global].decorations.items) |decoration| {
            if (decoration.rtype == .BuiltIn and decoration.literal_1 == @intFromEnum(builtin))
                return global;
        }
    }

    return null;
}

pub fn flushDescriptorSets(self: *const Self, allocator: std.mem.Allocator) RuntimeError!void {
    for (self.results) |*result| {
        try result.flushPtr(allocator);
    }
}

pub fn getResultMemorySize(self: *const Self, result: SpvWord) RuntimeError!usize {
    const variant = self.results[result].variant orelse return RuntimeError.InvalidSpirV;
    return switch (variant) {
        .AccessChain => |a| switch (a.value) {
            .Pointer => |ptr| switch (ptr.ptr) {
                .common => |value_ptr| value_ptr.getPlainMemorySize(),
                .f32_ptr => @sizeOf(f32),
                .i32_ptr => @sizeOf(i32),
                .u32_ptr => @sizeOf(u32),
            },
            else => a.value.getPlainMemorySize(),
        },
        else => (try self.results[result].getConstValue()).getPlainMemorySize(),
    };
}

pub fn hasResultDecoration(self: *const Self, result: SpvWord, decoration: spv.SpvDecoration) bool {
    for (self.results[result].decorations.items) |result_decoration| {
        if (result_decoration.rtype == decoration)
            return true;
    }
    return false;
}

pub fn hasResultOrMemberDecoration(self: *const Self, result: SpvWord, decoration: spv.SpvDecoration) bool {
    if (self.hasResultDecoration(result, decoration))
        return true;

    if (result >= self.results.len)
        return false;

    const type_word = switch ((self.results[result].variant orelse return false)) {
        .Variable => |variable| variable.type_word,
        else => return false,
    };
    const target_type_word = switch ((self.results[type_word].variant orelse return false)) {
        .Type => |t| switch (t) {
            .Pointer => |ptr| ptr.target,
            else => type_word,
        },
        else => return false,
    };
    const target_type = self.results[target_type_word].variant orelse return false;
    switch (target_type) {
        .Type => |t| switch (t) {
            .Structure => {
                for (self.results[target_type_word].decorations.items) |member_decoration| {
                    if (member_decoration.rtype == decoration)
                        return true;
                }
            },
            else => {},
        },
        else => {},
    }

    return false;
}

pub fn resetInvocation(self: *Self, allocator: std.mem.Allocator) void {
    var derivatives = self.derivatives.iterator();
    while (derivatives.next()) |entry| {
        entry.value_ptr.deinit(allocator);
    }
    self.derivatives.clearRetainingCapacity();
    self.clearPhiValues(allocator);

    for (self.results) |*result| {
        if (result.variant) |*variant| {
            switch (variant.*) {
                .AccessChain => |*access_chain| {
                    if (std.mem.allEqual(u8, std.mem.asBytes(&access_chain.value), 0xaa)) {
                        result.variant = null;
                        continue;
                    }
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
    self.it = self.mod.it;
    self.function_stack.clearRetainingCapacity();
    self.current_parameter_index = 0;
    self.current_function = null;
    self.current_label = null;
    self.previous_label = null;
    self.helper_invocation = false;
}
