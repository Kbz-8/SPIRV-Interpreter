const std = @import("std");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");
const lib = @import("lib.zig");
const Value = @import("Value.zig").Value;

const Runtime = @import("Runtime.zig");
const RuntimeError = Runtime.RuntimeError;

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Vec4f32 = lib.Vec4f32;
const Vec3f32 = lib.Vec3f32;
const Vec2f32 = lib.Vec2f32;

const Vec4i32 = lib.Vec4i32;
const Vec3i32 = lib.Vec3i32;
const Vec2i32 = lib.Vec2i32;

const Vec4u32 = lib.Vec4u32;
const Vec3u32 = lib.Vec3u32;
const Vec2u32 = lib.Vec2u32;

pub const Variant = enum {
    String,
    Extension,
    Type,
    Variable,
    Constant,
    Function,
    AccessChain,
    FunctionParameter,
    Label,
};

pub const Type = enum {
    Void,
    Bool,
    Int,
    Float,
    Vector,
    Vector4f32,
    Vector3f32,
    Vector2f32,
    Vector4i32,
    Vector3i32,
    Vector2i32,
    Vector4u32,
    Vector3u32,
    Vector2u32,
    Matrix,
    Array,
    RuntimeArray,
    Structure,
    Function,
    Image,
    Sampler,
    SampledImage,
    Pointer,
};

const ImageInfo = struct {
    dim: spv.SpvDim,
    depth: SpvByte,
    arrayed: SpvByte,
    ms: SpvByte,
    sampled: SpvByte,
    format: spv.SpvImageFormat,
    access: spv.SpvAccessQualifier,
};

pub const Decoration = struct {
    rtype: spv.SpvDecoration,
    literal_1: SpvWord,
    literal_2: ?SpvWord,
    index: SpvWord,
};

pub const TypeData = union(Type) {
    Void: struct {},
    Bool: struct {},
    Int: struct {
        bit_length: SpvWord,
        is_signed: bool,
    },
    Float: struct {
        bit_length: SpvWord,
    },
    Vector: struct {
        components_type_word: SpvWord,
        components_type: Type,
        member_count: SpvWord,
    },
    Vector4f32: struct {},
    Vector3f32: struct {},
    Vector2f32: struct {},
    Vector4i32: struct {},
    Vector3i32: struct {},
    Vector2i32: struct {},
    Vector4u32: struct {},
    Vector3u32: struct {},
    Vector2u32: struct {},
    Matrix: struct {
        column_type_word: SpvWord,
        column_type: Type,
        member_count: SpvWord,
    },
    Array: struct {
        components_type_word: SpvWord,
        components_type: Type,
        member_count: SpvWord,
        stride: SpvWord,
    },
    RuntimeArray: struct {
        components_type_word: SpvWord,
        components_type: Type,
        stride: SpvWord,
    },
    Structure: struct {
        members_type_word: []const SpvWord,
        members_offsets: []?SpvWord,
        member_names: std.ArrayList([]const u8),
    },
    Function: struct {
        source_location: usize,
        return_type: SpvWord,
        params: []const SpvWord,
    },
    Image: struct {},
    Sampler: struct {},
    SampledImage: struct {},
    Pointer: struct {
        storage_class: spv.SpvStorageClass,
        target: SpvWord,
    },

    pub fn getSize(self: *const TypeData, results: []const Self) usize {
        return switch (self.*) {
            .Bool => 1,
            .Int => |i| @divExact(i.bit_length, 8),
            .Float => |f| @divExact(f.bit_length, 8),
            .Vector => |v| results[v.components_type_word].variant.?.Type.getSize(results),
            .Array => |a| a.stride,
            .Matrix => |m| results[m.column_type_word].variant.?.Type.getSize(results),
            .RuntimeArray => |a| a.stride,
            .Structure => |s| blk: {
                var total: usize = 0;
                for (s.members_type_word, 0..) |type_word, i| {
                    if (i + 1 < s.members_offsets.len) {
                        if (s.members_offsets[i + 1]) |offset| {
                            total = offset;
                            continue;
                        }
                    }
                    total += results[type_word].variant.?.Type.getSize(results);
                }
                break :blk total;
            },
            .Vector4f32, .Vector4i32, .Vector4u32 => 4 * 4,
            .Vector3f32, .Vector3i32, .Vector3u32 => 3 * 4,
            .Vector2f32, .Vector2i32, .Vector2u32 => 2 * 4,
            else => 0,
        };
    }
};

pub const VariantData = union(Variant) {
    String: []const u8,
    Extension: struct {
        /// Should not be allocated but rather a pointer to a static array
        dispatcher: []?op.OpCodeExtFunc,
    },
    Type: TypeData,
    Variable: struct {
        storage_class: spv.SpvStorageClass,
        type_word: SpvWord,
        type: Type,
        value: Value,
    },
    Constant: struct {
        type_word: SpvWord,
        type: Type,
        value: Value,
    },
    Function: struct {
        source_location: usize,
        return_type: SpvWord,
        function_type: SpvWord,
        params: []SpvWord,
    },
    AccessChain: struct {
        target: SpvWord,
        value: Value,
    },
    FunctionParameter: struct {
        type_word: SpvWord,
        type: Type,
        value_ptr: ?*Value,
    },
    Label: struct {
        source_location: usize,
    },
};

const Self = @This();

name: ?[]const u8,
decorations: std.ArrayList(Decoration),
variant: ?VariantData,

pub fn init() Self {
    return .{
        .name = null,
        .decorations = .empty,
        .variant = null,
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    if (self.name) |name| {
        allocator.free(name);
    }
    if (self.variant) |*variant| {
        switch (variant.*) {
            .Type => |*t| switch (t.*) {
                .Function => |data| allocator.free(data.params),
                .Structure => |*data| {
                    allocator.free(data.members_type_word);
                    for (data.member_names.items) |name| {
                        allocator.free(name);
                    }
                    allocator.free(data.members_offsets);
                    data.member_names.deinit(allocator);
                },
                else => {},
            },
            .Constant => |*c| c.value.deinit(allocator),
            .Variable => |*v| v.value.deinit(allocator),
            .AccessChain => |*a| a.value.deinit(allocator),
            .Function => |f| allocator.free(f.params),
            else => {},
        }
    }
    self.decorations.deinit(allocator);
}

pub inline fn getValueTypeWord(self: *Self) RuntimeError!SpvWord {
    return switch ((try self.getVariant()).*) {
        .Variable => |v| v.type_word,
        .Constant => |c| c.type_word,
        .AccessChain => |a| a.target,
        .FunctionParameter => |p| p.type_word,
        else => RuntimeError.InvalidSpirV,
    };
}

pub inline fn getValueType(self: *Self) RuntimeError!Type {
    return switch ((try self.getVariant()).*) {
        .Variable => |v| v.type,
        .Constant => |c| c.type,
        .FunctionParameter => |p| p.type,
        else => RuntimeError.InvalidSpirV,
    };
}

pub inline fn getValue(self: *Self) RuntimeError!*Value {
    return switch ((try self.getVariant()).*) {
        .Variable => |*v| &v.value,
        .Constant => |*c| &c.value,
        .AccessChain => |*a| &a.value,
        .FunctionParameter => |*p| p.value_ptr orelse return RuntimeError.InvalidSpirV,
        else => RuntimeError.InvalidSpirV,
    };
}

pub inline fn getConstValue(self: *Self) RuntimeError!*const Value {
    return switch ((try self.getVariant()).*) {
        .Variable => |v| &v.value,
        .Constant => |c| &c.value,
        .AccessChain => |a| &a.value,
        .FunctionParameter => |p| p.value_ptr orelse return RuntimeError.InvalidSpirV,
        else => RuntimeError.InvalidSpirV,
    };
}

pub inline fn getVariant(self: *Self) RuntimeError!*VariantData {
    return &(self.variant orelse return RuntimeError.InvalidSpirV);
}

pub inline fn getConstVariant(self: *const Self) RuntimeError!*const VariantData {
    return &(self.variant orelse return RuntimeError.InvalidSpirV);
}

/// Performs a deep copy
pub fn dupe(self: *const Self, allocator: std.mem.Allocator) RuntimeError!Self {
    return .{
        .name = if (self.name) |name| allocator.dupe(u8, name) catch return RuntimeError.OutOfMemory else null,
        .decorations = self.decorations.clone(allocator) catch return RuntimeError.OutOfMemory,
        .variant = blk: {
            if (self.variant) |variant| {
                switch (variant) {
                    .String => |s| break :blk .{
                        .String = allocator.dupe(u8, s) catch return RuntimeError.OutOfMemory,
                    },
                    .Type => |t| switch (t) {
                        .Structure => |s| break :blk .{
                            .Type = .{
                                .Structure = .{
                                    .members_type_word = allocator.dupe(SpvWord, s.members_type_word) catch return RuntimeError.OutOfMemory,
                                    .members_offsets = allocator.dupe(?SpvWord, s.members_offsets) catch return RuntimeError.OutOfMemory,
                                    .member_names = blk2: {
                                        const member_names = s.member_names.clone(allocator) catch return RuntimeError.OutOfMemory;
                                        for (member_names.items, s.member_names.items) |*new_name, name| {
                                            new_name.* = allocator.dupe(u8, name) catch return RuntimeError.OutOfMemory;
                                        }
                                        break :blk2 member_names;
                                    },
                                },
                            },
                        },
                        .Function => |f| break :blk .{
                            .Type = .{
                                .Function = .{
                                    .source_location = f.source_location,
                                    .return_type = f.return_type,
                                    .params = allocator.dupe(SpvWord, f.params) catch return RuntimeError.OutOfMemory,
                                },
                            },
                        },
                        else => break :blk .{ .Type = t },
                    },
                    .Variable => |v| break :blk .{
                        .Variable = .{
                            .storage_class = v.storage_class,
                            .type_word = v.type_word,
                            .type = v.type,
                            .value = try v.value.dupe(allocator),
                        },
                    },
                    .Constant => |c| break :blk .{
                        .Constant = .{
                            .type_word = c.type_word,
                            .type = c.type,
                            .value = try c.value.dupe(allocator),
                        },
                    },
                    .Function => |f| break :blk .{
                        .Function = .{
                            .source_location = f.source_location,
                            .return_type = f.return_type,
                            .function_type = f.function_type,
                            .params = allocator.dupe(SpvWord, f.params) catch return RuntimeError.OutOfMemory,
                        },
                    },
                    else => break :blk variant,
                }
            }
            break :blk null;
        },
    };
}

pub fn resolveLaneBitWidth(target_type: TypeData, rt: *const Runtime) RuntimeError!SpvWord {
    return sw: switch (target_type) {
        .Bool => 8,
        .Float => |f| f.bit_length,
        .Int => |i| i.bit_length,
        .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
        .Vector4f32,
        .Vector3f32,
        .Vector2f32,
        .Vector4i32,
        .Vector3i32,
        .Vector2i32,
        .Vector4u32,
        .Vector3u32,
        .Vector2u32,
        => return 32,
        else => return RuntimeError.InvalidSpirV,
    };
}

pub fn resolveLaneCount(target_type: TypeData) RuntimeError!SpvWord {
    return switch (target_type) {
        .Bool, .Float, .Int => 1,
        .Vector => |v| v.member_count,
        .Vector4f32, .Vector4i32, .Vector4u32 => 4,
        .Vector3f32, .Vector3i32, .Vector3u32 => 3,
        .Vector2f32, .Vector2i32, .Vector2u32 => 2,
        else => return RuntimeError.InvalidSpirV,
    };
}

pub fn resolveSign(target_type: TypeData, rt: *const Runtime) RuntimeError!enum { signed, unsigned } {
    return sw: switch (target_type) {
        .Int => |i| if (i.is_signed) .signed else .unsigned,
        .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
        .Vector4i32 => .signed,
        .Vector3i32 => .signed,
        .Vector2i32 => .signed,
        .Vector4u32 => .unsigned,
        .Vector3u32 => .unsigned,
        .Vector2u32 => .unsigned,
        else => .unsigned,
    };
}

pub inline fn resolveType(self: *const Self, results: []const Self) *const Self {
    return if (self.resolveTypeWordOrNull()) |word| &results[word] else self;
}

pub fn resolveTypeWordOrNull(self: *const Self) ?SpvWord {
    return if (self.variant) |variant|
        switch (variant) {
            .Type => |t| switch (t) {
                .Pointer => |ptr| ptr.target,
                else => null,
            },
            else => null,
        }
    else
        null;
}

pub fn getMemberCounts(self: *const Self) usize {
    if (self.variant) |variant| {
        switch (variant) {
            .Type => |t| switch (t) {
                .Bool, .Int, .Float, .Image, .Sampler => return 1,
                .Vector => |v| return v.member_count,
                .Vector4f32, .Vector4i32, .Vector4u32 => return 4,
                .Vector3f32, .Vector3i32, .Vector3u32 => return 3,
                .Vector2f32, .Vector2i32, .Vector2u32 => return 2,
                .Matrix => |m| return m.member_count,
                .Array => |a| return a.member_count,
                .SampledImage => return 2,
                .Structure => |s| return s.members_type_word.len,
                .Function => |f| return f.params.len,
                else => {},
            },
            else => {},
        }
    }
    return 0;
}

pub fn flushPtr(self: *Self, allocator: std.mem.Allocator) RuntimeError!void {
    if (self.variant) |*variant| {
        switch (variant.*) {
            .Constant => |*c| try c.value.flushPtr(allocator),
            .Variable => |*v| try v.value.flushPtr(allocator),
            .AccessChain => |*a| try a.value.flushPtr(allocator),
            else => {},
        }
    }
}
