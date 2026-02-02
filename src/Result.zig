const std = @import("std");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");

const Runtime = @import("Runtime.zig");
const RuntimeError = Runtime.RuntimeError;

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

pub const Vec4f32 = @Vector(4, f32);
pub const Vec3f32 = @Vector(3, f32);
pub const Vec2f32 = @Vector(2, f32);

pub const Vec4i32 = @Vector(4, i32);
pub const Vec3i32 = @Vector(3, i32);
pub const Vec2i32 = @Vector(2, i32);

pub const Vec4u32 = @Vector(4, u32);
pub const Vec3u32 = @Vector(3, u32);
pub const Vec2u32 = @Vector(2, u32);

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

const Decoration = struct {
    rtype: spv.SpvDecoration,
    literal_1: SpvWord,
    literal_2: ?SpvWord,
    index: SpvWord,
};

pub const Value = union(Type) {
    Void: struct {},
    Bool: bool,
    Int: extern union {
        sint8: i8,
        sint16: i16,
        sint32: i32,
        sint64: i64,
        uint8: u8,
        uint16: u16,
        uint32: u32,
        uint64: u64,
    },
    Float: extern union {
        float16: f16,
        float32: f32,
        float64: f64,
    },
    Vector: []Value,
    Vector4f32: Vec4f32,
    Vector3f32: Vec3f32,
    Vector2f32: Vec2f32,
    Vector4i32: Vec4i32,
    Vector3i32: Vec3i32,
    Vector2i32: Vec2i32,
    Vector4u32: Vec4u32,
    Vector3u32: Vec3u32,
    Vector2u32: Vec2u32,
    Matrix: []Value,
    Array: []Value,
    RuntimeArray: ?[]Value,
    Structure: []Value,
    Function: noreturn,
    Image: struct {},
    Sampler: struct {},
    SampledImage: struct {},
    Pointer: union(enum) {
        common: *Value,
        f32_ptr: *f32,
        i32_ptr: *i32, //< For vectors specializations
        u32_ptr: *u32,
    },

    pub inline fn getCompositeDataOrNull(self: *const Value) ?[]Value {
        return switch (self.*) {
            .Vector, .Matrix, .Array, .Structure => |v| v,
            .RuntimeArray => |v| v,
            else => null,
        };
    }

    fn init(allocator: std.mem.Allocator, results: []const Self, target: SpvWord) RuntimeError!Value {
        const resolved = results[target].resolveType(results);
        const member_count = resolved.getMemberCounts();

        return switch (resolved.variant.?) {
            .Type => |t| switch (t) {
                .Bool => .{ .Bool = false },
                .Int => .{ .Int = .{ .uint64 = 0 } },
                .Float => .{ .Float = .{ .float64 = 0.0 } },
                .Vector => |v| blk: {
                    var self: Value = .{ .Vector = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer self.deinit(allocator);

                    for (self.Vector) |*value| {
                        value.* = try Value.init(allocator, results, v.components_type_word);
                    }
                    break :blk self;
                },
                .Vector4f32 => .{ .Vector4f32 = Vec4f32{ 0.0, 0.0, 0.0, 0.0 } },
                .Vector3f32 => .{ .Vector3f32 = Vec3f32{ 0.0, 0.0, 0.0 } },
                .Vector2f32 => .{ .Vector2f32 = Vec2f32{ 0.0, 0.0 } },
                .Vector4i32 => .{ .Vector4i32 = Vec4i32{ 0, 0, 0, 0 } },
                .Vector3i32 => .{ .Vector3i32 = Vec3i32{ 0, 0, 0 } },
                .Vector2i32 => .{ .Vector2i32 = Vec2i32{ 0, 0 } },
                .Vector4u32 => .{ .Vector4u32 = Vec4u32{ 0, 0, 0, 0 } },
                .Vector3u32 => .{ .Vector3u32 = Vec3u32{ 0, 0, 0 } },
                .Vector2u32 => .{ .Vector2u32 = Vec2u32{ 0, 0 } },
                .Matrix => |m| blk: {
                    var self: Value = .{ .Matrix = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer self.deinit(allocator);

                    for (self.Matrix) |*value| {
                        value.* = try Value.init(allocator, results, m.column_type_word);
                    }
                    break :blk self;
                },
                .Array => |a| blk: {
                    var self: Value = .{ .Array = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer self.deinit(allocator);

                    for (self.Array) |*value| {
                        value.* = try Value.init(allocator, results, a.components_type_word);
                    }
                    break :blk self;
                },
                .Structure => |s| blk: {
                    var self: Value = .{ .Structure = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer self.deinit(allocator);

                    for (self.Structure, s.members_type_word) |*value, member_type_word| {
                        value.* = try Value.init(allocator, results, member_type_word);
                    }
                    break :blk self;
                },
                .RuntimeArray => .{ .RuntimeArray = null },
                else => unreachable,
            },
            else => unreachable,
        };
    }

    /// Performs a deep copy
    pub fn dupe(self: *const Value, allocator: std.mem.Allocator) RuntimeError!Value {
        return switch (self.*) {
            .Vector => |v| .{
                .Vector = blk: {
                    const values = allocator.dupe(Value, v) catch return RuntimeError.OutOfMemory;
                    for (values, v) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            .Matrix => |m| .{
                .Matrix = blk: {
                    const values = allocator.dupe(Value, m) catch return RuntimeError.OutOfMemory;
                    for (values, m) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            .Array => |a| .{
                .Array = blk: {
                    const values = allocator.dupe(Value, a) catch return RuntimeError.OutOfMemory;
                    for (values, a) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            .RuntimeArray => |opt_a| .{
                .RuntimeArray = blk: {
                    if (opt_a) |a| {
                        const values = allocator.dupe(Value, a) catch return RuntimeError.OutOfMemory;
                        for (values, a) |*new_value, value| new_value.* = try value.dupe(allocator);
                        break :blk values;
                    } else {
                        break :blk null;
                    }
                },
            },
            .Structure => |s| .{
                .Structure = blk: {
                    const values = allocator.dupe(Value, s) catch return RuntimeError.OutOfMemory;
                    for (values, s) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            else => self.*,
        };
    }

    fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Vector, .Matrix, .Array, .Structure => |values| {
                for (values) |*value| value.deinit(allocator);
                allocator.free(values);
            },
            .RuntimeArray => |opt_values| if (opt_values) |values| {
                for (values) |*value| value.deinit(allocator);
                allocator.free(values);
            },
            else => {},
        }
    }
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
    },
    RuntimeArray: struct {
        components_type_word: SpvWord,
        components_type: Type,
    },
    Structure: struct {
        members_type_word: []const SpvWord,
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
                    data.member_names.deinit(allocator);
                },
                else => {},
            },
            .Constant => |*c| c.value.deinit(allocator),
            .Variable => |*v| v.value.deinit(allocator),
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
        else => .unsinged,
    };
}

pub fn resolveType(self: *const Self, results: []const Self) *const Self {
    return if (self.variant) |variant|
        switch (variant) {
            .Type => |t| switch (t) {
                .Pointer => |ptr| &results[ptr.target],
                else => self,
            },
            else => self,
        }
    else
        self;
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

pub fn initValue(allocator: std.mem.Allocator, member_count: usize, results: []const Self, resolved: *const Self) RuntimeError!Value {
    return switch (resolved.variant.?) {
        .Type => |t| switch (t) {
            .Void => .{ .Void = .{} },
            .Bool => .{ .Bool = false },
            .Int => .{ .Int = .{ .uint64 = 0 } },
            .Float => .{ .Float = .{ .float64 = 0.0 } },
            .Vector => |v| blk: {
                const value: Value = .{ .Vector = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                errdefer allocator.free(value.Vector);
                for (value.Vector) |*val| {
                    val.* = try Value.init(allocator, results, v.components_type_word);
                }
                break :blk value;
            },
            .Vector4f32 => .{ .Vector4f32 = Vec4f32{ 0.0, 0.0, 0.0, 0.0 } },
            .Vector3f32 => .{ .Vector3f32 = Vec3f32{ 0.0, 0.0, 0.0 } },
            .Vector2f32 => .{ .Vector2f32 = Vec2f32{ 0.0, 0.0 } },
            .Vector4i32 => .{ .Vector4i32 = Vec4i32{ 0, 0, 0, 0 } },
            .Vector3i32 => .{ .Vector3i32 = Vec3i32{ 0, 0, 0 } },
            .Vector2i32 => .{ .Vector2i32 = Vec2i32{ 0, 0 } },
            .Vector4u32 => .{ .Vector4u32 = Vec4u32{ 0, 0, 0, 0 } },
            .Vector3u32 => .{ .Vector3u32 = Vec3u32{ 0, 0, 0 } },
            .Vector2u32 => .{ .Vector2u32 = Vec2u32{ 0, 0 } },
            .Matrix => |m| blk: {
                const value: Value = .{ .Matrix = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                errdefer allocator.free(value.Matrix);
                for (value.Matrix) |*v| {
                    v.* = try Value.init(allocator, results, m.column_type_word);
                }
                break :blk value;
            },
            .Array => |a| blk: {
                const value: Value = .{ .Array = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                errdefer allocator.free(value.Array);
                for (value.Array) |*val| {
                    val.* = try Value.init(allocator, results, a.components_type_word);
                }
                break :blk value;
            },
            .RuntimeArray => |a| blk: {
                std.debug.print("test {d}\n", .{member_count});
                if (member_count == 0) {
                    break :blk Value{ .RuntimeArray = null };
                }
                const value: Value = .{ .RuntimeArray = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                errdefer allocator.free(value.RuntimeArray.?);
                for (value.RuntimeArray.?) |*val| {
                    val.* = try Value.init(allocator, results, a.components_type_word);
                }
                break :blk value;
            },
            .Structure => |s| blk: {
                const value: Value = .{ .Structure = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
                errdefer allocator.free(value.Structure);
                for (value.Structure, s.members_type_word) |*v, member_type_word| {
                    v.* = try Value.init(allocator, results, member_type_word);
                }
                break :blk value;
            },
            .Image => RuntimeError.ToDo,
            .Sampler => RuntimeError.ToDo,
            .SampledImage => RuntimeError.ToDo,
            else => RuntimeError.InvalidSpirV,
        },
        else => RuntimeError.InvalidSpirV,
    };
}
