const std = @import("std");
const spv = @import("spv.zig");

const RuntimeError = @import("Runtime.zig").RuntimeError;

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

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
    Void,
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
    Matrix: []Value,
    Array: struct {},
    RuntimeArray: struct {},
    Structure: []Value,
    Function,
    Image: struct {},
    Sampler: struct {},
    SampledImage: struct {},
    Pointer,

    fn initMembers(self: *Value, allocator: std.mem.Allocator, results: []const Self, target: SpvWord) RuntimeError!void {
        const resolved = results[target].resolveType(results);
        const member_count = resolved.getMemberCounts();

        switch (resolved.variant.?) {
            .Type => |t| switch (t) {
                .Bool, .Int, .Float => std.debug.assert(member_count == 1),
                .Structure => |s| {
                    _ = s;
                    //self.Structure = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory;
                    //for (self.Structure, s.members) |*value, member_id| {
                    //    value.* = switch (results[member_id].variant.?.Type) { // wtf ?
                    //        inline else => |tag| @unionInit(Value, @tagName(tag), undefined),
                    //    };
                    //}
                },
                .Matrix => |m| {
                    self.Matrix = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory;
                    for (self.Matrix) |*value| {
                        value.* = switch (m.column_type) {
                            inline else => |tag| @unionInit(Value, @tagName(tag), undefined),
                        };
                    }
                },
                .Array => |a| {
                    _ = a;
                },
                .Vector => |v| {
                    self.Vector = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory;
                    for (self.Vector) |*value| {
                        value.* = switch (v.components_type) {
                            inline else => |tag| @unionInit(Value, @tagName(tag), undefined),
                        };
                    }
                },
                else => {},
            },
            else => {},
        }
    }
};

//void spvm_member_allocate_typed_value(spvm_member_t val, spvm_result* results, spvm_word type)
//{
//    spvm_result_t type_info = spvm_state_get_type_info(results, &results[type]);
//    assert(type != 0);
//
//    if (type_info->value_type == spvm_value_type_void ||
//                    type_info->value_type == spvm_value_type_int ||
//                    type_info->value_type == spvm_value_type_float ||
//                    type_info->value_type == spvm_value_type_bool) {
//            assert(type_info->member_count == 1u);
//    } else {
//            spvm_member_allocate_value(val, type_info->member_count);
//    }
//
//    val->type = type;
//
//    if (type_info->value_type == spvm_value_type_struct) {
//            for (spvm_word i = 0; i < val->member_count; i++) {
//                    spvm_member_allocate_typed_value(&val->members[i], results, type_info->params[i]);
//            }
//    }
//    else if (type_info->value_type == spvm_value_type_matrix) {
//            for (spvm_word i = 0; i < val->member_count; i++)
//                    spvm_member_allocate_typed_value(&val->members[i], results, type_info->pointer);
//    }
//    else if (type_info->value_type == spvm_value_type_array) {
//            if (results[type_info->pointer].member_count > 0)
//                    for (spvm_word i = 0; i < val->member_count; i++)
//                            spvm_member_allocate_typed_value(&val->members[i], results, type_info->pointer);
//    } else if (type_info->value_type == spvm_value_type_vector) {
//            for (spvm_word i = 0; i < val->member_count; ++i)
//                    val->members[i].type = type_info->pointer;
//    } else {
//            // having nested images/samplers is not supported
//            assert(type_info->value_type != spvm_value_type_sampled_image);
//            assert(type_info->value_type != spvm_value_type_image);
//            assert(type_info->value_type != spvm_value_type_sampler);
//    }
//}

const Self = @This();

name: ?[]const u8,

decorations: std.ArrayList(Decoration),

parent: ?*const Self,

variant: ?union(Variant) {
    String: []const u8,
    Extension: struct {},
    Type: union(Type) {
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
            components_type: Type,
            member_count: SpvWord,
        },
        Matrix: struct {
            column_type_word: SpvWord,
            column_type: Type,
            member_count: SpvWord,
        },
        Array: struct {},
        RuntimeArray: struct {},
        Structure: struct {
            members: []const SpvWord,
            member_names: std.ArrayList([]const u8),
        },
        Function: struct {
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
    },
    Variable: struct {
        storage_class: spv.SpvStorageClass,
        values: []Value,
    },
    Constant: []Value,
    Function: struct {
        return_type: SpvWord,
        function_type: SpvWord,
        params: []const SpvWord,
    },
    AccessChain: struct {},
    FunctionParameter: struct {},
    Label: struct {},
},

pub fn init() Self {
    return .{
        .name = null,
        .parent = null,
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
                    allocator.free(data.members);
                    for (data.member_names.items) |name| {
                        allocator.free(name);
                    }
                    data.member_names.deinit(allocator);
                },
                else => {},
            },
            .Constant => |values| allocator.free(values),
            else => {},
        }
    }
    self.decorations.deinit(allocator);
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
                .Matrix => |m| return m.member_count,
                .SampledImage => return 2,
                .Structure => |s| return s.members.len,
                .Function => |f| return f.params.len,
                else => {},
            },
            else => {},
        }
    }
    return 0;
}

pub fn initConstantValue(self: *Self, allocator: std.mem.Allocator, results: []const Self, target: SpvWord) RuntimeError!void {
    const resolved = results[target].resolveType(results);
    const member_count = resolved.getMemberCounts();

    if (member_count == 0) return;

    self.variant = .{ .Constant = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory };
    errdefer switch (self.variant.?) {
        .Constant => |c| allocator.free(c),
        else => unreachable,
    };
    const values = self.variant.?.Constant;

    switch (resolved.variant.?) {
        .Type => |t| switch (t) {
            .Bool => values[0] = .{ .Bool = undefined },
            .Int => values[0] = .{ .Int = undefined },
            .Float => values[0] = .{ .Float = undefined },
            .Vector => |v| {
                for (values) |*value| {
                    value.* = switch (v.components_type) {
                        inline else => |tag| @unionInit(Value, @tagName(tag), undefined),
                    };
                }
            },
            .Matrix => |m| {
                for (values) |*value| {
                    try value.initMembers(allocator, results, m.column_type_word);
                }
            },
            .Array => |a| {
                _ = a;
            },
            .Structure => |s| {
                for (values, s.members) |*value, member_type| {
                    try value.initMembers(allocator, results, member_type);
                }
            },
            .Image => {},
            .Sampler => {}, // No op
            .SampledImage => {},
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidSpirV,
    }
}
