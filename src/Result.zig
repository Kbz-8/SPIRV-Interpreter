const std = @import("std");
const spv = @import("spv.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Type = enum {
    None,
    String,
    Extension,
    FunctionType,
    Type,
    Variable,
    Constant,
    Function,
    AccessChain,
    FunctionParameter,
    Label,
};

const ValueType = enum {
    Void,
    Bool,
    Int,
    Float,
    Vector,
    Matrix,
    Array,
    RuntimeArray,
    Structure,
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

const Self = @This();

name: ?[]const u8,

parent: ?*const Self,

member_names: std.ArrayList([]const u8),
members: std.ArrayList(spv.SpvMember),

decorations: std.ArrayList(Decoration),

res_type: Type,
type_data: union(Type) {
    None: struct {},
    String: []const u8,
    Extension: struct {},
    FunctionType: struct {
        return_type: SpvWord,
    },
    Type: struct {
        value_type: ValueType,
        data: union(ValueType) {
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
                components_type: ValueType,
            },
            Matrix: struct {
                column_type: ValueType,
            },
            Array: struct {},
            RuntimeArray: struct {},
            Structure: struct {},
            Image: struct {},
            Sampler: struct {},
            SampledImage: struct {},
            Pointer: struct {
                storage_class: spv.SpvStorageClass,
            },
        },
        member_count: SpvWord = 0,
        id: SpvWord = 0,
    },
    Variable: struct {},
    Constant: struct {},
    Function: struct {
        /// Allocated array
        params: []SpvWord,
    },
    AccessChain: struct {},
    FunctionParameter: struct {},
    Label: struct {},
},

pub fn init() Self {
    return .{
        .name = null,
        .parent = null,
        .member_names = .empty,
        .members = .empty,
        .decorations = .empty,
        .res_type = .None,
        .type_data = undefined,
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    if (self.name) |name| {
        allocator.free(name);
    }
    for (self.member_names.items) |name| {
        allocator.free(name);
    }
    // FIXME
    //switch (self.type_data) {
    //    .Function => |data| allocator.free(data.params),
    //    else => {},
    //}
    self.member_names.deinit(allocator);
    self.members.deinit(allocator);
    self.decorations.deinit(allocator);
}
