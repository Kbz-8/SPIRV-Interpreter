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
    Void: noreturn,
    Bool: bool,
    Int: union {
        sint8: i8,
        sint16: i16,
        sint32: i32,
        sint64: i64,
        uint8: u8,
        uint16: u16,
        uint32: u32,
        uint64: u64,
    },
    Float: union {
        float16: f16,
        float32: f32,
        float64: f64,
    },
    Vector: []Value,
    Matrix: []Value,
    Array: struct {},
    RuntimeArray: struct {},
    Structure: struct {},
    Function: noreturn,
    Image: struct {},
    Sampler: struct {},
    SampledImage: struct {},
    Pointer: noreturn,
};

const Self = @This();

name: ?[]const u8,

parent: ?*const Self,

member_names: std.ArrayList([]const u8),

decorations: std.ArrayList(Decoration),

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
            column_type: Type,
            member_count: SpvWord,
        },
        Array: struct {},
        RuntimeArray: struct {},
        Structure: struct {
            /// Allocated array
            members: []const SpvWord,
        },
        Function: struct {
            return_type: SpvWord,
            /// Allocated array
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
        value: Value,
    },
    Constant: Value,
    Function: struct {
        return_type: SpvWord,
        function_type: SpvWord,
        /// Allocated array
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
        .member_names = .empty,
        .decorations = .empty,
        .variant = null,
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    if (self.name) |name| {
        allocator.free(name);
    }
    for (self.member_names.items) |name| {
        allocator.free(name);
    }
    if (self.variant) |variant| {
        switch (variant) {
            .Type => |t| {
                switch (t) {
                    .Function => |data| allocator.free(data.params),
                    .Structure => |data| allocator.free(data.members),
                    else => {},
                }
            },
            else => {},
        }
    }
    self.member_names.deinit(allocator);
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

pub fn initConstantValue(self: *Self, results: []const Self, target: SpvWord) RuntimeError!void {
    _ = self;
    const resolved = results[target].resolveType(results);
    if (resolved.variant) |variant| {
        switch (variant) {
            .Type => |t| switch (t) {
                .Structure => |s|
                else => return RuntimeError.InvalidSpirV,
            },
            else => return RuntimeError.InvalidSpirV,
        }
    }
    return RuntimeError.InvalidSpirV;
}
