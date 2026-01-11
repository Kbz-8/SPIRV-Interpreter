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
    Matrix: []Value,
    Array: struct {},
    RuntimeArray: struct {},
    Structure: []Value,
    Function: struct {},
    Image: struct {},
    Sampler: struct {},
    SampledImage: struct {},
    Pointer: struct {},

    fn initMembers(self: *Value, allocator: std.mem.Allocator, results: []const Self, target: SpvWord) RuntimeError!void {
        const resolved = results[target].resolveType(results);
        const member_count = resolved.getMemberCounts();

        switch (resolved.variant.?) {
            .Type => |t| switch (t) {
                .Bool, .Int, .Float => std.debug.assert(member_count == 1),
                .Structure => |s| {
                    self.* = .{
                        .Structure = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory,
                    };
                    for (self.Structure, s.members) |*value, member| {
                        value.* = switch (member) {
                            inline else => |tag| @unionInit(Value, @tagName(tag), undefined),
                        };
                    }
                },
                .Matrix => |m| {
                    self.* = .{
                        .Matrix = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory,
                    };
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
                    self.* = .{
                        .Vector = allocator.alloc(Value, member_count) catch return RuntimeError.OutOfMemory,
                    };
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

    /// Performs a deep copy
    fn dupe(self: *const Value, allocator: std.mem.Allocator) RuntimeError!Value {
        return switch (self.*) {
            .Vector => |v| .{
                .Vector = allocator.dupe(Value, v) catch return RuntimeError.OutOfMemory,
            },
            .Matrix => |m| .{
                .Matrix = allocator.dupe(Value, m) catch return RuntimeError.OutOfMemory,
            },
            .Structure => |s| .{
                .Structure = allocator.dupe(Value, s) catch return RuntimeError.OutOfMemory,
            },
            else => self.*,
        };
    }

    fn deinit(self: *Value, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Structure => |values| allocator.free(values),
            .Matrix => |values| allocator.free(values),
            .Vector => |values| allocator.free(values),
            else => {},
        }
    }
};

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
            components_type_word: SpvWord,
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
            members_type_word: []const SpvWord,
            members: []Type,
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
    },
    Variable: struct {
        storage_class: spv.SpvStorageClass,
        values: []Value,
    },
    Constant: []Value,
    Function: struct {
        source_location: usize,
        return_type: SpvWord,
        function_type: SpvWord,
        params: []const SpvWord,
    },
    AccessChain: struct {
        target: SpvWord,
        values: []Value,
    },
    FunctionParameter: struct {},
    Label: struct {
        source_location: usize,
    },
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
                    allocator.free(data.members_type_word);
                    allocator.free(data.members);
                    for (data.member_names.items) |name| {
                        allocator.free(name);
                    }
                    data.member_names.deinit(allocator);
                },
                else => {},
            },
            .Constant => |values| {
                for (values) |*value| value.deinit(allocator);
                allocator.free(values);
            },
            .Variable => |v| {
                for (v.values) |*value| value.deinit(allocator);
                allocator.free(v.values);
            },
            else => {},
        }
    }
    self.decorations.deinit(allocator);
}

/// Performs a deep copy
pub fn dupe(self: *const Self, allocator: std.mem.Allocator) RuntimeError!Self {
    return .{
        .name = if (self.name) |name| allocator.dupe(u8, name) catch return RuntimeError.OutOfMemory else null,
        .decorations = self.decorations.clone(allocator) catch return RuntimeError.OutOfMemory,
        .parent = self.parent,
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
                                    .members = allocator.dupe(Type, s.members) catch return RuntimeError.OutOfMemory,
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
                        else => {},
                    },
                    .Variable => |v| break :blk .{
                        .Variable = .{
                            .storage_class = v.storage_class,
                            .values = blk2: {
                                const values = allocator.dupe(Value, v.values) catch return RuntimeError.OutOfMemory;
                                for (values, v.values) |*new_value, value| {
                                    new_value.* = try value.dupe(allocator);
                                }
                                break :blk2 values;
                            },
                        },
                    },
                    .Constant => |c| break :blk .{
                        .Constant = blk2: {
                            const values = allocator.dupe(Value, c) catch return RuntimeError.OutOfMemory;
                            for (values, c) |*new_value, value| {
                                new_value.* = try value.dupe(allocator);
                            }
                            break :blk2 values;
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
                    else => {},
                }
                break :blk variant;
            }
            break :blk null;
        },
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

pub fn initValues(allocator: std.mem.Allocator, values: []Value, results: []const Self, resolved: *const Self) RuntimeError!void {
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
            .Array => |a| { // TODO
                _ = a;
            },
            .Structure => |s| {
                for (values, s.members_type_word) |*value, member_type_word| {
                    try value.initMembers(allocator, results, member_type_word);
                }
            },
            .Image => {}, // TODO
            .Sampler => {}, // No op
            .SampledImage => {}, // TODO
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidSpirV,
    }
}
