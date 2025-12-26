const std = @import("std");
const spv = @import("spv.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const RType = enum {
    None,
    String,
    Extension,
    Function_type,
    Type,
    Variable,
    Constant,
    Function,
    Access_chain,
    Function_parameter,
    Label,
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
    literal_2: SpvWord,
    index: SpvWord,
};

const Self = @This();

name: ?[]const u8,
ptr: SpvWord,

storage_class: spv.SpvStorageClass,
parent: ?*const Self,

member_names: std.ArrayList([]const u8),
members: std.ArrayList(spv.SpvMember),

decorations: std.ArrayList(Decoration),

/// Only for functions
return_type: SpvWord,

rtype: RType,

pub fn init() Self {
    return std.mem.zeroInit(Self, .{
        .name = null,
        .parent = null,
        .member_names = std.ArrayList([]const u8).empty,
        .members = std.ArrayList(spv.SpvMember).empty,
        .decorations = std.ArrayList(Decoration).empty,
        .rtype = RType.None,
    });
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    if (self.name) |name| {
        allocator.free(name);
    }
    for (self.member_names.items) |name| {
        allocator.free(name);
    }
    self.member_names.deinit(allocator);
    self.members.deinit(allocator);
    self.decorations.deinit(allocator);
}
