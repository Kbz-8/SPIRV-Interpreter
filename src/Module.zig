const std = @import("std");
const lib = @import("lib.zig");
const spv_data = @import("spv_data.zig");

const SpvVoid = lib.SpvVoid;
const SpvByte = lib.SpvByte;
const SpvWord = lib.SpvWord;
const SpvBool = lib.SpvBool;
const spv = lib.spv;

const Interpreter = @import("Interpreter.zig");
const WordIterator = @import("WordIterator.zig");

const Self = @This();

const SpvEntryPoint = struct {
    exec_model: spv.SpvExecutionModel,
    id: SpvWord,
    name: []const u8,
    globals_count: SpvWord,
    globals: []const SpvWord,
};

const ModuleError = error{
    InvalidSpirV,
    InvalidMagic,
    OutOfMemory,
};

ctx: *const Interpreter,

it: WordIterator,

version_major: SpvByte,
version_minor: SpvByte,

generator_id: u16,
generator_version: u16,

bound: SpvWord,

code: []const SpvWord,

addressing: spv.SpvAddressingModel,
memory_model: spv.SpvMemoryModel,

entry_point_count: SpvWord,
entry_points: []const SpvEntryPoint,

local_size_x: SpvWord,
local_size_y: SpvWord,
local_size_z: SpvWord,

pub fn init(allocator: std.mem.Allocator, ctx: *const Interpreter, source: []const SpvWord) ModuleError!Self {
    var self: Self = std.mem.zeroInit(Self, .{
        .ctx = ctx,
        .code = allocator.dupe(SpvWord, source) catch return ModuleError.OutOfMemory,
        .local_size_x = 1,
        .local_size_y = 1,
        .local_size_z = 1,
    });
    errdefer allocator.free(self.code);

    self.it = WordIterator.init(self.code);

    const magic = self.it.next() orelse return ModuleError.InvalidSpirV;
    if (magic != lib.spv.SpvMagicNumber) return ModuleError.InvalidMagic;

    const version = self.it.next() orelse return ModuleError.InvalidSpirV;
    self.version_major = @intCast((version & 0x00FF0000) >> 16);
    self.version_minor = @intCast((version & 0x0000FF00) >> 8);

    const generator = self.it.next() orelse return ModuleError.InvalidSpirV;
    self.generator_id = @intCast((generator & 0xFFFF0000) >> 16);
    self.generator_version = @intCast(generator & 0x0000FFFF);

    self.bound = self.it.next() orelse return ModuleError.InvalidSpirV;

    _ = self.it.skip(); // Skip schema

    std.log.scoped(.SPIRV_Interpreter).debug(
        \\Loaded shader module with infos:
        \\    SPIR-V version: {d}.{d}
        \\    Generator:      {s} (ID {d}), encoded version {d}
    , .{
        self.version_major,
        self.version_minor,
        spv_data.vendorName(self.generator_id),
        self.generator_id,
        self.generator_version,
    });

    return self;
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    allocator.free(self.code);
}
