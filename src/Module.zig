const std = @import("std");
const lib = @import("lib.zig");

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

ctx: *const Interpreter,

it: WordIterator,

version_major: SpvByte,
version_minor: SpvByte,
generator_magic: SpvWord,

bound: SpvWord,

code: []const SpvWord,

addressing: spv.SpvAddressingModel,
memory_model: spv.SpvMemoryModel,

entry_point_count: SpvWord,
entry_points: []const SpvEntryPoint,

local_size_x: SpvWord,
local_size_y: SpvWord,
local_size_z: SpvWord,

pub fn init(allocator: std.mem.Allocator, ctx: *const Interpreter, source: []const SpvWord) !Self {
    var self: Self = std.mem.zeroInit(Self, .{
        .ctx = ctx,
        .code = try allocator.dupe(SpvWord, source),
    });

    self.it = WordIterator.init(self.code);

    return self;
}

pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
    allocator.free(self.code);
}
