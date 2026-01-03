const std = @import("std");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Module = @import("Module.zig");
const Result = @import("Result.zig");
const WordIterator = @import("WordIterator.zig");

const Self = @This();

pub const RuntimeError = error{
    InvalidSpirV,
    UnsupportedSpirV,
    OutOfMemory,
    Unreachable,
    Killed,
};

mod: *Module,
it: WordIterator,

current_function: ?*Result,

pub fn init(module: *Module) Self {
    return std.mem.zeroInit(Self, .{
        .mod = module,
        .it = module.it,
        .current_function = null,
    });
}

pub fn deinit(self: *const Self) void {
    _ = self;
}

pub fn callEntryPoint(self: *Self, entry: SpvWord) RuntimeError!void {
    _ = self;
    _ = entry;
}
