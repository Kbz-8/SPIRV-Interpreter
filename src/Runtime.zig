const std = @import("std");
const spv = @import("spv.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Module = @import("Module.zig");
const WordIterator = @import("WordIterator.zig");

const Self = @This();

pub const CallError = error{
    Unreachable,
    Killed,
    Error,
    InitEnd,
    ExecEnd,
};

module: *Module,
it: WordIterator,
stack_frames: std.SinglyLinkedList,

pub fn init(module: *Module) !Self {
    return .{
        .module = module,
        .it = module.it,
        .stack_frames = .{},
    };
}

pub fn deinit(self: *const Self) void {
    _ = self;
}

pub fn callEntryPoint(self: *Self, entry: SpvWord) CallError!void {
    _ = self;
    _ = entry;
}
