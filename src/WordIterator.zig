const std = @import("std");
const spv = @import("spv.zig");

const RuntimeError = @import("Runtime.zig").RuntimeError;

const SpvWord = spv.SpvWord;

const Self = @This();

buffer: []const SpvWord,
index: usize,
did_jump: bool,

pub fn init(buffer: []const SpvWord) Self {
    return .{
        .buffer = buffer,
        .index = 0,
        .did_jump = false,
    };
}

pub fn nextOrNull(self: *Self) ?SpvWord {
    const word = self.peek() orelse return null;
    self.index += 1;
    return word;
}

pub inline fn nextAsOrNull(self: *Self, comptime E: type) ?E {
    return if (self.nextOrNull()) |word| std.enums.fromInt(E, word) else null;
}

pub inline fn next(self: *Self) RuntimeError!SpvWord {
    return self.nextOrNull() orelse return RuntimeError.InvalidSpirV;
}

pub fn nextAs(self: *Self, comptime E: type) RuntimeError!E {
    return self.nextAsOrNull(E) orelse return RuntimeError.InvalidSpirV;
}

pub fn peek(self: *const Self) ?SpvWord {
    return if (self.index >= self.buffer.len) null else self.buffer[self.index];
}

pub fn skip(self: *Self) bool {
    if (self.index >= self.buffer.len) {
        return false;
    }
    self.index += 1;
    return true;
}

pub fn skipN(self: *Self, count: usize) bool {
    if (self.index >= self.buffer.len) {
        return false;
    }
    self.index += count;
    return true;
}

pub fn skipToEnd(self: *Self) void {
    self.index = self.buffer.len;
}

pub inline fn emitSourceLocation(self: *const Self) usize {
    return self.index;
}

pub inline fn jumpToSourceLocation(self: *Self, source_location: usize) bool {
    if (source_location > self.buffer.len) return false;
    self.index = source_location;
    self.did_jump = true;
    return true;
}
