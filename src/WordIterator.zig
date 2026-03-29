const std = @import("std");
const spv = @import("spv.zig");

const RuntimeError = @import("Runtime.zig").RuntimeError;

const SpvWord = spv.SpvWord;

const Self = @This();

buffer: []const SpvWord,
index: usize,
did_jump: bool,
next_force_skip: ?usize,

pub fn init(buffer: []const SpvWord) Self {
    return .{
        .buffer = buffer,
        .index = 0,
        .did_jump = false,
        .next_force_skip = null,
    };
}

pub inline fn nextOrNull(self: *Self) ?SpvWord {
    const word = self.peek() orelse return null;
    self.index += 1;
    return word;
}

/// self.index + index will be automatically skipped
pub inline fn forceSkipIndex(self: *Self, index: SpvWord) void {
    self.next_force_skip = self.index + index;
}

pub inline fn nextAsOrNull(self: *Self, comptime E: type) ?E {
    if (self.next_force_skip) |skip_index| {
        if (self.index == skip_index) {
            _ = self.skip();
            self.next_force_skip = null;
        }
    }
    return if (self.nextOrNull()) |word| std.enums.fromInt(E, word) else null;
}

pub inline fn next(self: *Self) RuntimeError!SpvWord {
    if (self.next_force_skip) |skip_index| {
        if (self.index == skip_index) {
            _ = self.skip();
            self.next_force_skip = null;
        }
    }
    return self.nextOrNull() orelse return RuntimeError.InvalidSpirV;
}

pub inline fn nextAs(self: *Self, comptime E: type) RuntimeError!E {
    if (self.next_force_skip) |skip_index| {
        if (self.index == skip_index) {
            _ = self.skip();
        }
    }
    return self.nextAsOrNull(E) orelse return RuntimeError.InvalidSpirV;
}

pub inline fn peek(self: *const Self) ?SpvWord {
    return if (self.index >= self.buffer.len) null else self.buffer[self.index];
}

pub inline fn skip(self: *Self) bool {
    if (self.index >= self.buffer.len) {
        return false;
    }
    self.index += 1;
    return true;
}

pub inline fn skipN(self: *Self, count: usize) bool {
    if (self.index >= self.buffer.len) {
        return false;
    }
    self.index += count;
    return true;
}

pub inline fn skipToEnd(self: *Self) void {
    self.index = self.buffer.len;
    self.did_jump = true;
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

/// Like jumpToSourceLocation without toggling self.did_jump
pub inline fn goToSourceLocation(self: *Self, source_location: usize) bool {
    if (source_location > self.buffer.len) return false;
    self.index = source_location;
    return true;
}
