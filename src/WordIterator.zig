const lib = @import("lib.zig");

const SpvWord = lib.SpvWord;

const Self = @This();

buffer: []const SpvWord,
index: usize,

pub fn init(buffer: []const SpvWord) Self {
    return .{
        .buffer = buffer,
        .index = 0,
    };
}

pub fn next(self: *Self) ?SpvWord {
    const word = self.peek() orelse return null;
    self.index += 1;
    return word;
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
