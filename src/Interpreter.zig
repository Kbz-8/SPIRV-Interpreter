const Self = @This();

pub fn init() !Self {
    return .{};
}

pub fn deinit(self: *const Self) void {
    _ = self;
}
