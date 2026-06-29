pub const spv = @import("spv");

pub const SpvCBool = c_int;
pub const SpvCWord = c_ulong;
pub const SpvCSize = c_ulong;

pub const Result = enum(c_int) {
    Success = 0,
    Barrier = 1,
    Killed = 2,
    InvalidEntryPoint = -1,
    InvalidSpirV = -2,
    InvalidValueType = -3,
    NotFound = -4,
    OutOfMemory = -5,
    OutOfBounds = -6,
    ToDo = -7,
    Unreachable = -8,
    UnsupportedSpirV = -9,
    UnsupportedExtension = -10,
    UnsupportedEndianness = -11,
    InvalidMagic = -12,
    Unknown = -13,
};

comptime {
    _ = @import("module.zig");
    _ = @import("runtime.zig");
}
