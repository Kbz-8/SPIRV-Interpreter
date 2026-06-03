pub const spv = @import("spv");

pub const SpvCBool = c_int;
pub const SpvCWord = c_ulong;
pub const SpvCSize = c_ulong;

pub const Result = enum(c_int) {
    Success = 0,
    Barrier = 1,
    Killed = 2,
    DivisionByZero = -1,
    InvalidEntryPoint = -2,
    InvalidSpirV = -3,
    InvalidValueType = -4,
    NotFound = -5,
    OutOfMemory = -6,
    OutOfBounds = -7,
    ToDo = -8,
    Unreachable = -9,
    UnsupportedSpirV = -10,
    UnsupportedExtension = -11,
    UnsupportedEndianness = -12,
    InvalidMagic = -13,
    Unknown = -14,
};

comptime {
    _ = @import("module.zig");
    _ = @import("runtime.zig");
}
