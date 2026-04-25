pub const spv = @import("spv");

pub const SpvCBool = c_int;
pub const SpvCWord = c_ulong;
pub const SpvCSize = c_ulong;

pub const Result = enum(c_int) {
    Success = 0,
    DivisionByZero = -1,
    InvalidEntryPoint = -2,
    InvalidSpirV = -3,
    InvalidValueType = -4,
    Killed = -5,
    NotFound = -6,
    OutOfMemory = -7,
    OutOfBounds = -8,
    ToDo = -9,
    Unreachable = -10,
    UnsupportedSpirV = -11,
    UnsupportedExtension = -12,
    UnsupportedEndianness = -13,
    InvalidMagic = -14,
    Unknown = -15,
};

comptime {
    _ = @import("module.zig");
    _ = @import("runtime.zig");
}
