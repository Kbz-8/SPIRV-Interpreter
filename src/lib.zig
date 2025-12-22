const std = @import("std");
pub const spv = @cImport(@cInclude("spirv.h"));

pub const Image = @import("Image.zig");
pub const Interpreter = @import("Interpreter.zig");
pub const Module = @import("Module.zig");
pub const State = @import("State.zig");
const opcode = @import("opcode.zig");

pub const SpvVoid = void;
pub const SpvByte = u8;
pub const SpvWord = u32;
pub const SpvBool = bool;

test {
    std.testing.refAllDecls(Image);
    std.testing.refAllDecls(Interpreter);
    std.testing.refAllDecls(Module);
    std.testing.refAllDecls(State);
    std.testing.refAllDecls(opcode);
}
