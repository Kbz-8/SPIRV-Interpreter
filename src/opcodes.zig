const std = @import("std");
const spv = @import("spv.zig");

const Module = @import("Module.zig");
const Runtime = @import("Runtime.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

pub const OpCodeSetupFunc = *const fn (SpvWord, *Module) void;
pub const OpCodeExecFunc = *const fn (SpvWord, *Runtime) void;

pub const SetupDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeSetupFunc).init(.{
        .Capability = OpCapability,
    });
};

fn OpCapability(word_count: SpvWord, mod: *Module) void {
    _ = word_count;
    _ = mod;
    std.debug.print("test\n", .{});
}
