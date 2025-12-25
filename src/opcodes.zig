const std = @import("std");
const spv = @import("spv.zig");

const Module = @import("Module.zig");
const Runtime = @import("Runtime.zig");
const WordIterator = @import("WordIterator.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

pub const OpCodeSetupFunc = *const fn (std.mem.Allocator, SpvWord, *Module) anyerror!void;
pub const OpCodeExecFunc = *const fn (std.mem.Allocator, SpvWord, *Runtime) anyerror!void;

pub const SetupDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeSetupFunc).init(.{
        .Capability = opCapability,
        .EntryPoint = opEntryPoint,
    });
};

fn opCapability(_: std.mem.Allocator, _: SpvWord, mod: *Module) !void {
    if (std.enums.fromInt(spv.SpvCapability, mod.it.next() orelse return)) |capability| {
        mod.capabilities.insert(capability);
    }
}

fn opEntryPoint(allocator: std.mem.Allocator, word_count: SpvWord, mod: *Module) !void {
    const entry = try mod.entry_points.addOne(allocator);
    entry.exec_model = std.enums.fromInt(spv.SpvExecutionModel, mod.it.next() orelse return) orelse return;
    entry.id = mod.it.next() orelse return;
    entry.name = try readString(allocator, &mod.it);

    var interface_count = word_count - @divExact(entry.name.len, 4) - 2;
    if (interface_count != 0) {
        entry.globals = try allocator.alloc(SpvWord, interface_count);
        var interface_index: u32 = 0;
        while (interface_count != 0) {
            entry.globals[interface_index] = mod.it.next() orelse return;
            interface_index += 1;
            interface_count -= 1;
        }
    }
}

fn readString(allocator: std.mem.Allocator, it: *WordIterator) ![]const u8 {
    var str: std.ArrayList(u8) = .empty;
    while (it.next()) |word| {
        (try str.addOne(allocator)).* = @truncate(word & 0x000000FF);
        (try str.addOne(allocator)).* = @truncate((word & 0x0000FF00) >> 8);
        (try str.addOne(allocator)).* = @truncate((word & 0x00FF0000) >> 16);
        (try str.addOne(allocator)).* = @truncate((word & 0xFF000000) >> 24);
        if (str.getLast() == 0) {
            break;
        }
    }
    return str.toOwnedSlice(allocator);
}
