const std = @import("std");
const spv = @import("spv.zig");

const Module = @import("Module.zig");
const Runtime = @import("Runtime.zig");
const Result = @import("Result.zig");
const WordIterator = @import("WordIterator.zig");

const RuntimeError = Runtime.RuntimeError;

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

// DUMB INDEV OPCODES TODO :
//     OpDecorate           X
//     OpMemberDecorate     X
//     OpTypeVoid           X
//     OpTypeFunction       X
//     OpTypeFloat          X
//     OpTypeVector         X
//     OpTypePointer        X
//     OpTypeStruct         X
//     OpTypeInt            X
//     OpConstant           X
//     OpVariable           X
//     OpFunction           X
//     OpLabel              X
//     OpCompositeConstruct X
//     OpAccessChain        X
//     OpStore              X
//     OpLoad               X
//     OpCompositeExtract   X
//     OpReturn             X
//     OpFunctionEnd        X

pub const OpCodeFunc = *const fn (std.mem.Allocator, SpvWord, *Runtime) RuntimeError!void;

pub const SetupDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeFunc).init(.{
        .Capability = opCapability,
        .EntryPoint = opEntryPoint,
        .ExecutionMode = opExecutionMode,
        .MemoryModel = opMemoryModel,
        .MemberName = opMemberName,
        .Name = opName,
        .Source = opSource,
        .SourceExtension = opSourceExtension,
    });
};

fn opCapability(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    if (rt.it.nextAs(spv.SpvCapability)) |capability| {
        rt.mod.capabilities.insert(capability);
    }
}

fn opEntryPoint(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const entry = rt.mod.entry_points.addOne(allocator) catch return RuntimeError.OutOfMemory;
    entry.exec_model = rt.it.nextAs(spv.SpvExecutionModel) orelse return RuntimeError.InvalidSpirV;
    entry.id = rt.it.next() orelse return RuntimeError.InvalidSpirV;
    entry.name = try readString(allocator, &rt.it);

    var interface_count = word_count - @divExact(entry.name.len, 4) - 2;
    entry.globals = try allocator.alloc(SpvWord, interface_count);
    if (interface_count != 0) {
        var interface_index: u32 = 0;
        while (interface_count != 0) {
            entry.globals[interface_index] = rt.it.next() orelse return RuntimeError.InvalidSpirV;
            interface_index += 1;
            interface_count -= 1;
        }
    }
}

fn opExecutionMode(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const mode = rt.it.nextAs(spv.SpvExecutionMode) orelse return RuntimeError.InvalidSpirV;

    switch (mode) {
        .LocalSize => {
            rt.mod.local_size_x = rt.it.next() orelse return RuntimeError.InvalidSpirV;
            rt.mod.local_size_y = rt.it.next() orelse return RuntimeError.InvalidSpirV;
            rt.mod.local_size_z = rt.it.next() orelse return RuntimeError.InvalidSpirV;
        },
        .Invocations => rt.mod.geometry_invocations = rt.it.next() orelse return RuntimeError.InvalidSpirV,
        .OutputVertices => rt.mod.geometry_output_count = rt.it.next() orelse return RuntimeError.InvalidSpirV,
        .InputPoints, .InputLines, .Triangles, .InputLinesAdjacency, .InputTrianglesAdjacency => rt.mod.geometry_input = @intFromEnum(mode),
        .OutputPoints, .OutputLineStrip, .OutputTriangleStrip => rt.mod.geometry_output = @intFromEnum(mode),
        else => {},
    }
}

fn opMemberName(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = rt.it.next() orelse return RuntimeError.InvalidSpirV;
    const memb = rt.it.next() orelse return RuntimeError.InvalidSpirV;

    var result = &rt.mod.results.items[id];

    if (memb + 1 > result.member_names.items.len) {
        _ = result.member_names.resize(allocator, memb + 1) catch return RuntimeError.OutOfMemory;
    }

    const slen = word_count - 2;
    result.member_names.items[memb] = try readStringN(allocator, &rt.it, slen);
}

fn opMemoryModel(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.addressing = rt.it.nextAs(spv.SpvAddressingModel) orelse return RuntimeError.InvalidSpirV;
    rt.mod.memory_model = rt.it.nextAs(spv.SpvMemoryModel) orelse return RuntimeError.InvalidSpirV;
}

fn opName(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = rt.it.next() orelse return RuntimeError.InvalidSpirV;
    if (id >= rt.mod.results.items.len) return RuntimeError.InvalidSpirV;
    var result = &rt.mod.results.items[id];
    result.* = Result.init();
    result.name = try readStringN(allocator, &rt.it, word_count - 1);
}

fn opSource(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    var file = rt.mod.files.addOne(allocator) catch return RuntimeError.OutOfMemory;
    file.lang = rt.it.nextAs(spv.SpvSourceLanguage) orelse return RuntimeError.InvalidSpirV;
    file.lang_version = rt.it.next() orelse return RuntimeError.InvalidSpirV;
    if (word_count > 2) {
        const id = rt.it.next() orelse return RuntimeError.InvalidSpirV;
        if (id >= rt.mod.results.items.len) return RuntimeError.InvalidSpirV;
        if (rt.mod.results.items[id].name) |name| {
            file.file_name = name;
        }
    }
    if (word_count > 3) {
        const id = rt.it.next() orelse return RuntimeError.InvalidSpirV;
        if (id >= rt.mod.results.items.len) return RuntimeError.InvalidSpirV;
        if (rt.mod.results.items[id].name) |name| {
            file.source = name;
        }
    }
}

fn opSourceExtension(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.extensions.append(allocator, try readStringN(allocator, &rt.it, word_count)) catch return RuntimeError.OutOfMemory;
}

fn readString(allocator: std.mem.Allocator, it: *WordIterator) RuntimeError![]const u8 {
    var str: std.ArrayList(u8) = .empty;
    while (it.next()) |word| {
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate(word & 0x000000FF);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0x0000FF00) >> 8);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0x00FF0000) >> 16);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0xFF000000) >> 24);
        if (str.getLast() == 0) {
            break;
        }
    }
    return str.toOwnedSlice(allocator);
}

fn readStringN(allocator: std.mem.Allocator, it: *WordIterator, n: usize) RuntimeError![]const u8 {
    var str = std.ArrayList(u8).initCapacity(allocator, n * 4) catch return RuntimeError.OutOfMemory;
    for (0..n) |_| {
        if (it.next()) |word| {
            str.addOneAssumeCapacity().* = @truncate(word & 0x000000FF);
            str.addOneAssumeCapacity().* = @truncate((word & 0x0000FF00) >> 8);
            str.addOneAssumeCapacity().* = @truncate((word & 0x00FF0000) >> 16);
            str.addOneAssumeCapacity().* = @truncate((word & 0xFF000000) >> 24);
            if (str.getLast() == 0) {
                break;
            }
        }
    }
    return str.toOwnedSlice(allocator);
}
