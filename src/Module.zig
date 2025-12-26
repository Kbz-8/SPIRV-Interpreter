const std = @import("std");
const builtin = @import("builtin");
const lib = @import("lib.zig");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const SpvMember = spv.SpvMember;
const SpvBinding = spv.SpvBinding;

const Result = @import("Result.zig");
const Runtime = @import("Runtime.zig");
const WordIterator = @import("WordIterator.zig");

const Self = @This();

const SpvEntryPoint = struct {
    exec_model: spv.SpvExecutionModel,
    id: SpvWord,
    name: []const u8,
    globals: []SpvWord,
};

const SpvSource = struct {
    file_name: []const u8,
    lang: spv.SpvSourceLanguage,
    lang_version: SpvWord,
    source: []const u8,
};

const ModuleError = error{
    InvalidSpirV,
    InvalidMagic,
    UnsupportedEndianness,
    OutOfMemory,
};

it: WordIterator,

version_major: SpvByte,
version_minor: SpvByte,

generator_id: u16,
generator_version: u16,

bound: SpvWord,

code: []const SpvWord,

addressing: spv.SpvAddressingModel,
memory_model: spv.SpvMemoryModel,

files: std.ArrayList(SpvSource),
extensions: std.ArrayList([]const u8),

results: std.ArrayList(Result),

entry_points: std.ArrayList(SpvEntryPoint),
capabilities: std.EnumSet(spv.SpvCapability),

local_size_x: SpvWord,
local_size_y: SpvWord,
local_size_z: SpvWord,

geometry_invocations: SpvWord,
geometry_output_count: SpvWord,
geometry_input: SpvWord,
geometry_output: SpvWord,

input_locations: std.AutoHashMap(SpvWord, *SpvMember),
output_locations: std.AutoHashMap(SpvWord, *SpvMember),
bindings: std.AutoHashMap(SpvBinding, *SpvMember),
push_constants: []SpvMember,

pub fn init(allocator: std.mem.Allocator, source: []const SpvWord) ModuleError!Self {
    var self: Self = std.mem.zeroInit(Self, .{
        .code = allocator.dupe(SpvWord, source) catch return ModuleError.OutOfMemory,
        .files = std.ArrayList(SpvSource).empty,
        .extensions = std.ArrayList([]const u8).empty,
        .results = std.ArrayList(Result).empty,
        .entry_points = std.ArrayList(SpvEntryPoint).empty,
        .capabilities = std.EnumSet(spv.SpvCapability).initEmpty(),
        .local_size_x = 1,
        .local_size_y = 1,
        .local_size_z = 1,
        .input_locations = std.AutoHashMap(SpvWord, *SpvMember).init(allocator),
        .output_locations = std.AutoHashMap(SpvWord, *SpvMember).init(allocator),
        .bindings = std.AutoHashMap(SpvBinding, *SpvMember).init(allocator),
    });
    errdefer self.deinit(allocator);

    self.it = WordIterator.init(self.code);

    const magic = self.it.next() orelse return ModuleError.InvalidSpirV;
    if (magic != spv.SpvMagicNumber) {
        return ModuleError.InvalidMagic;
    }
    if (!checkEndiannessFromSpvMagic(magic)) {
        return ModuleError.UnsupportedEndianness;
    }

    const version = self.it.next() orelse return ModuleError.InvalidSpirV;
    self.version_major = @intCast((version & 0x00FF0000) >> 16);
    self.version_minor = @intCast((version & 0x0000FF00) >> 8);

    const generator = self.it.next() orelse return ModuleError.InvalidSpirV;
    self.generator_id = @intCast((generator & 0xFFFF0000) >> 16);
    self.generator_version = @intCast(generator & 0x0000FFFF);

    self.bound = self.it.next() orelse return ModuleError.InvalidSpirV;
    self.results.resize(allocator, self.bound) catch return ModuleError.OutOfMemory;
    for (self.results.items) |*result| {
        result.* = Result.init();
    }

    _ = self.it.skip(); // Skip schema

    const prepassOps = std.EnumSet(spv.SpvOp).initMany(&[_]spv.SpvOp{
        spv.SpvOp.Name,
    });
    try self.pass(allocator, prepassOps); // Pre-pass
    try self.pass(allocator, prepassOps.complement()); // Setup pass

    if (std.process.hasEnvVarConstant("SPIRV_INTERPRETER_DEBUG_LOGS")) {
        var capability_set_names: std.ArrayList([]const u8) = .empty;
        defer capability_set_names.deinit(allocator);

        var it = self.capabilities.iterator();
        while (it.next()) |cap| {
            capability_set_names.append(allocator, @tagName(cap)) catch return ModuleError.OutOfMemory;
        }

        const capabilities = std.mem.join(allocator, ", ", capability_set_names.items) catch return ModuleError.OutOfMemory;
        defer allocator.free(capabilities);

        var entry_points_names = std.ArrayList([]const u8).initCapacity(allocator, self.entry_points.items.len) catch return ModuleError.OutOfMemory;
        defer entry_points_names.deinit(allocator);

        for (self.entry_points.items) |entry_point| {
            entry_points_names.appendAssumeCapacity(entry_point.name);
        }

        const entry_points = std.mem.join(allocator, ", ", entry_points_names.items) catch return ModuleError.OutOfMemory;
        defer allocator.free(entry_points);

        std.log.scoped(.SPIRV_Interpreter).debug(
            \\Loaded shader module with infos:
            \\    SPIR-V version:     {d}.{d}
            \\    Generator:          {s} (ID {d}), encoded version 0x{X}
            \\    Capabilities:       [{s}]
            \\    Entry points:       [{s}]
        , .{
            self.version_major,
            self.version_minor,
            spv.vendorName(self.generator_id),
            self.generator_id,
            self.generator_version,
            capabilities,
            entry_points,
        });
    }

    return self;
}

fn checkEndiannessFromSpvMagic(magic: SpvWord) bool {
    const bytes: [4]u8 = @bitCast(magic);
    if (0x03 == bytes[0] and 0x02 == bytes[1] and 0x23 == bytes[2] and 0x07 == bytes[3]) {
        return builtin.cpu.arch.endian() == .little;
    }
    if (0x07 == bytes[0] and 0x23 == bytes[1] and 0x02 == bytes[2] and 0x03 == bytes[3]) {
        return builtin.cpu.arch.endian() == .big;
    }
    return false;
}

fn pass(self: *Self, allocator: std.mem.Allocator, opcodes: std.EnumSet(spv.SpvOp)) ModuleError!void {
    var rt = Runtime.init(self);
    defer rt.deinit();
    while (rt.it.next()) |opcode_data| {
        const word_count = ((opcode_data & (~spv.SpvOpCodeMask)) >> spv.SpvWordCountShift) - 1;
        const opcode = (opcode_data & spv.SpvOpCodeMask);

        var it_tmp = rt.it; // Save because operations may iter on this iterator
        if (std.enums.fromInt(spv.SpvOp, opcode)) |spv_op| {
            if (opcodes.contains(spv_op)) {
                if (op.SetupDispatcher.get(spv_op)) |pfn| {
                    pfn(allocator, word_count, &rt) catch return ModuleError.InvalidSpirV;
                }
            }
        }
        _ = it_tmp.skipN(word_count);
        rt.it = it_tmp;
    }
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    allocator.free(self.code);
    self.input_locations.deinit();
    self.output_locations.deinit();
    self.bindings.deinit();
    for (self.entry_points.items) |entry| {
        allocator.free(entry.name);
        allocator.free(entry.globals);
    }
    self.entry_points.deinit(allocator);
    self.files.deinit(allocator);

    for (self.extensions.items) |ext| {
        allocator.free(ext);
    }
    self.extensions.deinit(allocator);

    for (self.results.items) |*result| {
        result.deinit(allocator);
    }
    self.results.deinit(allocator);
}
