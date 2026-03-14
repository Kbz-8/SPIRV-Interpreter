const std = @import("std");
const builtin = @import("builtin");
const lib = @import("lib.zig");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const SpvBinding = spv.SpvBinding;

const Result = @import("Result.zig");
const Runtime = @import("Runtime.zig");
const Value = @import("Value.zig").Value;
const WordIterator = @import("WordIterator.zig");

const Self = @This();

pub const ModuleOptions = struct {
    use_simd_vectors_specializations: bool = true,
};

const SpvEntryPoint = struct {
    exec_model: spv.SpvExecutionModel,
    id: SpvWord,
    name: []const u8,
    globals: []SpvWord,
};

pub const ModuleError = error{
    InvalidSpirV,
    InvalidMagic,
    UnsupportedEndianness,
    UnsupportedExtension,
    OutOfMemory,
};

const AllocatorWrapper = struct {
    child_allocator: std.mem.Allocator,
    total_bytes_allocated: usize = 0,

    pub fn allocator(self: *AllocatorWrapper) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, n: usize, alignment: std.mem.Alignment, ra: usize) ?[*]u8 {
        const self: *AllocatorWrapper = @ptrCast(@alignCast(ctx));
        self.total_bytes_allocated += alignment.toByteUnits() + n;
        return self.child_allocator.rawAlloc(n, alignment, ra);
    }

    fn resize(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *AllocatorWrapper = @ptrCast(@alignCast(ctx));
        return self.child_allocator.rawResize(buf, alignment, new_len, ret_addr);
    }

    fn remap(context: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, return_address: usize) ?[*]u8 {
        const self: *AllocatorWrapper = @ptrCast(@alignCast(context));
        return self.child_allocator.rawRemap(memory, alignment, new_len, return_address);
    }

    fn free(ctx: *anyopaque, buf: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *AllocatorWrapper = @ptrCast(@alignCast(ctx));
        return self.child_allocator.rawFree(buf, alignment, ret_addr);
    }
};

options: ModuleOptions,

it: WordIterator,

version_major: SpvByte,
version_minor: SpvByte,

generator_id: u16,
generator_version: u16,

bound: SpvWord,

code: []const SpvWord,

addressing: spv.SpvAddressingModel,
memory_model: spv.SpvMemoryModel,

extensions: std.ArrayList([]const u8),

results: []Result,

entry_points: std.ArrayList(SpvEntryPoint),
capabilities: std.EnumSet(spv.SpvCapability),

local_size_x: SpvWord,
local_size_y: SpvWord,
local_size_z: SpvWord,

geometry_invocations: SpvWord,
geometry_output_count: SpvWord,
geometry_input: SpvWord,
geometry_output: SpvWord,

input_locations: [lib.SPIRV_MAX_INPUT_LOCATIONS]SpvWord,
output_locations: [lib.SPIRV_MAX_OUTPUT_LOCATIONS]SpvWord,
bindings: [lib.SPIRV_MAX_SET][lib.SPIRV_MAX_SET_BINDINGS]SpvWord,
builtins: std.EnumMap(spv.SpvBuiltIn, SpvWord),
push_constants: []Value,

needed_runtime_bytes: usize,

pub fn init(allocator: std.mem.Allocator, source: []const SpvWord, options: ModuleOptions) ModuleError!Self {
    var self: Self = std.mem.zeroInit(Self, .{
        .options = options,
        .code = allocator.dupe(SpvWord, source) catch return ModuleError.OutOfMemory,
        .extensions = std.ArrayList([]const u8).empty,
        .entry_points = std.ArrayList(SpvEntryPoint).empty,
        .capabilities = std.EnumSet(spv.SpvCapability).initEmpty(),
        .local_size_x = 1,
        .local_size_y = 1,
        .local_size_z = 1,
    });
    errdefer allocator.free(self.code);

    op.initRuntimeDispatcher();

    var wrapped_allocator: AllocatorWrapper = .{ .child_allocator = allocator };

    self.it = WordIterator.init(self.code);

    const magic = self.it.next() catch return ModuleError.InvalidSpirV;
    if (magic != spv.SpvMagicNumber) {
        return ModuleError.InvalidMagic;
    }
    if (!checkEndiannessFromSpvMagic(magic)) {
        return ModuleError.UnsupportedEndianness;
    }

    const version = self.it.next() catch return ModuleError.InvalidSpirV;
    self.version_major = @intCast((version & 0x00FF0000) >> 16);
    self.version_minor = @intCast((version & 0x0000FF00) >> 8);

    const generator = self.it.next() catch return ModuleError.InvalidSpirV;
    self.generator_id = @intCast((generator & 0xFFFF0000) >> 16);
    self.generator_version = @intCast(generator & 0x0000FFFF);

    self.bound = self.it.next() catch return ModuleError.InvalidSpirV;
    self.results = wrapped_allocator.allocator().alloc(Result, self.bound) catch return ModuleError.OutOfMemory;
    errdefer allocator.free(self.results);

    for (self.results) |*result| {
        result.* = Result.init();
    }
    errdefer {
        for (self.results) |*result| {
            result.deinit(allocator);
        }
    }

    _ = self.it.skip(); // Skip schema

    try self.pass(allocator); // Setup pass
    try self.populateMaps();

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

    self.needed_runtime_bytes += wrapped_allocator.total_bytes_allocated;

    //@import("pretty").print(allocator, self.results, .{
    //    .tab_size = 4,
    //    .max_depth = 0,
    //    .struct_max_len = 0,
    //    .array_max_len = 0,
    //}) catch return ModuleError.OutOfMemory;

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

fn pass(self: *Self, allocator: std.mem.Allocator) ModuleError!void {
    var rt = Runtime.init(allocator, self) catch return ModuleError.OutOfMemory;
    defer rt.deinit(allocator);

    var wrapped_allocator: AllocatorWrapper = .{ .child_allocator = allocator };

    while (rt.it.nextOrNull()) |opcode_data| {
        const word_count = ((opcode_data & (~spv.SpvOpCodeMask)) >> spv.SpvWordCountShift) - 1;
        const opcode = (opcode_data & spv.SpvOpCodeMask);

        var it_tmp = rt.it; // Save because operations may iter on this iterator
        if (std.enums.fromInt(spv.SpvOp, opcode)) |spv_op| {
            if (op.SetupDispatcher.get(spv_op)) |pfn| {
                pfn(wrapped_allocator.allocator(), word_count, &rt) catch return ModuleError.InvalidSpirV;
            }
        }
        _ = it_tmp.skipN(word_count);
        rt.it = it_tmp;
    }

    self.needed_runtime_bytes += wrapped_allocator.total_bytes_allocated;
}

fn populateMaps(self: *Self) ModuleError!void {
    for (self.results, 0..) |result, id| {
        if (result.variant == null or std.meta.activeTag(result.variant.?) != .Variable)
            continue;

        var set: ?usize = null;
        var binding: ?usize = null;

        for (result.decorations.items) |decoration| {
            switch (result.variant.?.Variable.storage_class) {
                .Input => {
                    switch (decoration.rtype) {
                        .BuiltIn => self.builtins.put(
                            std.enums.fromInt(spv.SpvBuiltIn, decoration.literal_1) orelse return ModuleError.InvalidSpirV,
                            @intCast(id),
                        ),
                        .Location => self.input_locations[decoration.literal_1] = @intCast(id),
                        else => {},
                    }
                },
                .Output => {
                    if (decoration.rtype == .Location)
                        self.output_locations[decoration.literal_1] = @intCast(id);
                },
                .StorageBuffer,
                .Uniform,
                .UniformConstant,
                => {
                    switch (decoration.rtype) {
                        .Binding => binding = decoration.literal_1,
                        .DescriptorSet => set = decoration.literal_1,
                        else => {},
                    }
                },
                else => {},
            }
        }
        if (set != null and binding != null) {
            self.bindings[set.?][binding.?] = @intCast(id);
        }
    }
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    allocator.free(self.code);
    for (self.entry_points.items) |entry| {
        allocator.free(entry.name);
        allocator.free(entry.globals);
    }
    self.entry_points.deinit(allocator);

    for (self.extensions.items) |ext| {
        allocator.free(ext);
    }
    self.extensions.deinit(allocator);

    for (self.results) |*result| {
        result.deinit(allocator);
    }
    allocator.free(self.results);
}
