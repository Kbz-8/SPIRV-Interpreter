const std = @import("std");
const builtin = @import("builtin");
const lib = @import("lib.zig");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Result = @import("Result.zig");
const Runtime = @import("Runtime.zig");
const Value = @import("Value.zig").Value;
const WordIterator = @import("WordIterator.zig");

const Self = @This();

pub const ModuleOptions = struct {
    /// Also affects matrices
    use_simd_vectors_specializations: bool = true,
};

const SpvEntryPoint = struct {
    exec_model: spv.SpvExecutionModel,
    id: SpvWord,
    name: []const u8,
    globals: []SpvWord,
};

const BindingEntry = struct {
    set: SpvWord,
    binding: SpvWord,
    result: SpvWord,
};

pub const ModuleError = error{
    InvalidSpirV,
    InvalidMagic,
    UnsupportedEndianness,
    UnsupportedExtension,
    OutOfMemory,
};

pub const ReflectionInfos = struct {
    local_size_x: SpvWord,
    local_size_y: SpvWord,
    local_size_z: SpvWord,

    geometry_invocations: SpvWord,
    geometry_output_count: SpvWord,
    geometry_input: SpvWord,
    geometry_output: SpvWord,

    needs_derivatives: bool,
    has_control_barriers: bool,
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

input_locations: [lib.SPIRV_MAX_INPUT_LOCATIONS][4]SpvWord,
output_locations: [lib.SPIRV_MAX_OUTPUT_LOCATIONS][4]SpvWord,
bindings: std.ArrayList(BindingEntry),
builtins: std.EnumMap(spv.SpvBuiltIn, SpvWord),

reflection_infos: ReflectionInfos,

pub fn init(allocator: std.mem.Allocator, source: []const SpvWord, options: ModuleOptions) ModuleError!Self {
    var self: Self = std.mem.zeroInit(Self, .{
        .options = options,
        .code = allocator.dupe(SpvWord, source) catch return ModuleError.OutOfMemory,
        .extensions = std.ArrayList([]const u8).empty,
        .entry_points = std.ArrayList(SpvEntryPoint).empty,
        .bindings = std.ArrayList(BindingEntry).empty,
        .capabilities = std.EnumSet(spv.SpvCapability).initEmpty(),
    });
    errdefer allocator.free(self.code);
    errdefer self.bindings.deinit(allocator);

    op.initRuntimeDispatcher();

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
    self.results = allocator.alloc(Result, self.bound) catch return ModuleError.OutOfMemory;
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
    try self.applyDecorations(allocator);

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
    var rt = Runtime.init(allocator, self, undefined) catch return ModuleError.OutOfMemory;

    while (rt.it.nextOrNull()) |opcode_data| {
        const word_count = ((opcode_data & (~spv.SpvOpCodeMask)) >> spv.SpvWordCountShift) - 1;
        const opcode = (opcode_data & spv.SpvOpCodeMask);

        var it_tmp = rt.it; // Save because operations may iter on this iterator
        if (std.enums.fromInt(spv.SpvOp, opcode)) |spv_op| {
            if (op.SetupDispatcher.get(spv_op)) |pfn| {
                pfn(allocator, word_count, &rt) catch return ModuleError.InvalidSpirV;
            }
        }
        _ = it_tmp.skipN(word_count);
        rt.it = it_tmp;
    }

    for (self.results, rt.results) |*result, new_result| {
        result.deinit(allocator);
        result.* = new_result.dupe(allocator) catch return ModuleError.OutOfMemory;
    }
    rt.deinit(allocator);
}

fn resolveConstantWord(self: *const Self, id: SpvWord) ?SpvWord {
    if (id >= self.results.len) return null;

    const variant = self.results[id].variant orelse return null;
    return switch (variant) {
        .Constant => |c| switch (c.value) {
            .Int => |i| i.value.uint32,
            else => null,
        },
        else => null,
    };
}

fn findAccessChainToMember(self: *const Self, base_id: SpvWord, member_index: SpvWord) ?SpvWord {
    for (self.results, 0..) |*result, id| {
        const variant = result.variant orelse continue;

        switch (variant) {
            .AccessChain => |a| {
                if (a.base != base_id or a.indexes.len == 0) continue;

                const first_index = self.resolveConstantWord(a.indexes[0]) orelse continue;
                if (first_index == member_index) return @intCast(id);
            },
            else => {},
        }
    }

    return null;
}

fn resultComponent(self: *const Self, id: SpvWord) SpvWord {
    for (self.results[id].decorations.items) |decoration| {
        if (decoration.rtype == .Component)
            return decoration.literal_1;
    }
    return 0;
}

fn applyInterfaceDecoration(self: *Self, storage_class: spv.SpvStorageClass, decoration: Result.Decoration, id: SpvWord) ModuleError!void {
    switch (storage_class) {
        .Input => switch (decoration.rtype) {
            .BuiltIn => self.builtins.put(
                std.enums.fromInt(spv.SpvBuiltIn, decoration.literal_1) orelse return ModuleError.InvalidSpirV,
                id,
            ),
            .Location => self.input_locations[decoration.literal_1][self.resultComponent(id)] = id,
            else => {},
        },
        .Output => switch (decoration.rtype) {
            .BuiltIn => self.builtins.put(
                std.enums.fromInt(spv.SpvBuiltIn, decoration.literal_1) orelse return ModuleError.InvalidSpirV,
                id,
            ),
            .Location => self.output_locations[decoration.literal_1][self.resultComponent(id)] = id,
            else => {},
        },
        else => {},
    }
}

fn applyStructMemberInterfaceDecorations(self: *Self, storage_class: spv.SpvStorageClass, type_word: SpvWord, id: SpvWord) ModuleError!void {
    switch (storage_class) {
        .Input, .Output => {},
        else => return,
    }

    const type_result = &self.results[type_word];
    const target_type_word = if (type_result.variant) |variant| switch (variant) {
        .Type => |t| switch (t) {
            .Pointer => |ptr| ptr.target,
            else => type_word,
        },
        else => type_word,
    } else type_word;

    const target_result = &self.results[target_type_word];
    if (target_result.variant) |variant| {
        switch (variant) {
            .Type => |t| switch (t) {
                .Structure => {
                    for (target_result.decorations.items) |decoration| {
                        switch (decoration.rtype) {
                            .BuiltIn, .Location => {
                                const member_id = self.findAccessChainToMember(id, decoration.index) orelse continue;
                                try self.applyInterfaceDecoration(storage_class, decoration, member_id);
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            },
            else => {},
        }
    }
}

fn applyDecorations(self: *Self, allocator: std.mem.Allocator) ModuleError!void {
    const helpers = struct {
        fn applyValueLayout(results: []Result, value: *Value, type_word: SpvWord) ModuleError!void {
            const type_variant = results[type_word].variant orelse return;
            const type_data = switch (type_variant) {
                .Type => |type_data| type_data,
                else => return,
            };

            switch (value.*) {
                .Structure => |*structure| switch (type_data) {
                    .Structure => |type_structure| {
                        @memcpy(@constCast(structure.offsets), type_structure.members_offsets);
                        @memcpy(@constCast(structure.matrix_strides), type_structure.members_matrix_strides);

                        for (structure.values, type_structure.members_type_word) |*member_value, member_type_word| {
                            try applyValueLayout(results, member_value, member_type_word);
                        }
                    },
                    else => {},
                },
                .Array => |array| switch (type_data) {
                    .Array => |type_array| {
                        for (array.values) |*element| {
                            try applyValueLayout(results, element, type_array.components_type_word);
                        }
                    },
                    else => {},
                },
                .Matrix => |columns| switch (type_data) {
                    .Matrix => |type_matrix| {
                        for (columns) |*column| {
                            try applyValueLayout(results, column, type_matrix.column_type_word);
                        }
                    },
                    else => {},
                },
                .Vector => |elements| switch (type_data) {
                    .Vector => |type_vector| {
                        for (elements) |*element| {
                            try applyValueLayout(results, element, type_vector.components_type_word);
                        }
                    },
                    else => {},
                },
                else => {},
            }
        }
    };

    for (self.results, 0..) |*result, id| {
        if (result.variant == null)
            continue;

        var set: ?SpvWord = null;
        var binding: ?SpvWord = null;

        for (result.decorations.items) |decoration| {
            if (result.variant) |*variant| switch (variant.*) {
                .Variable => |v| {
                    try self.applyInterfaceDecoration(v.storage_class, decoration, @intCast(id));

                    switch (v.storage_class) {
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
                },
                .Type => |t| {
                    switch (t) {
                        .Structure => |*s| {
                            switch (decoration.rtype) {
                                .Offset => s.members_offsets[decoration.index] = decoration.literal_1,
                                .MatrixStride => s.members_matrix_strides[decoration.index] = decoration.literal_1,
                                else => {},
                            }
                        },
                        else => {},
                    }
                },
                else => {},
            };
        }

        if (result.variant) |*variant| switch (variant.*) {
            .Variable => |*v| {
                try self.applyStructMemberInterfaceDecorations(v.storage_class, v.type_word, @intCast(id));
                switch (v.storage_class) {
                    .StorageBuffer,
                    .Uniform,
                    .PushConstant,
                    => try helpers.applyValueLayout(self.results, &v.value, v.type_word),
                    else => {},
                }
            },
            else => {},
        };

        if (set != null and binding != null) {
            self.bindings.append(allocator, .{
                .set = set.?,
                .binding = binding.?,
                .result = @intCast(id),
            }) catch return ModuleError.OutOfMemory;
        }
    }
}

pub fn getBindingResult(self: *const Self, set: SpvWord, binding: SpvWord) ?SpvWord {
    for (self.bindings.items) |entry| {
        if (entry.set == set and entry.binding == binding) {
            return entry.result;
        }
    }
    return null;
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
    self.bindings.deinit(allocator);

    for (self.results) |*result| {
        result.deinit(allocator);
    }
    allocator.free(self.results);
}
