//! A runtime meant for actual shader invocations.

const std = @import("std");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");
const lib = @import("lib.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Module = @import("Module.zig");
const Result = @import("Result.zig");
const WordIterator = @import("WordIterator.zig");

const Self = @This();

pub const RuntimeError = error{
    DivisionByZero,
    InvalidEntryPoint,
    InvalidSpirV,
    InvalidValueType,
    Killed,
    NotFound,
    OutOfMemory,
    OutOfBounds,
    ToDo,
    Unreachable,
    UnsupportedSpirV,
    UnsupportedExtension,
};

pub const Function = struct {
    source_location: usize,
    result: *Result,
    ret: *Result,
};

mod: *Module,
it: WordIterator,

/// Local deep copy of module's results to be able to run multiple runtimes concurrently
results: []Result,

current_parameter_index: SpvWord,
current_function: ?*Result,
function_stack: std.ArrayList(Function),

pub fn init(allocator: std.mem.Allocator, module: *Module) RuntimeError!Self {
    return .{
        .mod = module,
        .it = module.it,
        .results = blk: {
            const results = allocator.dupe(Result, module.results) catch return RuntimeError.OutOfMemory;
            for (results, module.results) |*new_result, result| {
                new_result.* = result.dupe(allocator) catch return RuntimeError.OutOfMemory;
            }
            break :blk results;
        },
        .current_parameter_index = 0,
        .current_function = null,
        .function_stack = .empty,
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    for (self.results) |*result| {
        result.deinit(allocator);
    }
    allocator.free(self.results);
    self.function_stack.deinit(allocator);
}

pub fn getEntryPointByName(self: *const Self, name: []const u8) error{NotFound}!SpvWord {
    for (self.mod.entry_points.items, 0..) |entry_point, i| {
        if (blk: {
            // Not using std.mem.eql as entry point names may have longer size than their content
            for (0..@min(name.len, entry_point.name.len)) |j| {
                if (name[j] != entry_point.name[j]) break :blk false;
            }
            break :blk true;
        }) return @intCast(i);
    }
    return error.NotFound;
}

pub fn getResultByName(self: *const Self, name: []const u8) error{NotFound}!SpvWord {
    for (self.results, 0..) |result, i| {
        if (result.name) |result_name| {
            if (blk: {
                // Same as entry points
                for (0..@min(name.len, result_name.len)) |j| {
                    if (name[j] != result_name[j]) break :blk false;
                }
                break :blk true;
            }) return @intCast(i);
        }
    }
    return error.NotFound;
}

/// Calls an entry point, `entry_point_index` being the index of the entry point ordered by declaration in the bytecode
pub fn callEntryPoint(self: *Self, allocator: std.mem.Allocator, entry_point_index: SpvWord) RuntimeError!void {
    self.reset();

    if (entry_point_index > self.mod.entry_points.items.len) return RuntimeError.InvalidEntryPoint;

    {
        const entry_point_desc = &self.mod.entry_points.items[entry_point_index];
        const entry_point_result = &self.mod.results[entry_point_desc.id];
        if (entry_point_result.variant) |variant| {
            switch (variant) {
                .Function => |f| {
                    if (!self.it.jumpToSourceLocation(f.source_location)) return RuntimeError.InvalidEntryPoint;
                    self.function_stack.append(allocator, .{
                        .source_location = f.source_location,
                        .result = entry_point_result,
                        .ret = &self.results[f.return_type],
                    }) catch return RuntimeError.OutOfMemory;
                },
                else => return RuntimeError.InvalidEntryPoint,
            }
        } else {
            return RuntimeError.InvalidEntryPoint;
        }
    }

    self.it.did_jump = false; // To reset function jump
    while (self.it.nextOrNull()) |opcode_data| {
        const word_count = ((opcode_data & (~spv.SpvOpCodeMask)) >> spv.SpvWordCountShift) - 1;
        const opcode = (opcode_data & spv.SpvOpCodeMask);

        var it_tmp = self.it; // Save because operations may iter on this iterator
        if (op.runtime_dispatcher[opcode]) |pfn| {
            try pfn(allocator, word_count, self);
        }
        if (!self.it.did_jump) {
            _ = it_tmp.skipN(word_count);
            self.it = it_tmp;
        } else {
            self.it.did_jump = false;
        }
    }

    //@import("pretty").print(allocator, self.results, .{
    //    .tab_size = 4,
    //    .max_depth = 0,
    //    .struct_max_len = 0,
    //    .array_max_len = 0,
    //}) catch return RuntimeError.OutOfMemory;
}

pub fn readDescriptorSet(self: *const Self, output: []u8, set: SpvWord, binding: SpvWord) RuntimeError!void {
    if (set < lib.SPIRV_MAX_SET and binding < lib.SPIRV_MAX_SET_BINDINGS) {
        _ = try self.readValue(output, &self.results[self.mod.bindings[set][binding]].variant.?.Variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeDescriptorSet(self: *const Self, allocator: std.mem.Allocator, input: []const u8, set: SpvWord, binding: SpvWord) RuntimeError!void {
    if (set < lib.SPIRV_MAX_SET and binding < lib.SPIRV_MAX_SET_BINDINGS) {
        const variable = &self.results[self.mod.bindings[set][binding]].variant.?.Variable;

        const helper = struct {
            fn init(allocator2: std.mem.Allocator, len: usize, value: *Result.Value, type_word: SpvWord, results: []Result) RuntimeError!void {
                const resolved = results[type_word].resolveType(results);

                switch (value.*) {
                    .RuntimeArray => |a| if (a == null) {
                        const elem_size = resolved.variant.?.Type.getSize(results);
                        value.* = try Result.initValue(allocator2, std.math.divCeil(usize, len, elem_size) catch unreachable, results, resolved);
                    },
                    .Structure => |*s| for (s.*, 0..) |*elem, i| {
                        try @This().init(allocator2, len, elem, resolved.variant.?.Type.Structure.members_type_word[i], results);
                    },
                    else => {},
                }
            }
        };
        try helper.init(allocator, input.len, &variable.value, variable.type_word, self.results);

        //@import("pretty").print(allocator, variable, .{
        //    .tab_size = 4,
        //    .max_depth = 0,
        //    .struct_max_len = 0,
        //    .array_max_len = 0,
        //}) catch return RuntimeError.OutOfMemory;
        _ = try self.writeValue(input, &variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn readOutput(self: *const Self, output: []u8, result: SpvWord) RuntimeError!void {
    if (std.mem.indexOfScalar(SpvWord, &self.mod.output_locations, result)) |_| {
        _ = try self.readValue(output, &self.results[result].variant.?.Variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeInput(self: *const Self, input: []const u8, result: SpvWord) RuntimeError!void {
    if (std.mem.indexOfScalar(SpvWord, &self.mod.input_locations, result)) |_| {
        _ = try self.writeValue(input, &self.results[result].variant.?.Variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeBuiltIn(self: *const Self, input: []const u8, builtin: spv.SpvBuiltIn) RuntimeError!void {
    if (self.mod.builtins.get(builtin)) |result| {
        _ = try self.writeValue(input, &self.results[result].variant.?.Variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

fn reset(self: *Self) void {
    self.function_stack.clearRetainingCapacity();
    self.current_function = null;
}

fn readValue(self: *const Self, output: []u8, value: *const Result.Value) RuntimeError!usize {
    switch (value.*) {
        .Bool => |b| {
            output[0] = if (b == true) 1 else 0;
            return 1;
        },
        .Int => |i| {
            switch (i.bit_count) {
                8 => output[0] = @bitCast(i.value.uint8),
                16 => std.mem.copyForwards(u8, output[0..], std.mem.asBytes(&i.value.uint16)),
                32 => std.mem.copyForwards(u8, output[0..], std.mem.asBytes(&i.value.uint32)),
                64 => std.mem.copyForwards(u8, output[0..], std.mem.asBytes(&i.value.uint64)),
                else => return RuntimeError.InvalidValueType,
            }
            return @divExact(i.bit_count, 8);
        },
        .Float => |f| {
            switch (f.bit_count) {
                16 => std.mem.copyForwards(u8, output[0..], std.mem.asBytes(&f.value.float16)),
                32 => std.mem.copyForwards(u8, output[0..], std.mem.asBytes(&f.value.float32)),
                64 => std.mem.copyForwards(u8, output[0..], std.mem.asBytes(&f.value.float64)),
                else => return RuntimeError.InvalidValueType,
            }
            return @divExact(f.bit_count, 8);
        },
        .Vector4f32 => |vec| {
            inline for (0..4) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 4 * 4;
        },
        .Vector3f32 => |vec| {
            inline for (0..3) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 3 * 4;
        },
        .Vector2f32 => |vec| {
            inline for (0..2) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 2 * 4;
        },
        .Vector4i32 => |vec| {
            inline for (0..4) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 4 * 4;
        },
        .Vector3i32 => |vec| {
            inline for (0..3) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 3 * 4;
        },
        .Vector2i32 => |vec| {
            inline for (0..2) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 2 * 4;
        },
        .Vector4u32 => |vec| {
            inline for (0..4) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 4 * 4;
        },
        .Vector3u32 => |vec| {
            inline for (0..3) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 3 * 4;
        },
        .Vector2u32 => |vec| {
            inline for (0..2) |i| {
                std.mem.copyForwards(u8, output[(i * 4)..], std.mem.asBytes(&vec[i]));
            }
            return 2 * 4;
        },
        .Vector,
        .Matrix,
        .Array,
        .Structure,
        => |values| {
            var offset: usize = 0;
            for (values) |v| {
                offset += try self.readValue(output[offset..], &v);
            }
            return offset;
        },
        .RuntimeArray => |opt_values| if (opt_values) |values| {
            var offset: usize = 0;
            for (values) |v| {
                offset += try self.readValue(output[offset..], &v);
            }
            return offset;
        },
        else => return RuntimeError.InvalidValueType,
    }
    return 0;
}

fn writeValue(self: *const Self, input: []const u8, value: *Result.Value) RuntimeError!usize {
    switch (value.*) {
        .Bool => |*b| {
            b.* = if (input[0] != 0) true else false;
            return 1;
        },
        .Int => |*i| {
            switch (i.bit_count) {
                8 => i.value.uint8 = @bitCast(input[0]),
                16 => std.mem.copyForwards(u8, std.mem.asBytes(&i.value.uint16), input[0..2]),
                32 => std.mem.copyForwards(u8, std.mem.asBytes(&i.value.uint32), input[0..4]),
                64 => std.mem.copyForwards(u8, std.mem.asBytes(&i.value.uint64), input[0..8]),
                else => return RuntimeError.InvalidValueType,
            }
            return @divExact(i.bit_count, 8);
        },
        .Float => |*f| {
            switch (f.bit_count) {
                16 => std.mem.copyForwards(u8, std.mem.asBytes(&f.value.float16), input[0..2]),
                32 => std.mem.copyForwards(u8, std.mem.asBytes(&f.value.float32), input[0..4]),
                64 => std.mem.copyForwards(u8, std.mem.asBytes(&f.value.float64), input[0..8]),
                else => return RuntimeError.InvalidValueType,
            }
            return @divExact(f.bit_count, 8);
        },
        .Vector4f32 => |*vec| {
            inline for (0..4) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 4 * 4;
        },
        .Vector3f32 => |*vec| {
            inline for (0..3) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 3 * 4;
        },
        .Vector2f32 => |*vec| {
            inline for (0..2) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 2 * 4;
        },
        .Vector4i32 => |*vec| {
            inline for (0..4) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 4 * 4;
        },
        .Vector3i32 => |*vec| {
            inline for (0..3) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 3 * 4;
        },
        .Vector2i32 => |*vec| {
            inline for (0..2) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 2 * 4;
        },
        .Vector4u32 => |*vec| {
            inline for (0..4) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 4 * 4;
        },
        .Vector3u32 => |*vec| {
            inline for (0..3) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 3 * 4;
        },
        .Vector2u32 => |*vec| {
            inline for (0..2) |i| {
                const start = i * 4;
                const end = (i + 1) * 4;
                std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
            }
            return 2 * 4;
        },
        .Vector,
        .Matrix,
        .Array,
        .Structure,
        => |*values| {
            var offset: usize = 0;
            for (values.*) |*v| {
                offset += try self.writeValue(input[offset..], v);
            }
            return offset;
        },
        .RuntimeArray => |opt_values| if (opt_values) |*values| {
            var offset: usize = 0;
            for (values.*) |*v| {
                offset += try self.writeValue(input[offset..], v);
            }
            return offset;
        },
        else => return RuntimeError.InvalidValueType,
    }
    return 0;
}
