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

pub fn readOutput(self: *const Self, comptime T: type, output: []T, result: SpvWord) RuntimeError!void {
    if (std.mem.indexOfScalar(SpvWord, &self.mod.output_locations, result)) |_| {
        try self.readValue(T, output, &self.results[result].variant.?.Variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeInput(self: *const Self, comptime T: type, input: []const T, result: SpvWord) RuntimeError!void {
    if (std.mem.indexOfScalar(SpvWord, &self.mod.input_locations, result)) |_| {
        try self.writeValue(T, input, &self.results[result].variant.?.Variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn readDescriptorSet(self: *const Self, comptime T: type, output: *T, set: SpvWord, binding: SpvWord) RuntimeError!void {
    if (set < lib.SPIRV_MAX_SET and binding < lib.SPIRV_MAX_SET_BINDINGS) {
        try self.readValue(T, output, &self.results[self.mod.bindings[set][binding]].variant.?.Variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeDescriptorSet(self: *const Self, comptime T: type, allocator: std.mem.Allocator, input: *const T, set: SpvWord, binding: SpvWord) RuntimeError!void {
    if (set < lib.SPIRV_MAX_SET and binding < lib.SPIRV_MAX_SET_BINDINGS) {
        const variable = &self.results[self.mod.bindings[set][binding]].variant.?.Variable;
        switch (variable.value) {
            .RuntimeArray => {
                const resolved = self.results[variable.type_word].resolveType(self.results);
                variable.value = try Result.initValue(allocator, input.len, self.results, resolved);
            },
            .Vector, .Matrix, .Array, .Structure => |v| {
                
            },
        }
        try self.writeValue(T, input, &variable.value);
    } else {
        return RuntimeError.NotFound;
    }
}

fn reset(self: *Self) void {
    self.function_stack.clearRetainingCapacity();
    self.current_function = null;
}

fn readValue(self: *const Self, comptime T: type, output: []T, value: *const Result.Value) RuntimeError!void {
    switch (value.*) {
        .Bool => |b| {
            if (T == bool) {
                output[0] = b;
            } else {
                return RuntimeError.InvalidValueType;
            }
        },
        .Int => |i| {
            switch (T) {
                i8 => output[0] = i.sint8,
                i16 => output[0] = i.sint16,
                i32 => output[0] = i.sint32,
                i64 => output[0] = i.sint64,
                u8 => output[0] = i.uint8,
                u16 => output[0] = i.uint16,
                u32 => output[0] = i.uint32,
                u64 => output[0] = i.uint64,
                inline else => return RuntimeError.InvalidValueType,
            }
        },
        .Float => |f| {
            switch (T) {
                f16 => output[0] = f.float16,
                f32 => output[0] = f.float32,
                f64 => output[0] = f.float64,
                inline else => return RuntimeError.InvalidValueType,
            }
        },
        .Vector4f32 => |vec| inline for (0..4) |i| switch (T) {
            f32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector3f32 => |vec| inline for (0..3) |i| switch (T) {
            f32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector2f32 => |vec| inline for (0..2) |i| switch (T) {
            f32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector4i32 => |vec| inline for (0..4) |i| switch (T) {
            i32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector3i32 => |vec| inline for (0..3) |i| switch (T) {
            i32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector2i32 => |vec| inline for (0..2) |i| switch (T) {
            i32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector4u32 => |vec| inline for (0..4) |i| switch (T) {
            u32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector3u32 => |vec| inline for (0..3) |i| switch (T) {
            u32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector2u32 => |vec| inline for (0..2) |i| switch (T) {
            u32 => output[i] = vec[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Array,
        .Matrix,
        .Structure,
        .Vector,
        => |values| for (values, 0..) |v, i| try self.readValue(T, output[i..], &v),
        .RuntimeArray => |opt_values| if (opt_values) |values| {
            for (values, 0..) |v, i|
                try self.readValue(T, output[i..], &v);
        },
        else => return RuntimeError.InvalidValueType,
    }
}

fn writeValue(self: *const Self, comptime T: type, input: []const T, value: *Result.Value) RuntimeError!void {
    switch (value.*) {
        .Bool => |*b| {
            if (T == bool) {
                b.* = input[0];
            } else {
                return RuntimeError.InvalidValueType;
            }
        },
        .Int => |*i| {
            switch (T) {
                i8 => i.sint8 = input[0],
                i16 => i.sint16 = input[0],
                i32 => i.sint32 = input[0],
                i64 => i.sint64 = input[0],
                u8 => i.uint8 = input[0],
                u16 => i.uint16 = input[0],
                u32 => i.uint32 = input[0],
                u64 => i.uint64 = input[0],
                inline else => return RuntimeError.InvalidValueType,
            }
        },
        .Float => |*f| {
            switch (T) {
                f16 => f.float16 = input[0],
                f32 => f.float32 = input[0],
                f64 => f.float64 = input[0],
                inline else => return RuntimeError.InvalidValueType,
            }
        },
        .Vector4f32 => |*vec| inline for (0..4) |i| switch (T) {
            f32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector3f32 => |*vec| inline for (0..3) |i| switch (T) {
            f32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector2f32 => |*vec| inline for (0..2) |i| switch (T) {
            f32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector4i32 => |*vec| inline for (0..4) |i| switch (T) {
            i32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector3i32 => |*vec| inline for (0..3) |i| switch (T) {
            i32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector2i32 => |*vec| inline for (0..2) |i| switch (T) {
            i32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector4u32 => |*vec| inline for (0..4) |i| switch (T) {
            u32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector3u32 => |*vec| inline for (0..3) |i| switch (T) {
            u32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Vector2u32 => |*vec| inline for (0..2) |i| switch (T) {
            u32 => vec[i] = input[i],
            inline else => return RuntimeError.InvalidValueType,
        },
        .Array,
        .Matrix,
        .Structure,
        .Vector,
        => |*values| for (values.*, 0..) |*v, i| try self.writeValue(T, input[i..], v),
        .RuntimeArray => |opt_values| if (opt_values) |*values| {
            for (values.*, 0..) |*v, i|
                try self.writeValue(T, input[i..], v);
        },
        else => return RuntimeError.InvalidValueType,
    }
}
