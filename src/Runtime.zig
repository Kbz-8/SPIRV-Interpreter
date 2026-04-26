//! A runtime meant for actual shader invocations.

const std = @import("std");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");
const lib = @import("lib.zig");
const pretty = @import("pretty");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Image = @import("Image.zig");
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
    Unknown,
};

pub const SpecializationEntry = struct {
    id: SpvWord,
    offset: usize,
    size: usize,
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

current_label: ?SpvWord,
previous_label: ?SpvWord,

specialization_constants: std.AutoHashMapUnmanaged(u32, []const u8),

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
        .current_label = null,
        .previous_label = null,
        .specialization_constants = .empty,
    };
}

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    for (self.results) |*result| {
        result.deinit(allocator);
    }
    allocator.free(self.results);
    self.function_stack.deinit(allocator);
    var it = self.specialization_constants.iterator();
    while (it.next()) |entry| {
        allocator.free(entry.value_ptr.*);
    }
    self.specialization_constants.deinit(allocator);
}

pub fn addSpecializationInfo(self: *Self, allocator: std.mem.Allocator, entry: SpecializationEntry, data: []const u8) RuntimeError!void {
    const slice = allocator.dupe(u8, data[entry.offset .. entry.offset + entry.size]) catch return RuntimeError.OutOfMemory;
    self.specialization_constants.put(allocator, entry.id, slice) catch return RuntimeError.OutOfMemory;
}

pub fn getEntryPointByName(self: *const Self, name: []const u8) RuntimeError!SpvWord {
    for (self.mod.entry_points.items, 0..) |entry_point, i| {
        if (blk: {
            // Not using std.mem.eql as entry point names may have longer size than their content
            for (0..@min(name.len, entry_point.name.len)) |j| {
                if (name[j] != entry_point.name[j])
                    break :blk false;
            }
            if (entry_point.name.len != name.len and entry_point.name[name.len] != 0)
                break :blk false;
            break :blk true;
        }) return @intCast(i);
    }
    return RuntimeError.NotFound;
}

pub fn getResultByName(self: *const Self, name: []const u8) RuntimeError!SpvWord {
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
    return RuntimeError.NotFound;
}

pub fn getResultByLocation(self: *const Self, location: SpvWord, kind: enum { input, output }) RuntimeError!SpvWord {
    switch (kind) {
        .input => if (location < self.mod.input_locations.len) {
            return self.mod.input_locations[location];
        },
        .output => if (location < self.mod.output_locations.len) {
            return self.mod.output_locations[location];
        },
    }
    return RuntimeError.NotFound;
}

pub fn dumpResultsTable(self: *Self, allocator: std.mem.Allocator, writer: *std.Io.Writer) RuntimeError!void {
    const dump = pretty.dump(allocator, self.results, .{
        .tab_size = 4,
        .max_depth = 0,
        .struct_max_len = 0,
        .array_max_len = 0,
    }) catch return RuntimeError.OutOfMemory;
    defer allocator.free(dump);
    writer.print("{s}", .{dump}) catch return RuntimeError.Unknown;
    writer.flush() catch return RuntimeError.Unknown;
}

/// Calls an entry point, `entry_point_index` being the index of the entry point ordered by declaration in the bytecode
pub fn callEntryPoint(self: *Self, allocator: std.mem.Allocator, entry_point_index: SpvWord) RuntimeError!void {
    self.reset();

    if (entry_point_index > self.mod.entry_points.items.len)
        return RuntimeError.InvalidEntryPoint;

    // Spec constants pass
    try self.pass(allocator, .initMany(&.{
        .SpecConstantTrue,
        .SpecConstantFalse,
        .SpecConstantComposite,
        .SpecConstant,
        .SpecConstantOp,
    }));

    {
        const entry_point_desc = &self.mod.entry_points.items[entry_point_index];
        const entry_point_result = &self.mod.results[entry_point_desc.id];
        if (entry_point_result.variant) |variant| {
            switch (variant) {
                .Function => |f| {
                    if (!self.it.jumpToSourceLocation(f.source_location))
                        return RuntimeError.InvalidEntryPoint;
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

    // Execution pass
    try self.pass(allocator, null);
}

fn pass(self: *Self, allocator: std.mem.Allocator, op_set: ?std.EnumSet(spv.SpvOp)) RuntimeError!void {
    self.it.did_jump = false; // To reset function jump
    while (self.it.nextOrNull()) |opcode_data| {
        const word_count = ((opcode_data & (~spv.SpvOpCodeMask)) >> spv.SpvWordCountShift) - 1;
        const opcode = (opcode_data & spv.SpvOpCodeMask);

        if (op_set) |set| {
            @branchHint(.unlikely);
            if (!set.contains(@enumFromInt(opcode))) {
                _ = self.it.skipN(word_count);
                continue;
            }
        }

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
}

pub fn writeDescriptorSet(self: *const Self, input: []const u8, set: SpvWord, binding: SpvWord, descriptor_index: SpvWord) RuntimeError!void {
    if (set < lib.SPIRV_MAX_SET and binding < lib.SPIRV_MAX_SET_BINDINGS) {
        const value = &self.results[self.mod.bindings[set][binding]].variant.?.Variable.value;
        switch (value.*) {
            .Array => |arr| {
                if (descriptor_index >= arr.values.len)
                    return RuntimeError.NotFound;
                _ = try arr.values[descriptor_index].writeConst(input);
            },
            else => {
                if (descriptor_index != 0)
                    return RuntimeError.NotFound;
                _ = try value.writeConst(input);
            },
        }
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn readOutput(self: *const Self, output: []u8, result: SpvWord) RuntimeError!void {
    if (std.mem.indexOfScalar(SpvWord, &self.mod.output_locations, result)) |_| {
        _ = try self.results[result].variant.?.Variable.value.read(output);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeInput(self: *const Self, input: []const u8, result: SpvWord) RuntimeError!void {
    if (std.mem.indexOfScalar(SpvWord, &self.mod.input_locations, result)) |_| {
        _ = try self.results[result].variant.?.Variable.value.writeConst(input);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn writeBuiltIn(self: *const Self, input: []const u8, builtin: spv.SpvBuiltIn) RuntimeError!void {
    if (self.mod.builtins.get(builtin)) |result| {
        _ = try self.results[result].variant.?.Variable.value.writeConst(input);
    } else {
        return RuntimeError.NotFound;
    }
}

pub fn flushDescriptorSets(self: *const Self, allocator: std.mem.Allocator) RuntimeError!void {
    for (self.results) |*result| {
        try result.flushPtr(allocator);
    }
}

fn reset(self: *Self) void {
    self.function_stack.clearRetainingCapacity();
    self.current_function = null;
    self.current_label = null;
    self.previous_label = null;
}
