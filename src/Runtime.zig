const std = @import("std");
const spv = @import("spv.zig");
const op = @import("opcodes.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Module = @import("Module.zig");
const Result = @import("Result.zig");
const WordIterator = @import("WordIterator.zig");

const Self = @This();

pub const RuntimeError = error{
    InvalidSpirV,
    UnsupportedSpirV,
    OutOfMemory,
    Unreachable,
    Killed,
    InvalidEntryPoint,
    ToDo,
    DivisionByZero,
};

pub const Function = struct {
    source_location: usize,
    result: *Result,
};

mod: *Module,
it: WordIterator,

/// Local deep copy of module's results to be able to run multiple runtimes concurrently
results: []Result,

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
        if (std.enums.fromInt(spv.SpvOp, opcode)) |spv_op| {
            if (op.RuntimeDispatcher.get(spv_op)) |pfn| {
                try pfn(allocator, word_count, self);
            }
        }
        if (!self.it.did_jump) {
            _ = it_tmp.skipN(word_count);
            self.it = it_tmp;
        } else {
            self.it.did_jump = false;
            _ = it_tmp.skip();
        }
    }

    //@import("pretty").print(allocator, self.results, .{
    //    .tab_size = 4,
    //    .max_depth = 0,
    //    .struct_max_len = 0,
    //    .array_max_len = 0,
    //}) catch return RuntimeError.OutOfMemory;
}

pub fn readOutput(self: *const Self, comptime T: type, output: []T, result: SpvWord) error{NotFound}!void {
    if (std.mem.indexOf(SpvWord, self.mod.output_locations.items, &.{result})) |_| {
        self.readValue(T, output, &self.results[result].variant.?.Variable.value);
    } else {
        return error.NotFound;
    }
}

fn reset(self: *Self) void {
    self.function_stack.clearRetainingCapacity();
    self.current_function = null;
}

fn readValue(self: *const Self, comptime T: type, output: []T, value: *const Result.Value) void {
    switch (value.*) {
        .Bool => |b| {
            if (T == bool) {
                output[0] = b;
            } else {
                unreachable;
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
                inline else => unreachable,
            }
        },
        .Float => |f| {
            switch (T) {
                f16 => output[0] = f.float16,
                f32 => output[0] = f.float32,
                f64 => output[0] = f.float64,
                inline else => unreachable,
            }
        },
        .Vector => |values| for (values, 0..) |v, i| self.readValue(T, output[i..], &v),
        .Matrix => |values| for (values, 0..) |v, i| self.readValue(T, output[i..], &v),
        .Array => unreachable, // TODO
        .Structure => |values| for (values, 0..) |v, i| self.readValue(T, output[i..], &v),
        else => unreachable,
    }
}
