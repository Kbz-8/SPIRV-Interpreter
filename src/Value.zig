const std = @import("std");
const lib = @import("lib.zig");

const Result = @import("Result.zig");
const Runtime = @import("Runtime.zig");
const RuntimeError = Runtime.RuntimeError;

const SpvVoid = lib.SpvVoid;
const SpvByte = lib.SpvByte;
const SpvWord = lib.SpvWord;
const SpvBool = lib.SpvBool;

const Vec4f32 = lib.Vec4f32;
const Vec3f32 = lib.Vec3f32;
const Vec2f32 = lib.Vec2f32;

const Vec4i32 = lib.Vec4i32;
const Vec3i32 = lib.Vec3i32;
const Vec2i32 = lib.Vec2i32;

const Vec4u32 = lib.Vec4u32;
const Vec3u32 = lib.Vec3u32;
const Vec2u32 = lib.Vec2u32;

const Type = Result.Type;

pub const Value = union(Type) {
    const Self = @This();

    Void: struct {},
    Bool: bool,
    Int: struct {
        bit_count: usize,
        value: extern union {
            sint8: i8,
            sint16: i16,
            sint32: i32,
            sint64: i64,
            uint8: u8,
            uint16: u16,
            uint32: u32,
            uint64: u64,
        },
    },
    Float: struct {
        bit_count: usize,
        value: extern union {
            float16: f16,
            float32: f32,
            float64: f64,
        },
    },
    Vector: []Self,
    Vector4f32: Vec4f32,
    Vector3f32: Vec3f32,
    Vector2f32: Vec2f32,
    Vector4i32: Vec4i32,
    Vector3i32: Vec3i32,
    Vector2i32: Vec2i32,
    Vector4u32: Vec4u32,
    Vector3u32: Vec3u32,
    Vector2u32: Vec2u32,
    Matrix: []Self,
    Array: []Self,
    RuntimeArray: struct {
        type_word: SpvWord,
        data: []u8,
    },
    Structure: []Self,
    Function: noreturn,
    Image: struct {},
    Sampler: struct {},
    SampledImage: struct {},
    Pointer: struct {
        ptr: union(enum) {
            common: *Self,
            f32_ptr: *f32,
            i32_ptr: *i32, //< For vector specializations
            u32_ptr: *u32,
        },
        runtime_array_window: ?[]u8 = null,
    },

    pub inline fn getCompositeDataOrNull(self: *const Self) ?[]Self {
        return switch (self.*) {
            .Vector, .Matrix, .Array, .Structure => |v| v,
            else => null,
        };
    }

    pub fn init(allocator: std.mem.Allocator, results: []const Result, target: SpvWord) RuntimeError!Self {
        const resolved = results[target].resolveType(results);
        const member_count = resolved.getMemberCounts();

        return switch (resolved.variant.?) {
            .Type => |t| switch (t) {
                .Bool => .{ .Bool = false },
                .Int => |i| .{ .Int = .{
                    .bit_count = i.bit_length,
                    .value = .{ .uint64 = 0 },
                } },
                .Float => |f| .{ .Float = .{
                    .bit_count = f.bit_length,
                    .value = .{ .float64 = 0 },
                } },
                .Vector => |v| blk: {
                    var self: Self = .{ .Vector = allocator.alloc(Self, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer self.deinit(allocator);

                    for (self.Vector) |*value| {
                        value.* = try Self.init(allocator, results, v.components_type_word);
                    }
                    break :blk self;
                },
                .Vector4f32 => .{ .Vector4f32 = Vec4f32{ 0.0, 0.0, 0.0, 0.0 } },
                .Vector3f32 => .{ .Vector3f32 = Vec3f32{ 0.0, 0.0, 0.0 } },
                .Vector2f32 => .{ .Vector2f32 = Vec2f32{ 0.0, 0.0 } },
                .Vector4i32 => .{ .Vector4i32 = Vec4i32{ 0, 0, 0, 0 } },
                .Vector3i32 => .{ .Vector3i32 = Vec3i32{ 0, 0, 0 } },
                .Vector2i32 => .{ .Vector2i32 = Vec2i32{ 0, 0 } },
                .Vector4u32 => .{ .Vector4u32 = Vec4u32{ 0, 0, 0, 0 } },
                .Vector3u32 => .{ .Vector3u32 = Vec3u32{ 0, 0, 0 } },
                .Vector2u32 => .{ .Vector2u32 = Vec2u32{ 0, 0 } },
                .Matrix => |m| blk: {
                    var self: Self = .{ .Matrix = allocator.alloc(Self, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer self.deinit(allocator);

                    for (self.Matrix) |*value| {
                        value.* = try Self.init(allocator, results, m.column_type_word);
                    }
                    break :blk self;
                },
                .Array => |a| blk: {
                    var self: Self = .{ .Array = allocator.alloc(Self, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer self.deinit(allocator);

                    for (self.Array) |*value| {
                        value.* = try Self.init(allocator, results, a.components_type_word);
                    }
                    break :blk self;
                },
                .Structure => |s| blk: {
                    var self: Self = .{ .Structure = allocator.alloc(Self, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer self.deinit(allocator);

                    for (self.Structure, s.members_type_word) |*value, member_type_word| {
                        value.* = try Self.init(allocator, results, member_type_word);
                    }
                    break :blk self;
                },
                .RuntimeArray => |a| .{
                    .RuntimeArray = .{
                        .type_word = a.components_type_word,
                        .data = &.{},
                    },
                },
                else => unreachable,
            },
            else => unreachable,
        };
    }

    /// Performs a deep copy
    pub fn dupe(self: *const Self, allocator: std.mem.Allocator) RuntimeError!Self {
        return switch (self.*) {
            .Vector => |v| .{
                .Vector = blk: {
                    const values = allocator.dupe(Self, v) catch return RuntimeError.OutOfMemory;
                    for (values, v) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            .Matrix => |m| .{
                .Matrix = blk: {
                    const values = allocator.dupe(Self, m) catch return RuntimeError.OutOfMemory;
                    for (values, m) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            .Array => |a| .{
                .Array = blk: {
                    const values = allocator.dupe(Self, a) catch return RuntimeError.OutOfMemory;
                    for (values, a) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            .Structure => |s| .{
                .Structure = blk: {
                    const values = allocator.dupe(Self, s) catch return RuntimeError.OutOfMemory;
                    for (values, s) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            else => self.*,
        };
    }

    pub fn read(self: *const Self, output: []u8) RuntimeError!usize {
        switch (self.*) {
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
                    offset += try v.read(output[offset..]);
                }
                return offset;
            },
            else => return RuntimeError.InvalidValueType,
        }
        return 0;
    }

    pub fn writeConst(self: *Self, input: []const u8) RuntimeError!usize {
        return self.write(@constCast(input));
    }

    pub fn write(self: *Self, input: []u8) RuntimeError!usize {
        switch (self.*) {
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
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 4 * 4;
            },
            .Vector3f32 => |*vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 3 * 4;
            },
            .Vector2f32 => |*vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 2 * 4;
            },
            .Vector4i32 => |*vec| {
                inline for (0..4) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 4 * 4;
            },
            .Vector3i32 => |*vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 3 * 4;
            },
            .Vector2i32 => |*vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 2 * 4;
            },
            .Vector4u32 => |*vec| {
                inline for (0..4) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 4 * 4;
            },
            .Vector3u32 => |*vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    std.mem.copyForwards(u8, std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 3 * 4;
            },
            .Vector2u32 => |*vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
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
                    offset += try v.write(input[offset..]);
                }
                return offset;
            },
            .RuntimeArray => |*arr| arr.data = input[0..],
            else => return RuntimeError.InvalidValueType,
        }
        return 0;
    }

    pub fn flushPtr(self: *Self, allocator: std.mem.Allocator) RuntimeError!void {
        switch (self.*) {
            .Pointer => |*p| {
                if (p.runtime_array_window) |window| {
                    switch (p.ptr) {
                        .common => |ptr| {
                            _ = try ptr.read(window);
                            ptr.deinit(allocator);
                            allocator.destroy(ptr);
                        },
                        else => {},
                    }
                }
                p.runtime_array_window = null;
            },
            else => {},
        }
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Vector, .Matrix, .Array, .Structure => |values| {
                for (values) |*value| value.deinit(allocator);
                allocator.free(values);
            },
            else => {},
        }
    }
};
