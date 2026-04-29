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

pub const PrimitiveType = enum {
    Bool,
    Float,
    SInt,
    UInt,
};

pub const Value = union(Type) {
    const Self = @This();

    Void: struct {},
    Bool: bool,
    Int: struct {
        bit_count: usize,
        is_signed: bool,
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
    Array: struct {
        stride: SpvWord,
        values: []Self,
    },
    RuntimeArray: struct {
        type_word: SpvWord,
        stride: SpvWord,
        data: []u8,

        pub inline fn createValueFromIndex(self: *const @This(), allocator: std.mem.Allocator, results: []const Result, index: usize) RuntimeError!*Value {
            const value = allocator.create(Value) catch return RuntimeError.OutOfMemory;
            errdefer allocator.destroy(value);

            value.* = try Value.init(allocator, results, self.type_word, false);
            _ = try value.writeConst(self.data[self.getOffsetOfIndex(index)..]);

            return value;
        }

        pub inline fn createLocalValueFromIndex(self: *const @This(), allocator: std.mem.Allocator, results: []const Result, index: usize) RuntimeError!Value {
            var value = try Value.init(allocator, results, self.type_word, false);
            _ = try value.writeConst(self.data[self.getOffsetOfIndex(index)..]);
            return value;
        }

        pub inline fn getOffsetOfIndex(self: *const @This(), index: usize) usize {
            return self.stride * index;
        }

        pub inline fn getLen(self: *const @This()) usize {
            return @divTrunc(self.data.len, self.stride);
        }
    },
    Structure: struct {
        offsets: []const ?SpvWord,
        values: []Self,
    },
    Function: noreturn,
    Image: struct {
        type_word: SpvWord,
        driver_image: *anyopaque,
    },
    Sampler: struct {},
    SampledImage: struct {},
    Pointer: struct {
        ptr: union(enum) {
            common: *Self,
            f32_ptr: *f32,
            i32_ptr: *i32, //< For vector specializations
            u32_ptr: *u32,
        },

        /// Exact byte window in externally visible descriptor storage that
        /// corresponds to this pointer. For a pointer to struct member N this
        /// starts at the member offset, not at the containing struct.
        uniform_slice_window: ?[]u8 = null,

        /// Heap-owned value that backs a pointer into a materialized runtime
        /// array element. This may differ from ptr.common when the pointer is
        /// to a child/member of that materialized value.
        uniform_backing_value: ?*Self = null,
    },

    pub inline fn getCompositeDataOrNull(self: *const Self) ?[]Self {
        return switch (self.*) {
            .Structure => |*s| s.values,
            .Array => |*a| a.values,
            .Vector, .Matrix => |v| v,
            else => null,
        };
    }

    pub fn init(allocator: std.mem.Allocator, results: []const Result, target_type: SpvWord, is_externally_visible: bool) RuntimeError!Self {
        const resolved = results[target_type].resolveTypeWordOrNull() orelse target_type;
        const member_count = results[resolved].getMemberCounts();

        return switch (results[resolved].variant.?) {
            .Type => |t| switch (t) {
                .Void => .{ .Void = .{} },
                .Bool => .{ .Bool = false },
                .Int => |i| .{ .Int = .{
                    .bit_count = i.bit_length,
                    .is_signed = i.is_signed,
                    .value = .{ .uint64 = 0 },
                } },
                .Float => |f| .{ .Float = .{
                    .bit_count = f.bit_length,
                    .value = .{ .float64 = 0 },
                } },
                .Vector => |v| blk: {
                    const self: Self = .{ .Vector = allocator.alloc(Self, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer allocator.free(self.Vector);

                    for (self.Vector) |*value| {
                        value.* = try Self.init(allocator, results, v.components_type_word, is_externally_visible);
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
                    const self: Self = .{ .Matrix = allocator.alloc(Self, member_count) catch return RuntimeError.OutOfMemory };
                    errdefer allocator.free(self.Matrix);

                    for (self.Matrix) |*value| {
                        value.* = try Self.init(allocator, results, m.column_type_word, is_externally_visible);
                    }
                    break :blk self;
                },
                .Array => |a| blk: {
                    // If an array is in externally visible storage we treat it as a runtime array
                    if (is_externally_visible) {
                        break :blk .{
                            .RuntimeArray = .{
                                .type_word = a.components_type_word,
                                .stride = a.stride,
                                .data = &.{},
                            },
                        };
                    }

                    const self: Self = .{
                        .Array = .{
                            .stride = a.stride,
                            .values = allocator.alloc(Self, member_count) catch return RuntimeError.OutOfMemory,
                        },
                    };
                    errdefer allocator.free(self.Array.values);

                    for (self.Array.values) |*value| {
                        value.* = try Self.init(allocator, results, a.components_type_word, is_externally_visible);
                    }
                    break :blk self;
                },
                .Structure => |s| blk: {
                    const self: Self = .{
                        .Structure = .{
                            .offsets = allocator.dupe(?SpvWord, s.members_offsets) catch return RuntimeError.OutOfMemory,
                            .values = allocator.alloc(Self, member_count) catch return RuntimeError.OutOfMemory,
                        },
                    };
                    errdefer allocator.free(self.Structure.values);

                    for (self.Structure.values, s.members_type_word) |*value, type_word| {
                        value.* = try Self.init(allocator, results, type_word, is_externally_visible);
                    }
                    break :blk self;
                },
                .RuntimeArray => |a| .{
                    .RuntimeArray = .{
                        .type_word = a.components_type_word,
                        .stride = a.stride,
                        .data = &.{},
                    },
                },
                .Image => .{
                    .Image = .{
                        .type_word = resolved,
                        .driver_image = undefined,
                    },
                },
                .Sampler => RuntimeError.ToDo,
                .SampledImage => RuntimeError.ToDo,
                else => RuntimeError.InvalidSpirV,
            },
            else => RuntimeError.InvalidSpirV,
        };
    }

    /// Performs a deep copy
    pub fn dupe(self: *const Self, allocator: std.mem.Allocator) RuntimeError!Self {
        return switch (self.*) {
            .Vector => |v| .{
                .Vector = blk: {
                    const values = allocator.dupe(Self, v) catch return RuntimeError.OutOfMemory;
                    errdefer allocator.free(values);
                    for (values, v) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            .Matrix => |m| .{
                .Matrix = blk: {
                    const values = allocator.dupe(Self, m) catch return RuntimeError.OutOfMemory;
                    errdefer allocator.free(values);
                    for (values, m) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk values;
                },
            },
            .Array => |*a| .{
                .Array = blk: {
                    const values = allocator.dupe(Self, a.values) catch return RuntimeError.OutOfMemory;
                    errdefer allocator.free(values);
                    for (values, a.values) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk .{
                        .stride = a.stride,
                        .values = values,
                    };
                },
            },
            .Structure => |*s| .{
                .Structure = blk: {
                    const values = allocator.dupe(Self, s.values) catch return RuntimeError.OutOfMemory;
                    errdefer allocator.free(values);
                    for (values, s.values) |*new_value, value| new_value.* = try value.dupe(allocator);
                    break :blk .{
                        .offsets = allocator.dupe(?SpvWord, s.offsets) catch return RuntimeError.OutOfMemory,
                        .values = values,
                    };
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
                    16 => @memcpy(output[0..2], std.mem.asBytes(&i.value.uint16)),
                    32 => @memcpy(output[0..4], std.mem.asBytes(&i.value.uint32)),
                    64 => @memcpy(output[0..8], std.mem.asBytes(&i.value.uint64)),
                    else => return RuntimeError.InvalidValueType,
                }
                return @divExact(i.bit_count, 8);
            },
            .Float => |f| {
                switch (f.bit_count) {
                    16 => @memcpy(output[0..2], std.mem.asBytes(&f.value.float16)),
                    32 => @memcpy(output[0..4], std.mem.asBytes(&f.value.float32)),
                    64 => @memcpy(output[0..8], std.mem.asBytes(&f.value.float64)),
                    else => return RuntimeError.InvalidValueType,
                }
                return @divExact(f.bit_count, 8);
            },
            .Vector4f32 => |vec| {
                inline for (0..4) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 4 * 4;
            },
            .Vector3f32 => |vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 3 * 4;
            },
            .Vector2f32 => |vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 2 * 4;
            },
            .Vector4i32 => |vec| {
                inline for (0..4) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 4 * 4;
            },
            .Vector3i32 => |vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 3 * 4;
            },
            .Vector2i32 => |vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 2 * 4;
            },
            .Vector4u32 => |vec| {
                inline for (0..4) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 4 * 4;
            },
            .Vector3u32 => |vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 3 * 4;
            },
            .Vector2u32 => |vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= output.len or end > output.len) return RuntimeError.OutOfBounds;
                    @memcpy(output[start..end], std.mem.asBytes(&vec[i]));
                }
                return 2 * 4;
            },
            .Vector, .Matrix => |values| {
                var offset: usize = 0;
                for (values) |v| {
                    offset += try v.read(output[offset..]);
                }
                return offset;
            },
            .Array => |arr| {
                var offset: usize = 0;
                for (arr.values) |v| {
                    _ = try v.read(output[offset..]);
                    offset += arr.stride;
                }
                return offset;
            },
            .Structure => |s| {
                var end_offset: usize = 0;
                for (s.values, 0..) |v, i| {
                    const member_offset: usize = @intCast(s.offsets[i] orelse end_offset);
                    const read_size = try v.read(output[member_offset..]);
                    end_offset = @max(end_offset, member_offset + read_size);
                }
                return end_offset;
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
                    16 => @memcpy(std.mem.asBytes(&i.value.uint16), input[0..2]),
                    32 => @memcpy(std.mem.asBytes(&i.value.uint32), input[0..4]),
                    64 => @memcpy(std.mem.asBytes(&i.value.uint64), input[0..8]),
                    else => return RuntimeError.InvalidValueType,
                }
                return @divExact(i.bit_count, 8);
            },
            .Float => |*f| {
                switch (f.bit_count) {
                    16 => @memcpy(std.mem.asBytes(&f.value.float16), input[0..2]),
                    32 => @memcpy(std.mem.asBytes(&f.value.float32), input[0..4]),
                    64 => @memcpy(std.mem.asBytes(&f.value.float64), input[0..8]),
                    else => return RuntimeError.InvalidValueType,
                }
                return @divExact(f.bit_count, 8);
            },
            .Vector4f32 => |*vec| {
                inline for (0..4) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 4 * 4;
            },
            .Vector3f32 => |*vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 3 * 4;
            },
            .Vector2f32 => |*vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 2 * 4;
            },
            .Vector4i32 => |*vec| {
                inline for (0..4) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 4 * 4;
            },
            .Vector3i32 => |*vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 3 * 4;
            },
            .Vector2i32 => |*vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 2 * 4;
            },
            .Vector4u32 => |*vec| {
                inline for (0..4) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 4 * 4;
            },
            .Vector3u32 => |*vec| {
                inline for (0..3) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 3 * 4;
            },
            .Vector2u32 => |*vec| {
                inline for (0..2) |i| {
                    const start = i * 4;
                    const end = (i + 1) * 4;
                    if (start >= input.len or end > input.len) return RuntimeError.OutOfBounds;
                    @memcpy(std.mem.asBytes(&vec[i]), input[start..end]);
                }
                return 2 * 4;
            },
            .Vector, .Matrix => |*values| {
                var offset: usize = 0;
                for (values.*) |*v| {
                    offset += try v.write(input[offset..]);
                }
                return offset;
            },
            .Array => |*arr| {
                var offset: usize = 0;
                for (arr.values) |*v| {
                    _ = try v.write(input[offset..]);
                    offset += arr.stride;
                }
                return offset;
            },
            .Structure => |s| {
                var end_offset: usize = 0;
                for (s.values, 0..) |*v, i| {
                    const member_offset: usize = @intCast(s.offsets[i] orelse end_offset);
                    const write_size = try v.write(input[member_offset..]);
                    end_offset = @max(end_offset, member_offset + write_size);
                }
                return end_offset;
            },
            .RuntimeArray => |*arr| arr.data = input[0..],
            .Image => |*img| img.driver_image = @ptrFromInt(std.mem.bytesToValue(usize, input[0..])),
            else => return RuntimeError.InvalidValueType,
        }
        return 0;
    }

    pub fn getPlainMemorySize(self: *const Self) RuntimeError!usize {
        return switch (self.*) {
            .Bool => 1,
            .Int => |i| @divExact(i.bit_count, 8),
            .Float => |f| @divExact(f.bit_count, 8),
            .Vector4f32, .Vector4i32, .Vector4u32 => 4 * 4,
            .Vector3f32, .Vector3i32, .Vector3u32 => 3 * 4,
            .Vector2f32, .Vector2i32, .Vector2u32 => 2 * 4,
            .Vector, .Matrix => |values| blk: {
                var size: usize = 0;
                for (values) |v| {
                    size += try v.getPlainMemorySize();
                }
                break :blk size;
            },
            .Array => |arr| arr.stride * arr.values.len,
            .Structure => |s| blk: {
                var size: usize = 0;
                for (s.values, 0..) |v, i| {
                    const member_offset: usize = @intCast(s.offsets[i] orelse size);
                    size = @max(size, member_offset + try v.getPlainMemorySize());
                }
                break :blk size;
            },
            .RuntimeArray => |arr| arr.getLen(),
            else => return RuntimeError.InvalidValueType,
        };
    }

    pub inline fn getLaneCount(self: *const Self) RuntimeError!usize {
        return switch (self.*) {
            .Vector => |lanes| lanes.len,
            .Vector2i32, .Vector2u32, .Vector2f32 => 2,
            .Vector3i32, .Vector3u32, .Vector3f32 => 3,
            .Vector4i32, .Vector4u32, .Vector4f32 => 4,
            .Int, .Float, .Bool => 1,
            else => RuntimeError.InvalidSpirV,
        };
    }

    pub inline fn isScalar(self: *const Self) bool {
        return switch (self.*) {
            .Bool, .Int, .Float => true,
            else => false,
        };
    }

    pub inline fn isVector(self: *const Self) bool {
        return switch (self.*) {
            .Vector,
            .Vector2i32,
            .Vector2u32,
            .Vector2f32,
            .Vector3i32,
            .Vector3u32,
            .Vector3f32,
            .Vector4i32,
            .Vector4u32,
            .Vector4f32,
            => true,
            else => false,
        };
    }

    pub fn flushPtr(self: *Self, allocator: std.mem.Allocator) RuntimeError!void {
        switch (self.*) {
            .Pointer => |*p| {
                if (p.uniform_slice_window) |window| {
                    switch (p.ptr) {
                        .common => |ptr| {
                            _ = try ptr.read(window);
                        },
                        .f32_ptr => |ptr| {
                            if (window.len < @sizeOf(f32)) return RuntimeError.OutOfBounds;
                            std.mem.copyForwards(u8, window[0..@sizeOf(f32)], std.mem.asBytes(ptr));
                        },
                        .i32_ptr => |ptr| {
                            if (window.len < @sizeOf(i32)) return RuntimeError.OutOfBounds;
                            std.mem.copyForwards(u8, window[0..@sizeOf(i32)], std.mem.asBytes(ptr));
                        },
                        .u32_ptr => |ptr| {
                            if (window.len < @sizeOf(u32)) return RuntimeError.OutOfBounds;
                            std.mem.copyForwards(u8, window[0..@sizeOf(u32)], std.mem.asBytes(ptr));
                        },
                    }

                    if (p.uniform_backing_value) |backing| {
                        backing.deinit(allocator);
                        allocator.destroy(backing);
                        p.uniform_backing_value = null;
                    }

                    p.uniform_slice_window = null;
                }
            },
            else => {},
        }
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Vector, .Matrix => |*values| {
                for (values.*) |*value| value.deinit(allocator);
                allocator.free(values.*);
            },
            .Array => |*arr| {
                for (arr.values) |*value| value.deinit(allocator);
                allocator.free(arr.values);
            },
            .Structure => |*s| {
                for (s.values) |*value| value.deinit(allocator);
                allocator.free(s.values);
                allocator.free(s.offsets);
            },
            else => {},
        }
    }

    pub inline fn readLane(comptime T: PrimitiveType, comptime bits: u32, v: *const Value, lane_index: usize) RuntimeError!getPrimitiveFieldType(T, bits) {
        const TT = getPrimitiveFieldType(T, bits);

        return switch (v.*) {
            .Int => (try getPrimitiveField(T, bits, @constCast(v))).*,

            .Vector => |lanes| (try getPrimitiveField(T, bits, &lanes[lane_index])).*,

            .Vector2i32 => |*vec| switch (lane_index) {
                inline 0...1 => |i| blk: {
                    if (bits == 32) {
                        break :blk @as(TT, @bitCast(vec[i]));
                    } else {
                        return RuntimeError.InvalidSpirV;
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            },
            .Vector3i32 => |*vec| switch (lane_index) {
                inline 0...2 => |i| blk: {
                    if (bits == 32) {
                        break :blk @as(TT, @bitCast(vec[i]));
                    } else {
                        return RuntimeError.InvalidSpirV;
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            },
            .Vector4i32 => |*vec| switch (lane_index) {
                inline 0...3 => |i| blk: {
                    if (bits == 32) {
                        break :blk @as(TT, @bitCast(vec[i]));
                    } else {
                        return RuntimeError.InvalidSpirV;
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            },

            .Vector2u32 => |*vec| switch (lane_index) {
                inline 0...1 => |i| blk: {
                    if (bits == 32) {
                        break :blk @as(TT, @bitCast(vec[i]));
                    } else {
                        return RuntimeError.InvalidSpirV;
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            },
            .Vector3u32 => |*vec| switch (lane_index) {
                inline 0...2 => |i| blk: {
                    if (bits == 32) {
                        break :blk @as(TT, @bitCast(vec[i]));
                    } else {
                        return RuntimeError.InvalidSpirV;
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            },
            .Vector4u32 => |*vec| switch (lane_index) {
                inline 0...3 => |i| blk: {
                    if (bits == 32) {
                        break :blk @as(TT, @bitCast(vec[i]));
                    } else {
                        return RuntimeError.InvalidSpirV;
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            },

            else => RuntimeError.InvalidSpirV,
        };
    }

    pub inline fn writeLane(comptime T: PrimitiveType, comptime bits: u32, dst: *Value, lane_index: usize, value: getPrimitiveFieldType(T, bits)) RuntimeError!void {
        switch (dst.*) {
            .Int => (try getPrimitiveField(T, bits, dst)).* = value,

            .Vector => |lanes| try setScalarLaneValue(T, bits, &lanes[lane_index], value),

            .Vector2i32 => |*vec| switch (lane_index) {
                inline 0...1 => |i| if (bits == 32) {
                    vec[i] = @bitCast(value);
                } else {
                    return RuntimeError.InvalidSpirV;
                },
                else => return RuntimeError.InvalidSpirV,
            },
            .Vector3i32 => |*vec| switch (lane_index) {
                inline 0...2 => |i| if (bits == 32) {
                    vec[i] = @bitCast(value);
                } else {
                    return RuntimeError.InvalidSpirV;
                },
                else => return RuntimeError.InvalidSpirV,
            },
            .Vector4i32 => |*vec| switch (lane_index) {
                inline 0...3 => |i| if (bits == 32) {
                    vec[i] = @bitCast(value);
                } else {
                    return RuntimeError.InvalidSpirV;
                },
                else => return RuntimeError.InvalidSpirV,
            },

            .Vector2u32 => |*vec| switch (lane_index) {
                inline 0...1 => |i| if (bits == 32) {
                    vec[i] = @bitCast(value);
                } else {
                    return RuntimeError.InvalidSpirV;
                },
                else => return RuntimeError.InvalidSpirV,
            },
            .Vector3u32 => |*vec| switch (lane_index) {
                inline 0...2 => |i| if (bits == 32) {
                    vec[i] = @bitCast(value);
                } else {
                    return RuntimeError.InvalidSpirV;
                },
                else => return RuntimeError.InvalidSpirV,
            },
            .Vector4u32 => |*vec| switch (lane_index) {
                inline 0...3 => |i| if (bits == 32) {
                    vec[i] = @bitCast(value);
                } else {
                    return RuntimeError.InvalidSpirV;
                },
                else => return RuntimeError.InvalidSpirV,
            },

            else => return RuntimeError.InvalidSpirV,
        }
    }

    fn setScalarLaneValue(comptime value_type: PrimitiveType, comptime bits: u32, dst: *Value, v: getPrimitiveFieldType(value_type, bits)) RuntimeError!void {
        switch (bits) {
            inline 8, 16, 32, 64 => {
                dst.* = .{ .Int = .{
                    .bit_count = bits,
                    .is_signed = if (value_type == .SInt) true else false,
                    .value = switch (value_type) {
                        .SInt => switch (bits) {
                            8 => .{ .sint8 = v },
                            16 => .{ .sint16 = v },
                            32 => .{ .sint32 = v },
                            64 => .{ .sint64 = v },
                            else => unreachable,
                        },
                        .UInt => switch (bits) {
                            8 => .{ .uint8 = v },
                            16 => .{ .uint16 = v },
                            32 => .{ .uint32 = v },
                            64 => .{ .uint64 = v },
                            else => unreachable,
                        },
                        else => return RuntimeError.InvalidSpirV,
                    },
                } };
            },
            else => return RuntimeError.InvalidSpirV,
        }
    }

    pub fn getPrimitiveField(comptime T: PrimitiveType, comptime BitCount: SpvWord, v: *Value) RuntimeError!*getPrimitiveFieldType(T, BitCount) {
        if (std.meta.activeTag(v.*) == .Pointer) {
            return switch (v.Pointer.ptr) {
                .common => |value| getPrimitiveField(T, BitCount, value),
                .f32_ptr => |ptr| @ptrCast(@alignCast(ptr)),
                .u32_ptr => |ptr| @ptrCast(@alignCast(ptr)),
                .i32_ptr => |ptr| @ptrCast(@alignCast(ptr)),
            };
        }
        return switch (T) {
            .Bool => &v.Bool,
            .Float => switch (BitCount) {
                inline 16, 32, 64 => |i| &@field(v.Float.value, std.fmt.comptimePrint("float{}", .{i})),
                else => return RuntimeError.InvalidSpirV,
            },
            .SInt => switch (BitCount) {
                inline 8, 16, 32, 64 => |i| &@field(v.Int.value, std.fmt.comptimePrint("sint{}", .{i})),
                else => return RuntimeError.InvalidSpirV,
            },
            .UInt => switch (BitCount) {
                inline 8, 16, 32, 64 => |i| &@field(v.Int.value, std.fmt.comptimePrint("uint{}", .{i})),
                else => return RuntimeError.InvalidSpirV,
            },
        };
    }

    pub fn getPrimitiveFieldType(comptime T: PrimitiveType, comptime BitCount: SpvWord) type {
        return switch (T) {
            .Bool => bool,
            .Float => std.meta.Float(BitCount),
            .SInt => std.meta.Int(.signed, BitCount),
            .UInt => std.meta.Int(.unsigned, BitCount),
        };
    }

    pub fn resolveLaneBitWidth(self: *const Self) RuntimeError!SpvWord {
        return switch (self.*) {
            .Bool => 8,
            .Float => |f| @intCast(f.bit_count),
            .Int => |i| @intCast(i.bit_count),
            .Vector => |v| v[0].resolveLaneBitWidth(),
            .Vector4f32,
            .Vector3f32,
            .Vector2f32,
            .Vector4i32,
            .Vector3i32,
            .Vector2i32,
            .Vector4u32,
            .Vector3u32,
            .Vector2u32,
            => return 32,
            else => return RuntimeError.InvalidSpirV,
        };
    }

    pub fn resolveLaneCount(self: *const Self) RuntimeError!SpvWord {
        return switch (self.*) {
            .Bool, .Float, .Int => 1,
            .Vector => |v| @intCast(v.len),
            .Vector4f32, .Vector4i32, .Vector4u32 => 4,
            .Vector3f32, .Vector3i32, .Vector3u32 => 3,
            .Vector2f32, .Vector2i32, .Vector2u32 => 2,
            else => return RuntimeError.InvalidSpirV,
        };
    }

    pub fn resolveSign(self: *const Self) RuntimeError!enum { signed, unsigned } {
        return switch (self.*) {
            .Int => |i| if (i.is_signed) .signed else .unsigned,
            .Vector => |v| v[0].resolveSign(),
            .Vector4i32 => .signed,
            .Vector3i32 => .signed,
            .Vector2i32 => .signed,
            .Vector4u32 => .unsigned,
            .Vector3u32 => .unsigned,
            .Vector2u32 => .unsigned,
            else => .unsigned,
        };
    }
};
