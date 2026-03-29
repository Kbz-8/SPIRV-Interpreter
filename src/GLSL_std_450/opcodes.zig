const std = @import("std");
const spv = @import("../spv.zig");
const ext = @import("GLSL_std_450.zig");
const opc = @import("../opcodes.zig");
const zm = @import("zmath");

const Module = @import("../Module.zig");
const Runtime = @import("../Runtime.zig");
const Result = @import("../Result.zig");
const WordIterator = @import("../WordIterator.zig");
const value_ns = @import("../Value.zig");

const RuntimeError = Runtime.RuntimeError;

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Value = value_ns.Value;
const PrimitiveType = value_ns.PrimitiveType;

const MathOp = enum {
    Acos,
    Acosh,
    Asin,
    Asinh,
    Atan,
    Atan2,
    Atanh,
    Ceil,
    Cos,
    Cosh,
    Determinant,
    Exp,
    Exp2,
    FAbs,
    FClamp,
    FMax,
    FMin,
    FMix,
    FSign,
    Floor,
    Fract,
    IMix,
    InverseSqrt,
    Log,
    Log2,
    Modf,
    Pow,
    Round,
    RoundEven,
    SAbs,
    SClamp,
    SMax,
    SMin,
    SSign,
    Sin,
    Sinh,
    Sqrt,
    Tan,
    Tanh,
    Trunc,
    UClamp,
    UMax,
    UMin,
};

const IntBitOp = enum {
    FindILsb,
    FindSMsb,
    FindUMsb,
};

pub const OpCodeExtFunc = opc.OpCodeExtFunc;

/// Not an EnumMap as it is way too slow for this purpose
pub var runtime_dispatcher = [_]?OpCodeExtFunc{null} ** ext.GLSLOpMaxValue;

pub fn initRuntimeDispatcher() void {
    // zig fmt: off
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Ceil)]      = MathEngine(.Float, .Ceil).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Cos)]       = MathEngine(.Float, .Cos).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Exp)]       = MathEngine(.Float, .Exp).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Exp2)]      = MathEngine(.Float, .Exp2).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FAbs)]      = MathEngine(.Float, .FAbs).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FMax)]      = MathEngine(.Float, .FMax).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FSign)]     = MathEngine(.Float, .FSign).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Floor)]     = MathEngine(.Float, .Floor).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Length)]    = opLength;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Log)]       = MathEngine(.Float, .Log).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Log2)]      = MathEngine(.Float, .Log2).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Normalize)] = opNormalize;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Round)]     = MathEngine(.Float, .Round).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.SAbs)]      = MathEngine(.SInt,  .SAbs).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.SSign)]     = MathEngine(.SInt, .SSign).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Sin)]       = MathEngine(.Float, .Sin).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Sqrt)]      = MathEngine(.Float, .Sqrt).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Tan)]       = MathEngine(.Float, .Tan).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Trunc)]     = MathEngine(.Float, .Trunc).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FindILsb)] = IntBitEngine(.FindILsb).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FindSMsb)] = IntBitEngine(.FindSMsb).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FindUMsb)] = IntBitEngine(.FindUMsb).op;
    // zig fmt: on
}

fn isFloatOrF32Vector(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .float => true,
        .vector => |vec| vec.child == f32,
        else => false,
    };
}

fn MathEngine(comptime T: PrimitiveType, comptime Op: MathOp) type {
    return struct {
        fn opSingleOperator(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            const dst = try rt.results[id].getValue();
            const src = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

            const operator = struct {
                fn operation(comptime TT: type, x: TT) RuntimeError!TT {
                    if (comptime isFloatOrF32Vector(TT)) {
                        return switch (Op) {
                            .Ceil => @ceil(x),
                            .Cos => @cos(x),
                            .Exp => @exp(x),
                            .Exp2 => @exp2(x),
                            .FAbs => @abs(x),
                            .FSign => std.math.sign(x),
                            .Floor => @floor(x),
                            .Log => @log(x),
                            .Log2 => @log2(x),
                            .Round => @round(x),
                            .Sin => @sin(x),
                            .Sqrt => @sqrt(x),
                            .Tan => @tan(x),
                            .Trunc => @trunc(x),
                            else => return RuntimeError.InvalidSpirV,
                        };
                    } else {
                        return switch (Op) {
                            .SAbs => @intCast(@abs(x)),
                            .SSign => std.math.sign(x),
                            else => RuntimeError.InvalidSpirV,
                        };
                    }
                }

                fn applyScalar(bit_count: SpvWord, d: *Value, s: *const Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |bits| {
                            if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                            const ScalarT = Value.getPrimitiveFieldType(T, bits);
                            const d_field = try Value.getPrimitiveField(T, bits, d);
                            const s_field = try Value.getPrimitiveField(T, bits, @constCast(s));
                            d_field.* = try operation(ScalarT, s_field.*);
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }
            };

            switch (dst.*) {
                .Int, .Float => try operator.applyScalar(lane_bits, dst, src),

                .Vector => |dst_vec| for (dst_vec, src.Vector) |*d_lane, s_lane| {
                    try operator.applyScalar(lane_bits, d_lane, &s_lane);
                },

                .Vector4f32 => |*d| d.* = try operator.operation(@Vector(4, f32), src.Vector4f32),
                .Vector3f32 => |*d| d.* = try operator.operation(@Vector(3, f32), src.Vector3f32),
                .Vector2f32 => |*d| d.* = try operator.operation(@Vector(2, f32), src.Vector2f32),

                .Vector4i32 => |*d| d.* = try operator.operation(@Vector(4, i32), src.Vector4i32),
                .Vector3i32 => |*d| d.* = try operator.operation(@Vector(3, i32), src.Vector3i32),
                .Vector2i32 => |*d| d.* = try operator.operation(@Vector(2, i32), src.Vector2i32),

                .Vector4u32 => |*d| d.* = try operator.operation(@Vector(4, u32), src.Vector4u32),
                .Vector3u32 => |*d| d.* = try operator.operation(@Vector(3, u32), src.Vector3u32),
                .Vector2u32 => |*d| d.* = try operator.operation(@Vector(2, u32), src.Vector2u32),

                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn opDoubleOperators(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            const dst = try rt.results[id].getValue();
            const lhs = try rt.results[try rt.it.next()].getValue();
            const rhs = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

            const operator = struct {
                fn operation(comptime TT: type, l: TT, r: TT) RuntimeError!TT {
                    return switch (Op) {
                        .FMax => @max(l, r),
                        else => RuntimeError.InvalidSpirV,
                    };
                }

                fn applyScalar(bit_count: SpvWord, d: *Value, l: *const Value, r: *const Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |bits| {
                            if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                            const ScalarT = Value.getPrimitiveFieldType(T, bits);
                            const d_field = try Value.getPrimitiveField(T, bits, d);
                            const l_field = try Value.getPrimitiveField(T, bits, @constCast(l));
                            const r_field = try Value.getPrimitiveField(T, bits, @constCast(r));
                            d_field.* = try operation(ScalarT, l_field.*, r_field.*);
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }
            };

            switch (dst.*) {
                .Int, .Float => try operator.applyScalar(lane_bits, dst, lhs, rhs),

                .Vector => |dst_vec| for (dst_vec, lhs.Vector, rhs.Vector) |*d_lane, l_lane, r_lane| {
                    try operator.applyScalar(lane_bits, d_lane, &l_lane, &r_lane);
                },

                .Vector4f32 => |*d| d.* = try operator.operation(@Vector(4, f32), lhs.Vector4f32, rhs.Vector4f32),
                .Vector3f32 => |*d| d.* = try operator.operation(@Vector(3, f32), lhs.Vector3f32, rhs.Vector3f32),
                .Vector2f32 => |*d| d.* = try operator.operation(@Vector(2, f32), lhs.Vector2f32, rhs.Vector2f32),

                .Vector4i32 => |*d| d.* = try operator.operation(@Vector(4, i32), lhs.Vector4i32, rhs.Vector4i32),
                .Vector3i32 => |*d| d.* = try operator.operation(@Vector(3, i32), lhs.Vector3i32, rhs.Vector3i32),
                .Vector2i32 => |*d| d.* = try operator.operation(@Vector(2, i32), lhs.Vector2i32, rhs.Vector2i32),

                .Vector4u32 => |*d| d.* = try operator.operation(@Vector(4, u32), lhs.Vector4u32, rhs.Vector4u32),
                .Vector3u32 => |*d| d.* = try operator.operation(@Vector(3, u32), lhs.Vector3u32, rhs.Vector3u32),
                .Vector2u32 => |*d| d.* = try operator.operation(@Vector(2, u32), lhs.Vector2u32, rhs.Vector2u32),

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn IntBitEngine(comptime op_kind: IntBitOp) type {
    return struct {
        inline fn findILsb32(x: u32) i32 {
            if (x == 0) return -1;
            return @intCast(@ctz(x));
        }

        inline fn findUMsb32(x: u32) i32 {
            if (x == 0) return -1;
            return 31 - @as(i32, @intCast(@clz(x)));
        }

        inline fn findSMsb32(x: i32) i32 {
            if (x == 0 or x == -1) return -1;

            if (x > 0) {
                return findUMsb32(@bitCast(x));
            }

            return findUMsb32(@bitCast(~x));
        }

        inline fn computeSigned(x: i32) i32 {
            return switch (op_kind) {
                .FindILsb => findILsb32(@bitCast(x)),
                .FindSMsb => findSMsb32(x),
                .FindUMsb => findUMsb32(@bitCast(x)),
            };
        }

        inline fn computeUnsigned(x: u32) u32 {
            const result: i32 = switch (op_kind) {
                .FindILsb => findILsb32(x),
                .FindSMsb => findSMsb32(@bitCast(x)),
                .FindUMsb => findUMsb32(x),
            };
            return @bitCast(result);
        }

        fn readSourceLane(src: *const Value, lane_index: usize) RuntimeError!u32 {
            return switch (op_kind) {
                .FindSMsb => @bitCast(try Value.readLane(.SInt, 32, src, lane_index)),
                .FindILsb, .FindUMsb => try Value.readLane(.UInt, 32, src, lane_index),
            };
        }

        fn writeDestLane(dst: *Value, lane_index: usize, bits: u32, dst_is_signed: bool) RuntimeError!void {
            if (dst_is_signed) {
                try Value.writeLane(.SInt, 32, dst, lane_index, @as(i32, @bitCast(bits)));
            } else {
                try Value.writeLane(.UInt, 32, dst, lane_index, bits);
            }
        }

        fn apply(dst: *Value, src: *const Value, lane_count: usize, dst_is_signed: bool) RuntimeError!void {
            for (0..lane_count) |lane_index| {
                const src_bits = try readSourceLane(src, lane_index);

                const out_bits: u32 = if (dst_is_signed)
                    @bitCast(computeSigned(@bitCast(src_bits)))
                else
                    computeUnsigned(src_bits);

                try writeDestLane(dst, lane_index, out_bits, dst_is_signed);
            }
        }

        fn op(
            _: std.mem.Allocator,
            target_type_id: SpvWord,
            id: SpvWord,
            _: SpvWord,
            rt: *Runtime,
        ) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            const dst = try rt.results[id].getValue();
            const src = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
            if (lane_bits != 32)
                return RuntimeError.InvalidSpirV;

            const lane_count = try Result.resolveLaneCount(target_type);
            const dst_sign = try Result.resolveSign(target_type, rt);

            try apply(dst, src, lane_count, dst_sign == .signed);
        }
    };
}

fn opLength(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const src = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            var sum: std.meta.Float(bits) = 0.0;
            const d_field = try Value.getPrimitiveField(.Float, bits, dst);

            if (bits == 32) { // More likely to be SIMD if f32
                switch (src.*) {
                    .Vector4f32 => |src_vec| {
                        d_field.* = zm.length4(src_vec)[0];
                        return;
                    },
                    .Vector3f32 => |src_vec| {
                        d_field.* = zm.length3(zm.f32x4(src_vec[0], src_vec[1], src_vec[2], 0.0))[0];
                        return;
                    },
                    .Vector2f32 => |src_vec| {
                        d_field.* = zm.length2(zm.f32x4(src_vec[0], src_vec[1], 0.0, 0.0))[0];
                        return;
                    },
                    else => {},
                }
            }

            switch (src.*) {
                .Float => {
                    // Fast path
                    const s_field = try Value.getPrimitiveField(.Float, bits, src);
                    d_field.* = s_field.*;
                    return;
                },
                .Vector => |src_vec| for (src_vec) |*s_lane| {
                    const s_field = try Value.getPrimitiveField(.Float, bits, s_lane);
                    sum += s_field.*;
                },
                else => return RuntimeError.InvalidSpirV,
            }

            d_field.* = @sqrt(sum);
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opNormalize(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const src = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            if (bits == 32) { // More likely to be SIMD if f32
                switch (src.*) {
                    .Vector4f32 => |src_vec| {
                        dst.Vector4f32 = zm.normalize4(src_vec);
                        return;
                    },
                    .Vector3f32 => |src_vec| {
                        const normed = zm.normalize3(zm.f32x4(src_vec[0], src_vec[1], src_vec[2], 0.0));
                        dst.Vector3f32[0] = normed[0];
                        dst.Vector3f32[1] = normed[1];
                        dst.Vector3f32[2] = normed[2];
                        return;
                    },
                    .Vector2f32 => |src_vec| {
                        const normed = zm.normalize2(zm.f32x4(src_vec[0], src_vec[1], 0.0, 0.0));
                        dst.Vector2f32[0] = normed[0];
                        dst.Vector2f32[1] = normed[1];
                        return;
                    },
                    else => {},
                }
            }

            var sum: std.meta.Float(bits) = 0.0;

            switch (src.*) {
                .Float => {
                    const s_field = try Value.getPrimitiveField(.Float, bits, src);
                    sum = s_field.*;
                },
                .Vector => |src_vec| for (src_vec) |*s_lane| {
                    const s_field = try Value.getPrimitiveField(.Float, bits, s_lane);
                    sum += s_field.*;
                },
                else => return RuntimeError.InvalidSpirV,
            }

            sum = @sqrt(sum);

            switch (dst.*) {
                .Vector => |dst_vec| for (dst_vec, src.Vector) |*d_lane, *s_lane| {
                    const d_field = try Value.getPrimitiveField(.Float, bits, d_lane);
                    const s_field = try Value.getPrimitiveField(.Float, bits, s_lane);
                    d_field.* = s_field.* / sum;
                },
                else => return RuntimeError.InvalidSpirV,
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}
