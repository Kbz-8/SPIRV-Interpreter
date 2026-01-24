const std = @import("std");
const spv = @import("../spv.zig");
const ext = @import("GLSL_std_450.zig");
const opc = @import("../opcodes.zig");

const Module = @import("../Module.zig");
const Runtime = @import("../Runtime.zig");
const Result = @import("../Result.zig");
const WordIterator = @import("../WordIterator.zig");

const RuntimeError = Runtime.RuntimeError;
const ValueType = opc.ValueType;

const getValuePrimitiveField = opc.getValuePrimitiveField;
const getValuePrimitiveFieldType = opc.getValuePrimitiveFieldType;

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

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

pub const OpCodeExtFunc = opc.OpCodeExtFunc;

/// Not an EnumMap as it is way too slow for this purpose
pub var runtime_dispatcher = [_]?OpCodeExtFunc{null} ** ext.GLSLOpMaxValue;

pub fn initRuntimeDispatcher() void {
    // zig fmt: off
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Cos)]       = MathEngine(.Float, .Cos).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FMax)]      = MathEngine(.Float, .FMax).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Length)]    = opLength;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Normalize)] = opNormalize;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Sin)]       = MathEngine(.Float, .Sin).opSingleOperator;
    // zig fmt: on
}

fn MathEngine(comptime T: ValueType, comptime Op: MathOp) type {
    return struct {
        fn opSingleOperator(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            const dst = try rt.results[id].getValue();
            const src = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

            const operator = struct {
                fn operation(comptime TT: type, x: TT) RuntimeError!TT {
                    return switch (Op) {
                        .Sin => @sin(x),
                        .Cos => @cos(x),
                        else => RuntimeError.InvalidSpirV,
                    };
                }

                fn applyScalar(bit_count: SpvWord, d: *Result.Value, s: *const Result.Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |bits| {
                            if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                            const ScalarT = getValuePrimitiveFieldType(T, bits);
                            const d_field = try getValuePrimitiveField(T, bits, d);
                            const s_field = try getValuePrimitiveField(T, bits, @constCast(s));
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

                //.Vector4i32 => |*d| d.* = try operator.operation(@Vector(4, i32), src.Vector4i32),
                //.Vector3i32 => |*d| d.* = try operator.operation(@Vector(3, i32), src.Vector3i32),
                //.Vector2i32 => |*d| d.* = try operator.operation(@Vector(2, i32), src.Vector2i32),

                //.Vector4u32 => |*d| d.* = try operator.operation(@Vector(4, u32), src.Vector4u32),
                //.Vector3u32 => |*d| d.* = try operator.operation(@Vector(3, u32), src.Vector3u32),
                //.Vector2u32 => |*d| d.* = try operator.operation(@Vector(2, u32), src.Vector2u32),

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

                fn applyScalar(bit_count: SpvWord, d: *Result.Value, l: *const Result.Value, r: *const Result.Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |bits| {
                            if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                            const ScalarT = getValuePrimitiveFieldType(T, bits);
                            const d_field = try getValuePrimitiveField(T, bits, d);
                            const l_field = try getValuePrimitiveField(T, bits, @constCast(l));
                            const r_field = try getValuePrimitiveField(T, bits, @constCast(r));
                            d_field.* = try operation(ScalarT, l_field.*, r_field.*);
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }

                inline fn applySIMDVector(comptime ElemT: type, comptime N: usize, d: *@Vector(N, ElemT), l: *const @Vector(N, ElemT), r: *const @Vector(N, ElemT)) RuntimeError!void {
                    inline for (0..N) |i| {
                        d[i] = try operation(ElemT, l[i], r[i]);
                    }
                }
            };

            switch (dst.*) {
                .Int, .Float => try operator.applyScalar(lane_bits, dst, lhs, rhs),

                .Vector => |dst_vec| for (dst_vec, lhs.Vector, rhs.Vector) |*d_lane, l_lane, r_lane| {
                    try operator.applyScalar(lane_bits, d_lane, &l_lane, &r_lane);
                },

                .Vector4f32 => |*d| try operator.applySIMDVector(f32, 4, d, &lhs.Vector4f32, &rhs.Vector4f32),
                .Vector3f32 => |*d| try operator.applySIMDVector(f32, 3, d, &lhs.Vector3f32, &rhs.Vector3f32),
                .Vector2f32 => |*d| try operator.applySIMDVector(f32, 2, d, &lhs.Vector2f32, &rhs.Vector2f32),

                .Vector4i32 => |*d| try operator.applySIMDVector(i32, 4, d, &lhs.Vector4i32, &rhs.Vector4i32),
                .Vector3i32 => |*d| try operator.applySIMDVector(i32, 3, d, &lhs.Vector3i32, &rhs.Vector3i32),
                .Vector2i32 => |*d| try operator.applySIMDVector(i32, 2, d, &lhs.Vector2i32, &rhs.Vector2i32),

                .Vector4u32 => |*d| try operator.applySIMDVector(u32, 4, d, &lhs.Vector4u32, &rhs.Vector4u32),
                .Vector3u32 => |*d| try operator.applySIMDVector(u32, 3, d, &lhs.Vector3u32, &rhs.Vector3u32),
                .Vector2u32 => |*d| try operator.applySIMDVector(u32, 2, d, &lhs.Vector2u32, &rhs.Vector2u32),

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

inline fn sumSIMDVector(comptime ElemT: type, comptime N: usize, d: *ElemT, v: *const @Vector(N, ElemT)) void {
    inline for (0..N) |i| {
        d.* += v[i];
    }
}

fn opLength(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const src = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            var sum: std.meta.Float(bits) = 0.0;
            const d_field = try getValuePrimitiveField(.Float, bits, dst);

            if (bits == 32) { // More likely to be SIMD if f32
                switch (src.*) {
                    .Vector4f32 => |src_vec| sumSIMDVector(f32, 4, &sum, &src_vec),
                    .Vector3f32 => |src_vec| sumSIMDVector(f32, 3, &sum, &src_vec),
                    .Vector2f32 => |src_vec| sumSIMDVector(f32, 2, &sum, &src_vec),
                    else => {},
                }
            }

            switch (src.*) {
                .Float => {
                    // Fast path
                    const s_field = try getValuePrimitiveField(.Float, bits, src);
                    d_field.* = s_field.*;
                    return;
                },
                .Vector => |src_vec| for (src_vec) |*s_lane| {
                    const s_field = try getValuePrimitiveField(.Float, bits, s_lane);
                    sum += s_field.*;
                },
                .Vector4f32, .Vector3f32, .Vector2f32 => {},
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
            var sum: std.meta.Float(bits) = 0.0;

            if (bits == 32) { // More likely to be SIMD if f32
                switch (src.*) {
                    .Vector4f32 => |src_vec| sumSIMDVector(f32, 4, &sum, &src_vec),
                    .Vector3f32 => |src_vec| sumSIMDVector(f32, 3, &sum, &src_vec),
                    .Vector2f32 => |src_vec| sumSIMDVector(f32, 2, &sum, &src_vec),
                    else => {},
                }
            }

            switch (src.*) {
                .Float => {
                    const s_field = try getValuePrimitiveField(.Float, bits, src);
                    sum = s_field.*;
                },
                .Vector => |src_vec| for (src_vec) |*s_lane| {
                    const s_field = try getValuePrimitiveField(.Float, bits, s_lane);
                    sum += s_field.*;
                },
                .Vector4f32, .Vector3f32, .Vector2f32 => {},
                else => return RuntimeError.InvalidSpirV,
            }

            sum = @sqrt(sum);

            if (bits == 32) {
                switch (dst.*) {
                    .Vector4f32 => |*dst_vec| inline for (0..4) |i| {
                        dst_vec[i] = src.Vector4f32[i] / sum;
                    },
                    .Vector3f32 => |*dst_vec| inline for (0..3) |i| {
                        dst_vec[i] = src.Vector3f32[i] / sum;
                    },
                    .Vector2f32 => |*dst_vec| inline for (0..2) |i| {
                        dst_vec[i] = src.Vector2f32[i] / sum;
                    },
                    else => {},
                }
            }

            switch (dst.*) {
                .Vector => |dst_vec| for (dst_vec, src.Vector) |*d_lane, *s_lane| {
                    const d_field = try getValuePrimitiveField(.Float, bits, d_lane);
                    const s_field = try getValuePrimitiveField(.Float, bits, s_lane);
                    d_field.* = s_field.* / sum;
                },
                .Vector4f32, .Vector3f32, .Vector2f32 => {},
                else => return RuntimeError.InvalidSpirV,
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}
