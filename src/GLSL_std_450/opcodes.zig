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
    Degrees,
    Exp,
    Exp2,
    FAbs,
    FClamp,
    FMax,
    Fma,
    FMin,
    FMix,
    FSign,
    Floor,
    Fract,
    InverseSqrt,
    Log,
    Log2,
    NClamp,
    NMax,
    NMin,
    Pow,
    Radians,
    Round,
    RoundEven,
    SAbs,
    SClamp,
    SMax,
    SMin,
    SSign,
    Sin,
    Sinh,
    SmoothStep,
    Sqrt,
    Step,
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

const NormPackKind = enum {
    Snorm,
    Unorm,
};

pub const OpCodeExtFunc = opc.OpCodeExtFunc;

/// Not an EnumMap as it is way too slow for this purpose
pub var runtime_dispatcher: [ext.GLSLOpMaxValue]?OpCodeExtFunc = @splat(null);

pub fn initRuntimeDispatcher() void {
    // zig fmt: off
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Acos)]                  = MathEngine(.Float, .Acos).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Acosh)]                 = MathEngine(.Float, .Acosh).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Asin)]                  = MathEngine(.Float, .Asin).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Asinh)]                 = MathEngine(.Float, .Asinh).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Atan)]                  = MathEngine(.Float, .Atan).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Atan2)]                 = MathEngine(.Float, .Atan2).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Atanh)]                 = MathEngine(.Float, .Atanh).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Ceil)]                  = MathEngine(.Float, .Ceil).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Cos)]                   = MathEngine(.Float, .Cos).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Cosh)]                  = MathEngine(.Float, .Cosh).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Cross)]                 = opCross;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Degrees)]               = MathEngine(.Float, .Degrees).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Determinant)]           = opDeterminant;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Distance)]              = opDistance;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Exp)]                   = MathEngine(.Float, .Exp).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Exp2)]                  = MathEngine(.Float, .Exp2).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FAbs)]                  = MathEngine(.Float, .FAbs).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FClamp)]                = MathEngine(.Float, .FClamp).opTripleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FMax)]                  = MathEngine(.Float, .FMax).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FMin)]                  = MathEngine(.Float, .FMin).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FMix)]                  = opFMix;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FSign)]                 = MathEngine(.Float, .FSign).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FaceForward)]           = opFaceForward;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FindILsb)]              = IntBitEngine(.FindILsb).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FindSMsb)]              = IntBitEngine(.FindSMsb).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FindUMsb)]              = IntBitEngine(.FindUMsb).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Floor)]                 = MathEngine(.Float, .Floor).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Fma)]                   = MathEngine(.Float, .Fma).opTripleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Fract)]                 = MathEngine(.Float, .Fract).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Frexp)]                 = opFrexp;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.FrexpStruct)]           = opFrexpStruct;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.IMix)]                  = opIMix;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.InterpolateAtCentroid)] = opInterpolateAt;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.InterpolateAtOffset)]   = opInterpolateAt;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.InterpolateAtSample)]   = opInterpolateAt;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.InverseSqrt)]           = MathEngine(.Float, .InverseSqrt).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Ldexp)]                 = opLdexp;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Length)]                = opLength;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Log)]                   = MathEngine(.Float, .Log).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Log2)]                  = MathEngine(.Float, .Log2).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.MatrixInverse)]         = opMatrixInverse;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Modf)]                  = opModf;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.ModfStruct)]            = opModfStruct;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.NClamp)]                = MathEngine(.Float, .NClamp).opTripleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.NMax)]                  = MathEngine(.Float, .NMax).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.NMin)]                  = MathEngine(.Float, .NMin).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Normalize)]             = opNormalize;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.PackDouble2x32)]        = opPackDouble2x32Engine;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.PackHalf2x16)]          = opPackHalf2x16Engine;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.PackSnorm2x16)]         = PackNormEngine(2, 16, .Snorm).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.PackSnorm4x8)]          = PackNormEngine(4, 8, .Snorm).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.PackUnorm2x16)]         = PackNormEngine(2, 16, .Unorm).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.PackUnorm4x8)]          = PackNormEngine(4, 8, .Unorm).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Pow)]                   = MathEngine(.Float, .Pow).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Radians)]               = MathEngine(.Float, .Radians).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Reflect)]               = opReflect;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Refract)]               = opRefract;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Round)]                 = MathEngine(.Float, .Round).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.RoundEven)]             = MathEngine(.Float, .RoundEven).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.SAbs)]                  = MathEngine(.SInt,  .SAbs).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.SClamp)]                = MathEngine(.SInt, .SClamp).opTripleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.SMax)]                  = MathEngine(.SInt, .SMax).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.SMin)]                  = MathEngine(.SInt, .SMin).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.SSign)]                 = MathEngine(.SInt, .SSign).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Sin)]                   = MathEngine(.Float, .Sin).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Sinh)]                  = MathEngine(.Float, .Sinh).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.SmoothStep)]            = MathEngine(.Float, .SmoothStep).opTripleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Sqrt)]                  = MathEngine(.Float, .Sqrt).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Step)]                  = MathEngine(.Float, .Step).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Tan)]                   = MathEngine(.Float, .Tan).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Tanh)]                  = MathEngine(.Float, .Tanh).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.Trunc)]                 = MathEngine(.Float, .Trunc).opSingleOperator;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UClamp)]                = MathEngine(.UInt, .UClamp).opTripleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UMax)]                  = MathEngine(.UInt, .UMax).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UMin)]                  = MathEngine(.UInt, .UMin).opDoubleOperators;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UnpackDouble2x32)]      = opUnpackDouble2x32Engine;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UnpackHalf2x16)]        = opUnpackHalf2x16Engine;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UnpackSnorm2x16)]       = UnpackNormEngine(2, 16, .Snorm).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UnpackSnorm4x8)]        = UnpackNormEngine(4, 8, .Snorm).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UnpackUnorm2x16)]       = UnpackNormEngine(2, 16, .Unorm).op;
    runtime_dispatcher[@intFromEnum(ext.GLSLOp.UnpackUnorm4x8)]        = UnpackNormEngine(4, 8, .Unorm).op;
    // zig fmt: on
}

fn roundEven(comptime T: type, x: T) T {
    const lower = @floor(x);
    const upper = lower + 1.0;
    const fraction = x - lower;
    if (fraction < 0.5) return lower;
    if (fraction > 0.5) return upper;
    return if (@mod(lower, 2.0) == 0.0) lower else upper;
}

inline fn nMin(comptime T: type, x: T, y: T) T {
    if (std.math.isNan(x)) return y;
    if (std.math.isNan(y)) return x;
    return if (y < x) y else x;
}

inline fn nMax(comptime T: type, x: T, y: T) T {
    if (std.math.isNan(x)) return y;
    if (std.math.isNan(y)) return x;
    return if (x < y) y else x;
}

fn writeFloatLane(comptime bits: u32, dst: *Value, lane_index: usize, value: std.meta.Float(bits)) RuntimeError!void {
    if (std.meta.activeTag(dst.*) == .Pointer) {
        switch (dst.Pointer.ptr) {
            .common => |ptr| return writeFloatLane(bits, ptr, lane_index, value),
            else => {
                if (lane_index != 0) return RuntimeError.InvalidSpirV;
                (try Value.getPrimitiveField(.Float, bits, dst)).* = value;
                return;
            },
        }
    }
    try Value.writeLane(.Float, bits, dst, lane_index, value);
}

fn writeIntLane(comptime T: PrimitiveType, comptime bits: u32, dst: *Value, lane_index: usize, value: Value.getPrimitiveFieldType(T, bits)) RuntimeError!void {
    if (std.meta.activeTag(dst.*) == .Pointer) {
        switch (dst.Pointer.ptr) {
            .common => |ptr| return writeIntLane(T, bits, ptr, lane_index, value),
            else => {
                if (lane_index != 0) return RuntimeError.InvalidSpirV;
                (try Value.getPrimitiveField(T, bits, dst)).* = value;
                return;
            },
        }
    }
    try Value.writeLane(T, bits, dst, lane_index, value);
}

fn getPointeeOrSelf(value: *Value) RuntimeError!*Value {
    if (std.meta.activeTag(value.*) != .Pointer) return value;
    return switch (value.Pointer.ptr) {
        .common => |ptr| ptr,
        else => value,
    };
}

fn readFloatScalarAs(comptime T: type, src: *const Value) RuntimeError!T {
    const lane_bits = try src.resolveLaneBitWidth();
    if (try src.resolveLaneCount() != 1) return RuntimeError.InvalidSpirV;
    return switch (lane_bits) {
        inline 16, 32, 64 => |bits| @floatCast(try Value.readLane(.Float, bits, src, 0)),
        else => RuntimeError.InvalidSpirV,
    };
}

fn writeIntegralLaneAsI32(dst: *Value, lane_index: usize, value: i32) RuntimeError!void {
    const lane_bits = try dst.resolveLaneBitWidth();
    const sign = try dst.resolveSign();
    return switch (lane_bits) {
        inline 8, 16, 32, 64 => |bits| {
            if (sign == .signed) {
                try writeIntLane(.SInt, bits, dst, lane_index, @intCast(value));
            } else {
                try writeIntLane(.UInt, bits, dst, lane_index, @intCast(value));
            }
        },
        else => RuntimeError.InvalidSpirV,
    };
}

fn readIntegralLaneAsI32(src: *const Value, lane_index: usize) RuntimeError!i32 {
    const lane_bits = try src.resolveLaneBitWidth();
    const sign = try src.resolveSign();
    return switch (lane_bits) {
        inline 8, 16, 32, 64 => |bits| blk: {
            if (sign == .signed) {
                const value = try Value.readLane(.SInt, bits, src, lane_index);
                if (bits > 32) {
                    break :blk std.math.cast(i32, value) orelse if (value < 0)
                        std.math.minInt(i32)
                    else
                        std.math.maxInt(i32);
                }
                break :blk @intCast(value);
            } else {
                const value = try Value.readLane(.UInt, bits, src, lane_index);
                const bits32: u32 = if (bits > 32)
                    std.math.cast(u32, value) orelse std.math.maxInt(u32)
                else
                    @intCast(value);
                break :blk @bitCast(bits32);
            }
        },
        else => RuntimeError.InvalidSpirV,
    };
}

fn readInt32Bits(src: *const Value, lane_index: usize) RuntimeError!u32 {
    const lane_bits = try src.resolveLaneBitWidth();
    if (lane_bits != 32) return RuntimeError.InvalidSpirV;
    const sign = try src.resolveSign();
    return if (sign == .signed)
        @bitCast(try Value.readLane(.SInt, 32, src, lane_index))
    else
        try Value.readLane(.UInt, 32, src, lane_index);
}

fn writeInt32Bits(target_type: Result.TypeData, rt: *Runtime, dst: *Value, lane_index: usize, bits: u32) RuntimeError!void {
    const sign = try Result.resolveSign(target_type, rt);
    if (sign == .signed) {
        try writeIntLane(.SInt, 32, dst, lane_index, @as(i32, @bitCast(bits)));
    } else {
        try writeIntLane(.UInt, 32, dst, lane_index, bits);
    }
}

fn MathEngine(comptime T: PrimitiveType, comptime Op: MathOp) type {
    return struct {
        fn operation1(comptime TT: type, x: TT) RuntimeError!TT {
            if (comptime T == .Float) {
                return switch (Op) {
                    .Acos => std.math.acos(x),
                    .Acosh => if (TT == f16) RuntimeError.UnsupportedSpirV else std.math.acosh(x),
                    .Asin => std.math.asin(x),
                    .Asinh => if (TT == f16) RuntimeError.UnsupportedSpirV else std.math.asinh(x),
                    .Atan => std.math.atan(x),
                    .Atanh => if (TT == f16) RuntimeError.UnsupportedSpirV else std.math.atanh(x),
                    .Ceil => @ceil(x),
                    .Cos => @cos(x),
                    .Cosh => if (TT == f16) RuntimeError.UnsupportedSpirV else (@exp(x) + @exp(-x)) / 2, // std.math.cosh is not precise enough for Vulkan CTS
                    .Degrees => std.math.radiansToDegrees(x),
                    .Exp => @exp(x),
                    .Exp2 => @exp2(x),
                    .FAbs => @abs(x),
                    .FSign => std.math.sign(x),
                    .Floor => @floor(x),
                    .Fract => x - @floor(x),
                    .InverseSqrt => 1.0 / @sqrt(x),
                    .Log => @log(x),
                    .Log2 => @log2(x),
                    .Radians => std.math.degreesToRadians(x),
                    .Round => @round(x),
                    .RoundEven => roundEven(TT, x),
                    .Sin => @sin(x),
                    .Sinh => if (TT == f16) RuntimeError.UnsupportedSpirV else (@exp(x) - @exp(-x)) / 2, // std.math.sinh is not precise enough for Vulkan CTS
                    .Sqrt => @sqrt(x),
                    .Tan => @tan(x),
                    .Tanh => if (TT == f16) RuntimeError.UnsupportedSpirV else std.math.tanh(x),
                    .Trunc => @trunc(x),
                    else => RuntimeError.InvalidSpirV,
                };
            } else if (comptime T == .SInt) {
                return switch (Op) {
                    .SAbs => @intCast(@abs(x)),
                    .SSign => std.math.sign(x),
                    else => RuntimeError.InvalidSpirV,
                };
            } else {
                return RuntimeError.InvalidSpirV;
            }
        }

        fn operation2(comptime TT: type, l: TT, r: TT) RuntimeError!TT {
            if (comptime T == .Float) {
                return switch (Op) {
                    .Atan2 => if (TT == f16) RuntimeError.UnsupportedSpirV else std.math.atan2(l, r),
                    .FMax => if (l < r) r else l,
                    .FMin => if (r < l) r else l,
                    .NMax => nMax(TT, l, r),
                    .NMin => nMin(TT, l, r),
                    .Pow => @exp(@log(l) * r),
                    .Step => if (r < l) 0.0 else 1.0,
                    else => RuntimeError.InvalidSpirV,
                };
            } else if (comptime T == .SInt) {
                return switch (Op) {
                    .SMax => @max(l, r),
                    .SMin => @min(l, r),
                    else => RuntimeError.InvalidSpirV,
                };
            } else if (comptime T == .UInt) {
                return switch (Op) {
                    .UMax => @max(l, r),
                    .UMin => @min(l, r),
                    else => RuntimeError.InvalidSpirV,
                };
            } else {
                return RuntimeError.InvalidSpirV;
            }
        }

        fn operation3(comptime TT: type, x: TT, y: TT, z: TT) RuntimeError!TT {
            if (comptime T == .Float) {
                return switch (Op) {
                    .FClamp => std.math.clamp(x, y, z),
                    .Fma => @mulAdd(TT, x, y, z),
                    .NClamp => nMin(TT, nMax(TT, x, y), z),
                    .SmoothStep => blk: {
                        const t = std.math.clamp((z - x) / (y - x), 0.0, 1.0);
                        break :blk t * t * (3.0 - 2.0 * t);
                    },
                    else => RuntimeError.InvalidSpirV,
                };
            } else if (comptime T == .SInt) {
                return switch (Op) {
                    .SClamp => std.math.clamp(x, y, z),
                    else => RuntimeError.InvalidSpirV,
                };
            } else if (comptime T == .UInt) {
                return switch (Op) {
                    .UClamp => std.math.clamp(x, y, z),
                    else => RuntimeError.InvalidSpirV,
                };
            } else {
                return RuntimeError.InvalidSpirV;
            }
        }

        fn opSingleOperator(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            const dst = try rt.results[id].getValue();
            const src = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
            const lane_count = try Result.resolveLaneCount(target_type);

            switch (lane_bits) {
                inline 8, 16, 32, 64 => |bits| {
                    if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;
                    const ScalarT = Value.getPrimitiveFieldType(T, bits);
                    for (0..lane_count) |lane_index| {
                        const s = try readIntOrFloatLane(T, bits, src, lane_index);
                        const result = try operation1(ScalarT, s);
                        try writeIntOrFloatLane(T, bits, dst, lane_index, result);
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn opDoubleOperators(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            const dst = try rt.results[id].getValue();
            const lhs = try rt.results[try rt.it.next()].getValue();
            const rhs = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
            const lane_count = try Result.resolveLaneCount(target_type);

            switch (lane_bits) {
                inline 8, 16, 32, 64 => |bits| {
                    if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;
                    const ScalarT = Value.getPrimitiveFieldType(T, bits);
                    for (0..lane_count) |lane_index| {
                        const l = try readIntOrFloatLane(T, bits, lhs, lane_index);
                        const r = try readIntOrFloatLane(T, bits, rhs, lane_index);
                        const result = try operation2(ScalarT, l, r);
                        try writeIntOrFloatLane(T, bits, dst, lane_index, result);
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn opTripleOperators(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            const dst = try rt.results[id].getValue();
            const x = try rt.results[try rt.it.next()].getValue();
            const y = try rt.results[try rt.it.next()].getValue();
            const z = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
            const lane_count = try Result.resolveLaneCount(target_type);

            switch (lane_bits) {
                inline 8, 16, 32, 64 => |bits| {
                    if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;
                    const ScalarT = Value.getPrimitiveFieldType(T, bits);
                    for (0..lane_count) |lane_index| {
                        const xv = try readIntOrFloatLane(T, bits, x, lane_index);
                        const yv = try readIntOrFloatLane(T, bits, y, lane_index);
                        const zv = try readIntOrFloatLane(T, bits, z, lane_index);
                        const result = try operation3(ScalarT, xv, yv, zv);
                        try writeIntOrFloatLane(T, bits, dst, lane_index, result);
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn readIntOrFloatLane(comptime T: PrimitiveType, comptime bits: u32, src: *const Value, lane_index: usize) RuntimeError!Value.getPrimitiveFieldType(T, bits) {
    return switch (T) {
        .Float => Value.readLane(.Float, bits, src, lane_index),
        .SInt, .UInt => Value.readLane(T, bits, src, lane_index),
        else => RuntimeError.InvalidSpirV,
    };
}

fn writeIntOrFloatLane(comptime T: PrimitiveType, comptime bits: u32, dst: *Value, lane_index: usize, value: Value.getPrimitiveFieldType(T, bits)) RuntimeError!void {
    return switch (T) {
        .Float => writeFloatLane(bits, dst, lane_index, value),
        .SInt, .UInt => writeIntLane(T, bits, dst, lane_index, value),
        else => RuntimeError.InvalidSpirV,
    };
}

fn opFMix(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const x = try rt.results[try rt.it.next()].getValue();
    const y = try rt.results[try rt.it.next()].getValue();
    const a = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            const FloatT = Value.getPrimitiveFieldType(.Float, bits);
            for (0..lane_count) |lane_index| {
                const xv = try Value.readLane(.Float, bits, x, lane_index);
                const yv = try Value.readLane(.Float, bits, y, lane_index);
                const av = try Value.readLane(.Float, bits, a, lane_index);
                try writeFloatLane(bits, dst, lane_index, @as(FloatT, xv * (1.0 - av) + yv * av));
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opIMix(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const x = try rt.results[try rt.it.next()].getValue();
    const y = try rt.results[try rt.it.next()].getValue();
    const a = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);
    const sign = try Result.resolveSign(target_type, rt);

    switch (lane_bits) {
        inline 8, 16, 32, 64 => |bits| {
            for (0..lane_count) |lane_index| {
                const use_y = try Value.readLane(.Bool, 8, a, lane_index);
                if (sign == .signed) {
                    const value = if (use_y)
                        try Value.readLane(.SInt, bits, y, lane_index)
                    else
                        try Value.readLane(.SInt, bits, x, lane_index);
                    try writeIntLane(.SInt, bits, dst, lane_index, value);
                } else {
                    const value = if (use_y)
                        try Value.readLane(.UInt, bits, y, lane_index)
                    else
                        try Value.readLane(.UInt, bits, x, lane_index);
                    try writeIntLane(.UInt, bits, dst, lane_index, value);
                }
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
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
                try writeIntLane(.SInt, 32, dst, lane_index, @as(i32, @bitCast(bits)));
            } else {
                try writeIntLane(.UInt, 32, dst, lane_index, bits);
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

        fn op(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
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

fn opCross(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const lhs = try rt.results[try rt.it.next()].getValue();
    const rhs = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    if (try Result.resolveLaneCount(target_type) != 3)
        return RuntimeError.InvalidSpirV;

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            const FloatT = Value.getPrimitiveFieldType(.Float, bits);
            const lx = try Value.readLane(.Float, bits, lhs, 0);
            const ly = try Value.readLane(.Float, bits, lhs, 1);
            const lz = try Value.readLane(.Float, bits, lhs, 2);
            const rx = try Value.readLane(.Float, bits, rhs, 0);
            const ry = try Value.readLane(.Float, bits, rhs, 1);
            const rz = try Value.readLane(.Float, bits, rhs, 2);

            try writeFloatLane(bits, dst, 0, @as(FloatT, ly * rz - lz * ry));
            try writeFloatLane(bits, dst, 1, @as(FloatT, lz * rx - lx * rz));
            try writeFloatLane(bits, dst, 2, @as(FloatT, lx * ry - ly * rx));
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn dotFloat(comptime bits: u32, lhs: *const Value, rhs: *const Value, lane_count: usize) RuntimeError!std.meta.Float(bits) {
    var sum: std.meta.Float(bits) = 0.0;
    for (0..lane_count) |lane_index| {
        const l = try Value.readLane(.Float, bits, lhs, lane_index);
        const r = try Value.readLane(.Float, bits, rhs, lane_index);
        sum += l * r;
    }
    return sum;
}

fn opLength(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const src = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try src.resolveLaneCount();

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            const d_field = try Value.getPrimitiveField(.Float, bits, dst);
            if (bits == 32) {
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

            d_field.* = @sqrt(try dotFloat(bits, src, src, lane_count));
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opDistance(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const p0 = try rt.results[try rt.it.next()].getValue();
    const p1 = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try p0.resolveLaneCount();

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            var sum: std.meta.Float(bits) = 0.0;
            for (0..lane_count) |lane_index| {
                const d = try Value.readLane(.Float, bits, p0, lane_index) - try Value.readLane(.Float, bits, p1, lane_index);
                sum += d * d;
            }
            (try Value.getPrimitiveField(.Float, bits, dst)).* = @sqrt(sum);
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opNormalize(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const src = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            if (bits == 32) {
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

            const len = @sqrt(try dotFloat(bits, src, src, lane_count));
            for (0..lane_count) |lane_index| {
                const value = try Value.readLane(.Float, bits, src, lane_index) / len;
                try writeFloatLane(bits, dst, lane_index, value);
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opFaceForward(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const n = try rt.results[try rt.it.next()].getValue();
    const i = try rt.results[try rt.it.next()].getValue();
    const nref = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            const d = try dotFloat(bits, nref, i, lane_count);
            const scale: std.meta.Float(bits) = if (d < 0.0) 1.0 else -1.0;
            for (0..lane_count) |lane_index| {
                try writeFloatLane(bits, dst, lane_index, scale * try Value.readLane(.Float, bits, n, lane_index));
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opReflect(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const i = try rt.results[try rt.it.next()].getValue();
    const n = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            const d = try dotFloat(bits, n, i, lane_count);
            for (0..lane_count) |lane_index| {
                const iv = try Value.readLane(.Float, bits, i, lane_index);
                const nv = try Value.readLane(.Float, bits, n, lane_index);
                try writeFloatLane(bits, dst, lane_index, iv - 2.0 * d * nv);
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opRefract(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const i = try rt.results[try rt.it.next()].getValue();
    const n = try rt.results[try rt.it.next()].getValue();
    const eta_value = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            const FloatT = std.meta.Float(bits);
            const eta = try readFloatScalarAs(FloatT, eta_value);
            const d = try dotFloat(bits, n, i, lane_count);
            const k = 1.0 - eta * eta * (1.0 - d * d);
            for (0..lane_count) |lane_index| {
                const out = if (k < 0.0) 0.0 else blk: {
                    const iv = try Value.readLane(.Float, bits, i, lane_index);
                    const nv = try Value.readLane(.Float, bits, n, lane_index);
                    break :blk eta * iv - (eta * d + @sqrt(k)) * nv;
                };
                try writeFloatLane(bits, dst, lane_index, @as(FloatT, out));
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opModf(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const x = try rt.results[try rt.it.next()].getValue();
    const whole_dst = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| for (0..lane_count) |lane_index| {
            const xv = try Value.readLane(.Float, bits, x, lane_index);
            const whole = @trunc(xv);
            try writeFloatLane(bits, dst, lane_index, xv - whole);
            try writeFloatLane(bits, whole_dst, lane_index, whole);
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opModfStruct(_: std.mem.Allocator, _: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const dst = try rt.results[id].getValue();
    const x = try rt.results[try rt.it.next()].getValue();

    switch (dst.*) {
        .Structure => |*s| {
            if (s.values.len < 2) return RuntimeError.InvalidSpirV;
            const lane_bits = try x.resolveLaneBitWidth();
            const lane_count = try x.resolveLaneCount();
            switch (lane_bits) {
                inline 16, 32, 64 => |bits| for (0..lane_count) |lane_index| {
                    const xv = try Value.readLane(.Float, bits, x, lane_index);
                    const whole = @trunc(xv);
                    try writeFloatLane(bits, &s.values[0], lane_index, xv - whole);
                    try writeFloatLane(bits, &s.values[1], lane_index, whole);
                },
                else => return RuntimeError.InvalidSpirV,
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn frexpScalar(comptime T: type, x: T) struct { sig: T, exp: i32 } {
    if (x == 0.0)
        return .{ .sig = 0.0, .exp = 0 };

    const exp = @as(i32, @intFromFloat(@floor(@log2(@abs(x))))) + 1;
    return .{ .sig = x / @exp2(@as(T, @floatFromInt(exp))), .exp = exp };
}

fn opFrexp(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const x = try rt.results[try rt.it.next()].getValue();
    const exp_dst = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| for (0..lane_count) |lane_index| {
            const result = frexpScalar(std.meta.Float(bits), try Value.readLane(.Float, bits, x, lane_index));
            try writeFloatLane(bits, dst, lane_index, result.sig);
            try writeIntegralLaneAsI32(exp_dst, lane_index, result.exp);
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opFrexpStruct(_: std.mem.Allocator, _: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const dst = try rt.results[id].getValue();
    const x = try rt.results[try rt.it.next()].getValue();

    switch (dst.*) {
        .Structure => |*s| {
            if (s.values.len < 2) return RuntimeError.InvalidSpirV;
            const lane_bits = try x.resolveLaneBitWidth();
            const lane_count = try x.resolveLaneCount();
            switch (lane_bits) {
                inline 16, 32, 64 => |bits| for (0..lane_count) |lane_index| {
                    const result = frexpScalar(std.meta.Float(bits), try Value.readLane(.Float, bits, x, lane_index));
                    try writeFloatLane(bits, &s.values[0], lane_index, result.sig);
                    try writeIntegralLaneAsI32(&s.values[1], lane_index, result.exp);
                },
                else => return RuntimeError.InvalidSpirV,
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opLdexp(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const x = try rt.results[try rt.it.next()].getValue();
    const exp_value = try rt.results[try rt.it.next()].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);
    const exp_lane_count = try exp_value.resolveLaneCount();

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| for (0..lane_count) |lane_index| {
            const exp_lane = if (exp_lane_count == 1) 0 else lane_index;
            const exponent = try readIntegralLaneAsI32(exp_value, exp_lane);
            const scale = @exp2(@as(std.meta.Float(bits), @floatFromInt(exponent)));
            try writeFloatLane(bits, dst, lane_index, try Value.readLane(.Float, bits, x, lane_index) * scale);
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

inline fn validateResultLaneShape(target_type: Result.TypeData, rt: *Runtime, comptime lane_bits: SpvWord, comptime lane_count: usize) RuntimeError!void {
    if (try Result.resolveLaneBitWidth(target_type, rt) != lane_bits) return RuntimeError.InvalidSpirV;
    if (try Result.resolveLaneCount(target_type) != @as(SpvWord, @intCast(lane_count))) return RuntimeError.InvalidSpirV;
}

inline fn validateValueLaneShape(value: *const Value, comptime lane_bits: SpvWord, comptime lane_count: usize) RuntimeError!void {
    if (try value.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;
    if (try value.resolveLaneCount() != @as(SpvWord, @intCast(lane_count))) return RuntimeError.InvalidSpirV;
}

fn PackNormEngine(comptime lanes: usize, comptime field_bits: u5, comptime kind: NormPackKind) type {
    return struct {
        fn op(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            try validateResultLaneShape(target_type, rt, 32, 1);

            const dst = try rt.results[id].getValue();
            const v = try rt.results[try rt.it.next()].getValue();
            try validateValueLaneShape(v, 32, lanes);

            var pack: u32 = 0;
            inline for (0..lanes) |lane_index| {
                const lane_shift: u5 = field_bits * @as(u5, @intCast(lane_index));
                const lane_bits = switch (kind) {
                    .Snorm => blk: {
                        const x = std.math.clamp(try Value.readLane(.Float, 32, v, lane_index), -1.0, 1.0);
                        const signed_value: std.meta.Int(.signed, field_bits) = @intCast(@as(i32, @intFromFloat(@round(x * snormScale(field_bits)))));
                        break :blk @as(u32, @bitCast(@as(i32, signed_value))) & fieldMask(field_bits);
                    },
                    .Unorm => blk: {
                        const x = std.math.clamp(try Value.readLane(.Float, 32, v, lane_index), 0.0, 1.0);
                        const unsigned_value: std.meta.Int(.unsigned, field_bits) = @intCast(@as(u32, @intFromFloat(@round(x * unormScale(field_bits)))));
                        break :blk @as(u32, unsigned_value);
                    },
                };
                pack |= lane_bits << lane_shift;
            }

            try writeInt32Bits(target_type, rt, dst, 0, pack);
        }
    };
}

fn UnpackNormEngine(comptime lanes: usize, comptime field_bits: u5, comptime kind: NormPackKind) type {
    return struct {
        fn op(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[target_type_id].getVariant()).Type;
            try validateResultLaneShape(target_type, rt, 32, lanes);

            const dst = try rt.results[id].getValue();
            const src = try rt.results[try rt.it.next()].getValue();
            try validateValueLaneShape(src, 32, 1);
            const p = try readInt32Bits(src, 0);

            inline for (0..lanes) |lane_index| {
                const lane_shift: u5 = field_bits * @as(u5, @intCast(lane_index));
                const raw: std.meta.Int(.unsigned, field_bits) = @truncate(p >> lane_shift);
                const value = switch (kind) {
                    .Snorm => blk: {
                        const signed_value: std.meta.Int(.signed, field_bits) = @bitCast(raw);
                        break :blk @max(@as(f32, @floatFromInt(signed_value)) / snormScale(field_bits), -1.0);
                    },
                    .Unorm => @as(f32, @floatFromInt(raw)) / unormScale(field_bits),
                };
                try writeFloatLane(32, dst, lane_index, value);
            }
        }
    };
}

fn opPackHalf2x16Engine(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    try validateResultLaneShape(target_type, rt, 32, 1);

    const dst = try rt.results[id].getValue();
    const v = try rt.results[try rt.it.next()].getValue();
    try validateValueLaneShape(v, 32, 2);

    const lo: u16 = @bitCast(@as(f16, @floatCast(try Value.readLane(.Float, 32, v, 0))));
    const hi: u16 = @bitCast(@as(f16, @floatCast(try Value.readLane(.Float, 32, v, 1))));
    try writeInt32Bits(target_type, rt, dst, 0, @as(u32, lo) | (@as(u32, hi) << 16));
}

fn opUnpackHalf2x16Engine(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    try validateResultLaneShape(target_type, rt, 32, 2);

    const dst = try rt.results[id].getValue();
    const src = try rt.results[try rt.it.next()].getValue();
    try validateValueLaneShape(src, 32, 1);
    const p = try readInt32Bits(src, 0);

    inline for (0..2) |lane_index| {
        const raw: u16 = @truncate(p >> @intCast(16 * lane_index));
        try writeFloatLane(32, dst, lane_index, @as(f32, @floatCast(@as(f16, @bitCast(raw)))));
    }
}

fn opPackDouble2x32Engine(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    try validateResultLaneShape(target_type, rt, 64, 1);

    const dst = try rt.results[id].getValue();
    const v = try rt.results[try rt.it.next()].getValue();
    try validateValueLaneShape(v, 32, 2);

    const lo = try readInt32Bits(v, 0);
    const hi = try readInt32Bits(v, 1);
    const bits = @as(u64, lo) | (@as(u64, hi) << 32);
    try writeFloatLane(64, dst, 0, @as(f64, @bitCast(bits)));
}

fn opUnpackDouble2x32Engine(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    try validateResultLaneShape(target_type, rt, 32, 2);

    const dst = try rt.results[id].getValue();
    const v = try rt.results[try rt.it.next()].getValue();
    try validateValueLaneShape(v, 64, 1);

    const bits: u64 = @bitCast(try Value.readLane(.Float, 64, v, 0));
    try writeIntLane(.UInt, 32, dst, 0, @truncate(bits));
    try writeIntLane(.UInt, 32, dst, 1, @truncate(bits >> 32));
}

fn snormScale(comptime field_bits: u5) f32 {
    return @as(f32, @floatFromInt((@as(u32, 1) << (field_bits - 1)) - 1));
}

fn unormScale(comptime field_bits: u5) f32 {
    return @as(f32, @floatFromInt((@as(u32, 1) << field_bits) - 1));
}

fn fieldMask(comptime field_bits: u5) u32 {
    return (@as(u32, 1) << field_bits) - 1;
}

fn readMatrixElement(comptime bits: u32, matrix: *const Value, row: usize, col: usize) RuntimeError!std.meta.Float(bits) {
    return switch (matrix.*) {
        .Matrix => |columns| Value.readLane(.Float, bits, &columns[col], row),
        else => RuntimeError.InvalidSpirV,
    };
}

fn writeMatrixElement(comptime bits: u32, matrix: *Value, row: usize, col: usize, value: std.meta.Float(bits)) RuntimeError!void {
    return switch (matrix.*) {
        .Matrix => |columns| writeFloatLane(bits, &columns[col], row, value),
        else => RuntimeError.InvalidSpirV,
    };
}

fn matrixSize(matrix: *const Value) RuntimeError!usize {
    return switch (matrix.*) {
        .Matrix => |columns| blk: {
            if (columns.len == 0 or columns.len > 4) return RuntimeError.InvalidSpirV;
            const rows = try columns[0].resolveLaneCount();
            if (rows != @as(SpvWord, @intCast(columns.len))) return RuntimeError.InvalidSpirV;
            break :blk columns.len;
        },
        else => RuntimeError.InvalidSpirV,
    };
}

fn determinant(comptime bits: u32, src: *const Value, n: usize) RuntimeError!std.meta.Float(bits) {
    var m: [16]std.meta.Float(bits) = undefined;
    for (0..n) |row| {
        for (0..n) |col| {
            m[row * n + col] = try readMatrixElement(bits, src, row, col);
        }
    }

    var det: std.meta.Float(bits) = 1.0;
    for (0..n) |i| {
        var pivot = i;
        var pivot_abs = @abs(m[i * n + i]);
        var r = i + 1;
        while (r < n) : (r += 1) {
            const candidate_abs = @abs(m[r * n + i]);
            if (candidate_abs > pivot_abs) {
                pivot = r;
                pivot_abs = candidate_abs;
            }
        }
        if (pivot_abs == 0.0) return 0.0;
        if (pivot != i) {
            for (0..n) |col| {
                const tmp = m[i * n + col];
                m[i * n + col] = m[pivot * n + col];
                m[pivot * n + col] = tmp;
            }
            det = -det;
        }
        const pivot_value = m[i * n + i];
        det *= pivot_value;
        r = i + 1;
        while (r < n) : (r += 1) {
            const factor = m[r * n + i] / pivot_value;
            var col = i + 1;
            while (col < n) : (col += 1) {
                m[r * n + col] -= factor * m[i * n + col];
            }
        }
    }
    return det;
}

fn opDeterminant(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const src = try rt.results[try rt.it.next()].getValue();
    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const n = try matrixSize(src);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| (try Value.getPrimitiveField(.Float, bits, dst)).* = try determinant(bits, src, n),
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opMatrixInverse(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const src = try rt.results[try rt.it.next()].getValue();
    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const n = try matrixSize(src);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            var aug: [32]std.meta.Float(bits) = undefined;
            const width = n * 2;
            for (0..n) |row| {
                for (0..n) |col| {
                    aug[row * width + col] = try readMatrixElement(bits, src, row, col);
                    aug[row * width + n + col] = if (row == col) 1.0 else 0.0;
                }
            }

            for (0..n) |i| {
                var pivot = i;
                var pivot_abs = @abs(aug[i * width + i]);
                var r = i + 1;
                while (r < n) : (r += 1) {
                    const candidate_abs = @abs(aug[r * width + i]);
                    if (candidate_abs > pivot_abs) {
                        pivot = r;
                        pivot_abs = candidate_abs;
                    }
                }
                if (pivot_abs == 0.0) return RuntimeError.InvalidSpirV;
                if (pivot != i) {
                    for (0..width) |col| {
                        const tmp = aug[i * width + col];
                        aug[i * width + col] = aug[pivot * width + col];
                        aug[pivot * width + col] = tmp;
                    }
                }

                const pivot_value = aug[i * width + i];
                for (0..width) |col| aug[i * width + col] /= pivot_value;

                for (0..n) |row| {
                    if (row == i) continue;
                    const factor = aug[row * width + i];
                    for (0..width) |col| {
                        aug[row * width + col] -= factor * aug[i * width + col];
                    }
                }
            }

            for (0..n) |row| {
                for (0..n) |col| {
                    try writeMatrixElement(bits, dst, row, col, aug[row * width + n + col]);
                }
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opInterpolateAt(_: std.mem.Allocator, target_type_id: SpvWord, id: SpvWord, opcode: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[target_type_id].getVariant()).Type;
    const dst = try rt.results[id].getValue();
    const interpolant = try getPointeeOrSelf(try rt.results[try rt.it.next()].getValue());

    if (opcode == @intFromEnum(ext.GLSLOp.InterpolateAtSample) or opcode == @intFromEnum(ext.GLSLOp.InterpolateAtOffset)) {
        _ = try rt.it.next();
    }

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);
    if (lane_bits != 32) return RuntimeError.InvalidSpirV;

    for (0..lane_count) |lane_index| {
        try writeFloatLane(32, dst, lane_index, try Value.readLane(.Float, 32, interpolant, lane_index));
    }
}
