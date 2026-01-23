const std = @import("std");
const spv = @import("spv.zig");

const Module = @import("Module.zig");
const Runtime = @import("Runtime.zig");
const Result = @import("Result.zig");
const WordIterator = @import("WordIterator.zig");

const RuntimeError = Runtime.RuntimeError;

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

// OpExtInstImport
// OpExtInst Sin
// OpExtInst Cos
// OpExtInst Length
// OpExtInst Normalize
// OpExtInst FMax

const ValueType = enum {
    Bool,
    Float,
    SInt,
    UInt,
};

const MathOp = enum {
    Add,
    Div,
    MatrixTimesMatrix,
    MatrixTimesScalar,
    MatrixTimesVector,
    Mod,
    Mul,
    Rem,
    Sub,
    VectorTimesMatrix,
    VectorTimesScalar,
};

const CondOp = enum {
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    NotEqual,
    LogicalEqual,
    LogicalNotEqual,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
};

const BitOp = enum {
    BitCount,
    BitFieldInsert,
    BitFieldSExtract,
    BitFieldUExtract,
    BitReverse,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Not,
    ShiftLeft,
    ShiftRight,
    ShiftRightArithmetic,
};

pub const OpCodeFunc = *const fn (std.mem.Allocator, SpvWord, *Runtime) RuntimeError!void;

pub const SetupDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeFunc).init(.{
        .BitCount = autoSetupConstant,
        .BitFieldInsert = autoSetupConstant,
        .BitFieldSExtract = autoSetupConstant,
        .BitFieldUExtract = autoSetupConstant,
        .BitReverse = autoSetupConstant,
        .Bitcast = autoSetupConstant,
        .BitwiseAnd = autoSetupConstant,
        .BitwiseOr = autoSetupConstant,
        .BitwiseXor = autoSetupConstant,
        .Capability = opCapability,
        .CompositeConstruct = autoSetupConstant,
        .Constant = opConstant,
        .ConvertFToS = autoSetupConstant,
        .ConvertFToU = autoSetupConstant,
        .ConvertPtrToU = autoSetupConstant,
        .ConvertSToF = autoSetupConstant,
        .ConvertUToF = autoSetupConstant,
        .ConvertUToPtr = autoSetupConstant,
        .Decorate = opDecorate,
        .Dot = autoSetupConstant,
        .EntryPoint = opEntryPoint,
        .ExecutionMode = opExecutionMode,
        .FAdd = autoSetupConstant,
        .FConvert = autoSetupConstant,
        .FDiv = autoSetupConstant,
        .FMod = autoSetupConstant,
        .FMul = autoSetupConstant,
        .FOrdEqual = autoSetupConstant,
        .FOrdGreaterThan = autoSetupConstant,
        .FOrdGreaterThanEqual = autoSetupConstant,
        .FOrdLessThan = autoSetupConstant,
        .FOrdLessThanEqual = autoSetupConstant,
        .FOrdNotEqual = autoSetupConstant,
        .FSub = autoSetupConstant,
        .FUnordEqual = autoSetupConstant,
        .FUnordGreaterThan = autoSetupConstant,
        .FUnordGreaterThanEqual = autoSetupConstant,
        .FUnordLessThan = autoSetupConstant,
        .FUnordLessThanEqual = autoSetupConstant,
        .FUnordNotEqual = autoSetupConstant,
        .Function = opFunction,
        .FunctionCall = autoSetupConstant,
        .FunctionEnd = opFunctionEnd,
        .FunctionParameter = opFunctionParameter,
        .IAdd = autoSetupConstant,
        .IEqual = autoSetupConstant,
        .IMul = autoSetupConstant,
        .INotEqual = autoSetupConstant,
        .ISub = autoSetupConstant,
        .Label = opLabel,
        .Load = autoSetupConstant,
        .LogicalAnd = autoSetupConstant,
        .LogicalEqual = autoSetupConstant,
        .LogicalNot = autoSetupConstant,
        .LogicalNotEqual = autoSetupConstant,
        .LogicalOr = autoSetupConstant,
        .MatrixTimesMatrix = autoSetupConstant,
        .MatrixTimesScalar = autoSetupConstant,
        .MatrixTimesVector = autoSetupConstant,
        .MemberDecorate = opDecorateMember,
        .MemberName = opMemberName,
        .MemoryModel = opMemoryModel,
        .Name = opName,
        .Not = autoSetupConstant,
        .QuantizeToF16 = autoSetupConstant,
        .SConvert = autoSetupConstant,
        .SDiv = autoSetupConstant,
        .SGreaterThan = autoSetupConstant,
        .SGreaterThanEqual = autoSetupConstant,
        .SLessThan = autoSetupConstant,
        .SLessThanEqual = autoSetupConstant,
        .SMod = autoSetupConstant,
        .SatConvertSToU = autoSetupConstant,
        .SatConvertUToS = autoSetupConstant,
        .ShiftLeftLogical = autoSetupConstant,
        .ShiftRightArithmetic = autoSetupConstant,
        .ShiftRightLogical = autoSetupConstant,
        .Source = opSource,
        .SourceExtension = opSourceExtension,
        .TypeArray = opTypeArray,
        .TypeBool = opTypeBool,
        .TypeFloat = opTypeFloat,
        .TypeFunction = opTypeFunction,
        .TypeInt = opTypeInt,
        .TypeMatrix = opTypeMatrix,
        .TypePointer = opTypePointer,
        .TypeStruct = opTypeStruct,
        .TypeVector = opTypeVector,
        .TypeVoid = opTypeVoid,
        .UConvert = autoSetupConstant,
        .UDiv = autoSetupConstant,
        .UGreaterThan = autoSetupConstant,
        .UGreaterThanEqual = autoSetupConstant,
        .ULessThan = autoSetupConstant,
        .ULessThanEqual = autoSetupConstant,
        .UMod = autoSetupConstant,
        .Variable = opVariable,
        .VectorTimesMatrix = autoSetupConstant,
        .VectorTimesScalar = autoSetupConstant,
    });
};

/// Not an EnumMap as it is way too slow for this purpose
pub var runtime_dispatcher = [_]?OpCodeFunc{null} ** spv.SpvOpMaxValue;

pub fn initRuntimeDispatcher() void {
    // zig fmt: off
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AccessChain)]            = opAccessChain;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BitCount)]               = BitEngine(.UInt, .BitCount).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BitFieldInsert)]         = BitEngine(.UInt, .BitFieldInsert).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BitFieldSExtract)]       = BitEngine(.SInt, .BitFieldSExtract).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BitFieldUExtract)]       = BitEngine(.UInt, .BitFieldUExtract).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BitReverse)]             = BitEngine(.UInt, .BitReverse).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Bitcast)]                = opBitcast;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BitwiseAnd)]             = BitEngine(.UInt, .BitwiseAnd).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BitwiseOr)]              = BitEngine(.UInt, .BitwiseOr).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BitwiseXor)]             = BitEngine(.UInt, .BitwiseXor).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Branch)]                 = opBranch;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.BranchConditional)]      = opBranchConditional;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.CompositeConstruct)]     = opCompositeConstruct;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.CompositeExtract)]       = opCompositeExtract;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertFToS)]            = ConversionEngine(.Float, .SInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertFToU)]            = ConversionEngine(.Float, .UInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertSToF)]            = ConversionEngine(.SInt, .Float).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertUToF)]            = ConversionEngine(.UInt, .Float).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.CopyMemory)]             = opCopyMemory;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Dot)]                    = opDot;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FAdd)]                   = MathEngine(.Float, .Add).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FConvert)]               = ConversionEngine(.Float, .Float).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FDiv)]                   = MathEngine(.Float, .Div).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FMod)]                   = MathEngine(.Float, .Mod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FMul)]                   = MathEngine(.Float, .Mul).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdEqual)]              = CondEngine(.Float, .Equal).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdGreaterThan)]        = CondEngine(.Float, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdGreaterThanEqual)]   = CondEngine(.Float, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdLessThan)]           = CondEngine(.Float, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdLessThanEqual)]      = CondEngine(.Float, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdNotEqual)]           = CondEngine(.Float, .NotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FSub)]                   = MathEngine(.Float, .Sub).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordEqual)]            = CondEngine(.Float, .Equal).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordGreaterThan)]      = CondEngine(.Float, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordGreaterThanEqual)] = CondEngine(.Float, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordLessThan)]         = CondEngine(.Float, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordLessThanEqual)]    = CondEngine(.Float, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordNotEqual)]         = CondEngine(.Float, .NotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FunctionCall)]           = opFunctionCall;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IAdd)]                   = MathEngine(.SInt, .Add).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IEqual)]                 = CondEngine(.SInt, .Equal).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IMul)]                   = MathEngine(.SInt, .Mul).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.INotEqual)]              = CondEngine(.SInt, .NotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ISub)]                   = MathEngine(.SInt, .Sub).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Kill)]                   = opKill;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Load)]                   = opLoad;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalAnd)]             = CondEngine(.Float, .LogicalAnd).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalEqual)]           = CondEngine(.Float, .LogicalEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalNot)]             = CondEngine(.Float, .LogicalNot).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalNotEqual)]        = CondEngine(.Float, .LogicalNotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalOr)]              = CondEngine(.Float, .LogicalOr).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesMatrix)]      = MathEngine(.Float, .MatrixTimesMatrix).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesScalar)]      = MathEngine(.Float, .MatrixTimesScalar).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesVector)]      = MathEngine(.Float, .MatrixTimesVector).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Not)]                    = BitEngine(.UInt, .Not).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Return)]                 = opReturn;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ReturnValue)]            = opReturnValue;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SConvert)]               = ConversionEngine(.SInt, .SInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SDiv)]                   = MathEngine(.SInt, .Div).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SGreaterThan)]           = CondEngine(.SInt, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SGreaterThanEqual)]      = CondEngine(.SInt, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SLessThan)]              = CondEngine(.SInt, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SLessThanEqual)]         = CondEngine(.SInt, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SMod)]                   = MathEngine(.SInt, .Mod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ShiftLeftLogical)]       = BitEngine(.UInt, .ShiftLeft).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ShiftRightArithmetic)]   = BitEngine(.SInt, .ShiftRightArithmetic).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ShiftRightLogical)]      = BitEngine(.UInt, .ShiftRight).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Store)]                  = opStore;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UConvert)]               = ConversionEngine(.UInt, .UInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UDiv)]                   = MathEngine(.UInt, .Div).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UGreaterThan)]           = CondEngine(.UInt, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UGreaterThanEqual)]      = CondEngine(.UInt, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ULessThan)]              = CondEngine(.UInt, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ULessThanEqual)]         = CondEngine(.UInt, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UMod)]                   = MathEngine(.UInt, .Mod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorTimesMatrix)]      = MathEngine(.Float, .VectorTimesMatrix).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorTimesScalar)]      = MathEngine(.Float, .VectorTimesScalar).op;
    // zig fmt: on
}

fn BitEngine(comptime T: ValueType, comptime Op: BitOp) type {
    if (T == .Float) @compileError("Invalid value type");
    return struct {
        fn op(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const value = try rt.results[try rt.it.next()].getValue();
            const op1_value = try rt.results[try rt.it.next()].getValue();
            const op2_value: ?*Result.Value = switch (Op) {
                .Not, .BitCount, .BitReverse => null,
                else => try rt.results[try rt.it.next()].getValue(),
            };

            const size = sw: switch (target_type) {
                .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => 32,
                .Int => |i| i.bit_length,
                else => return RuntimeError.InvalidSpirV,
            };

            const operator = struct {
                inline fn bitMask(bits: u64) u64 {
                    return if (bits >= 32) ~@as(u64, 0) else (@as(u64, 0x1) << @intCast(bits)) - 1;
                }

                inline fn bitInsert(comptime TT: type, base: TT, insert: TT, offset: u64, count: u64) TT {
                    const mask: TT = @intCast(bitMask(count) << @intCast(offset));
                    return @as(TT, @intCast((base & ~mask) | ((insert << @intCast(offset)) & mask)));
                }

                inline fn bitExtract(comptime TT: type, v: TT, offset: TT, count: u64) TT {
                    return (v >> @intCast(offset)) & @as(TT, @intCast(bitMask(count)));
                }

                fn operation(comptime TT: type, rt2: *Runtime, op1: TT, op2: ?TT) RuntimeError!TT {
                    switch (Op) {
                        .BitCount => return @bitSizeOf(TT),
                        .BitReverse => return @bitReverse(op1),
                        .Not => return ~op1,
                        else => {},
                    }
                    return if (op2) |v2|
                        switch (Op) {
                            .BitFieldInsert => blk: {
                                const offset = try rt2.results[try rt2.it.next()].getValue();
                                const count = try rt2.results[try rt2.it.next()].getValue();
                                break :blk bitInsert(TT, op1, v2, offset.Int.uint64, count.Int.uint64);
                            },
                            .BitFieldSExtract => blk: {
                                if (T == .UInt) return RuntimeError.InvalidSpirV;
                                const count = try rt2.results[try rt2.it.next()].getValue();
                                break :blk bitExtract(TT, op1, v2, count.Int.uint64);
                            },
                            .BitFieldUExtract => blk: {
                                if (T == .SInt) return RuntimeError.InvalidSpirV;
                                const count = try rt2.results[try rt2.it.next()].getValue();
                                break :blk bitExtract(TT, op1, v2, count.Int.uint64);
                            },
                            .BitwiseAnd => op1 & v2,
                            .BitwiseOr => op1 | v2,
                            .BitwiseXor => op1 ^ v2,
                            .ShiftLeft => op1 << @intCast(v2),
                            .ShiftRight, .ShiftRightArithmetic => op1 >> @intCast(v2),
                            else => return RuntimeError.InvalidSpirV,
                        }
                    else
                        RuntimeError.InvalidSpirV;
                }

                fn process(rt2: *Runtime, bit_count: SpvWord, v: *Result.Value, op1_v: *const Result.Value, op2_v: ?*const Result.Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |i| {
                            (try getValuePrimitiveField(T, i, v)).* = try operation(
                                getValuePrimitiveFieldType(T, i),
                                rt2,
                                (try getValuePrimitiveField(T, i, @constCast(op1_v))).*,
                                if (op2_v) |v2|
                                    (try getValuePrimitiveField(T, i, @constCast(v2))).*
                                else
                                    null,
                            );
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }
            };

            switch (value.*) {
                .Int => try operator.process(rt, size, value, op1_value, op2_value),
                .Vector => |vec| for (vec, op1_value.Vector, 0..) |*val, op1_v, i|
                    try operator.process(rt, size, val, &op1_v, if (op2_value) |op2_v| &op2_v.Vector[i] else null),
                // No bit manipulation on VectorXf32
                .Vector4i32 => |*vec| inline for (0..4) |i| {
                    vec[i] = try operator.operation(i32, rt, op1_value.Vector4i32[i], if (op2_value) |op2_v| op2_v.Vector4i32[i] else null);
                },
                .Vector3i32 => |*vec| inline for (0..3) |i| {
                    vec[i] = try operator.operation(i32, rt, op1_value.Vector3i32[i], if (op2_value) |op2_v| op2_v.Vector3i32[i] else null);
                },
                .Vector2i32 => |*vec| inline for (0..2) |i| {
                    vec[i] = try operator.operation(i32, rt, op1_value.Vector2i32[i], if (op2_value) |op2_v| op2_v.Vector2i32[i] else null);
                },
                .Vector4u32 => |*vec| inline for (0..4) |i| {
                    vec[i] = try operator.operation(u32, rt, op1_value.Vector4u32[i], if (op2_value) |op2_v| op2_v.Vector4u32[i] else null);
                },
                .Vector3u32 => |*vec| inline for (0..3) |i| {
                    vec[i] = try operator.operation(u32, rt, op1_value.Vector3u32[i], if (op2_value) |op2_v| op2_v.Vector3u32[i] else null);
                },
                .Vector2u32 => |*vec| inline for (0..2) |i| {
                    vec[i] = try operator.operation(u32, rt, op1_value.Vector2u32[i], if (op2_value) |op2_v| op2_v.Vector2u32[i] else null);
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn CondEngine(comptime T: ValueType, comptime Op: CondOp) type {
    return struct {
        fn op(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            sw: switch ((try rt.results[try rt.it.next()].getVariant()).Type) {
                .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
                .Bool => {},
                else => return RuntimeError.InvalidSpirV,
            }

            const value = try rt.results[try rt.it.next()].getValue();
            const op1_result = &rt.results[try rt.it.next()];
            const op1_type = try op1_result.getValueTypeWord();
            const op1_value = try op1_result.getValue();
            const op2_value: ?*Result.Value = switch (Op) {
                .LogicalNot => null,
                else => try rt.results[try rt.it.next()].getValue(),
            };

            const size = sw: switch ((try rt.results[op1_type].getVariant()).Type) {
                .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => 32,
                .Float => |f| if (T == .Float) f.bit_length else return RuntimeError.InvalidSpirV,
                .Int => |i| if (T == .SInt or T == .UInt) i.bit_length else return RuntimeError.InvalidSpirV,
                else => return RuntimeError.InvalidSpirV,
            };

            const operator = struct {
                fn operation(comptime TT: type, op1: TT, op2: ?TT) RuntimeError!bool {
                    return switch (Op) {
                        .Equal, .LogicalEqual => op1 == op2 orelse return RuntimeError.InvalidSpirV,
                        .NotEqual, .LogicalNotEqual => op1 != op2 orelse return RuntimeError.InvalidSpirV,
                        .Greater => op1 > op2 orelse return RuntimeError.InvalidSpirV,
                        .GreaterEqual => op1 >= op2 orelse return RuntimeError.InvalidSpirV,
                        .Less => op1 < op2 orelse return RuntimeError.InvalidSpirV,
                        .LessEqual => op1 <= op2 orelse return RuntimeError.InvalidSpirV,
                        .LogicalAnd => (op1 != @as(TT, 0)) and ((op2 orelse return RuntimeError.InvalidSpirV) != @as(TT, 0)),
                        .LogicalOr => (op1 != @as(TT, 0)) or ((op2 orelse return RuntimeError.InvalidSpirV) != @as(TT, 0)),
                        .LogicalNot => (op1 == @as(TT, 0)),
                    };
                }

                fn process(bit_count: SpvWord, v: *Result.Value, op1_v: *const Result.Value, op2_v: ?*const Result.Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |i| {
                            if (i == 8 and T == .Float) { // No f8
                                return RuntimeError.InvalidSpirV;
                            }
                            v.Bool = try operation(
                                getValuePrimitiveFieldType(T, i),
                                (try getValuePrimitiveField(T, i, @constCast(op1_v))).*,
                                if (op2_v) |val| (try getValuePrimitiveField(T, i, @constCast(val))).* else null,
                            );
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }
            };

            switch (value.*) {
                .Bool => try operator.process(size, value, op1_value, op2_value),
                .Vector => |vec| for (vec, op1_value.Vector, 0..) |*val, op1_v, i| {
                    try operator.process(size, val, &op1_v, if (op2_value) |op2_v| &op2_v.Vector[i] else null);
                },
                // No Vector specializations for booleans
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn ConversionEngine(comptime From: ValueType, comptime To: ValueType) type {
    return struct {
        fn op(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const value = try rt.results[try rt.it.next()].getValue();
            const op_result = &rt.results[try rt.it.next()];
            const op_type = try op_result.getValueTypeWord();
            const op_value = try op_result.getValue();

            const from_size = sw: switch ((try rt.results[op_type].getVariant()).Type) {
                .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => 32,
                .Float => |f| if (From == .Float) f.bit_length else return RuntimeError.InvalidSpirV,
                .Int => |i| if (From == .SInt or From == .UInt) i.bit_length else return RuntimeError.InvalidSpirV,
                else => return RuntimeError.InvalidSpirV,
            };

            const to_size = sw: switch (target_type) {
                .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => 32,
                .Float => |f| if (To == .Float) f.bit_length else return RuntimeError.InvalidSpirV,
                .Int => |i| if (To == .SInt or To == .UInt) i.bit_length else return RuntimeError.InvalidSpirV,
                else => return RuntimeError.InvalidSpirV,
            };

            const operator = struct {
                fn process(from_bit_count: SpvWord, to_bit_count: SpvWord, to: *Result.Value, from: *Result.Value) RuntimeError!void {
                    switch (to_bit_count) {
                        inline 8, 16, 32, 64 => |i| {
                            if (i == 8 and To == .Float) {
                                return RuntimeError.InvalidSpirV; // No f8
                            }

                            const ToType = getValuePrimitiveFieldType(To, i);
                            (try getValuePrimitiveField(To, i, to)).* = std.math.lossyCast(
                                ToType,
                                switch (from_bit_count) {
                                    inline 8, 16, 32, 64 => |j| blk: {
                                        if (j == 8 and From == .Float) {
                                            return RuntimeError.InvalidSpirV; // Same
                                        }
                                        break :blk (try getValuePrimitiveField(From, j, from)).*;
                                    },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                            );
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }

                fn processVecSpe(comptime T: type, from_bit_count: SpvWord, from: *Result.Value, index: usize) RuntimeError!T {
                    return switch (from.*) {
                        .Vector3f32 => |vec| std.math.lossyCast(T, vec[index]),
                        .Vector2f32 => |vec| std.math.lossyCast(T, vec[index]),
                        .Vector4i32 => |vec| std.math.lossyCast(T, vec[index]),
                        .Vector3i32 => |vec| std.math.lossyCast(T, vec[index]),
                        .Vector2i32 => |vec| std.math.lossyCast(T, vec[index]),
                        .Vector4u32 => |vec| std.math.lossyCast(T, vec[index]),
                        .Vector3u32 => |vec| std.math.lossyCast(T, vec[index]),
                        .Vector2u32 => |vec| std.math.lossyCast(T, vec[index]),
                        inline else => switch (from_bit_count) {
                            inline 8, 16, 32, 64 => |i| std.math.lossyCast(T, blk: {
                                if (i == 8 and From == .Float) {
                                    return RuntimeError.InvalidSpirV;
                                }
                                break :blk (try getValuePrimitiveField(From, i, from)).*;
                            }),
                            else => return RuntimeError.InvalidSpirV,
                        },
                    };
                }
            };

            switch (value.*) {
                .Float => if (To == .Float) try operator.process(from_size, to_size, value, op_value) else return RuntimeError.InvalidSpirV,
                .Int => if (To == .SInt or To == .UInt) try operator.process(from_size, to_size, value, op_value) else return RuntimeError.InvalidSpirV,
                .Vector => |vec| for (vec, op_value.Vector) |*val, *op_v| try operator.process(from_size, to_size, val, op_v),
                .Vector4f32 => |*vec| inline for (0..4) |i| {
                    vec[i] = try operator.processVecSpe(f32, from_size, op_value, i);
                },
                .Vector3f32 => |*vec| inline for (0..3) |i| {
                    vec[i] = try operator.processVecSpe(f32, from_size, op_value, i);
                },
                .Vector2f32 => |*vec| inline for (0..2) |i| {
                    vec[i] = try operator.processVecSpe(f32, from_size, op_value, i);
                },
                .Vector4i32 => |*vec| inline for (0..4) |i| {
                    vec[i] = try operator.processVecSpe(i32, from_size, op_value, i);
                },
                .Vector3i32 => |*vec| inline for (0..3) |i| {
                    vec[i] = try operator.processVecSpe(i32, from_size, op_value, i);
                },
                .Vector2i32 => |*vec| inline for (0..2) |i| {
                    vec[i] = try operator.processVecSpe(i32, from_size, op_value, i);
                },
                .Vector4u32 => |*vec| inline for (0..4) |i| {
                    vec[i] = try operator.processVecSpe(u32, from_size, op_value, i);
                },
                .Vector3u32 => |*vec| inline for (0..3) |i| {
                    vec[i] = try operator.processVecSpe(u32, from_size, op_value, i);
                },
                .Vector2u32 => |*vec| inline for (0..2) |i| {
                    vec[i] = try operator.processVecSpe(u32, from_size, op_value, i);
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn MathEngine(comptime T: ValueType, comptime Op: MathOp) type {
    return struct {
        fn op(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const value = try rt.results[try rt.it.next()].getValue();
            const op1_value = try rt.results[try rt.it.next()].getValue();
            const op2_value = try rt.results[try rt.it.next()].getValue();

            const size = sw: switch (target_type) {
                .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => 32,
                .Float => |f| if (T == .Float) f.bit_length else return RuntimeError.InvalidSpirV,
                .Int => |i| if (T == .SInt or T == .UInt) i.bit_length else return RuntimeError.InvalidSpirV,
                else => return RuntimeError.InvalidSpirV,
            };

            const operator = struct {
                fn operation(comptime TT: type, op1: TT, op2: TT) RuntimeError!TT {
                    return switch (Op) {
                        .Add => if (@typeInfo(TT) == .int) @addWithOverflow(op1, op2)[0] else op1 + op2,
                        .Sub => if (@typeInfo(TT) == .int) @subWithOverflow(op1, op2)[0] else op1 - op2,
                        .Mul => if (@typeInfo(TT) == .int) @mulWithOverflow(op1, op2)[0] else op1 * op2,
                        .Div => blk: {
                            if (op2 == 0) return RuntimeError.DivisionByZero;
                            break :blk if (@typeInfo(TT) == .int) @divTrunc(op1, op2) else op1 / op2;
                        },
                        .Mod => if (op2 == 0) return RuntimeError.DivisionByZero else @mod(op1, op2),
                        .Rem => if (op2 == 0) return RuntimeError.DivisionByZero else @rem(op1, op2),
                        else => return RuntimeError.InvalidSpirV,
                    };
                }

                fn process(bit_count: SpvWord, v: *Result.Value, op1_v: *const Result.Value, op2_v: *const Result.Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |i| {
                            if (i == 8 and T == .Float) { // No f8
                                return RuntimeError.InvalidSpirV;
                            }
                            (try getValuePrimitiveField(T, i, v)).* = try operation(
                                getValuePrimitiveFieldType(T, i),
                                (try getValuePrimitiveField(T, i, @constCast(op1_v))).*,
                                (try getValuePrimitiveField(T, i, @constCast(op2_v))).*,
                            );
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }
            };

            switch (value.*) {
                .Float => if (T == .Float) try operator.process(size, value, op1_value, op2_value) else return RuntimeError.InvalidSpirV,
                .Int => if (T == .SInt or T == .UInt) try operator.process(size, value, op1_value, op2_value) else return RuntimeError.InvalidSpirV,
                .Vector => |vec| for (vec, op1_value.Vector, 0..) |*val, op1_v, i| {
                    switch (Op) {
                        .VectorTimesScalar => try operator.process(size, val, &op1_v, op2_value),
                        else => try operator.process(size, val, &op1_v, &op2_value.Vector[i]),
                    }
                },
                .Vector4f32 => |*vec| inline for (0..4) |i| {
                    switch (Op) {
                        .VectorTimesScalar => vec[i] = op1_value.Vector4f32[i] * op2_value.Float.float32,
                        else => vec[i] = try operator.operation(f32, op1_value.Vector4f32[i], op2_value.Vector4f32[i]),
                    }
                },
                .Vector3f32 => |*vec| inline for (0..3) |i| {
                    switch (Op) {
                        .VectorTimesScalar => vec[i] = op1_value.Vector3f32[i] * op2_value.Float.float32,
                        else => vec[i] = try operator.operation(f32, op1_value.Vector3f32[i], op2_value.Vector3f32[i]),
                    }
                },
                .Vector2f32 => |*vec| inline for (0..2) |i| {
                    switch (Op) {
                        .VectorTimesScalar => vec[i] = op1_value.Vector2f32[i] * op2_value.Float.float32,
                        else => vec[i] = try operator.operation(f32, op1_value.Vector2f32[i], op2_value.Vector2f32[i]),
                    }
                },
                .Vector4i32 => |*vec| inline for (0..4) |i| {
                    vec[i] = try operator.operation(i32, op1_value.Vector4i32[i], op2_value.Vector4i32[i]);
                },
                .Vector3i32 => |*vec| inline for (0..3) |i| {
                    vec[i] = try operator.operation(i32, op1_value.Vector3i32[i], op2_value.Vector3i32[i]);
                },
                .Vector2i32 => |*vec| inline for (0..2) |i| {
                    vec[i] = try operator.operation(i32, op1_value.Vector2i32[i], op2_value.Vector2i32[i]);
                },
                .Vector4u32 => |*vec| inline for (0..4) |i| {
                    vec[i] = try operator.operation(u32, op1_value.Vector4u32[i], op2_value.Vector4u32[i]);
                },
                .Vector3u32 => |*vec| inline for (0..3) |i| {
                    vec[i] = try operator.operation(u32, op1_value.Vector3u32[i], op2_value.Vector3u32[i]);
                },
                .Vector2u32 => |*vec| inline for (0..2) |i| {
                    vec[i] = try operator.operation(u32, op1_value.Vector2u32[i], op2_value.Vector2u32[i]);
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn addDecoration(allocator: std.mem.Allocator, rt: *Runtime, target: SpvWord, decoration_type: spv.SpvDecoration, member: ?SpvWord) RuntimeError!void {
    var decoration = rt.mod.results[target].decorations.addOne(allocator) catch return RuntimeError.OutOfMemory;
    decoration.rtype = decoration_type;
    decoration.index = if (member) |memb| memb else 0;

    switch (decoration_type) {
        .SpecId,
        .ArrayStride,
        .MatrixStride,
        .BuiltIn,
        .UniformId,
        .Stream,
        .Location,
        .Component,
        .Index,
        .Binding,
        .DescriptorSet,
        .Offset,
        .XfbBuffer,
        .XfbStride,
        .FuncParamAttr,
        .FPRoundingMode,
        .FPFastMathMode,
        .InputAttachmentIndex,
        .Alignment,
        .MaxByteOffset,
        .AlignmentId,
        .MaxByteOffsetId,
        .SecondaryViewportRelativeNV,
        .CounterBuffer,
        .UserSemantic,
        .UserTypeGOOGLE,
        => {
            decoration.literal_1 = try rt.it.next();
            decoration.literal_2 = null;
        },
        .LinkageAttributes => {
            decoration.literal_1 = try rt.it.next();
            decoration.literal_2 = try rt.it.next();
        },
        else => {},
    }
}

fn autoSetupConstant(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try setupConstant(allocator, rt);
}

fn opBitcast(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const to_value = try rt.results[try rt.it.next()].getValue();
    const from_value = try rt.results[try rt.it.next()].getValue();

    const caster = struct {
        /// Asumes that values passed are primitives ints or floats
        fn cast(to: *Result.Value, from: *const Result.Value) RuntimeError!void {
            const from_bytes: u64 = switch (from.*) {
                .Float => |f| @bitCast(f.float64),
                .Int => |i| i.uint64,
                else => return RuntimeError.InvalidSpirV,
            };

            switch (to.*) {
                .Float => |*f| f.float64 = @bitCast(from_bytes),
                .Int => |*i| i.uint64 = from_bytes,
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };

    switch (to_value.*) {
        .Int, .Float => try caster.cast(to_value, from_value),
        .Vector => |vec| for (vec, from_value.Vector) |*t, *f| try caster.cast(t, f),
        // TODO: vectors specializations
        else => return RuntimeError.InvalidSpirV,
    }
}

fn copyValue(dst: *Result.Value, src: *const Result.Value) void {
    if (src.getCompositeDataOrNull()) |src_slice| {
        if (dst.getCompositeDataOrNull()) |dst_slice| {
            for (0..@min(dst_slice.len, src_slice.len)) |i| {
                copyValue(&dst_slice[i], &src_slice[i]);
            }
        } else {
            unreachable;
        }
    } else {
        dst.* = src.*;
    }
}

fn getValuePrimitiveField(comptime T: ValueType, comptime BitCount: SpvWord, v: *Result.Value) RuntimeError!*getValuePrimitiveFieldType(T, BitCount) {
    return switch (T) {
        .Bool => &v.Bool,
        .Float => switch (BitCount) {
            inline 16, 32, 64 => |i| &@field(v.Float, std.fmt.comptimePrint("float{}", .{i})),
            else => return RuntimeError.InvalidSpirV,
        },
        .SInt => switch (BitCount) {
            inline 8, 16, 32, 64 => |i| &@field(v.Int, std.fmt.comptimePrint("sint{}", .{i})),
            else => return RuntimeError.InvalidSpirV,
        },
        .UInt => switch (BitCount) {
            inline 8, 16, 32, 64 => |i| &@field(v.Int, std.fmt.comptimePrint("uint{}", .{i})),
            else => return RuntimeError.InvalidSpirV,
        },
    };
}

fn getValuePrimitiveFieldType(comptime T: ValueType, comptime BitCount: SpvWord) type {
    return switch (T) {
        .Bool => bool,
        .Float => std.meta.Float(BitCount),
        .SInt => std.meta.Int(.signed, BitCount),
        .UInt => std.meta.Int(.unsigned, BitCount),
    };
}

fn opAccessChain(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();
    const base_id = try rt.it.next();

    const base = &rt.results[base_id];
    var value_ptr = try base.getValue();

    const index_count = word_count - 3;
    rt.results[id].variant = .{
        .AccessChain = .{
            .target = var_type,
            .value = blk: {
                for (0..index_count) |_| {
                    const member = &rt.results[try rt.it.next()];
                    const member_value = switch ((try member.getVariant()).*) {
                        .Constant => |c| &c.value,
                        .Variable => |v| &v.value,
                        else => return RuntimeError.InvalidSpirV,
                    };
                    switch (member_value.*) {
                        .Int => |i| {
                            switch (value_ptr.*) {
                                .Vector, .Matrix, .Array, .Structure => |v| {
                                    if (i.uint32 > v.len) return RuntimeError.InvalidSpirV;
                                    value_ptr = &v[i.uint32];
                                },
                                //.Vector4f32 => |v| {
                                //    if (i.uint32 > 4) return RuntimeError.InvalidSpirV;
                                //    break :blk .{
                                //        .Float = .{ .float32 = v[i.uint32] },
                                //    };
                                //},
                                //.Vector2f32 => |v| {
                                //    if (i.uint32 > 2) return RuntimeError.InvalidSpirV;
                                //    break :blk .{
                                //        .Float = .{ .float32 = v[i.uint32] },
                                //    };
                                //},
                                else => return RuntimeError.InvalidSpirV,
                            }
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }
                break :blk value_ptr;
            },
        },
    };
}

fn opBranch(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    _ = rt.it.jumpToSourceLocation(switch ((try rt.results[id].getVariant()).*) {
        .Label => |l| l.source_location,
        else => return RuntimeError.InvalidSpirV,
    });
}

fn opBranchConditional(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const cond_value = try rt.results[try rt.it.next()].getValue();
    const true_branch = switch ((try rt.results[try rt.it.next()].getVariant()).*) {
        .Label => |l| l.source_location,
        else => return RuntimeError.InvalidSpirV,
    };
    const false_branch = switch ((try rt.results[try rt.it.next()].getVariant()).*) {
        .Label => |l| l.source_location,
        else => return RuntimeError.InvalidSpirV,
    };
    if (cond_value.Bool) {
        _ = rt.it.jumpToSourceLocation(true_branch);
    } else {
        _ = rt.it.jumpToSourceLocation(false_branch);
    }
}

fn opCapability(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.capabilities.insert(try rt.it.nextAs(spv.SpvCapability));
}

fn opCompositeConstruct(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const id = try rt.it.next();

    const index_count = word_count - 2;
    const value = &(try rt.results[id].getVariant()).Constant.value;
    if (value.getCompositeDataOrNull()) |target| {
        for (target[0..index_count]) |*elem| {
            const elem_value = (try rt.results[try rt.it.next()].getVariant()).Constant.value;
            elem.* = elem_value;
        }
        return;
    }

    switch (value.*) {
        .Vector4f32 => |*vec| inline for (0..4) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Float.float32;
        },
        .Vector3f32 => |*vec| inline for (0..3) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Float.float32;
        },
        .Vector2f32 => |*vec| inline for (0..2) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Float.float32;
        },
        .Vector4i32 => |*vec| inline for (0..4) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.sint32;
        },
        .Vector3i32 => |*vec| inline for (0..3) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.sint32;
        },
        .Vector2i32 => |*vec| inline for (0..2) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.sint32;
        },
        .Vector4u32 => |*vec| inline for (0..4) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.uint32;
        },
        .Vector3u32 => |*vec| inline for (0..3) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.uint32;
        },
        .Vector2u32 => |*vec| inline for (0..2) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.uint32;
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opCompositeExtract(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const res_type = try rt.it.next();
    const id = try rt.it.next();
    const composite_id = try rt.it.next();
    const index_count = word_count - 3;

    rt.results[id].variant = .{
        .Constant = .{
            .type_word = res_type,
            .type = switch ((try rt.results[res_type].getVariant()).*) {
                .Type => |t| @as(Result.Type, t),
                else => return RuntimeError.InvalidSpirV,
            },
            .value = blk: {
                var composite = (try rt.results[composite_id].getVariant()).Constant.value;
                for (0..index_count) |_| {
                    const member_id = try rt.it.next();
                    if (composite.getCompositeDataOrNull()) |v| {
                        composite = v[member_id];
                        continue;
                    }
                    switch (composite) {
                        .Vector4f32 => |v| break :blk .{ .Float = .{ .float32 = v[member_id] } },
                        .Vector3f32 => |v| break :blk .{ .Float = .{ .float32 = v[member_id] } },
                        .Vector2f32 => |v| break :blk .{ .Float = .{ .float32 = v[member_id] } },
                        .Vector4i32 => |v| break :blk .{ .Int = .{ .sint32 = v[member_id] } },
                        .Vector3i32 => |v| break :blk .{ .Int = .{ .sint32 = v[member_id] } },
                        .Vector2i32 => |v| break :blk .{ .Int = .{ .sint32 = v[member_id] } },
                        .Vector4u32 => |v| break :blk .{ .Int = .{ .uint32 = v[member_id] } },
                        .Vector3u32 => |v| break :blk .{ .Int = .{ .uint32 = v[member_id] } },
                        .Vector2u32 => |v| break :blk .{ .Int = .{ .uint32 = v[member_id] } },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }
                break :blk try composite.dupe(allocator);
            },
        },
    };
}

fn opConstant(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    // No check on null and sizes, absolute trust in this shit
    switch (target.variant.?.Constant.value) {
        .Int => |*i| {
            if (word_count - 2 != 1) {
                const low = @as(u64, try rt.it.next());
                const high = @as(u64, try rt.it.next());
                i.uint64 = (high << 32) | low;
            } else {
                i.uint32 = try rt.it.next();
            }
        },
        .Float => |*f| {
            if (word_count - 2 != 1) {
                const low = @as(u64, try rt.it.next());
                const high = @as(u64, try rt.it.next());
                f.float64 = @bitCast((high << 32) | low);
            } else {
                f.float32 = @bitCast(try rt.it.next());
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opCopyMemory(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try rt.it.next();
    const source = try rt.it.next();
    copyValue(try rt.results[target].getValue(), try rt.results[source].getValue());
}

fn opDecorate(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try rt.it.next();
    const decoration_type = try rt.it.nextAs(spv.SpvDecoration);
    try addDecoration(allocator, rt, target, decoration_type, null);
}

fn opDecorateMember(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try rt.it.next();
    const member = try rt.it.next();
    const decoration_type = try rt.it.nextAs(spv.SpvDecoration);
    try addDecoration(allocator, rt, target, decoration_type, member);
}

fn opDot(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
    var value = try rt.results[try rt.it.next()].getValue();
    const op1_value = try rt.results[try rt.it.next()].getValue();
    const op2_value = try rt.results[try rt.it.next()].getValue();

    const size = switch (target_type) {
        .Float => |f| f.bit_length,
        else => return RuntimeError.InvalidSpirV,
    };

    value.Float.float64 = 0.0;

    switch (op1_value.*) {
        .Vector => |vec| for (vec, op2_value.Vector) |*op1_v, *op2_v| {
            switch (size) {
                inline 16, 32, 64 => |i| {
                    (try getValuePrimitiveField(.Float, i, value)).* += (try getValuePrimitiveField(.Float, i, op1_v)).* * (try getValuePrimitiveField(.Float, i, op2_v)).*;
                },
                else => return RuntimeError.InvalidSpirV,
            }
        },
        .Vector4f32 => |*vec| inline for (0..4) |i| {
            value.Float.float32 += vec[i] * op2_value.Vector4f32[i];
        },
        .Vector3f32 => |*vec| inline for (0..3) |i| {
            value.Float.float32 += vec[i] * op2_value.Vector3f32[i];
        },
        .Vector2f32 => |*vec| inline for (0..2) |i| {
            value.Float.float32 += vec[i] * op2_value.Vector2f32[i];
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opEntryPoint(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const entry = rt.mod.entry_points.addOne(allocator) catch return RuntimeError.OutOfMemory;
    entry.exec_model = try rt.it.nextAs(spv.SpvExecutionModel);
    entry.id = try rt.it.next();
    entry.name = try readString(allocator, &rt.it);

    var interface_count = word_count - @divExact(entry.name.len, 4) - 2;
    entry.globals = try allocator.alloc(SpvWord, interface_count);
    if (interface_count != 0) {
        var interface_index: u32 = 0;
        while (interface_count != 0) {
            entry.globals[interface_index] = try rt.it.next();
            interface_index += 1;
            interface_count -= 1;
        }
    }
}

fn opExecutionMode(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const mode = try rt.it.nextAs(spv.SpvExecutionMode);

    switch (mode) {
        .LocalSize => {
            rt.mod.local_size_x = try rt.it.next();
            rt.mod.local_size_y = try rt.it.next();
            rt.mod.local_size_z = try rt.it.next();
        },
        .Invocations => rt.mod.geometry_invocations = try rt.it.next(),
        .OutputVertices => rt.mod.geometry_output_count = try rt.it.next(),
        .InputPoints, .InputLines, .Triangles, .InputLinesAdjacency, .InputTrianglesAdjacency => rt.mod.geometry_input = @intFromEnum(mode),
        .OutputPoints, .OutputLineStrip, .OutputTriangleStrip => rt.mod.geometry_output = @intFromEnum(mode),
        else => {},
    }
}

fn opFunction(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const return_type = try rt.it.next();
    const id = try rt.it.next();
    _ = rt.it.skip(); // Skip function control
    const function_type_id = try rt.it.next();

    const source_location = rt.it.emitSourceLocation();

    rt.mod.results[id].variant = .{
        .Function = .{
            .source_location = source_location,
            .return_type = return_type,
            .function_type = function_type_id,
            .params = params: {
                if (rt.mod.results[function_type_id].variant) |variant| {
                    const params_count = switch (variant) {
                        .Type => |t| switch (t) {
                            .Function => |f| f.params.len,
                            else => return RuntimeError.InvalidSpirV,
                        },
                        else => return RuntimeError.InvalidSpirV,
                    };
                    break :params allocator.alloc(SpvWord, params_count) catch return RuntimeError.OutOfMemory;
                }
                return RuntimeError.InvalidSpirV;
            },
        },
    };

    rt.mod.results[function_type_id].variant.?.Type.Function.source_location = source_location;

    rt.current_function = &rt.mod.results[id];
}

fn opFunctionCall(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const ret = &rt.results[try rt.it.next()];
    const func = &rt.results[try rt.it.next()];

    for ((try func.getVariant()).Function.params) |param| {
        const arg = &rt.results[try rt.it.next()];
        ((try rt.results[param].getVariant()).*).FunctionParameter.value_ptr = try arg.getValue();
    }
    rt.function_stack.items[rt.function_stack.items.len - 1].source_location = rt.it.emitSourceLocation();
    const source_location = (try func.getVariant()).Function.source_location;
    rt.function_stack.append(allocator, .{
        .source_location = source_location,
        .result = func,
        .ret = ret,
    }) catch return RuntimeError.OutOfMemory;
    if (!rt.it.jumpToSourceLocation(source_location)) return RuntimeError.InvalidSpirV;
    rt.current_parameter_index = 0;
}

fn opFunctionEnd(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.current_function = null;
}

fn opFunctionParameter(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();

    const target = &rt.mod.results[id];

    const resolved = rt.mod.results[var_type].resolveType(rt.mod.results);
    const member_count = resolved.getMemberCounts();
    if (member_count == 0) {
        return RuntimeError.InvalidSpirV;
    }
    target.variant = .{
        .FunctionParameter = .{
            .type_word = var_type,
            .type = switch ((try resolved.getConstVariant()).*) {
                .Type => |t| @as(Result.Type, t),
                else => return RuntimeError.InvalidSpirV,
            },
            .value_ptr = null,
        },
    };
    (try (rt.current_function orelse return RuntimeError.InvalidSpirV).getVariant()).Function.params[rt.current_parameter_index] = id;
    rt.current_parameter_index += 1;
}

fn opLabel(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Label = .{
            .source_location = rt.it.emitSourceLocation() - 2, // Original label location
        },
    };
}

fn opKill(_: std.mem.Allocator, _: SpvWord, _: *Runtime) RuntimeError!void {
    return RuntimeError.Killed;
}

fn opLoad(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const id = try rt.it.next();
    const ptr_id = try rt.it.next();
    copyValue(try rt.results[id].getValue(), try rt.results[ptr_id].getValue());
}

fn opMemberName(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const memb = try rt.it.next();

    var result = &rt.mod.results[id];

    if (result.variant == null) {
        result.variant = .{
            .Type = .{
                .Structure = .{
                    .members_type_word = undefined,
                    .members = undefined,
                    .member_names = .empty,
                },
            },
        };
    }

    switch (result.variant.?) {
        .Type => |*t| switch (t.*) {
            .Structure => |*s| {
                if (memb + 1 > s.member_names.items.len) {
                    _ = s.member_names.resize(allocator, memb + 1) catch return RuntimeError.OutOfMemory;
                }
                const slen = word_count - 2;
                s.member_names.items[memb] = try readStringN(allocator, &rt.it, slen);
            },
            else => unreachable,
        },
        else => unreachable,
    }
}

fn opMemoryModel(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.addressing = try rt.it.nextAs(spv.SpvAddressingModel);
    rt.mod.memory_model = try rt.it.nextAs(spv.SpvMemoryModel);
}

fn opName(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    var result = &rt.mod.results[id];
    result.name = try readStringN(allocator, &rt.it, word_count - 1);
}

fn opReturn(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.function_stack.pop();
    if (rt.function_stack.getLastOrNull()) |function| {
        _ = rt.it.jumpToSourceLocation(function.source_location);
        rt.current_function = function.result;
    } else {
        rt.current_function = null;
        rt.it.skipToEnd();
    }
}

fn opReturnValue(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    if (rt.function_stack.getLastOrNull()) |function| {
        var ret_res = rt.results[try rt.it.next()];
        copyValue(try function.ret.getValue(), try ret_res.getValue());
    } else {
        return RuntimeError.InvalidSpirV; // No current function ???
    }

    _ = rt.function_stack.pop();
    if (rt.function_stack.getLastOrNull()) |function| {
        _ = rt.it.jumpToSourceLocation(function.source_location);
        rt.current_function = function.result;
    } else {
        rt.current_function = null;
        rt.it.skipToEnd();
    }
}

fn opSource(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    var file = rt.mod.files.addOne(allocator) catch return RuntimeError.OutOfMemory;
    file.lang = try rt.it.nextAs(spv.SpvSourceLanguage);
    file.lang_version = try rt.it.next();
    if (word_count > 2) {
        const id = try rt.it.next();
        if (id >= rt.mod.results.len) return RuntimeError.InvalidSpirV;
        if (rt.mod.results[id].name) |name| {
            file.file_name = name;
        }
    }
    if (word_count > 3) {
        const id = try rt.it.next();
        if (id >= rt.mod.results.len) return RuntimeError.InvalidSpirV;
        if (rt.mod.results[id].name) |name| {
            file.source = name;
        }
    }
}

fn opSourceExtension(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.extensions.append(allocator, try readStringN(allocator, &rt.it, word_count)) catch return RuntimeError.OutOfMemory;
}

fn opStore(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const ptr_id = try rt.it.next();
    const val_id = try rt.it.next();
    copyValue(try rt.results[ptr_id].getValue(), try rt.results[val_id].getValue());
}

fn opTypeArray(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const components_type_word = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Array = .{
                .components_type_word = components_type_word,
                .components_type = switch ((try rt.mod.results[components_type_word].getVariant()).*) {
                    .Type => |t| @as(Result.Type, t),
                    else => return RuntimeError.InvalidSpirV,
                },
                .member_count = try rt.it.next(),
            },
        },
    };
}

fn opTypeBool(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Bool = .{},
        },
    };
}

fn opTypeFloat(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Float = .{
                .bit_length = try rt.it.next(),
            },
        },
    };
}

fn opTypeFunction(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Function = .{
                .source_location = 0,
                .return_type = try rt.it.next(),
                .params = blk: {
                    const params = allocator.alloc(SpvWord, word_count - 2) catch return RuntimeError.OutOfMemory;
                    errdefer allocator.free(params);
                    for (params) |*param| {
                        param.* = try rt.it.next();
                    }
                    break :blk params;
                },
            },
        },
    };
}

fn opTypeInt(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Int = .{
                .bit_length = try rt.it.next(),
                .is_signed = if (try rt.it.next() != 0) true else false,
            },
        },
    };
}

fn opTypeMatrix(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const column_type_word = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Matrix = .{
                .column_type_word = column_type_word,
                .column_type = switch ((try rt.mod.results[column_type_word].getVariant()).*) {
                    .Type => |t| @as(Result.Type, t),
                    else => return RuntimeError.InvalidSpirV,
                },
                .member_count = try rt.it.next(),
            },
        },
    };
}

fn opTypePointer(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Pointer = .{
                .storage_class = try rt.it.nextAs(spv.SpvStorageClass),
                .target = try rt.it.next(),
            },
        },
    };
}

fn opTypeStruct(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const members_type_word, const members = blk: {
        const members_type_word = allocator.alloc(SpvWord, word_count - 1) catch return RuntimeError.OutOfMemory;
        errdefer allocator.free(members_type_word);

        const members = allocator.alloc(Result.Type, word_count - 1) catch return RuntimeError.OutOfMemory;
        errdefer allocator.free(members);

        for (members_type_word, members) |*member_type_word, *member| {
            member_type_word.* = try rt.it.next();
            member.* = rt.mod.results[member_type_word.*].variant.?.Type;
        }
        break :blk .{ members_type_word, members };
    };

    if (rt.mod.results[id].variant) |*variant| {
        switch (variant.*) {
            .Type => |*t| switch (t.*) {
                .Structure => |*s| {
                    s.members_type_word = members_type_word;
                    s.members = members;
                },
                else => unreachable,
            },
            else => unreachable,
        }
    } else {
        rt.mod.results[id].variant = .{
            .Type = .{
                .Structure = .{
                    .members_type_word = members_type_word,
                    .members = members,
                    .member_names = .empty,
                },
            },
        };
    }
}

fn opTypeVector(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const components_type_word = try rt.it.next();
    var components_type_size: usize = 0;
    const components_type_concrete = try rt.mod.results[components_type_word].getVariant();
    const components_type = switch (components_type_concrete.*) {
        .Type => |t| blk: {
            switch (t) {
                .Int => |i| components_type_size = i.bit_length,
                .Float => |f| components_type_size = f.bit_length,
                else => {},
            }
            break :blk @as(Result.Type, t);
        },
        else => return RuntimeError.InvalidSpirV,
    };
    const member_count = try rt.it.next();
    rt.mod.results[id].variant = .{ .Type = blk: {
        if (components_type_size == 32 and rt.mod.options.use_simd_vectors_specializations) {
            switch (components_type) {
                .Float => switch (member_count) {
                    2 => break :blk .{ .Vector2f32 = .{} },
                    3 => break :blk .{ .Vector3f32 = .{} },
                    4 => break :blk .{ .Vector4f32 = .{} },
                    else => {},
                },
                .Int => {
                    const is_signed = components_type_concrete.Type.Int.is_signed;
                    switch (member_count) {
                        2 => break :blk if (is_signed) .{ .Vector2i32 = .{} } else .{ .Vector2u32 = .{} },
                        3 => break :blk if (is_signed) .{ .Vector3i32 = .{} } else .{ .Vector3u32 = .{} },
                        4 => break :blk if (is_signed) .{ .Vector4i32 = .{} } else .{ .Vector4u32 = .{} },
                        else => {},
                    }
                },
                else => {},
            }
        }
        break :blk .{
            .Vector = .{
                .components_type_word = components_type_word,
                .components_type = components_type,
                .member_count = member_count,
            },
        };
    } };
}

fn opTypeVoid(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Void = .{},
        },
    };
}

fn opVariable(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();
    const storage_class = try rt.it.nextAs(spv.SpvStorageClass);
    const initializer: ?SpvWord = if (word_count >= 4) try rt.it.next() else null;

    const target = &rt.mod.results[id];

    const resolved = rt.mod.results[var_type].resolveType(rt.mod.results);
    const member_count = resolved.getMemberCounts();
    if (member_count == 0) {
        return RuntimeError.InvalidSpirV;
    }
    target.variant = .{
        .Variable = .{
            .storage_class = storage_class,
            .type_word = var_type,
            .type = switch ((try resolved.getConstVariant()).*) {
                .Type => |t| @as(Result.Type, t),
                else => return RuntimeError.InvalidSpirV,
            },
            .value = try Result.initValue(allocator, member_count, rt.mod.results, resolved),
        },
    };

    _ = initializer;
}

fn readString(allocator: std.mem.Allocator, it: *WordIterator) RuntimeError![]const u8 {
    var str: std.ArrayList(u8) = .empty;
    while (it.nextOrNull()) |word| {
        if (word == 0) break;
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate(word & 0x000000FF);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0x0000FF00) >> 8);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0x00FF0000) >> 16);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0xFF000000) >> 24);
        if (str.getLast() == 0) break;
    }
    return str.toOwnedSlice(allocator);
}

fn readStringN(allocator: std.mem.Allocator, it: *WordIterator, n: usize) RuntimeError![]const u8 {
    var str = std.ArrayList(u8).initCapacity(allocator, n * 4) catch return RuntimeError.OutOfMemory;
    for (0..n) |_| {
        if (it.nextOrNull()) |word| {
            if (word == 0) break;
            str.addOneAssumeCapacity().* = @truncate(word & 0x000000FF);
            str.addOneAssumeCapacity().* = @truncate((word & 0x0000FF00) >> 8);
            str.addOneAssumeCapacity().* = @truncate((word & 0x00FF0000) >> 16);
            str.addOneAssumeCapacity().* = @truncate((word & 0xFF000000) >> 24);
            if (str.getLast() == 0) break;
        }
    }
    return str.toOwnedSlice(allocator);
}

fn setupConstant(allocator: std.mem.Allocator, rt: *Runtime) RuntimeError!*Result {
    const res_type = try rt.it.next();
    const id = try rt.it.next();
    const target = &rt.mod.results[id];

    const resolved = rt.mod.results[res_type].resolveType(rt.mod.results);
    const member_count = resolved.getMemberCounts();
    target.variant = .{
        .Constant = .{
            .value = try Result.initValue(allocator, member_count, rt.mod.results, resolved),
            .type_word = res_type,
            .type = switch ((try resolved.getConstVariant()).*) {
                .Type => |t| @as(Result.Type, t),
                else => return RuntimeError.InvalidSpirV,
            },
        },
    };
    return target;
}
