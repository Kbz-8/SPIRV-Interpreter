const std = @import("std");
const spv = @import("spv.zig");
const zm = @import("zmath");

const GLSL_std_450 = @import("GLSL_std_450/opcodes.zig");

const Module = @import("Module.zig");
const Runtime = @import("Runtime.zig");
const Result = @import("Result.zig");
const value_ns = @import("Value.zig");
const WordIterator = @import("WordIterator.zig");

const RuntimeError = Runtime.RuntimeError;

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

const Value = value_ns.Value;
const PrimitiveType = value_ns.PrimitiveType;

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
    Negate,
};

const CondOp = enum {
    Equal,
    Greater,
    GreaterEqual,
    IsFinite,
    IsInf,
    IsNan,
    IsNormal,
    Less,
    LessEqual,
    LogicalAnd,
    LogicalEqual,
    LogicalNot,
    LogicalNotEqual,
    LogicalOr,
    NotEqual,
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
pub const OpCodeExtFunc = *const fn (std.mem.Allocator, SpvWord, SpvWord, SpvWord, *Runtime) RuntimeError!void;

pub const SetupDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeFunc).init(.{
        .AtomicAnd = autoSetupConstant,
        .AtomicCompareExchange = autoSetupConstant,
        .AtomicExchange = autoSetupConstant,
        .AtomicIAdd = autoSetupConstant,
        .AtomicIDecrement = autoSetupConstant,
        .AtomicIIncrement = autoSetupConstant,
        .AtomicISub = autoSetupConstant,
        .AtomicLoad = autoSetupConstant,
        .AtomicOr = autoSetupConstant,
        .AtomicSMax = autoSetupConstant,
        .AtomicSMin = autoSetupConstant,
        .AtomicUMax = autoSetupConstant,
        .AtomicUMin = autoSetupConstant,
        .AtomicXor = autoSetupConstant,
        .AccessChain = setupAccessChain,
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
        .CompositeInsert = autoSetupConstant,
        .Constant = opConstant,
        .ConstantComposite = opConstantComposite,
        .ConvertFToS = autoSetupConstant,
        .ConvertFToU = autoSetupConstant,
        .ConvertPtrToU = autoSetupConstant,
        .ConvertSToF = autoSetupConstant,
        .ConvertUToF = autoSetupConstant,
        .ConvertUToPtr = autoSetupConstant,
        .Decorate = opDecorate,
        .DecorationGroup = opDecorationGroup,
        .Dot = autoSetupConstant,
        .EntryPoint = opEntryPoint,
        .ExecutionMode = opExecutionMode,
        .ExtInst = autoSetupConstant,
        .ExtInstImport = opExtInstImport,
        .FAdd = autoSetupConstant,
        .FConvert = autoSetupConstant,
        .FDiv = autoSetupConstant,
        .FMod = autoSetupConstant,
        .FMul = autoSetupConstant,
        .FNegate = autoSetupConstant,
        .FOrdEqual = autoSetupConstant,
        .FOrdGreaterThan = autoSetupConstant,
        .FOrdGreaterThanEqual = autoSetupConstant,
        .FOrdLessThan = autoSetupConstant,
        .FOrdLessThanEqual = autoSetupConstant,
        .FOrdNotEqual = autoSetupConstant,
        .FRem = autoSetupConstant,
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
        .GroupDecorate = opGroupDecorate,
        .GroupMemberDecorate = opGroupMemberDecorate,
        .IAdd = autoSetupConstant,
        .IAddCarry = autoSetupConstant,
        .IEqual = autoSetupConstant,
        .ImageRead = autoSetupConstant,
        .InBoundsAccessChain = setupAccessChain,
        .IMul = autoSetupConstant,
        .INotEqual = autoSetupConstant,
        .ISub = autoSetupConstant,
        .ISubBorrow = autoSetupConstant,
        .IsFinite = autoSetupConstant,
        .IsInf = autoSetupConstant,
        .IsNan = autoSetupConstant,
        .IsNormal = autoSetupConstant,
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
        .Phi = autoSetupConstant,
        .QuantizeToF16 = autoSetupConstant,
        .SConvert = autoSetupConstant,
        .SDiv = autoSetupConstant,
        .SGreaterThan = autoSetupConstant,
        .SGreaterThanEqual = autoSetupConstant,
        .SLessThan = autoSetupConstant,
        .SLessThanEqual = autoSetupConstant,
        .SMod = autoSetupConstant,
        .SMulExtended = autoSetupConstant,
        .SNegate = autoSetupConstant,
        .SRem = autoSetupConstant,
        .SatConvertSToU = autoSetupConstant,
        .SatConvertUToS = autoSetupConstant,
        .Select = autoSetupConstant,
        .ShiftLeftLogical = autoSetupConstant,
        .ShiftRightArithmetic = autoSetupConstant,
        .ShiftRightLogical = autoSetupConstant,
        .SourceExtension = opSourceExtension,
        .TypeArray = opTypeArray,
        .TypeBool = opTypeBool,
        .TypeFloat = opTypeFloat,
        .TypeFunction = opTypeFunction,
        .TypeImage = opTypeImage,
        .TypeInt = opTypeInt,
        .TypeMatrix = opTypeMatrix,
        .TypePointer = opTypePointer,
        .TypeRuntimeArray = opTypeRuntimeArray,
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
        .UMulExtended = autoSetupConstant,
        .Undef = autoSetupConstant,
        .Variable = opVariable,
        .VectorShuffle = autoSetupConstant,
        .VectorTimesMatrix = autoSetupConstant,
        .VectorTimesScalar = autoSetupConstant,
    });
};

/// Not an EnumMap as it is way too slow for this purpose
pub var runtime_dispatcher = [_]?OpCodeFunc{null} ** spv.SpvOpMaxValue;

pub fn initRuntimeDispatcher() void {
    // zig fmt: off
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AccessChain)]            = opAccessChain;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicLoad)]             = opLoad;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicStore)]            = opAtomicStore;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicIAdd)]             = MathEngine(.SInt, .Add, true).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicISub)]             = MathEngine(.SInt, .Sub, true).op;
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
    runtime_dispatcher[@intFromEnum(spv.SpvOp.CompositeInsert)]        = opCompositeInsert;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertFToS)]            = ConversionEngine(.Float, .SInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertFToU)]            = ConversionEngine(.Float, .UInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertSToF)]            = ConversionEngine(.SInt, .Float).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertUToF)]            = ConversionEngine(.UInt, .Float).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.CopyMemory)]             = opCopyMemory;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Dot)]                    = opDot;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ExtInst)]                = opExtInst;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FAdd)]                   = MathEngine(.Float, .Add, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FConvert)]               = ConversionEngine(.Float, .Float).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FDiv)]                   = MathEngine(.Float, .Div, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FMod)]                   = MathEngine(.Float, .Mod, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FMul)]                   = MathEngine(.Float, .Mul, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FNegate)]                = MathEngine(.Float, .Negate, false).opSingle;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdEqual)]              = CondEngine(.Float, .Equal).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdGreaterThan)]        = CondEngine(.Float, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdGreaterThanEqual)]   = CondEngine(.Float, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdLessThan)]           = CondEngine(.Float, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdLessThanEqual)]      = CondEngine(.Float, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FOrdNotEqual)]           = CondEngine(.Float, .NotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FRem)]                   = MathEngine(.Float, .Rem, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FSub)]                   = MathEngine(.Float, .Sub, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordEqual)]            = CondEngine(.Float, .Equal).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordGreaterThan)]      = CondEngine(.Float, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordGreaterThanEqual)] = CondEngine(.Float, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordLessThan)]         = CondEngine(.Float, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordLessThanEqual)]    = CondEngine(.Float, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FUnordNotEqual)]         = CondEngine(.Float, .NotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FunctionCall)]           = opFunctionCall;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IAdd)]                   = MathEngine(.SInt, .Add, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IAddCarry)]              = opIAddCarry;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ISubBorrow)]             = opISubBorrow;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IEqual)]                 = CondEngine(.SInt, .Equal).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IMul)]                   = MathEngine(.SInt, .Mul, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.INotEqual)]              = CondEngine(.SInt, .NotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ISub)]                   = MathEngine(.SInt, .Sub, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.InBoundsAccessChain)]    = opAccessChain;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IsFinite)]               = CondEngine(.Float, .IsNan).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IsInf)]                  = CondEngine(.Float, .IsInf).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IsNan)]                  = CondEngine(.Float, .IsNan).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IsNormal)]               = CondEngine(.Float, .IsNan).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Kill)]                   = opKill;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Label)]                  = opLabel;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Load)]                   = opLoad;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalAnd)]             = CondEngine(.Bool, .LogicalAnd).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalEqual)]           = CondEngine(.Bool, .LogicalEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalNot)]             = CondEngine(.Bool, .LogicalNot).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalNotEqual)]        = CondEngine(.Bool, .LogicalNotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.LogicalOr)]              = CondEngine(.Bool, .LogicalOr).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesMatrix)]      = MathEngine(.Float, .MatrixTimesMatrix, false).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesScalar)]      = MathEngine(.Float, .MatrixTimesScalar, false).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesVector)]      = MathEngine(.Float, .MatrixTimesVector, false).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Not)]                    = BitEngine(.UInt, .Not).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Phi)]                    = opPhi;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Return)]                 = opReturn;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ReturnValue)]            = opReturnValue;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SConvert)]               = ConversionEngine(.SInt, .SInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SDiv)]                   = MathEngine(.SInt, .Div, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SGreaterThan)]           = CondEngine(.SInt, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SGreaterThanEqual)]      = CondEngine(.SInt, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SLessThan)]              = CondEngine(.SInt, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SLessThanEqual)]         = CondEngine(.SInt, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SMod)]                   = MathEngine(.SInt, .Mod, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SNegate)]                = MathEngine(.SInt, .Negate, false).opSingle;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SRem)]                   = MathEngine(.SInt, .Rem, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Select)]                 = opSelect;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ShiftLeftLogical)]       = BitEngine(.UInt, .ShiftLeft).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ShiftRightArithmetic)]   = BitEngine(.SInt, .ShiftRightArithmetic).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ShiftRightLogical)]      = BitEngine(.UInt, .ShiftRight).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SpecConstant)]           = opSpecConstant;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SpecConstantComposite)]  = opConstantComposite;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SpecConstantFalse)]      = opSpecConstantFalse;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SpecConstantOp)]         = opSpecConstantOp;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SpecConstantTrue)]       = opSpecConstantTrue;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Store)]                  = opStore;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UConvert)]               = ConversionEngine(.UInt, .UInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UDiv)]                   = MathEngine(.UInt, .Div, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UGreaterThan)]           = CondEngine(.UInt, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UGreaterThanEqual)]      = CondEngine(.UInt, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ULessThan)]              = CondEngine(.UInt, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ULessThanEqual)]         = CondEngine(.UInt, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UMod)]                   = MathEngine(.UInt, .Mod, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorShuffle)]          = opVectorShuffle;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorTimesMatrix)]      = MathEngine(.Float, .VectorTimesMatrix, false).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorTimesScalar)]      = MathEngine(.Float, .VectorTimesScalar, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SMulExtended)]           = opSMulExtended;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UMulExtended)]           = opUMulExtended;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageRead)]              = opImageRead;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageWrite)]             = opImageWrite;
    // zig fmt: on

    // Extensions init
    GLSL_std_450.initRuntimeDispatcher();
}

fn extEqlName(a: []const u8, b: []const u8) bool {
    for (0..@min(a.len, b.len)) |i| {
        if (a[i] != b[i]) return false;
    }
    return true;
}

const extensions_map = std.StaticStringMapWithEql([]?OpCodeExtFunc, extEqlName).initComptime(.{
    .{ "GLSL.std.450", GLSL_std_450.runtime_dispatcher[0..] },
});

fn BitOperator(comptime T: PrimitiveType, comptime Op: BitOp) type {
    return struct {
        comptime {
            if (T == .Float) @compileError("Invalid value type");
        }

        const max_operator_count: usize = 4;

        inline fn isUnaryOp() bool {
            return comptime switch (Op) {
                .Not, .BitCount, .BitReverse => true,
                else => false,
            };
        }

        inline fn isBinaryOp() bool {
            return !isUnaryOp() and !isTernaryOp() and !isQuaternaryOp(); // flemme d'ajouter les opérateurs à chaque fois
        }

        inline fn isTernaryOp() bool {
            return comptime switch (Op) {
                .BitFieldUExtract, .BitFieldSExtract => true,
                else => false,
            };
        }

        inline fn isQuaternaryOp() bool {
            return comptime switch (Op) {
                .BitFieldInsert => true,
                else => false,
            };
        }

        inline fn getOperatorsCount() usize {
            return if (isUnaryOp())
                1
            else if (isBinaryOp())
                2
            else if (isTernaryOp())
                3
            else
                4;
        }

        inline fn bitInsert(comptime TT: type, base: TT, insert: TT, offset: u64, count: u64) TT {
            const info = @typeInfo(TT);
            if (info != .int) @compileError("must be an integer type");

            const bits: u32 = info.int.bits;
            const U = std.meta.Int(.unsigned, bits);

            if (count == 0) return base;

            const base_u: U = @bitCast(base);
            const insert_u: U = @bitCast(insert);

            const field_mask: U = if (count == bits)
                ~@as(U, 0)
            else
                (@as(U, 1) << @intCast(count)) - 1;

            const shift: std.math.Log2Int(U) = @truncate(offset);

            const positioned_mask: U = @shlWithOverflow(field_mask, shift)[0];
            const positioned_insert: U = @shlWithOverflow(insert_u & field_mask, shift)[0];

            return @bitCast((base_u & ~positioned_mask) | positioned_insert);
        }

        inline fn bitExtract(comptime TT: type, comptime signed_result: bool, base: TT, offset: u64, count: u64) TT {
            const info = @typeInfo(TT);
            if (info != .int) @compileError("must be an integer type");

            const bits: u32 = info.int.bits;

            if (count == 0) return @as(TT, 0);

            const U = std.meta.Int(.unsigned, bits);
            const base_u: U = @bitCast(base);

            const field: U = if (count == bits)
                base_u
            else
                (base_u >> @intCast(offset)) &
                    ((@as(U, 1) << @intCast(count)) - 1);

            const result: U = if (!signed_result or count == bits) blk: {
                break :blk field;
            } else blk: {
                const sign_bit: U = @as(U, 1) << @intCast(count - 1);
                if ((field & sign_bit) != 0) {
                    break :blk field | (~@as(U, 0) << @intCast(count));
                }
                break :blk field;
            };

            return @bitCast(result);
        }

        inline fn operationUnary(comptime TT: type, op1: TT) RuntimeError!TT {
            return switch (Op) {
                .BitCount => blk: {
                    const bit_set: std.bit_set.IntegerBitSet(@bitSizeOf(TT)) = .{
                        .mask = @bitCast(op1),
                    };
                    break :blk @as(TT, @intCast(bit_set.count()));
                },
                .BitReverse => @bitReverse(op1),
                .Not => ~op1,
                else => RuntimeError.InvalidSpirV,
            };
        }

        inline fn operationBinary(comptime TT: type, op1: TT, op2: TT) RuntimeError!TT {
            return switch (Op) {
                .BitwiseAnd => op1 & op2,
                .BitwiseOr => op1 | op2,
                .BitwiseXor => op1 ^ op2,
                .ShiftLeft => op1 << @intCast(op2),
                .ShiftRight, .ShiftRightArithmetic => op1 >> @intCast(op2),

                else => RuntimeError.InvalidSpirV,
            };
        }

        inline fn operationTernary(comptime TT: type, op1: TT, op2: TT, op3: *const Value) RuntimeError!TT {
            return switch (Op) {
                .BitFieldSExtract => blk: {
                    if (T != .SInt) return RuntimeError.InvalidSpirV;
                    break :blk bitExtract(TT, true, op1, @intCast(op2), op3.Int.value.uint64);
                },
                .BitFieldUExtract => blk: {
                    if (T != .UInt) return RuntimeError.InvalidSpirV;
                    break :blk bitExtract(TT, false, op1, @intCast(op2), op3.Int.value.uint64);
                },
                else => RuntimeError.InvalidSpirV,
            };
        }

        inline fn operationQuaternary(comptime TT: type, op1: TT, op2: TT, op3: *const Value, op4: *const Value) RuntimeError!TT {
            return switch (Op) {
                .BitFieldInsert => bitInsert(TT, op1, op2, op3.Int.value.uint64, op4.Int.value.uint64),
                else => RuntimeError.InvalidSpirV,
            };
        }

        fn applyScalarBits(bit_count: SpvWord, dst: *Value, ops: [max_operator_count]?*const Value) RuntimeError!void {
            switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| {
                    const TT = Value.getPrimitiveFieldType(T, bits);

                    const out: TT = blk: {
                        const a = try Value.readLane(T, bits, ops[0].?, 0);

                        if (comptime isUnaryOp()) break :blk try operationUnary(TT, a);

                        const b = try Value.readLane(T, bits, ops[1].?, 0);

                        if (comptime isBinaryOp()) break :blk try operationBinary(TT, a, b);
                        if (comptime isTernaryOp()) break :blk try operationTernary(TT, a, b, ops[2].?);
                        if (comptime isQuaternaryOp()) break :blk try operationQuaternary(TT, a, b, ops[2].?, ops[3].?);
                    };

                    try Value.writeLane(T, bits, dst, 0, out);
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn applyVectorBits(lane_bits: SpvWord, dst: *Value, ops: [max_operator_count]?*const Value) RuntimeError!void {
            const dst_len = try dst.getLaneCount();

            switch (lane_bits) {
                inline 8, 16, 32, 64 => |bits| {
                    const TT = Value.getPrimitiveFieldType(T, bits);

                    for (0..dst_len) |i| {
                        const out: TT = blk: {
                            const a = try Value.readLane(T, bits, ops[0].?, if (ops[0].?.isVector()) i else 0);

                            if (comptime isUnaryOp()) break :blk try operationUnary(TT, a);

                            const b = try Value.readLane(T, bits, ops[1].?, if (ops[1].?.isVector()) i else 0);

                            if (comptime isBinaryOp()) break :blk try operationBinary(TT, a, b);
                            if (comptime isTernaryOp()) break :blk try operationTernary(TT, a, b, ops[2].?);
                            if (comptime isQuaternaryOp()) break :blk try operationQuaternary(TT, a, b, ops[2].?, ops[3].?);
                        };

                        try Value.writeLane(T, bits, dst, i, out);
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn BitEngine(comptime T: PrimitiveType, comptime Op: BitOp) type {
    return struct {
        fn op(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const operator = BitOperator(T, Op);

            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const dst = try rt.results[try rt.it.next()].getValue();

            var ops = [_]?*Value{null} ** operator.max_operator_count;
            ops[0] = try rt.results[try rt.it.next()].getValue();

            if (comptime operator.getOperatorsCount() >= 2) ops[1] = try rt.results[try rt.it.next()].getValue();
            if (comptime operator.getOperatorsCount() >= 3) ops[2] = try rt.results[try rt.it.next()].getValue();
            if (comptime operator.getOperatorsCount() >= 4) ops[3] = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

            switch (dst.*) {
                .Int => try operator.applyScalarBits(lane_bits, dst, ops),

                .Vector,
                .Vector2i32,
                .Vector3i32,
                .Vector4i32,
                .Vector2u32,
                .Vector3u32,
                .Vector4u32,
                => try operator.applyVectorBits(lane_bits, dst, ops),

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn CondOperator(comptime T: PrimitiveType, comptime Op: CondOp) type {
    return struct {
        inline fn isUnaryOp() bool {
            return comptime switch (Op) {
                .IsFinite, .IsInf, .IsNan, .IsNormal, .LogicalNot => true,
                else => false,
            };
        }

        inline fn operationBinary(comptime TT: type, a: TT, b: TT) RuntimeError!bool {
            if (comptime TT == bool) {
                switch (Op) {
                    .LogicalAnd => return a and b,
                    .LogicalOr => return a or b,
                    else => {},
                }
            }
            return switch (Op) {
                .Equal, .LogicalEqual => a == b,
                .NotEqual, .LogicalNotEqual => a != b,
                .Greater => a > b,
                .GreaterEqual => a >= b,
                .Less => a < b,
                .LessEqual => a <= b,
                else => RuntimeError.InvalidSpirV,
            };
        }

        inline fn operationUnary(comptime TT: type, a: TT) RuntimeError!bool {
            if (comptime TT == bool) {
                switch (Op) {
                    .LogicalNot => return !a,
                    else => {},
                }
            }
            return switch (Op) {
                .IsFinite => std.math.isFinite(a),
                .IsInf => std.math.isInf(a),
                .IsNan => std.math.isNan(a),
                .IsNormal => std.math.isNormal(a),
                else => RuntimeError.InvalidSpirV,
            };
        }

        fn applyScalarBits(bit_count: SpvWord, dst_bool: *Value, a_v: *const Value, b_v: ?*const Value) RuntimeError!void {
            switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| {
                    if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                    const TT = Value.getPrimitiveFieldType(T, bits);
                    const a = (try Value.getPrimitiveField(T, bits, @constCast(a_v))).*;

                    if (comptime isUnaryOp()) {
                        dst_bool.Bool = try operationUnary(TT, a);
                    } else {
                        const b_ptr = b_v orelse return RuntimeError.InvalidSpirV;
                        const b = (try Value.getPrimitiveField(T, bits, @constCast(b_ptr))).*;
                        dst_bool.Bool = try operationBinary(TT, a, b);
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        inline fn laneRhsPtr(op2_value: ?*Value, index: usize) ?*const Value {
            if (comptime Op == .LogicalNot) return null;
            const v = op2_value orelse return null;
            return &v.Vector[index];
        }

        inline fn applyFixedVectorBinary(
            comptime ElemT: type,
            comptime N: usize,
            dst: []Value,
            op1: *@Vector(N, ElemT),
            op2: *@Vector(N, ElemT),
        ) RuntimeError!void {
            inline for (0..N) |i| dst[i].Bool = try operationBinary(ElemT, op1[i], op2[i]);
        }

        inline fn applyFixedVectorUnary(
            comptime ElemT: type,
            comptime N: usize,
            dst: []Value,
            op1: *@Vector(N, ElemT),
        ) RuntimeError!void {
            inline for (0..N) |i| dst[i].Bool = try operationUnary(ElemT, op1[i]);
        }
    };
}

fn CondEngine(comptime T: PrimitiveType, comptime Op: CondOp) type {
    return struct {
        fn op(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            sw: switch ((try rt.results[try rt.it.next()].getVariant()).Type) {
                .Vector => |v| continue :sw (try rt.results[v.components_type_word].getVariant()).Type,
                .Bool => {},
                else => return RuntimeError.InvalidSpirV,
            }

            const dst = try rt.results[try rt.it.next()].getValue();

            const op1_result = &rt.results[try rt.it.next()];
            const op1_type = try op1_result.getValueTypeWord();
            const op1_value = try op1_result.getValue();

            const operator = CondOperator(T, Op);

            const op2_value: ?*Value = if (comptime operator.isUnaryOp()) null else try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth((try rt.results[op1_type].getVariant()).Type, rt);

            switch (dst.*) {
                .Bool => try operator.applyScalarBits(lane_bits, dst, op1_value, op2_value),

                .Vector => |dst_vec| {
                    switch (op1_value.*) {
                        .Vector => |op1_vec| for (dst_vec, op1_vec, 0..) |*d_lane, a_lane, i| {
                            const b_ptr = operator.laneRhsPtr(op2_value, i);
                            try operator.applyScalarBits(lane_bits, d_lane, &a_lane, b_ptr);
                        },

                        .Vector4f32 => |*op1_vec| {
                            if (comptime operator.isUnaryOp())
                                try operator.applyFixedVectorUnary(f32, 4, dst_vec, op1_vec)
                            else
                                try operator.applyFixedVectorBinary(f32, 4, dst_vec, op1_vec, &op2_value.?.Vector4f32);
                        },
                        .Vector3f32 => |*op1_vec| {
                            if (comptime operator.isUnaryOp())
                                try operator.applyFixedVectorUnary(f32, 3, dst_vec, op1_vec)
                            else
                                try operator.applyFixedVectorBinary(f32, 3, dst_vec, op1_vec, &op2_value.?.Vector3f32);
                        },
                        .Vector2f32 => |*op1_vec| {
                            if (comptime operator.isUnaryOp())
                                try operator.applyFixedVectorUnary(f32, 2, dst_vec, op1_vec)
                            else
                                try operator.applyFixedVectorBinary(f32, 2, dst_vec, op1_vec, &op2_value.?.Vector2f32);
                        },

                        //.Vector4i32 => |*op1_vec| {
                        //    if (comptime operator.isUnaryOp())
                        //        try operator.applyFixedVectorUnary(i32, 4, dst_vec, op1_vec)
                        //    else
                        //        try operator.applyFixedVectorBinary(i32, 4, dst_vec, op1_vec, &op2_value.?.Vector4i32);
                        //},
                        //.Vector3i32 => |*op1_vec| {
                        //    if (comptime operator.isUnaryOp())
                        //        try operator.applyFixedVectorUnary(i32, 3, dst_vec, op1_vec)
                        //    else
                        //        try operator.applyFixedVectorBinary(i32, 3, dst_vec, op1_vec, &op2_value.?.Vector3i32);
                        //},
                        //.Vector2i32 => |*op1_vec| {
                        //    if (comptime operator.isUnaryOp())
                        //        try operator.applyFixedVectorUnary(i32, 2, dst_vec, op1_vec)
                        //    else
                        //        try operator.applyFixedVectorBinary(i32, 2, dst_vec, op1_vec, &op2_value.?.Vector2i32);
                        //},

                        //.Vector4u32 => |*op1_vec| {
                        //    if (comptime operator.isUnaryOp())
                        //        try operator.applyFixedVectorUnary(u32, 4, dst_vec, op1_vec)
                        //    else
                        //        try operator.applyFixedVectorBinary(u32, 4, dst_vec, op1_vec, &op2_value.?.Vector4u32);
                        //},
                        //.Vector3u32 => |*op1_vec| {
                        //    if (comptime operator.isUnaryOp())
                        //        try operator.applyFixedVectorUnary(u32, 3, dst_vec, op1_vec)
                        //    else
                        //        try operator.applyFixedVectorBinary(u32, 3, dst_vec, op1_vec, &op2_value.?.Vector3u32);
                        //},
                        //.Vector2u32 => |*op1_vec| {
                        //    if (comptime operator.isUnaryOp())
                        //        try operator.applyFixedVectorUnary(u32, 2, dst_vec, op1_vec)
                        //    else
                        //        try operator.applyFixedVectorBinary(u32, 2, dst_vec, op1_vec, &op2_value.?.Vector2u32);
                        //},
                        else => return RuntimeError.InvalidSpirV,
                    }
                },

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn ConversionEngine(comptime from_kind: PrimitiveType, comptime to_kind: PrimitiveType) type {
    return struct {
        fn op(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const dst_value = try rt.results[try rt.it.next()].getValue();

            const src_result = &rt.results[try rt.it.next()];
            const src_type_word = try src_result.getValueTypeWord();
            const src_value = try src_result.getValue();

            const from_bits = try Result.resolveLaneBitWidth((try rt.results[src_type_word].getVariant()).Type, rt);
            const to_bits = try Result.resolveLaneBitWidth(target_type, rt);

            const caster = struct {
                fn castLane(comptime ToT: type, from_bit_count: SpvWord, from: *Value) RuntimeError!ToT {
                    return switch (from_bit_count) {
                        inline 8, 16, 32, 64 => |bits| blk: {
                            if (bits == 8 and from_kind == .Float) return RuntimeError.InvalidSpirV; // No f8
                            const v = (try Value.getPrimitiveField(from_kind, bits, from)).*;
                            break :blk std.math.lossyCast(ToT, v);
                        },
                        else => return RuntimeError.InvalidSpirV,
                    };
                }

                fn applyScalar(from_bit_count: SpvWord, to_bit_count: SpvWord, dst: *Value, from: *Value) RuntimeError!void {
                    switch (to_bit_count) {
                        inline 8, 16, 32, 64 => |bits| {
                            if (bits == 8 and to_kind == .Float) return RuntimeError.InvalidSpirV; // No f8
                            const ToT = Value.getPrimitiveFieldType(to_kind, bits);
                            (try Value.getPrimitiveField(to_kind, bits, dst)).* = try castLane(ToT, from_bit_count, from);
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }

                fn castSIMDVector(comptime ToT: type, comptime N: usize, dst_arr: *@Vector(N, ToT), src_arr: *const @Vector(N, ToT)) void {
                    inline for (0..N) |i| dst_arr[i] = std.math.lossyCast(ToT, src_arr[i]);
                }

                fn castSIMDVectorFromOther(comptime ToT: type, comptime FromT: type, comptime N: usize, dst_arr: *@Vector(N, ToT), src_arr: *const @Vector(N, FromT)) void {
                    inline for (0..N) |i| dst_arr[i] = std.math.lossyCast(ToT, src_arr[i]);
                }
            };

            switch (dst_value.*) {
                .Float => {
                    if (to_kind != .Float) return RuntimeError.InvalidSpirV;
                    try caster.applyScalar(from_bits, to_bits, dst_value, src_value);
                },
                .Int => {
                    if (to_kind != .SInt and to_kind != .UInt) return RuntimeError.InvalidSpirV;
                    try caster.applyScalar(from_bits, to_bits, dst_value, src_value);
                },
                .Vector => |dst_vec| {
                    const src_vec = src_value.Vector;
                    if (dst_vec.len != src_vec.len) return RuntimeError.InvalidSpirV;
                    for (dst_vec, src_vec) |*d_lane, *s_lane| {
                        try caster.applyScalar(from_bits, to_bits, d_lane, s_lane);
                    }
                },

                .Vector4f32 => |*dst| switch (src_value.*) {
                    .Vector4f32 => caster.castSIMDVector(f32, 4, dst, &src_value.Vector4f32),
                    .Vector4i32 => caster.castSIMDVectorFromOther(f32, i32, 4, dst, &src_value.Vector4i32),
                    .Vector4u32 => caster.castSIMDVectorFromOther(f32, u32, 4, dst, &src_value.Vector4u32),
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3f32 => |*dst| switch (src_value.*) {
                    .Vector3f32 => caster.castSIMDVector(f32, 3, dst, &src_value.Vector3f32),
                    .Vector3i32 => caster.castSIMDVectorFromOther(f32, i32, 3, dst, &src_value.Vector3i32),
                    .Vector3u32 => caster.castSIMDVectorFromOther(f32, u32, 3, dst, &src_value.Vector3u32),
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector2f32 => |*dst| switch (src_value.*) {
                    .Vector2f32 => caster.castSIMDVector(f32, 2, dst, &src_value.Vector2f32),
                    .Vector2i32 => caster.castSIMDVectorFromOther(f32, i32, 2, dst, &src_value.Vector2i32),
                    .Vector2u32 => caster.castSIMDVectorFromOther(f32, u32, 2, dst, &src_value.Vector2u32),
                    else => return RuntimeError.InvalidSpirV,
                },

                .Vector4i32 => |*dst| switch (src_value.*) {
                    .Vector4f32 => caster.castSIMDVectorFromOther(i32, f32, 4, dst, &src_value.Vector4f32),
                    .Vector4i32 => caster.castSIMDVector(i32, 4, dst, &src_value.Vector4i32),
                    .Vector4u32 => caster.castSIMDVectorFromOther(i32, u32, 4, dst, &src_value.Vector4u32),
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3i32 => |*dst| switch (src_value.*) {
                    .Vector3f32 => caster.castSIMDVectorFromOther(i32, f32, 3, dst, &src_value.Vector3f32),
                    .Vector3i32 => caster.castSIMDVector(i32, 3, dst, &src_value.Vector3i32),
                    .Vector3u32 => caster.castSIMDVectorFromOther(i32, u32, 3, dst, &src_value.Vector3u32),
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector2i32 => |*dst| switch (src_value.*) {
                    .Vector2f32 => caster.castSIMDVectorFromOther(i32, f32, 2, dst, &src_value.Vector2f32),
                    .Vector2i32 => caster.castSIMDVector(i32, 2, dst, &src_value.Vector2i32),
                    .Vector2u32 => caster.castSIMDVectorFromOther(i32, u32, 2, dst, &src_value.Vector2u32),
                    else => return RuntimeError.InvalidSpirV,
                },

                .Vector4u32 => |*dst| switch (src_value.*) {
                    .Vector4f32 => caster.castSIMDVectorFromOther(u32, f32, 4, dst, &src_value.Vector4f32),
                    .Vector4i32 => caster.castSIMDVectorFromOther(u32, i32, 4, dst, &src_value.Vector4i32),
                    .Vector4u32 => caster.castSIMDVector(u32, 4, dst, &src_value.Vector4u32),
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3u32 => |*dst| switch (src_value.*) {
                    .Vector3f32 => caster.castSIMDVectorFromOther(u32, f32, 3, dst, &src_value.Vector3f32),
                    .Vector3i32 => caster.castSIMDVectorFromOther(u32, i32, 3, dst, &src_value.Vector3i32),
                    .Vector3u32 => caster.castSIMDVector(u32, 3, dst, &src_value.Vector3u32),
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector2u32 => |*dst| switch (src_value.*) {
                    .Vector2f32 => caster.castSIMDVectorFromOther(u32, f32, 2, dst, &src_value.Vector2f32),
                    .Vector2i32 => caster.castSIMDVectorFromOther(u32, i32, 2, dst, &src_value.Vector2i32),
                    .Vector2u32 => caster.castSIMDVector(u32, 2, dst, &src_value.Vector2u32),
                    else => return RuntimeError.InvalidSpirV,
                },

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn MathEngine(comptime T: PrimitiveType, comptime Op: MathOp, comptime IsAtomic: bool) type {
    return struct {
        fn op(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const dst = try rt.results[try rt.it.next()].getValue();
            const lhs = try rt.results[try rt.it.next()].getValue();

            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();

            var lhs_save: ?Value = null;

            if (comptime IsAtomic) {
                _ = rt.it.skip(); // scope
                _ = rt.it.skip(); // semantic
                lhs_save = try lhs.dupe(arena.allocator());
            }

            const rhs = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

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

                fn applyScalar(bit_count: SpvWord, d: *Value, l: *Value, r: *Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |bits| {
                            if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                            const ScalarT = Value.getPrimitiveFieldType(T, bits);
                            const d_field = try Value.getPrimitiveField(T, bits, d);
                            const l_field = try Value.getPrimitiveField(T, bits, l);
                            const r_field = try Value.getPrimitiveField(T, bits, r);
                            d_field.* = try operation(ScalarT, l_field.*, r_field.*);
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }

                inline fn applyVectorTimesScalarF32(d: []Value, l: []const Value, r: f32) void {
                    for (d, l) |*d_v, l_v| {
                        d_v.Float.value.float32 = l_v.Float.value.float32 * r;
                    }
                }

                inline fn applySIMDVector(comptime ElemT: type, comptime N: usize, d: *@Vector(N, ElemT), l: *const @Vector(N, ElemT), r: *const @Vector(N, ElemT)) RuntimeError!void {
                    inline for (0..N) |i| {
                        d[i] = try operation(ElemT, l[i], r[i]);
                    }
                }

                inline fn applyVectorSIMDTimesScalarF32(comptime N: usize, d: *@Vector(N, f32), l: *const @Vector(N, f32), r: f32) void {
                    inline for (0..N) |i| {
                        d[i] = l[i] * r;
                    }
                }

                inline fn applySIMDVectorf32(comptime N: usize, d: *@Vector(N, f32), l: *const @Vector(N, f32), r: *const Value) RuntimeError!void {
                    switch (Op) {
                        .VectorTimesScalar => applyVectorSIMDTimesScalarF32(N, d, l, r.Float.value.float32),
                        else => {
                            const rh: *const @Vector(N, f32) = switch (N) {
                                2 => &r.Vector2f32,
                                3 => &r.Vector3f32,
                                4 => &r.Vector4f32,
                                else => unreachable,
                            };
                            try applySIMDVector(f32, N, d, l, rh);
                        },
                    }
                }
            };

            switch (dst.*) {
                .Int, .Float => try operator.applyScalar(lane_bits, dst, lhs, rhs),

                .Vector => |dst_vec| switch (Op) {
                    .VectorTimesScalar => operator.applyVectorTimesScalarF32(dst_vec, lhs.Vector, rhs.Float.value.float32),
                    else => for (dst_vec, lhs.Vector, rhs.Vector) |*d_lane, *l_lane, *r_lane| {
                        try operator.applyScalar(lane_bits, d_lane, l_lane, r_lane);
                    },
                },

                .Vector4f32 => |*d| try operator.applySIMDVectorf32(4, d, &lhs.Vector4f32, rhs),
                .Vector3f32 => |*d| try operator.applySIMDVectorf32(3, d, &lhs.Vector3f32, rhs),
                .Vector2f32 => |*d| try operator.applySIMDVectorf32(2, d, &lhs.Vector2f32, rhs),

                .Vector4i32 => |*d| try operator.applySIMDVector(i32, 4, d, &lhs.Vector4i32, &rhs.Vector4i32),
                .Vector3i32 => |*d| try operator.applySIMDVector(i32, 3, d, &lhs.Vector3i32, &rhs.Vector3i32),
                .Vector2i32 => |*d| try operator.applySIMDVector(i32, 2, d, &lhs.Vector2i32, &rhs.Vector2i32),

                .Vector4u32 => |*d| try operator.applySIMDVector(u32, 4, d, &lhs.Vector4u32, &rhs.Vector4u32),
                .Vector3u32 => |*d| try operator.applySIMDVector(u32, 3, d, &lhs.Vector3u32, &rhs.Vector3u32),
                .Vector2u32 => |*d| try operator.applySIMDVector(u32, 2, d, &lhs.Vector2u32, &rhs.Vector2u32),

                else => return RuntimeError.InvalidSpirV,
            }

            if (comptime IsAtomic) {
                try copyValue(lhs, dst);
                try copyValue(dst, &lhs_save.?);
                try lhs.flushPtr(allocator);
            }
        }

        fn opSingle(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const dst = try rt.results[try rt.it.next()].getValue();
            const val = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

            const operator = struct {
                fn operation(comptime TT: type, ope: TT) RuntimeError!TT {
                    return switch (Op) {
                        .Negate => if (@typeInfo(TT) == .int) std.math.negate(ope) catch return RuntimeError.InvalidSpirV else -ope,
                        else => return RuntimeError.InvalidSpirV,
                    };
                }

                fn applyScalar(bit_count: SpvWord, d: *Value, v: *Value) RuntimeError!void {
                    switch (bit_count) {
                        inline 8, 16, 32, 64 => |bits| {
                            if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                            const ScalarT = Value.getPrimitiveFieldType(T, bits);
                            const d_field = try Value.getPrimitiveField(T, bits, d);
                            const v_field = try Value.getPrimitiveField(T, bits, v);
                            d_field.* = try operation(ScalarT, v_field.*);
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }

                inline fn applySIMDVector(comptime ElemT: type, comptime N: usize, d: *@Vector(N, ElemT), v: *const @Vector(N, ElemT)) RuntimeError!void {
                    inline for (0..N) |i| {
                        d[i] = try operation(ElemT, v[i]);
                    }
                }
            };

            switch (dst.*) {
                .Int, .Float => try operator.applyScalar(lane_bits, dst, val),

                .Vector => |dst_vec| for (dst_vec, val.Vector) |*d_lane, *v_lane| {
                    try operator.applyScalar(lane_bits, d_lane, v_lane);
                },

                .Vector4f32 => |*d| try operator.applySIMDVector(f32, 4, d, &val.Vector4f32),
                .Vector3f32 => |*d| try operator.applySIMDVector(f32, 3, d, &val.Vector3f32),
                .Vector2f32 => |*d| try operator.applySIMDVector(f32, 2, d, &val.Vector2f32),

                .Vector4i32 => |*d| try operator.applySIMDVector(i32, 4, d, &val.Vector4i32),
                .Vector3i32 => |*d| try operator.applySIMDVector(i32, 3, d, &val.Vector3i32),
                .Vector2i32 => |*d| try operator.applySIMDVector(i32, 2, d, &val.Vector2i32),

                .Vector4u32 => |*d| try operator.applySIMDVector(u32, 4, d, &val.Vector4u32),
                .Vector3u32 => |*d| try operator.applySIMDVector(u32, 3, d, &val.Vector3u32),
                .Vector2u32 => |*d| try operator.applySIMDVector(u32, 2, d, &val.Vector2u32),

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn addDecoration(allocator: std.mem.Allocator, rt: *Runtime, target: SpvWord, decoration_type: spv.SpvDecoration, member: ?SpvWord) RuntimeError!void {
    var decoration = rt.results[target].decorations.addOne(allocator) catch return RuntimeError.OutOfMemory;
    decoration.rtype = decoration_type;
    decoration.index = if (member) |memb| memb else 0;

    switch (decoration_type) {
        .Alignment,
        .AlignmentId,
        .ArrayStride,
        .Binding,
        .BuiltIn,
        .Component,
        .CounterBuffer,
        .DescriptorSet,
        .FPFastMathMode,
        .FPRoundingMode,
        .FuncParamAttr,
        .Index,
        .InputAttachmentIndex,
        .Location,
        .MatrixStride,
        .MaxByteOffset,
        .MaxByteOffsetId,
        .Offset,
        .SecondaryViewportRelativeNV,
        .SpecId,
        .Stream,
        .UniformId,
        .UserSemantic,
        .UserTypeGOOGLE,
        .XfbBuffer,
        .XfbStride,
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

fn cloneDecorationTo(allocator: std.mem.Allocator, rt: *Runtime, target: SpvWord, decoration: *const Result.Decoration, member: ?SpvWord) RuntimeError!void {
    const out = rt.results[target].decorations.addOne(allocator) catch return RuntimeError.OutOfMemory;
    out.* = decoration.*;
    out.index = if (member) |m| m else decoration.index;
}

fn autoSetupConstant(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try setupConstant(allocator, rt);
}

fn setupAccessChain(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();
    const base_id = try rt.it.next();

    const index_count: usize = @intCast(word_count - 3);
    const indexes = allocator.alloc(SpvWord, index_count) catch return RuntimeError.OutOfMemory;
    errdefer allocator.free(indexes);

    for (indexes) |*index| {
        index.* = try rt.it.next();
    }

    if (rt.results[id].variant) |*variant| {
        switch (variant.*) {
            .AccessChain => |*a| {
                allocator.free(a.indexes);
                a.value.deinit(allocator);
            },
            else => {},
        }
    }

    rt.results[id].variant = .{
        .AccessChain = .{
            .target = var_type,
            .base = base_id,
            .indexes = indexes,
            .value = try Value.init(allocator, rt.results, var_type, false),
        },
    };
}

fn copyValue(dst: *Value, src: *const Value) RuntimeError!void {
    const helpers = struct {
        inline fn copySlice(dst_slice: []Value, src_slice: []const Value) RuntimeError!void {
            for (0..@min(dst_slice.len, src_slice.len)) |i| {
                try copyValue(&dst_slice[i], &src_slice[i]);
            }
        }

        inline fn getDstSlice(v: *Value) ?[]Value {
            return switch (v.*) {
                .Vector, .Matrix => |s| s,
                .Array => |a| a.values,
                .Structure => |s| s.values,
                else => null,
            };
        }

        inline fn writeF32(dst_f32_ptr: *f32, src_v: *const Value) RuntimeError!void {
            switch (src_v.*) {
                .Pointer => |src_ptr| switch (src_ptr.ptr) {
                    .f32_ptr => |src_f32_ptr| dst_f32_ptr.* = src_f32_ptr.*,
                    .common => |src_val_ptr| dst_f32_ptr.* = src_val_ptr.Float.value.float32,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Float => |f| dst_f32_ptr.* = f.value.float32,
                else => return RuntimeError.InvalidSpirV,
            }
        }

        inline fn writeI32(dst_i32_ptr: *i32, src_v: *const Value) RuntimeError!void {
            switch (src_v.*) {
                .Pointer => |src_ptr| switch (src_ptr.ptr) {
                    .i32_ptr => |src_i32_ptr| dst_i32_ptr.* = src_i32_ptr.*,
                    .common => |src_val_ptr| dst_i32_ptr.* = src_val_ptr.Int.value.sint32,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Int => |i| dst_i32_ptr.* = i.value.sint32,
                else => return RuntimeError.InvalidSpirV,
            }
        }

        inline fn writeU32(dst_u32_ptr: *u32, src_v: *const Value) RuntimeError!void {
            switch (src_v.*) {
                .Pointer => |src_ptr| switch (src_ptr.ptr) {
                    .u32_ptr => |src_u32_ptr| dst_u32_ptr.* = src_u32_ptr.*,
                    .common => |src_val_ptr| dst_u32_ptr.* = src_val_ptr.Int.value.uint32,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Int => |i| dst_u32_ptr.* = i.value.uint32,
                else => return RuntimeError.InvalidSpirV,
            }
        }

        inline fn readF32(dst_v: *Value, src_f32_ptr: *const f32) RuntimeError!void {
            switch (dst_v.*) {
                .Pointer => |dst_ptr| switch (dst_ptr.ptr) {
                    .f32_ptr => |dst_f32_ptr| dst_f32_ptr.* = src_f32_ptr.*,
                    .common => |dst_val_ptr| dst_val_ptr.Float.value.float32 = src_f32_ptr.*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Float => |*f| f.value.float32 = src_f32_ptr.*,
                else => return RuntimeError.InvalidSpirV,
            }
        }

        inline fn readI32(dst_v: *Value, src_i32_ptr: *const i32) RuntimeError!void {
            switch (dst_v.*) {
                .Pointer => |dst_ptr| switch (dst_ptr.ptr) {
                    .i32_ptr => |dst_i32_ptr| dst_i32_ptr.* = src_i32_ptr.*,
                    .common => |dst_val_ptr| dst_val_ptr.Int.value.sint32 = src_i32_ptr.*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Int => |*i| i.value.sint32 = src_i32_ptr.*,
                else => return RuntimeError.InvalidSpirV,
            }
        }

        inline fn readU32(dst_v: *Value, src_u32_ptr: *const u32) RuntimeError!void {
            switch (dst_v.*) {
                .Pointer => |dst_ptr| switch (dst_ptr.ptr) {
                    .u32_ptr => |dst_u32_ptr| dst_u32_ptr.* = src_u32_ptr.*,
                    .common => |dst_val_ptr| dst_val_ptr.Int.value.uint32 = src_u32_ptr.*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Int => |*i| i.value.uint32 = src_u32_ptr.*,
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };

    if (std.meta.activeTag(dst.*) == .Pointer) {
        switch (dst.Pointer.ptr) {
            .common => |dst_val_ptr| return switch (src.*) {
                .Pointer => |src_ptr| switch (src_ptr.ptr) {
                    .common => |src_val_ptr| try copyValue(dst_val_ptr, src_val_ptr),
                    else => dst_val_ptr.* = src.*,
                },
                else => try copyValue(dst_val_ptr, src),
            },
            .f32_ptr => |dst_f32_ptr| try helpers.writeF32(dst_f32_ptr, src),
            .i32_ptr => |dst_i32_ptr| try helpers.writeI32(dst_i32_ptr, src),
            .u32_ptr => |dst_u32_ptr| try helpers.writeU32(dst_u32_ptr, src),
        }
    }

    switch (src.*) {
        .Vector, .Matrix => |src_slice| {
            const dst_slice = helpers.getDstSlice(dst);
            try helpers.copySlice(dst_slice.?, src_slice);
        },
        .Array => |a| {
            const dst_slice = helpers.getDstSlice(dst);
            try helpers.copySlice(dst_slice.?, a.values);
        },
        .Structure => |s| {
            const dst_slice = helpers.getDstSlice(dst);
            try helpers.copySlice(dst_slice.?, s.values);
        },
        .Pointer => |ptr| switch (ptr.ptr) {
            .common => |src_val_ptr| try copyValue(dst, src_val_ptr),
            .f32_ptr => |src_f32_ptr| try helpers.readF32(dst, src_f32_ptr),
            .i32_ptr => |src_i32_ptr| try helpers.readI32(dst, src_i32_ptr),
            .u32_ptr => |src_u32_ptr| try helpers.readU32(dst, src_u32_ptr),
        },
        .RuntimeArray => |src_arr| switch (dst.*) {
            .RuntimeArray => |dst_arr| @memcpy(dst_arr.data, src_arr.data),
            .Pointer => |dst_ptr| switch (dst_ptr.ptr) {
                .common => |dst_ptr_ptr| switch (dst_ptr_ptr.*) {
                    .RuntimeArray => |dst_arr| @memcpy(dst_arr.data, src_arr.data),
                    else => return RuntimeError.InvalidSpirV,
                },
                else => return RuntimeError.InvalidSpirV,
            },
            else => return RuntimeError.InvalidSpirV,
        },
        else => dst.* = src.*,
    }
}

fn opAccessChain(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();
    const base_id = try rt.it.next();

    const base = &rt.results[base_id];
    var value_ptr = try base.getValue();

    const index_count: usize = @intCast(word_count - 3);

    const indexes, const free_responsability = blk: {
        if (rt.results[id].variant) |*variant| {
            switch (variant.*) {
                .AccessChain => |*a| {
                    if (a.indexes.len != index_count)
                        return RuntimeError.InvalidSpirV;
                    try a.value.flushPtr(allocator);
                    a.value.deinit(allocator);
                    break :blk .{ a.indexes, false };
                },
                else => {},
            }
        }
        break :blk .{ allocator.alloc(SpvWord, index_count) catch return RuntimeError.OutOfMemory, true };
    };
    errdefer if (free_responsability) allocator.free(indexes);

    rt.results[id].variant = .{
        .AccessChain = .{
            .target = var_type,
            .base = base_id,
            .indexes = indexes,
            .value = blk: {
                const helpers = struct {
                    fn advanceWindow(window: ?[]u8, offset: usize) RuntimeError!?[]u8 {
                        if (window) |w| {
                            if (offset > w.len) return RuntimeError.OutOfBounds;
                            return w[offset..];
                        }
                        return null;
                    }

                    fn advanceWindowSized(window: ?[]u8, offset: usize, size: usize) RuntimeError!?[]u8 {
                        if (window) |w| {
                            if (offset > w.len or size > w.len - offset) return RuntimeError.OutOfBounds;
                            return w[offset .. offset + size];
                        }
                        return null;
                    }
                };

                var uniform_slice_window: ?[]u8 = null;
                var uniform_backing_value: ?*Value = null;

                if (std.meta.activeTag(value_ptr.*) == .Pointer) {
                    const ptr = value_ptr.Pointer;
                    uniform_slice_window = ptr.uniform_slice_window;
                    uniform_backing_value = ptr.uniform_backing_value;
                    switch (ptr.ptr) {
                        .common => |common| value_ptr = common,
                        else => return RuntimeError.InvalidSpirV,
                    }
                }

                for (0..index_count) |index| {
                    const index_id = try rt.it.next();
                    indexes[index] = index_id;
                    const member = &rt.results[index_id];
                    const member_value = switch ((try member.getVariant()).*) {
                        .Constant => |c| &c.value,
                        .Variable => |v| &v.value,
                        else => return RuntimeError.InvalidSpirV,
                    };

                    switch (member_value.*) {
                        .Int => |i| {
                            if (std.meta.activeTag(value_ptr.*) == .Pointer) {
                                const ptr = value_ptr.Pointer;
                                uniform_slice_window = ptr.uniform_slice_window;
                                uniform_backing_value = ptr.uniform_backing_value;
                                switch (ptr.ptr) {
                                    .common => |common| value_ptr = common,
                                    else => return RuntimeError.InvalidSpirV,
                                }
                            }

                            const component_index: usize = @intCast(i.value.uint32);

                            switch (value_ptr.*) {
                                .Vector, .Matrix => |v| {
                                    if (component_index >= v.len) return RuntimeError.OutOfBounds;
                                    var offset: usize = 0;
                                    for (v[0..component_index]) |*element| {
                                        offset += try element.getPlainMemorySize();
                                    }
                                    uniform_slice_window = try helpers.advanceWindow(uniform_slice_window, offset);
                                    value_ptr = &v[component_index];
                                },
                                .Array => |a| {
                                    if (component_index >= a.values.len) return RuntimeError.OutOfBounds;
                                    uniform_slice_window = try helpers.advanceWindow(uniform_slice_window, component_index * a.stride);
                                    value_ptr = &a.values[component_index];
                                },
                                .Structure => |s| {
                                    if (component_index >= s.values.len) return RuntimeError.OutOfBounds;
                                    var end_offset: usize = 0;
                                    for (s.values[0..component_index], 0..) |*field, field_index| {
                                        const field_offset: usize = @intCast(s.offsets[field_index] orelse end_offset);
                                        end_offset = @max(end_offset, field_offset + try field.getPlainMemorySize());
                                    }
                                    const member_offset: usize = @intCast(s.offsets[component_index] orelse end_offset);
                                    uniform_slice_window = try helpers.advanceWindow(uniform_slice_window, member_offset);
                                    value_ptr = &s.values[component_index];
                                },
                                .RuntimeArray => |*arr| {
                                    if (component_index >= arr.getLen()) return RuntimeError.OutOfBounds;

                                    const element_offset = arr.getOffsetOfIndex(component_index);
                                    if (element_offset > arr.data.len or arr.stride > arr.data.len - element_offset)
                                        return RuntimeError.OutOfBounds;

                                    const backing = try arr.createValueFromIndex(allocator, rt.results, component_index);
                                    errdefer {
                                        backing.deinit(allocator);
                                        allocator.destroy(backing);
                                    }

                                    if (uniform_backing_value) |old_backing| {
                                        old_backing.deinit(allocator);
                                        allocator.destroy(old_backing);
                                    }

                                    value_ptr = backing;
                                    uniform_backing_value = backing;
                                    uniform_slice_window = arr.data[element_offset .. element_offset + arr.stride];
                                },
                                .Vector4f32 => |*v| switch (component_index) {
                                    inline 0...3 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .f32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(f32), @sizeOf(f32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                .Vector3f32 => |*v| switch (component_index) {
                                    inline 0...2 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .f32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(f32), @sizeOf(f32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                .Vector2f32 => |*v| switch (component_index) {
                                    inline 0...1 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .f32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(f32), @sizeOf(f32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                .Vector4i32 => |*v| switch (component_index) {
                                    inline 0...3 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .i32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(i32), @sizeOf(i32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                .Vector3i32 => |*v| switch (component_index) {
                                    inline 0...2 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .i32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(i32), @sizeOf(i32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                .Vector2i32 => |*v| switch (component_index) {
                                    inline 0...1 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .i32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(i32), @sizeOf(i32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                .Vector4u32 => |*v| switch (component_index) {
                                    inline 0...3 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .u32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(u32), @sizeOf(u32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                .Vector3u32 => |*v| switch (component_index) {
                                    inline 0...2 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .u32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(u32), @sizeOf(u32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                .Vector2u32 => |*v| switch (component_index) {
                                    inline 0...1 => |idx| break :blk .{ .Pointer = .{
                                        .ptr = .{ .u32_ptr = &v[idx] },
                                        .uniform_slice_window = try helpers.advanceWindowSized(uniform_slice_window, idx * @sizeOf(u32), @sizeOf(u32)),
                                        .uniform_backing_value = uniform_backing_value,
                                    } },
                                    else => return RuntimeError.InvalidSpirV,
                                },
                                else => return RuntimeError.InvalidSpirV,
                            }
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                }
                break :blk .{
                    .Pointer = .{
                        .ptr = .{ .common = value_ptr },
                        .uniform_slice_window = uniform_slice_window,
                        .uniform_backing_value = uniform_backing_value,
                    },
                };
            },
        },
    };
}
fn opAtomicStore(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const ptr_id = try rt.it.next();
    _ = rt.it.skip(); // scope
    _ = rt.it.skip(); // semantic
    const val_id = try rt.it.next();
    try copyValue(try rt.results[ptr_id].getValue(), try rt.results[val_id].getValue());
}

fn opBitcast(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const to_value = try rt.results[try rt.it.next()].getValue();
    const from_value = try rt.results[try rt.it.next()].getValue();

    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();
    const local_allocator = arena.allocator();

    const size = try to_value.getPlainMemorySize();
    const bytes = local_allocator.alloc(u8, size) catch return RuntimeError.OutOfMemory;
    _ = try from_value.read(bytes);
    _ = try to_value.write(bytes);
}

fn opBranch(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.previous_label = rt.current_label;
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
    rt.previous_label = rt.current_label;
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
        .RuntimeArray => |arr| {
            var offset: usize = 0;

            for (0..index_count) |_| {
                // DOES NOT WORK : FIXME
                const elem_value = (try rt.results[try rt.it.next()].getVariant()).Constant.value;
                std.mem.copyForwards(u8, arr.data[offset..(offset + arr.stride)], std.mem.asBytes(&elem_value));
                offset += arr.stride;
            }
        },
        .Vector4f32 => |*vec| inline for (0..4) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Float.value.float32;
        },
        .Vector3f32 => |*vec| inline for (0..3) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Float.value.float32;
        },
        .Vector2f32 => |*vec| inline for (0..2) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Float.value.float32;
        },
        .Vector4i32 => |*vec| inline for (0..4) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.value.sint32;
        },
        .Vector3i32 => |*vec| inline for (0..3) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.value.sint32;
        },
        .Vector2i32 => |*vec| inline for (0..2) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.value.sint32;
        },
        .Vector4u32 => |*vec| inline for (0..4) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.value.uint32;
        },
        .Vector3u32 => |*vec| inline for (0..3) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.value.uint32;
        },
        .Vector2u32 => |*vec| inline for (0..2) |i| {
            vec[i] = (try rt.results[try rt.it.next()].getVariant()).Constant.value.Int.value.uint32;
        },
        else => return RuntimeError.InvalidValueType,
    }
}

fn opCompositeExtract(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const res_type = try rt.it.next();
    const id = try rt.it.next();
    const composite_id = try rt.it.next();
    const index_count: usize = @intCast(word_count - 3);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const arena_allocator = arena.allocator();

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
                        .RuntimeArray => |arr| composite = try arr.createLocalValueFromIndex(arena_allocator, rt.results, member_id),
                        .Vector4f32 => |v| break :blk .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (member_id) {
                            inline 0...3 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        .Vector3f32 => |v| break :blk .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (member_id) {
                            inline 0...2 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        .Vector2f32 => |v| break :blk .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (member_id) {
                            inline 0...1 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        .Vector4i32 => |v| break :blk .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (member_id) {
                            inline 0...3 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        .Vector3i32 => |v| break :blk .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (member_id) {
                            inline 0...2 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        .Vector2i32 => |v| break :blk .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (member_id) {
                            inline 0...1 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        .Vector4u32 => |v| break :blk .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (member_id) {
                            inline 0...3 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        .Vector3u32 => |v| break :blk .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (member_id) {
                            inline 0...2 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        .Vector2u32 => |v| break :blk .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (member_id) {
                            inline 0...1 => |idx| v[idx],
                            else => return RuntimeError.OutOfBounds,
                        } } } },
                        else => return RuntimeError.InvalidValueType,
                    }
                }
                break :blk try composite.dupe(allocator);
            },
        },
    };
}

fn opCompositeInsert(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next();
    const id = try rt.it.next();
    const object = try rt.results[try rt.it.next()].getValue();
    const composite = try rt.results[try rt.it.next()].getValue();

    const target = try rt.results[id].getValue();

    try copyValue(target, composite);

    const index_count = word_count - 4;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const helpers = struct {
        fn insertAt(
            alloc: std.mem.Allocator,
            results: []const Result,
            current: *Value,
            object_value: *const Value,
            indices: []const SpvWord,
        ) RuntimeError!void {
            if (indices.len == 0) {
                try copyValue(current, object_value);
                return;
            }

            const index = indices[0];

            if (current.getCompositeDataOrNull()) |children| {
                if (index >= children.len) return RuntimeError.OutOfBounds;
                return insertAt(alloc, results, &children[index], object_value, indices[1..]);
            }

            switch (current.*) {
                .Structure => |*s| {
                    if (index >= s.values.len) return RuntimeError.OutOfBounds;
                    return insertAt(alloc, results, &s.values[index], object_value, indices[1..]);
                },

                .RuntimeArray => |*arr| {
                    if (index >= arr.getLen()) return RuntimeError.OutOfBounds;

                    const elem_offset = arr.getOffsetOfIndex(index);

                    if (indices.len == 1) {
                        _ = try object_value.read(arr.data[elem_offset..]);
                        return;
                    }

                    var elem_value = try arr.createLocalValueFromIndex(alloc, results, elem_offset);
                    try insertAt(alloc, results, &elem_value, object_value, indices[1..]);
                    _ = try elem_value.read(arr.data[elem_offset..]);
                },

                .Vector4f32 => |*v| switch (index) {
                    inline 0...3 => |i| v[i] = (try Value.getPrimitiveField(.Float, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3f32 => |*v| switch (index) {
                    inline 0...2 => |i| v[i] = (try Value.getPrimitiveField(.Float, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector2f32 => |*v| switch (index) {
                    inline 0...1 => |i| v[i] = (try Value.getPrimitiveField(.Float, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },

                .Vector4i32 => |*v| switch (index) {
                    inline 0...3 => |i| v[i] = (try Value.getPrimitiveField(.SInt, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3i32 => |*v| switch (index) {
                    inline 0...2 => |i| v[i] = (try Value.getPrimitiveField(.SInt, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector2i32 => |*v| switch (index) {
                    inline 0...1 => |i| v[i] = (try Value.getPrimitiveField(.SInt, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },

                .Vector4u32 => |*v| switch (index) {
                    inline 0...3 => |i| v[i] = (try Value.getPrimitiveField(.UInt, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3u32 => |*v| switch (index) {
                    inline 0...2 => |i| v[i] = (try Value.getPrimitiveField(.UInt, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector2u32 => |*v| switch (index) {
                    inline 0...1 => |i| v[i] = (try Value.getPrimitiveField(.UInt, 32, @constCast(object_value))).*,
                    else => return RuntimeError.InvalidSpirV,
                },

                else => return RuntimeError.InvalidValueType,
            }
        }
    };

    const indices = try arena.allocator().alloc(SpvWord, index_count);
    for (indices) |*idx| idx.* = try rt.it.next();

    try helpers.insertAt(arena.allocator(), rt.results, target, object, indices);
}

fn opConstant(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    switch (target.variant.?.Constant.value) {
        .Int => |*i| {
            if (word_count - 2 != 1) {
                const low = @as(u64, try rt.it.next());
                const high = @as(u64, try rt.it.next());
                i.value.uint64 = (high << 32) | low;
            } else {
                i.value.uint32 = try rt.it.next();
            }
        },
        .Float => |*f| {
            if (word_count - 2 != 1) {
                const low = @as(u64, try rt.it.next());
                const high = @as(u64, try rt.it.next());
                f.value.float64 = @bitCast((high << 32) | low);
            } else {
                f.value.float32 = @bitCast(try rt.it.next());
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opConstantComposite(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    const target_value = try target.getValue();
    if (target_value.getCompositeDataOrNull()) |*values| {
        for (values.*) |*element| {
            try copyValue(element, try rt.results[try rt.it.next()].getValue());
        }
        return;
    }

    switch (target_value.*) {
        .Vector4f32 => |*v| inline for (0..4) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Float.value.float32;
        },
        .Vector3f32 => |*v| inline for (0..3) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Float.value.float32;
        },
        .Vector2f32 => |*v| inline for (0..2) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Float.value.float32;
        },
        .Vector4i32 => |*v| inline for (0..4) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Int.value.sint32;
        },
        .Vector3i32 => |*v| inline for (0..3) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Int.value.sint32;
        },
        .Vector2i32 => |*v| inline for (0..2) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Int.value.sint32;
        },
        .Vector4u32 => |*v| inline for (0..4) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Int.value.uint32;
        },
        .Vector3u32 => |*v| inline for (0..3) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Int.value.uint32;
        },
        .Vector2u32 => |*v| inline for (0..2) |i| {
            v[i] = (try rt.results[try rt.it.next()].getValue()).Int.value.uint32;
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn writeMulExtendedBits(comptime bits: u32, dst: *Value, lane_index: usize, value: Value.getPrimitiveFieldType(.UInt, bits)) RuntimeError!void {
    switch (dst.*) {
        .Int => |*i| {
            if (i.bit_count != bits) return RuntimeError.InvalidSpirV;
            if (i.is_signed) {
                switch (bits) {
                    8 => i.value.sint8 = @bitCast(value),
                    16 => i.value.sint16 = @bitCast(value),
                    32 => i.value.sint32 = @bitCast(value),
                    64 => i.value.sint64 = @bitCast(value),
                    else => unreachable,
                }
            } else {
                switch (bits) {
                    8 => i.value.uint8 = value,
                    16 => i.value.uint16 = value,
                    32 => i.value.uint32 = value,
                    64 => i.value.uint64 = value,
                    else => unreachable,
                }
            }
        },
        .Vector => |lanes| try writeMulExtendedBits(bits, &lanes[lane_index], 0, value),
        .Vector2i32 => |*v| switch (lane_index) {
            inline 0...1 => |i| if (bits == 32) {
                v[i] = @bitCast(value);
            } else {
                return RuntimeError.InvalidSpirV;
            },
            else => return RuntimeError.InvalidSpirV,
        },
        .Vector3i32 => |*v| switch (lane_index) {
            inline 0...2 => |i| if (bits == 32) {
                v[i] = @bitCast(value);
            } else {
                return RuntimeError.InvalidSpirV;
            },
            else => return RuntimeError.InvalidSpirV,
        },
        .Vector4i32 => |*v| switch (lane_index) {
            inline 0...3 => |i| if (bits == 32) {
                v[i] = @bitCast(value);
            } else {
                return RuntimeError.InvalidSpirV;
            },
            else => return RuntimeError.InvalidSpirV,
        },
        .Vector2u32 => |*v| switch (lane_index) {
            inline 0...1 => |i| if (bits == 32) {
                v[i] = @bitCast(value);
            } else {
                return RuntimeError.InvalidSpirV;
            },
            else => return RuntimeError.InvalidSpirV,
        },
        .Vector3u32 => |*v| switch (lane_index) {
            inline 0...2 => |i| if (bits == 32) {
                v[i] = @bitCast(value);
            } else {
                return RuntimeError.InvalidSpirV;
            },
            else => return RuntimeError.InvalidSpirV,
        },
        .Vector4u32 => |*v| switch (lane_index) {
            inline 0...3 => |i| if (bits == 32) {
                v[i] = @bitCast(value);
            } else {
                return RuntimeError.InvalidSpirV;
            },
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opMulExtended(comptime is_signed: bool, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result Type
    const result_id = try rt.it.next();
    const lhs = try rt.results[try rt.it.next()].getValue();
    const rhs = try rt.results[try rt.it.next()].getValue();

    const result = try rt.results[result_id].getValue();
    const result_members = switch (result.*) {
        .Structure => |*s| s.values,
        else => return RuntimeError.InvalidSpirV,
    };
    if (result_members.len != 2) return RuntimeError.InvalidSpirV;

    const low_dst = &result_members[0];
    const high_dst = &result_members[1];

    const lane_count = try lhs.resolveLaneCount();
    if (try rhs.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;
    if (try low_dst.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;
    if (try high_dst.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;

    const lane_bits = try lhs.resolveLaneBitWidth();
    if (try rhs.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;
    if (try low_dst.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;
    if (try high_dst.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;

    switch (lane_bits) {
        inline 8, 16, 32, 64 => |bits| {
            const UIntT = Value.getPrimitiveFieldType(.UInt, bits);
            const WideUIntT = std.meta.Int(.unsigned, bits * 2);

            for (0..lane_count) |lane_index| {
                const product_bits: WideUIntT = if (is_signed) blk: {
                    const SIntT = Value.getPrimitiveFieldType(.SInt, bits);
                    const WideSIntT = std.meta.Int(.signed, bits * 2);
                    const l: SIntT = try Value.readLane(.SInt, bits, lhs, lane_index);
                    const r: SIntT = try Value.readLane(.SInt, bits, rhs, lane_index);
                    const product: WideSIntT = @as(WideSIntT, l) * @as(WideSIntT, r);
                    break :blk @bitCast(product);
                } else blk: {
                    const l: UIntT = try Value.readLane(.UInt, bits, lhs, lane_index);
                    const r: UIntT = try Value.readLane(.UInt, bits, rhs, lane_index);
                    break :blk @as(WideUIntT, l) * @as(WideUIntT, r);
                };

                const low: UIntT = @truncate(product_bits);
                const high: UIntT = @truncate(product_bits >> bits);

                try writeMulExtendedBits(bits, low_dst, lane_index, low);
                try writeMulExtendedBits(bits, high_dst, lane_index, high);
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opIAddCarry(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result Type
    const result_id = try rt.it.next();
    const lhs = try rt.results[try rt.it.next()].getValue();
    const rhs = try rt.results[try rt.it.next()].getValue();

    const result = try rt.results[result_id].getValue();
    const result_members = switch (result.*) {
        .Structure => |*s| s.values,
        else => return RuntimeError.InvalidSpirV,
    };
    if (result_members.len != 2) return RuntimeError.InvalidSpirV;

    const value_dst = &result_members[0];
    const carry_dst = &result_members[1];

    const lane_count = try lhs.resolveLaneCount();
    if (try rhs.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;
    if (try value_dst.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;
    if (try carry_dst.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;

    const lane_bits = try lhs.resolveLaneBitWidth();
    if (try rhs.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;
    if (try value_dst.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;
    if (try carry_dst.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;

    switch (lane_bits) {
        inline 8, 16, 32, 64 => |bits| {
            const UIntT = Value.getPrimitiveFieldType(.UInt, bits);

            for (0..lane_count) |lane_index| {
                const l: UIntT = try Value.readLane(.UInt, bits, lhs, lane_index);
                const r: UIntT = try Value.readLane(.UInt, bits, rhs, lane_index);
                const add_result = @addWithOverflow(l, r);
                const sum = add_result[0];
                const carry: UIntT = @intCast(add_result[1]);

                try writeMulExtendedBits(bits, value_dst, lane_index, sum);
                try writeMulExtendedBits(bits, carry_dst, lane_index, carry);
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opISubBorrow(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result Type
    const result_id = try rt.it.next();
    const lhs = try rt.results[try rt.it.next()].getValue();
    const rhs = try rt.results[try rt.it.next()].getValue();

    const result = try rt.results[result_id].getValue();
    const result_members = switch (result.*) {
        .Structure => |*s| s.values,
        else => return RuntimeError.InvalidSpirV,
    };
    if (result_members.len != 2) return RuntimeError.InvalidSpirV;

    const value_dst = &result_members[0];
    const borrow_dst = &result_members[1];

    const lane_count = try lhs.resolveLaneCount();
    if (try rhs.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;
    if (try value_dst.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;
    if (try borrow_dst.resolveLaneCount() != lane_count) return RuntimeError.InvalidSpirV;

    const lane_bits = try lhs.resolveLaneBitWidth();
    if (try rhs.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;
    if (try value_dst.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;
    if (try borrow_dst.resolveLaneBitWidth() != lane_bits) return RuntimeError.InvalidSpirV;

    switch (lane_bits) {
        inline 8, 16, 32, 64 => |bits| {
            const UIntT = Value.getPrimitiveFieldType(.UInt, bits);

            for (0..lane_count) |lane_index| {
                const l: UIntT = try Value.readLane(.UInt, bits, lhs, lane_index);
                const r: UIntT = try Value.readLane(.UInt, bits, rhs, lane_index);
                const sub_result = @subWithOverflow(l, r);
                const diff = sub_result[0];
                const borrow: UIntT = @intCast(sub_result[1]);

                try writeMulExtendedBits(bits, value_dst, lane_index, diff);
                try writeMulExtendedBits(bits, borrow_dst, lane_index, borrow);
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opUMulExtended(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    try opMulExtended(false, rt);
}

fn opSMulExtended(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    try opMulExtended(true, rt);
}

fn opSpecConstant(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const location = rt.it.emitSourceLocation();
    _ = rt.it.skip();
    const result_id = try rt.it.next();
    _ = rt.it.goToSourceLocation(location);

    try opConstant(allocator, word_count, rt);

    const result = &rt.results[result_id];

    for (result.decorations.items) |decoration| {
        if (decoration.rtype == .SpecId) {
            if (rt.specialization_constants.get(decoration.literal_1)) |data| {
                _ = try (try result.getValue()).writeConst(data);
            }
        }
    }
}

fn opSpecConstantTrue(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    switch (target.variant.?.Constant.value) {
        .Bool => |*b| b.* = true,
        else => return RuntimeError.InvalidSpirV,
    }

    for (target.decorations.items) |decoration| {
        if (decoration.rtype == .SpecId) {
            if (rt.specialization_constants.get(decoration.literal_1)) |data| {
                _ = try (try target.getValue()).writeConst(data);
            }
        }
    }
}

fn opSpecConstantFalse(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    switch (target.variant.?.Constant.value) {
        .Bool => |*b| b.* = false,
        else => return RuntimeError.InvalidSpirV,
    }

    for (target.decorations.items) |decoration| {
        if (decoration.rtype == .SpecId) {
            if (rt.specialization_constants.get(decoration.literal_1)) |data| {
                _ = try (try target.getValue()).writeConst(data);
            }
        }
    }
}

fn opSpecConstantOp(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    if (word_count < 3)
        return RuntimeError.InvalidSpirV;

    const start_location = rt.it.emitSourceLocation();

    _ = try setupConstant(allocator, rt);
    const inner_op = try rt.it.nextAs(spv.SpvOp);

    _ = rt.it.goToSourceLocation(start_location);
    rt.it.forceSkipIndex(2);

    const pfn = runtime_dispatcher[@intFromEnum(inner_op)] orelse return RuntimeError.UnsupportedSpirV;
    try pfn(allocator, word_count - 1, rt);
}

fn opCopyMemory(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try rt.it.next();
    const source = try rt.it.next();
    try copyValue(try rt.results[target].getValue(), try rt.results[source].getValue());
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

fn opDecorationGroup(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
}

fn opGroupDecorate(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const decoration_group = try rt.it.next();

    if (word_count < 2) return RuntimeError.InvalidSpirV;

    const group_result = &rt.results[decoration_group];

    for (0..(word_count - 1)) |_| {
        const target = try rt.it.next();

        for (group_result.decorations.items) |*decoration| {
            try cloneDecorationTo(allocator, rt, target, decoration, null);
        }
    }
}

fn opGroupMemberDecorate(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const decoration_group = try rt.it.next();

    if (word_count < 3) return RuntimeError.InvalidSpirV;
    if (((word_count - 1) % 2) != 0) return RuntimeError.InvalidSpirV;

    const group_result = &rt.results[decoration_group];
    const pair_count = @divExact(word_count - 1, 2);

    for (0..pair_count) |_| {
        const target = try rt.it.next();
        const member = try rt.it.next();

        for (group_result.decorations.items) |*decoration| {
            try cloneDecorationTo(allocator, rt, target, decoration, member);
        }
    }
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

    value.Float.value.float64 = 0.0;

    switch (op1_value.*) {
        .Vector => |vec| for (vec, op2_value.Vector) |*op1_v, *op2_v| {
            switch (size) {
                inline 16, 32, 64 => |i| {
                    (try Value.getPrimitiveField(.Float, i, value)).* += (try Value.getPrimitiveField(.Float, i, op1_v)).* * (try Value.getPrimitiveField(.Float, i, op2_v)).*;
                },
                else => return RuntimeError.InvalidSpirV,
            }
        },
        .Vector4f32 => |vec| value.Float.value.float32 = zm.dot4(vec, op2_value.Vector4f32)[0],
        .Vector3f32 => |vec| {
            const op2_vec = op2_value.Vector3f32;
            value.Float.value.float32 = zm.dot3(zm.f32x4(vec[0], vec[1], vec[2], 0.0), zm.f32x4(op2_vec[0], op2_vec[1], op2_vec[2], 0.0))[0];
        },
        .Vector2f32 => |vec| {
            const op2_vec = op2_value.Vector2f32;
            value.Float.value.float32 = zm.dot2(zm.f32x4(vec[0], vec[1], 0.0, 0.0), zm.f32x4(op2_vec[0], op2_vec[1], 0.0, 0.0))[0];
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
        .InputPoints,
        .InputLines,
        .Triangles,
        .InputLinesAdjacency,
        .InputTrianglesAdjacency,
        => rt.mod.geometry_input = @intFromEnum(mode),
        .OutputPoints,
        .OutputLineStrip,
        .OutputTriangleStrip,
        => rt.mod.geometry_output = @intFromEnum(mode),
        else => {},
    }
}

fn opExtInst(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = try rt.it.next();
    const id = try rt.it.next();
    const set = try rt.it.next();
    const inst = try rt.it.next();

    switch ((try rt.results[set].getVariant()).*) {
        .Extension => |ext| if (ext.dispatcher[inst]) |pfn| {
            try pfn(allocator, target_type, id, word_count, rt);
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opExtInstImport(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const name = try readStringN(allocator, &rt.it, word_count - 1);
    rt.results[id].name = name;
    rt.results[id].variant = .{
        .Extension = .{
            .dispatcher = if (extensions_map.get(name)) |map| map else return RuntimeError.UnsupportedExtension,
        },
    };
}

fn opFunction(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const return_type = try rt.it.next();
    const id = try rt.it.next();
    _ = rt.it.skip(); // Skip function control
    const function_type_id = try rt.it.next();

    const source_location = rt.it.emitSourceLocation();

    rt.results[id].variant = .{
        .Function = .{
            .source_location = source_location,
            .return_type = return_type,
            .function_type = function_type_id,
            .params = params: {
                if (rt.results[function_type_id].variant) |variant| {
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

    rt.results[function_type_id].variant.?.Type.Function.source_location = source_location;

    rt.current_function = &rt.results[id];
    rt.current_parameter_index = 0;
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

    const target = &rt.results[id];

    const resolved = rt.results[var_type].resolveType(rt.results);
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

fn opImageRead(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result Type
    const result_id = try rt.it.next();
    const image = &rt.results[try rt.it.next()];
    const coordinate = try rt.results[try rt.it.next()].getValue();
    const dst = try rt.results[result_id].getValue();

    const driver_image = switch ((try image.getValue()).*) {
        .Image => |img| img.driver_image,
        else => return RuntimeError.InvalidSpirV,
    };

    const helpers = struct {
        fn readCoordLane(coord: *const Value, lane_index: usize) RuntimeError!i32 {
            return switch (coord.*) {
                .Int => |i| {
                    if (lane_index != 0) return RuntimeError.OutOfBounds;
                    return if (i.is_signed) i.value.sint32 else @intCast(i.value.uint32);
                },
                .Vector => |lanes| {
                    if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
                    return readCoordLane(&lanes[lane_index], 0);
                },
                .Vector4i32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3i32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2i32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector4u32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| @intCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3u32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| @intCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2u32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| @intCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn writeFloatTexel(dst_value: *Value, texel: Runtime.Vec4(f32)) RuntimeError!void {
            switch (dst_value.*) {
                .Vector4f32 => |*v| v.* = .{ texel.x, texel.y, texel.z, texel.w },
                .Vector3f32 => |*v| v.* = .{ texel.x, texel.y, texel.z },
                .Vector2f32 => |*v| v.* = .{ texel.x, texel.y },
                .Vector => |lanes| {
                    if (lanes.len > 4) return RuntimeError.InvalidSpirV;
                    const values = [_]f32{ texel.x, texel.y, texel.z, texel.w };
                    for (lanes, 0..) |*lane, i| {
                        switch (lane.*) {
                            .Float => |*f| f.value.float32 = values[i],
                            else => return RuntimeError.InvalidValueType,
                        }
                    }
                },
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn writeIntTexel(dst_value: *Value, texel: Runtime.Vec4(u32)) RuntimeError!void {
            switch (dst_value.*) {
                .Vector4i32 => |*v| v.* = .{ @bitCast(texel.x), @bitCast(texel.y), @bitCast(texel.z), @bitCast(texel.w) },
                .Vector3i32 => |*v| v.* = .{ @bitCast(texel.x), @bitCast(texel.y), @bitCast(texel.z) },
                .Vector2i32 => |*v| v.* = .{ @bitCast(texel.x), @bitCast(texel.y) },
                .Vector4u32 => |*v| v.* = .{ texel.x, texel.y, texel.z, texel.w },
                .Vector3u32 => |*v| v.* = .{ texel.x, texel.y, texel.z },
                .Vector2u32 => |*v| v.* = .{ texel.x, texel.y },
                .Vector => |lanes| {
                    if (lanes.len > 4) return RuntimeError.InvalidSpirV;
                    const values = [_]u32{ texel.x, texel.y, texel.z, texel.w };
                    for (lanes, 0..) |*lane, i| {
                        switch (lane.*) {
                            .Int => |*int| int.value.uint32 = values[i],
                            else => return RuntimeError.InvalidValueType,
                        }
                    }
                },
                else => return RuntimeError.InvalidValueType,
            }
        }
    };

    const x = try helpers.readCoordLane(coordinate, 0);
    const y = helpers.readCoordLane(coordinate, 1) catch 0;
    const z = helpers.readCoordLane(coordinate, 2) catch 0;

    switch (dst.*) {
        .Vector4f32, .Vector3f32, .Vector2f32 => try helpers.writeFloatTexel(dst, try rt.image_api.readImageFloat4(driver_image, x, y, z)),
        .Vector4i32, .Vector3i32, .Vector2i32, .Vector4u32, .Vector3u32, .Vector2u32 => try helpers.writeIntTexel(dst, try rt.image_api.readImageInt4(driver_image, x, y, z)),
        .Vector => |lanes| {
            if (lanes.len == 0) return RuntimeError.InvalidSpirV;
            switch (lanes[0]) {
                .Float => try helpers.writeFloatTexel(dst, try rt.image_api.readImageFloat4(driver_image, x, y, z)),
                .Int => try helpers.writeIntTexel(dst, try rt.image_api.readImageInt4(driver_image, x, y, z)),
                else => return RuntimeError.InvalidValueType,
            }
        },
        else => return RuntimeError.InvalidValueType,
    }
}

fn opImageWrite(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const image = &rt.results[try rt.it.next()];
    const coordinate = try rt.results[try rt.it.next()].getValue();
    const texel = try rt.results[try rt.it.next()].getValue();

    const driver_image = switch ((try image.getValue()).*) {
        .Image => |img| img.driver_image,
        else => return RuntimeError.InvalidSpirV,
    };

    const helpers = struct {
        fn readCoordLane(coord: *const Value, lane_index: usize) RuntimeError!i32 {
            return switch (coord.*) {
                .Int => |i| {
                    if (lane_index != 0) return RuntimeError.OutOfBounds;
                    return if (i.is_signed) i.value.sint32 else @intCast(i.value.uint32);
                },
                .Vector => |lanes| {
                    if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
                    return readCoordLane(&lanes[lane_index], 0);
                },
                .Vector4i32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3i32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2i32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector4u32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| @intCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3u32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| @intCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2u32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| @intCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn readFloatLane(texel_value: *const Value, lane_index: usize) RuntimeError!f32 {
            return switch (texel_value.*) {
                .Float => |f| {
                    if (lane_index != 0) return RuntimeError.OutOfBounds;
                    return f.value.float32;
                },
                .Vector => |lanes| {
                    if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
                    return readFloatLane(&lanes[lane_index], 0);
                },
                .Vector4f32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3f32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2f32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn readIntLane(texel_value: *const Value, lane_index: usize) RuntimeError!u32 {
            return switch (texel_value.*) {
                .Int => |i| {
                    if (lane_index != 0) return RuntimeError.OutOfBounds;
                    return if (i.is_signed) @bitCast(i.value.sint32) else i.value.uint32;
                },
                .Vector => |lanes| {
                    if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
                    return readIntLane(&lanes[lane_index], 0);
                },
                .Vector4i32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| @bitCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3i32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| @bitCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2i32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| @bitCast(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector4u32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3u32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2u32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| v[idx],
                    else => return RuntimeError.OutOfBounds,
                },
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn readFloatTexel(texel_value: *const Value) RuntimeError!Runtime.Vec4(f32) {
            return .{
                .x = try readFloatLane(texel_value, 0),
                .y = readFloatLane(texel_value, 1) catch 0.0,
                .z = readFloatLane(texel_value, 2) catch 0.0,
                .w = readFloatLane(texel_value, 3) catch 0.0,
            };
        }

        fn readIntTexel(texel_value: *const Value) RuntimeError!Runtime.Vec4(u32) {
            return .{
                .x = try readIntLane(texel_value, 0),
                .y = readIntLane(texel_value, 1) catch 0,
                .z = readIntLane(texel_value, 2) catch 0,
                .w = readIntLane(texel_value, 3) catch 0,
            };
        }
    };

    const x = try helpers.readCoordLane(coordinate, 0);
    const y = helpers.readCoordLane(coordinate, 1) catch 0;
    const z = helpers.readCoordLane(coordinate, 2) catch 0;

    switch (texel.*) {
        .Float,
        .Vector4f32,
        .Vector3f32,
        .Vector2f32,
        => try rt.image_api.writeImageFloat4(driver_image, x, y, z, try helpers.readFloatTexel(texel)),
        .Int,
        .Vector4i32,
        .Vector3i32,
        .Vector2i32,
        .Vector4u32,
        .Vector3u32,
        .Vector2u32,
        => try rt.image_api.writeImageInt4(driver_image, x, y, z, try helpers.readIntTexel(texel)),
        .Vector => |lanes| {
            if (lanes.len == 0) return RuntimeError.InvalidSpirV;
            switch (lanes[0]) {
                .Float => try rt.image_api.writeImageFloat4(driver_image, x, y, z, try helpers.readFloatTexel(texel)),
                .Int => try rt.image_api.writeImageInt4(driver_image, x, y, z, try helpers.readIntTexel(texel)),
                else => return RuntimeError.InvalidValueType,
            }
        },
        else => return RuntimeError.InvalidValueType,
    }
}

fn opLabel(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.current_label = id;
    if (rt.results[id].variant == null) {
        rt.results[id].variant = .{
            .Label = .{
                .source_location = rt.it.emitSourceLocation() - 2, // Original label location
            },
        };
    }
}

fn opKill(_: std.mem.Allocator, _: SpvWord, _: *Runtime) RuntimeError!void {
    return RuntimeError.Killed;
}

fn opLoad(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const id = try rt.it.next();
    const ptr_id = try rt.it.next();
    try copyValue(try rt.results[id].getValue(), try rt.results[ptr_id].getValue());
}

fn opMemberName(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const memb = try rt.it.next();

    var result = &rt.results[id];

    if (result.variant == null) {
        result.variant = .{
            .Type = .{
                .Structure = .{
                    .members_type_word = undefined,
                    .members_offsets = undefined,
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
    var result = &rt.results[id];
    result.name = try readStringN(allocator, &rt.it, word_count - 1);
}

fn opPhi(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result type
    const id = try rt.it.next();

    const predecessor = rt.previous_label orelse return RuntimeError.InvalidSpirV;
    const pair_count = @divExact(word_count - 2, 2);

    for (0..pair_count) |_| {
        const value_id = try rt.it.next();
        const parent_label_id = try rt.it.next();

        if (parent_label_id == predecessor) {
            try copyValue(try rt.results[id].getValue(), try rt.results[value_id].getValue());
            return;
        }
    }
    return RuntimeError.InvalidSpirV;
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
        try copyValue(try function.ret.getValue(), try ret_res.getValue());
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

fn opSelect(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const id = try rt.it.next();
    const cond = try rt.it.next();
    const obj1 = try rt.it.next();
    const obj2 = try rt.it.next();

    const target_val = try rt.results[id].getValue();
    const cond_val = try rt.results[cond].getValue();
    const obj1_val = try rt.results[obj1].getValue();
    const obj2_val = try rt.results[obj2].getValue();

    if (target_val.getCompositeDataOrNull()) |*targets| {
        for (
            targets.*,
            cond_val.getCompositeDataOrNull().?,
            obj1_val.getCompositeDataOrNull().?,
            obj2_val.getCompositeDataOrNull().?,
        ) |*t, c, o1, o2| {
            try copyValue(t, if (c.Bool) &o1 else &o2);
        }
        return;
    }

    switch (target_val.*) {
        .Bool, .Int, .Float => try copyValue(target_val, if (cond_val.Bool) obj1_val else obj2_val),

        .Vector4f32 => |*v| {
            const cond_vec = @Vector(4, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
                cond_val.Vector[2].Bool,
                cond_val.Vector[3].Bool,
            };
            v.* = @select(f32, cond_vec, obj1_val.Vector4f32, obj2_val.Vector4f32);
        },
        .Vector3f32 => |*v| {
            const cond_vec = @Vector(3, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
                cond_val.Vector[2].Bool,
            };
            v.* = @select(f32, cond_vec, obj1_val.Vector3f32, obj2_val.Vector3f32);
        },
        .Vector2f32 => |*v| {
            const cond_vec = @Vector(2, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
            };
            v.* = @select(f32, cond_vec, obj1_val.Vector2f32, obj2_val.Vector2f32);
        },

        .Vector4i32 => |*v| {
            const cond_vec = @Vector(4, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
                cond_val.Vector[2].Bool,
                cond_val.Vector[3].Bool,
            };
            v.* = @select(i32, cond_vec, obj1_val.Vector4i32, obj2_val.Vector4i32);
        },
        .Vector3i32 => |*v| {
            const cond_vec = @Vector(3, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
                cond_val.Vector[2].Bool,
            };
            v.* = @select(i32, cond_vec, obj1_val.Vector3i32, obj2_val.Vector3i32);
        },
        .Vector2i32 => |*v| {
            const cond_vec = @Vector(2, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
            };
            v.* = @select(i32, cond_vec, obj1_val.Vector2i32, obj2_val.Vector2i32);
        },

        .Vector4u32 => |*v| {
            const cond_vec = @Vector(4, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
                cond_val.Vector[2].Bool,
                cond_val.Vector[3].Bool,
            };
            v.* = @select(u32, cond_vec, obj1_val.Vector4u32, obj2_val.Vector4u32);
        },
        .Vector3u32 => |*v| {
            const cond_vec = @Vector(3, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
                cond_val.Vector[2].Bool,
            };
            v.* = @select(u32, cond_vec, obj1_val.Vector3u32, obj2_val.Vector3u32);
        },
        .Vector2u32 => |*v| {
            const cond_vec = @Vector(2, bool){
                cond_val.Vector[0].Bool,
                cond_val.Vector[1].Bool,
            };
            v.* = @select(u32, cond_vec, obj1_val.Vector2u32, obj2_val.Vector2u32);
        },
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opSourceExtension(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.extensions.append(allocator, try readStringN(allocator, &rt.it, word_count)) catch return RuntimeError.OutOfMemory;
}

fn opStore(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const ptr_id = try rt.it.next();
    const val_id = try rt.it.next();
    try copyValue(try rt.results[ptr_id].getValue(), try rt.results[val_id].getValue());
}

fn opTypeArray(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    var target = &rt.results[id];
    const components_type_word = try rt.it.next();
    const components_type_data = &((try rt.results[components_type_word].getVariant()).*).Type;
    target.variant = .{
        .Type = .{
            .Array = .{
                .components_type_word = components_type_word,
                .components_type = switch ((try rt.results[components_type_word].getVariant()).*) {
                    .Type => |t| @as(Result.Type, t),
                    else => return RuntimeError.InvalidSpirV,
                },
                .member_count = switch ((try rt.results[try rt.it.next()].getValue()).*) {
                    .Int => |i| if (!i.is_signed) @intCast(i.value.uint64) else switch (i.bit_count) {
                        8 => @intCast(i.value.sint8),
                        16 => @intCast(i.value.sint8),
                        32 => @intCast(i.value.sint8),
                        64 => @intCast(i.value.sint8),
                        else => return RuntimeError.InvalidSpirV,
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .stride = blk: {
                    for (target.decorations.items) |decoration| {
                        if (decoration.rtype == .ArrayStride)
                            break :blk decoration.literal_1;
                    }
                    break :blk @intCast(components_type_data.getSize(rt.results));
                },
            },
        },
    };
}

fn opTypeBool(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.results[id].variant = .{
        .Type = .{
            .Bool = .{},
        },
    };
}

fn opTypeFloat(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.results[id].variant = .{
        .Type = .{
            .Float = .{
                .bit_length = try rt.it.next(),
            },
        },
    };
}

fn opTypeFunction(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.results[id].variant = .{
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

fn opTypeImage(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    _ = rt.it.skip(); // TODO: sampled type management
    rt.results[id].variant = .{
        .Type = .{
            .Image = .{
                .dim = try rt.it.nextAs(spv.SpvDim),
                .depth = @truncate(try rt.it.next()),
                .arrayed = @truncate(try rt.it.next()),
                .ms = @truncate(try rt.it.next()),
                .sampled = @truncate(try rt.it.next()),
                .format = try rt.it.nextAs(spv.SpvImageFormat),
                .access = null,
            },
        },
    };
    if (word_count > 8) {
        rt.results[id].variant.?.Type.Image.access = try rt.it.nextAs(spv.SpvAccessQualifier);
    }
}

fn opTypeInt(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.results[id].variant = .{
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
    rt.results[id].variant = .{
        .Type = .{
            .Matrix = .{
                .column_type_word = column_type_word,
                .column_type = switch ((try rt.results[column_type_word].getVariant()).*) {
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
    rt.results[id].variant = .{
        .Type = .{
            .Pointer = .{
                .storage_class = try rt.it.nextAs(spv.SpvStorageClass),
                .target = try rt.it.next(),
            },
        },
    };
}

fn opTypeRuntimeArray(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    var target = &rt.results[id];
    const components_type_word = try rt.it.next();
    const components_type_data = &((try rt.results[components_type_word].getVariant()).*).Type;
    target.variant = .{
        .Type = .{
            .RuntimeArray = .{
                .components_type_word = components_type_word,
                .components_type = @as(Result.Type, components_type_data.*),
                .stride = blk: {
                    for (target.decorations.items) |decoration| {
                        if (decoration.rtype == .ArrayStride)
                            break :blk decoration.literal_1;
                    }
                    break :blk @intCast(components_type_data.getSize(rt.results));
                },
            },
        },
    };
}

fn opTypeStruct(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const members_type_word = blk: {
        const members_type_word = allocator.alloc(SpvWord, word_count - 1) catch return RuntimeError.OutOfMemory;
        errdefer allocator.free(members_type_word);

        for (members_type_word) |*member_type_word| {
            member_type_word.* = try rt.it.next();
        }
        break :blk members_type_word;
    };
    const members_offsets = allocator.alloc(?SpvWord, word_count - 1) catch return RuntimeError.OutOfMemory;
    @memset(members_offsets, null);

    if (rt.results[id].variant) |*variant| {
        switch (variant.*) {
            .Type => |*t| switch (t.*) {
                .Structure => |*s| {
                    s.members_type_word = members_type_word;
                    s.members_offsets = members_offsets;
                },
                else => unreachable,
            },
            else => unreachable,
        }
    } else {
        rt.results[id].variant = .{
            .Type = .{
                .Structure = .{
                    .members_type_word = members_type_word,
                    .members_offsets = members_offsets,
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
    const components_type_concrete = try rt.results[components_type_word].getVariant();
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
    rt.results[id].variant = .{
        .Type = blk: {
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
        },
    };
}

fn opTypeVoid(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.results[id].variant = .{
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

    const target = &rt.results[id];

    const resolved_word = if (rt.results[var_type].resolveTypeWordOrNull()) |word| word else var_type;
    const resolved = &rt.results[resolved_word];

    const resolved_type = switch ((try resolved.getConstVariant()).*) {
        .Type => |t| @as(Result.Type, t),
        else => return RuntimeError.InvalidSpirV,
    };

    const externally_visible_data_storages = [_]spv.SpvStorageClass{
        .Uniform,
        .StorageBuffer,
    };

    const is_externally_visible = std.mem.containsAtLeastScalar(spv.SpvStorageClass, &externally_visible_data_storages, 1, storage_class);

    target.variant = .{
        .Variable = .{
            .storage_class = storage_class,
            .type_word = resolved_word,
            .type = resolved_type,
            .value = try Value.init(allocator, rt.results, resolved_word, is_externally_visible),
        },
    };

    _ = initializer;
}

fn opVectorShuffle(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = allocator;
    _ = try rt.it.next();

    const result_id = try rt.it.next();
    const vector_1_id = try rt.it.next();
    const vector_2_id = try rt.it.next();

    const dst = try rt.results[result_id].getValue();
    const vector_1 = try rt.results[vector_1_id].getValue();
    const vector_2 = try rt.results[vector_2_id].getValue();

    const Impl = struct {
        fn readLane(src: *const Value, lane_index: usize) RuntimeError!Value {
            return switch (src.*) {
                .Vector => |lanes| {
                    if (lane_index >= lanes.len) return RuntimeError.InvalidSpirV;
                    return lanes[lane_index];
                },

                .Vector2f32 => |lanes| return .{ .Float = .{
                    .bit_count = 32,
                    .value = .{ .float32 = switch (lane_index) {
                        inline 0...1 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },
                .Vector3f32 => |lanes| return .{ .Float = .{
                    .bit_count = 32,
                    .value = .{ .float32 = switch (lane_index) {
                        inline 0...2 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },
                .Vector4f32 => |lanes| return .{ .Float = .{
                    .bit_count = 32,
                    .value = .{ .float32 = switch (lane_index) {
                        inline 0...3 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },

                .Vector2i32 => |lanes| return .{ .Int = .{
                    .bit_count = 32,
                    .is_signed = true,
                    .value = .{ .sint32 = switch (lane_index) {
                        inline 0...1 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },
                .Vector3i32 => |lanes| return .{ .Int = .{
                    .bit_count = 32,
                    .is_signed = true,
                    .value = .{ .sint32 = switch (lane_index) {
                        inline 0...2 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },
                .Vector4i32 => |lanes| return .{ .Int = .{
                    .bit_count = 32,
                    .is_signed = true,
                    .value = .{ .sint32 = switch (lane_index) {
                        inline 0...3 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },

                .Vector2u32 => |lanes| return .{ .Int = .{
                    .bit_count = 32,
                    .is_signed = false,
                    .value = .{ .uint32 = switch (lane_index) {
                        inline 0...1 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },
                .Vector3u32 => |lanes| return .{ .Int = .{
                    .bit_count = 32,
                    .is_signed = false,
                    .value = .{ .uint32 = switch (lane_index) {
                        inline 0...2 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },
                .Vector4u32 => |lanes| return .{ .Int = .{
                    .bit_count = 32,
                    .is_signed = false,
                    .value = .{ .uint32 = switch (lane_index) {
                        inline 0...3 => |idx| lanes[idx],
                        else => return RuntimeError.OutOfBounds,
                    } },
                } },

                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn writeLane(dst_value: *Value, lane_index: usize, lane_value: Value) RuntimeError!void {
            switch (dst_value.*) {
                .Vector => |lanes| {
                    if (lane_index >= lanes.len) return RuntimeError.InvalidSpirV;
                    lanes[lane_index] = lane_value;
                },

                .Vector2f32 => |*lanes| switch (lane_value) {
                    .Float => |f| {
                        if (f.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...1 => |i| lanes[i] = f.value.float32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3f32 => |*lanes| switch (lane_value) {
                    .Float => |f| {
                        if (f.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...2 => |i| lanes[i] = f.value.float32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector4f32 => |*lanes| switch (lane_value) {
                    .Float => |f| {
                        if (f.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...3 => |i| lanes[i] = f.value.float32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },

                .Vector2i32 => |*lanes| switch (lane_value) {
                    .Int => |i| {
                        if (i.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...1 => |idx| lanes[idx] = i.value.sint32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3i32 => |*lanes| switch (lane_value) {
                    .Int => |i| {
                        if (i.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...2 => |idx| lanes[idx] = i.value.sint32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector4i32 => |*lanes| switch (lane_value) {
                    .Int => |i| {
                        if (i.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...3 => |idx| lanes[idx] = i.value.sint32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },

                .Vector2u32 => |*lanes| switch (lane_value) {
                    .Int => |i| {
                        if (i.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...1 => |idx| lanes[idx] = i.value.uint32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector3u32 => |*lanes| switch (lane_value) {
                    .Int => |i| {
                        if (i.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...2 => |idx| lanes[idx] = i.value.uint32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .Vector4u32 => |*lanes| switch (lane_value) {
                    .Int => |i| {
                        if (i.bit_count != 32) return RuntimeError.InvalidSpirV;
                        switch (lane_index) {
                            inline 0...3 => |idx| lanes[idx] = i.value.uint32,
                            else => return RuntimeError.InvalidSpirV,
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };

    const dst_lane_count = try dst.resolveLaneCount();
    const vector_1_lane_count = try vector_1.resolveLaneCount();
    const vector_2_lane_count = try vector_2.resolveLaneCount();

    for (0..dst_lane_count) |lane_index| {
        const selector = try rt.it.next();

        if (selector == std.math.maxInt(u32)) {
            continue;
        }

        const lane_value = if (selector < vector_1_lane_count)
            try Impl.readLane(vector_1, selector)
        else blk: {
            const rhs_index = selector - vector_1_lane_count;
            if (rhs_index >= vector_2_lane_count) return RuntimeError.InvalidSpirV;
            break :blk try Impl.readLane(vector_2, rhs_index);
        };

        try Impl.writeLane(dst, lane_index, lane_value);
    }
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
    const target = &rt.results[id];

    const resolved = rt.results[res_type].resolveType(rt.results);
    target.variant = .{
        .Constant = .{
            .value = try Value.init(allocator, rt.results, res_type, false),
            .type_word = res_type,
            .type = switch ((try resolved.getConstVariant()).*) {
                .Type => |t| @as(Result.Type, t),
                else => return RuntimeError.InvalidSpirV,
            },
        },
    };
    return target;
}
