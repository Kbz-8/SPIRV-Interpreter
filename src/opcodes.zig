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

const ImageOp = enum {
    Fetch,
    Gather,
    QueryLevels,
    QueryLod,
    QuerySamples,
    QuerySize,
    QuerySizeLod,
    Read,
    Resolve,
    SampleDrefExplicitLod,
    SampleDrefImplicitLod,
    SampleExplicitLod,
    SampleImplicitLod,
    SampleProjDrefExplicitLod,
    SampleProjDrefImplicitLod,
    SampleProjExplicitLod,
    SampleProjImplicitLod,
    Write,
};

const AtomicOp = enum {
    Add,
    And,
    CompareExchange,
    Decrement,
    Exchange,
    Increment,
    MaxSigned,
    MaxUnsigned,
    MinSigned,
    MinUnsigned,
    Or,
    Sub,
    Xor,
};

pub const OpCodeFunc = *const fn (std.mem.Allocator, SpvWord, *Runtime) RuntimeError!void;
pub const OpCodeExtFunc = *const fn (std.mem.Allocator, SpvWord, SpvWord, SpvWord, *Runtime) RuntimeError!void;

pub const SetupDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeFunc).init(.{
        .AccessChain = setupAccessChain,
        .All = autoSetupConstant,
        .Any = autoSetupConstant,
        .ArrayLength = autoSetupConstant,
        .AtomicAnd = setupAtomic,
        .AtomicCompareExchange = setupAtomic,
        .AtomicExchange = setupAtomic,
        .AtomicIAdd = setupAtomic,
        .AtomicIDecrement = setupAtomic,
        .AtomicIIncrement = setupAtomic,
        .AtomicISub = setupAtomic,
        .AtomicLoad = setupAtomic,
        .AtomicOr = setupAtomic,
        .AtomicSMax = setupAtomic,
        .AtomicSMin = setupAtomic,
        .AtomicStore = setupAtomicStore,
        .AtomicUMax = setupAtomic,
        .AtomicUMin = setupAtomic,
        .AtomicXor = setupAtomic,
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
        .ConstantFalse = opConstantFalse,
        .ConstantNull = opConstantNull,
        .ConstantTrue = opConstantTrue,
        .ControlBarrier = opControlBarrierSetup,
        .ConvertFToS = autoSetupConstant,
        .ConvertFToU = autoSetupConstant,
        .ConvertPtrToU = autoSetupConstant,
        .ConvertSToF = autoSetupConstant,
        .ConvertUToF = autoSetupConstant,
        .ConvertUToPtr = autoSetupConstant,
        .CopyObject = autoSetupConstant,
        .DPdx = opDerivativeSetup,
        .DPdxCoarse = opDerivativeSetup,
        .DPdxFine = opDerivativeSetup,
        .DPdy = opDerivativeSetup,
        .DPdyCoarse = opDerivativeSetup,
        .DPdyFine = opDerivativeSetup,
        .Fwidth = opDerivativeSetup,
        .FwidthCoarse = opDerivativeSetup,
        .FwidthFine = opDerivativeSetup,
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
        .IMul = autoSetupConstant,
        .INotEqual = autoSetupConstant,
        .ISub = autoSetupConstant,
        .ISubBorrow = autoSetupConstant,
        .Image = autoSetupConstant,
        .ImageFetch = autoSetupConstant,
        .ImageGather = autoSetupConstant,
        .ImageQueryLevels = autoSetupConstant,
        .ImageQueryLod = opDerivativeSetup,
        .ImageQuerySamples = autoSetupConstant,
        .ImageQuerySize = autoSetupConstant,
        .ImageQuerySizeLod = autoSetupConstant,
        .ImageRead = autoSetupConstant,
        .ImageSampleExplicitLod = autoSetupConstant,
        .ImageSampleImplicitLod = opDerivativeSetup,
        .ImageSampleDrefExplicitLod = autoSetupConstant,
        .ImageSampleDrefImplicitLod = opDerivativeSetup,
        .ImageSampleProjDrefExplicitLod = autoSetupConstant,
        .ImageSampleProjDrefImplicitLod = opDerivativeSetup,
        .ImageSampleProjExplicitLod = autoSetupConstant,
        .ImageSampleProjImplicitLod = opDerivativeSetup,
        .ImageTexelPointer = autoSetupConstant,
        .InBoundsAccessChain = setupAccessChain,
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
        .OuterProduct = autoSetupConstant,
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
        .SampledImage = autoSetupConstant,
        .SatConvertSToU = autoSetupConstant,
        .SatConvertUToS = autoSetupConstant,
        .Select = autoSetupConstant,
        .ShiftLeftLogical = autoSetupConstant,
        .ShiftRightArithmetic = autoSetupConstant,
        .ShiftRightLogical = autoSetupConstant,
        .SourceExtension = opSourceExtension,
        .SpecConstant = opSpecConstant,
        .SpecConstantComposite = opConstantComposite,
        .SpecConstantFalse = opSpecConstantFalse,
        .SpecConstantOp = opSpecConstantOp,
        .SpecConstantTrue = opSpecConstantTrue,
        .Transpose = autoSetupConstant,
        .TypeArray = opTypeArray,
        .TypeBool = opTypeBool,
        .TypeFloat = opTypeFloat,
        .TypeFunction = opTypeFunction,
        .TypeImage = opTypeImage,
        .TypeInt = opTypeInt,
        .TypeMatrix = opTypeMatrix,
        .TypePointer = opTypePointer,
        .TypeRuntimeArray = opTypeRuntimeArray,
        .TypeSampledImage = opTypeSampledImage,
        .TypeSampler = opTypeSampler,
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
        .VectorExtractDynamic = autoSetupConstant,
        .VectorInsertDynamic = autoSetupConstant,
        .VectorShuffle = autoSetupConstant,
        .VectorTimesMatrix = autoSetupConstant,
        .VectorTimesScalar = autoSetupConstant,
    });
};

/// Not an EnumMap as it is way too slow for this purpose
pub var runtime_dispatcher: [spv.SpvOpMaxValue]?OpCodeFunc = @splat(null);

pub fn initRuntimeDispatcher() void {
    // zig fmt: off
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AccessChain)]            = opAccessChain;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.All)]                    = opAll;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Any)]                    = opAny;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicAnd)]              = AtomicEngine(.And).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicCompareExchange)]  = AtomicEngine(.CompareExchange).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicExchange)]         = AtomicEngine(.Exchange).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicIAdd)]             = AtomicEngine(.Add).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicIDecrement)]       = AtomicEngine(.Decrement).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicIIncrement)]       = AtomicEngine(.Increment).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicISub)]             = AtomicEngine(.Sub).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicLoad)]             = opLoad;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicOr)]               = AtomicEngine(.Or).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicSMax)]             = AtomicEngine(.MaxSigned).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicSMin)]             = AtomicEngine(.MinSigned).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicStore)]            = opAtomicStore;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicUMax)]             = AtomicEngine(.MaxUnsigned).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicUMin)]             = AtomicEngine(.MinUnsigned).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.AtomicXor)]              = AtomicEngine(.Xor).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ArrayLength)]            = opArrayLength;
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
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConstantNull)]           = opConstantNull;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ControlBarrier)]         = opControlBarrier;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertFToS)]            = ConversionEngine(.Float, .SInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertFToU)]            = ConversionEngine(.Float, .UInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertSToF)]            = ConversionEngine(.SInt, .Float).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ConvertUToF)]            = ConversionEngine(.UInt, .Float).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.CopyObject)]             = opCopyObject;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.CopyMemory)]             = opCopyMemory;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.DPdx)]                   = DerivativeEngine(.x).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.DPdxCoarse)]             = DerivativeEngine(.x).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.DPdxFine)]               = DerivativeEngine(.x).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.DPdy)]                   = DerivativeEngine(.y).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.DPdyCoarse)]             = DerivativeEngine(.y).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.DPdyFine)]               = DerivativeEngine(.y).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Fwidth)]                 = opFwidth;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FwidthCoarse)]           = opFwidth;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.FwidthFine)]             = opFwidth;
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
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IEqual)]                 = CondEngine(.SInt, .Equal).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IMul)]                   = MathEngine(.SInt, .Mul, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.INotEqual)]              = CondEngine(.SInt, .NotEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ISub)]                   = MathEngine(.SInt, .Sub, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ISubBorrow)]             = opISubBorrow;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Image)]                  = ImageEngine(.Resolve).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageFetch)]             = ImageEngine(.Fetch).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageGather)]            = ImageEngine(.Gather).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageQueryLevels)]       = ImageEngine(.QueryLevels).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageQueryLod)]          = ImageEngine(.QueryLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageQuerySamples)]      = ImageEngine(.QuerySamples).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageQuerySize)]         = ImageEngine(.QuerySize).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageQuerySizeLod)]      = ImageEngine(.QuerySizeLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageRead)]              = ImageEngine(.Read).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageSampleExplicitLod)] = ImageEngine(.SampleExplicitLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageSampleImplicitLod)] = ImageEngine(.SampleImplicitLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageSampleDrefExplicitLod)] = ImageEngine(.SampleDrefExplicitLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageSampleDrefImplicitLod)] = ImageEngine(.SampleDrefImplicitLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageSampleProjDrefExplicitLod)] = ImageEngine(.SampleProjDrefExplicitLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageSampleProjDrefImplicitLod)] = ImageEngine(.SampleProjDrefImplicitLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageSampleProjExplicitLod)] = ImageEngine(.SampleProjExplicitLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageSampleProjImplicitLod)] = ImageEngine(.SampleProjImplicitLod).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageTexelPointer)]      = opImageTexelPointer;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ImageWrite)]             = ImageEngine(.Write).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.InBoundsAccessChain)]    = opAccessChain;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.IsHelperInvocationEXT)]   = opIsHelperInvocationEXT;
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
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesMatrix)]      = MathEngine(.Float, .MatrixTimesMatrix, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesScalar)]      = MathEngine(.Float, .MatrixTimesScalar, false).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MatrixTimesVector)]      = MathEngine(.Float, .MatrixTimesVector, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.MemoryBarrier)]          = opMemoryBarrier;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Not)]                    = BitEngine(.UInt, .Not).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.OuterProduct)]           = opOuterProduct;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Phi)]                    = opPhi;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.QuantizeToF16)]          = opQuantizeToF16;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Return)]                 = opReturn;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ReturnValue)]            = opReturnValue;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SConvert)]               = ConversionEngine(.SInt, .SInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SDiv)]                   = MathEngine(.SInt, .Div, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SGreaterThan)]           = CondEngine(.SInt, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SGreaterThanEqual)]      = CondEngine(.SInt, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SLessThan)]              = CondEngine(.SInt, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SLessThanEqual)]         = CondEngine(.SInt, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SMod)]                   = MathEngine(.SInt, .Mod, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SMulExtended)]           = opSMulExtended;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SNegate)]                = MathEngine(.SInt, .Negate, false).opSingle;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SRem)]                   = MathEngine(.SInt, .Rem, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.SampledImage)]           = opSampledImage;
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
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Switch)]                 = opSwitch;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.TerminateInvocation)]    = opKill;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.DemoteToHelperInvocation)] = opDemoteToHelperInvocation;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Transpose)]              = opTranspose;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UConvert)]               = ConversionEngine(.UInt, .UInt).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UDiv)]                   = MathEngine(.UInt, .Div, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UGreaterThan)]           = CondEngine(.UInt, .Greater).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UGreaterThanEqual)]      = CondEngine(.UInt, .GreaterEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ULessThan)]              = CondEngine(.UInt, .Less).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.ULessThanEqual)]         = CondEngine(.UInt, .LessEqual).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UMod)]                   = MathEngine(.UInt, .Mod, false).op;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.UMulExtended)]           = opUMulExtended;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Unreachable)]            = opUnreachable;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.Variable)]               = opVariable;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorExtractDynamic)]   = opVectorExtractDynamic;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorInsertDynamic)]    = opVectorInsertDynamic;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorShuffle)]          = opVectorShuffle;
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorTimesMatrix)]      = MathEngine(.Float, .VectorTimesMatrix, false).op; // TODO
    runtime_dispatcher[@intFromEnum(spv.SpvOp.VectorTimesScalar)]      = MathEngine(.Float, .VectorTimesScalar, false).op;
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

fn opAll(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const dst_value = try rt.results[try rt.it.next()].getValue();
    const vec_value = try rt.results[try rt.it.next()].getValue();

    switch (dst_value.*) {
        .Bool => |*b| b.* = blk: {
            switch (vec_value.*) {
                .Vector => |vec| for (vec[0..]) |elem| {
                    switch (elem) {
                        .Bool => |val| {
                            if (!val)
                                break :blk false;
                        },
                        else => return RuntimeError.InvalidValueType,
                    }
                },
                else => return RuntimeError.InvalidValueType,
            }

            break :blk true;
        },
        else => return RuntimeError.InvalidValueType,
    }
}

fn opAny(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const dst_value = try rt.results[try rt.it.next()].getValue();
    const vec_value = try rt.results[try rt.it.next()].getValue();

    switch (dst_value.*) {
        .Bool => |*b| b.* = blk: {
            switch (vec_value.*) {
                .Vector => |vec| for (vec[0..]) |elem| {
                    switch (elem) {
                        .Bool => |val| {
                            if (val)
                                break :blk true;
                        },
                        else => return RuntimeError.InvalidValueType,
                    }
                },
                else => return RuntimeError.InvalidValueType,
            }

            break :blk false;
        },
        else => return RuntimeError.InvalidValueType,
    }
}

fn BitEngine(comptime T: PrimitiveType, comptime Op: BitOp) type {
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

            if (count == 0 or offset >= bits) return base;

            const actual_count: u64 = @min(count, @as(u64, bits) - offset);

            const base_u: U = @bitCast(base);
            const insert_u: U = @bitCast(insert);

            const field_mask: U = if (actual_count == bits)
                ~@as(U, 0)
            else
                (@as(U, 1) << @intCast(actual_count)) - 1;

            const shift: std.math.Log2Int(U) = @truncate(offset);

            const positioned_mask: U = @shlWithOverflow(field_mask, shift)[0];
            const positioned_insert: U = @shlWithOverflow(insert_u & field_mask, shift)[0];

            return @bitCast((base_u & ~positioned_mask) | positioned_insert);
        }

        inline fn bitExtract(comptime TT: type, comptime signed_result: bool, base: TT, offset: u64, count: u64) TT {
            const info = @typeInfo(TT);
            if (info != .int) @compileError("must be an integer type");

            const bits: u32 = info.int.bits;

            if (count == 0 or offset >= bits) return @as(TT, 0);

            const actual_count: u64 = @min(count, @as(u64, bits) - offset);

            const U = std.meta.Int(.unsigned, bits);
            const base_u: U = @bitCast(base);
            const shift: std.math.Log2Int(U) = @truncate(offset);

            const field: U = if (actual_count == bits)
                base_u
            else
                (base_u >> shift) &
                    ((@as(U, 1) << @intCast(actual_count)) - 1);

            const result: U = if (!signed_result or actual_count == bits) blk: {
                break :blk field;
            } else blk: {
                const sign_bit: U = @as(U, 1) << @intCast(actual_count - 1);
                if ((field & sign_bit) != 0) {
                    break :blk field | (~@as(U, 0) << @intCast(actual_count));
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

                else => RuntimeError.InvalidSpirV,
            };
        }

        inline fn operationShift(comptime TT: type, op1: TT, amount: u64) RuntimeError!TT {
            if (amount >= @bitSizeOf(TT)) return @as(TT, 0);
            const shift: std.math.Log2Int(TT) = @intCast(amount);
            return switch (Op) {
                .ShiftLeft => op1 << shift,
                .ShiftRight, .ShiftRightArithmetic => op1 >> shift,
                else => RuntimeError.InvalidSpirV,
            };
        }

        inline fn operationTernary(comptime TT: type, op1: TT, op2: u64, op3: u64) RuntimeError!TT {
            return switch (Op) {
                .BitFieldSExtract => blk: {
                    if (T != .SInt) return RuntimeError.InvalidSpirV;
                    break :blk bitExtract(TT, true, op1, op2, op3);
                },
                .BitFieldUExtract => blk: {
                    if (T != .UInt) return RuntimeError.InvalidSpirV;
                    break :blk bitExtract(TT, false, op1, op2, op3);
                },
                else => RuntimeError.InvalidSpirV,
            };
        }

        inline fn operationQuaternary(comptime TT: type, op1: TT, op2: TT, op3: u64, op4: u64) RuntimeError!TT {
            return switch (Op) {
                .BitFieldInsert => bitInsert(TT, op1, op2, op3, op4),
                else => RuntimeError.InvalidSpirV,
            };
        }

        fn readIntegerLaneAsU64(value: *const Value, lane_index: usize) RuntimeError!u64 {
            const lane_bits = try value.resolveLaneBitWidth();
            const sign = try value.resolveSign();
            return switch (lane_bits) {
                inline 8, 16, 32, 64 => |bits| blk: {
                    if (sign == .signed) {
                        const lane = try Value.readLane(.SInt, bits, value, lane_index);
                        const U = std.meta.Int(.unsigned, bits);
                        break :blk @as(u64, @as(U, @bitCast(lane)));
                    }
                    break :blk @intCast(try Value.readLane(.UInt, bits, value, lane_index));
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn applyScalarBits(bit_count: SpvWord, dst: *Value, ops: [max_operator_count]?*const Value) RuntimeError!void {
            switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| {
                    const TT = Value.getPrimitiveFieldType(T, bits);

                    const out: TT = blk: {
                        const a = try Value.readLane(T, bits, ops[0].?, 0);

                        if (comptime isUnaryOp()) break :blk try operationUnary(TT, a);

                        if (comptime isBinaryOp()) {
                            if (comptime Op == .ShiftLeft or Op == .ShiftRight or Op == .ShiftRightArithmetic) {
                                const amount = try readIntegerLaneAsU64(ops[1].?, 0);
                                break :blk try operationShift(TT, a, amount);
                            }
                            const b = try Value.readLane(T, bits, ops[1].?, 0);
                            break :blk try operationBinary(TT, a, b);
                        }
                        if (comptime isTernaryOp()) {
                            const offset = try readIntegerLaneAsU64(ops[1].?, 0);
                            const count = try readIntegerLaneAsU64(ops[2].?, 0);
                            break :blk try operationTernary(TT, a, offset, count);
                        }
                        if (comptime isQuaternaryOp()) {
                            const b = try Value.readLane(T, bits, ops[1].?, 0);
                            const offset = try readIntegerLaneAsU64(ops[2].?, 0);
                            const count = try readIntegerLaneAsU64(ops[3].?, 0);
                            break :blk try operationQuaternary(TT, a, b, offset, count);
                        }
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

                            if (comptime isBinaryOp()) {
                                if (comptime Op == .ShiftLeft or Op == .ShiftRight or Op == .ShiftRightArithmetic) {
                                    const amount = try readIntegerLaneAsU64(ops[1].?, if (ops[1].?.isVector()) i else 0);
                                    break :blk try operationShift(TT, a, amount);
                                }
                                const b = try Value.readLane(T, bits, ops[1].?, if (ops[1].?.isVector()) i else 0);
                                break :blk try operationBinary(TT, a, b);
                            }
                            if (comptime isTernaryOp()) {
                                const offset = try readIntegerLaneAsU64(ops[1].?, if (ops[1].?.isVector()) i else 0);
                                const count = try readIntegerLaneAsU64(ops[2].?, if (ops[2].?.isVector()) i else 0);
                                break :blk try operationTernary(TT, a, offset, count);
                            }
                            if (comptime isQuaternaryOp()) {
                                const b = try Value.readLane(T, bits, ops[1].?, if (ops[1].?.isVector()) i else 0);
                                const offset = try readIntegerLaneAsU64(ops[2].?, if (ops[2].?.isVector()) i else 0);
                                const count = try readIntegerLaneAsU64(ops[3].?, if (ops[3].?.isVector()) i else 0);
                                break :blk try operationQuaternary(TT, a, b, offset, count);
                            }
                        };

                        try Value.writeLane(T, bits, dst, i, out);
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn op(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const dst = try rt.results[try rt.it.next()].getValue();

            var ops: [max_operator_count]?*Value = @splat(null);
            ops[0] = try rt.results[try rt.it.next()].getValue();

            if (comptime getOperatorsCount() >= 2) ops[1] = try rt.results[try rt.it.next()].getValue();
            if (comptime getOperatorsCount() >= 3) ops[2] = try rt.results[try rt.it.next()].getValue();
            if (comptime getOperatorsCount() >= 4) ops[3] = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

            switch (dst.*) {
                .Int => try applyScalarBits(lane_bits, dst, ops),

                .Vector,
                .Vector2i32,
                .Vector3i32,
                .Vector4i32,
                .Vector2u32,
                .Vector3u32,
                .Vector4u32,
                => try applyVectorBits(lane_bits, dst, ops),

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn CondEngine(comptime T: PrimitiveType, comptime Op: CondOp) type {
    return struct {
        inline fn isUnaryOp() bool {
            return comptime switch (Op) {
                .IsFinite,
                .IsInf,
                .IsNan,
                .IsNormal,
                .LogicalNot,
                => true,
                else => false,
            };
        }

        inline fn operationBinary(comptime TT: type, a: TT, b: anytype) RuntimeError!bool {
            if (comptime TT == bool and @TypeOf(b) == bool) {
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
            if (comptime std.meta.activeTag(@typeInfo(TT)) == .float) {
                switch (Op) {
                    .IsFinite => return std.math.isFinite(a),
                    .IsInf => return std.math.isInf(a),
                    .IsNan => return std.math.isNan(a),
                    .IsNormal => return std.math.isNormal(a),
                    else => {},
                }
            }
            return RuntimeError.InvalidSpirV;
        }

        fn applyScalarBits(bit_count: SpvWord, dst_bool: *Value, a_v: *const Value, b_v: ?*const Value) RuntimeError!void {
            if (comptime T == .Bool) {
                const a = switch (a_v.*) {
                    .Bool => |value| value,
                    else => return RuntimeError.InvalidSpirV,
                };
                if (comptime isUnaryOp()) {
                    dst_bool.Bool = try operationUnary(bool, a);
                } else {
                    const b_ptr = b_v orelse return RuntimeError.InvalidSpirV;
                    const b = switch (b_ptr.*) {
                        .Bool => |value| value,
                        else => return RuntimeError.InvalidSpirV,
                    };
                    dst_bool.Bool = try operationBinary(bool, a, b);
                }
                return;
            }

            switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| {
                    if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                    const TT = Value.getPrimitiveFieldType(T, bits);
                    const a = try Value.readLane(T, bits, a_v, 0);

                    if (comptime isUnaryOp()) {
                        dst_bool.Bool = try operationUnary(TT, a);
                    } else {
                        const b_ptr = b_v orelse return RuntimeError.InvalidSpirV;
                        const b = try Value.readLane(T, bits, b_ptr, 0);
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

        inline fn applyFixedVectorBinary(comptime ElemT: type, comptime N: usize, dst: []Value, op1: *@Vector(N, ElemT), op2: *const Value) RuntimeError!void {
            inline for (0..N) |i| {
                dst[i].Bool = switch (op2.*) {
                    .Vector4f32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .float and i < 4)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    .Vector3f32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .float and i < 3)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    .Vector2f32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .float and i < 2)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    .Vector4i32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .int and i < 4)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    .Vector3i32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .int and i < 3)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    .Vector2i32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .int and i < 2)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    .Vector4u32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .int and i < 4)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    .Vector3u32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .int and i < 3)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    .Vector2u32 => |vec| if (comptime std.meta.activeTag(@typeInfo(ElemT)) == .int and i < 2)
                        try operationBinary(ElemT, op1[i], vec[i])
                    else
                        return RuntimeError.InvalidSpirV,

                    else => return RuntimeError.InvalidValueType,
                };
            }
        }

        inline fn applyFixedVectorUnary(comptime ElemT: type, comptime N: usize, dst: []Value, op1: *@Vector(N, ElemT)) RuntimeError!void {
            inline for (0..N) |i| dst[i].Bool = try operationUnary(ElemT, op1[i]);
        }

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

            const op2_value: ?*Value = if (comptime isUnaryOp()) null else try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth((try rt.results[op1_type].getVariant()).Type, rt);

            switch (dst.*) {
                .Bool => try applyScalarBits(lane_bits, dst, op1_value, op2_value),

                .Vector => |dst_vec| {
                    switch (lane_bits) {
                        inline 8, 16, 32, 64 => |bits| {
                            if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;
                            const TT = Value.getPrimitiveFieldType(T, bits);
                            for (dst_vec, 0..) |*d_lane, i| {
                                const a = try Value.readLane(T, bits, op1_value, i);
                                d_lane.Bool = if (comptime isUnaryOp()) blk: {
                                    break :blk try operationUnary(TT, a);
                                } else blk: {
                                    const b = try Value.readLane(T, bits, op2_value.?, i);
                                    break :blk try operationBinary(TT, a, b);
                                };
                            }
                        },
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
        fn castLane(comptime ToT: type, from_bit_count: SpvWord, from: *const Value, lane_index: usize) RuntimeError!ToT {
            return switch (from_bit_count) {
                inline 8, 16, 32, 64 => |bits| blk: {
                    if (bits == 8 and from_kind == .Float) return RuntimeError.InvalidSpirV; // No f8
                    const v = try Value.readLane(from_kind, bits, from, lane_index);
                    if (comptime from_kind != .Float and to_kind != .Float) {
                        const to_bits = @bitSizeOf(ToT);
                        const FromUInt = std.meta.Int(.unsigned, bits);
                        const ToUInt = std.meta.Int(.unsigned, to_bits);

                        const src_bits: FromUInt = @bitCast(v);
                        const dst_bits: ToUInt = if (to_bits < bits)
                            @truncate(src_bits)
                        else if (from_kind == .SInt)
                            @bitCast(@as(std.meta.Int(.signed, to_bits), @intCast(v)))
                        else
                            @intCast(src_bits);
                        break :blk @bitCast(dst_bits);
                    }
                    break :blk std.math.lossyCast(ToT, v);
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn applyLane(from_bit_count: SpvWord, to_bit_count: SpvWord, dst: *Value, from: *const Value, lane_index: usize) RuntimeError!void {
            switch (to_bit_count) {
                inline 8, 16, 32, 64 => |bits| {
                    if (bits == 8 and to_kind == .Float) return RuntimeError.InvalidSpirV; // No f8
                    const ToT = Value.getPrimitiveFieldType(to_kind, bits);
                    try Value.writeLane(to_kind, bits, dst, lane_index, try castLane(ToT, from_bit_count, from, lane_index));
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn applyDerivativeLane(from_bit_count: SpvWord, to_bit_count: SpvWord, dst: *Value, from: *const Value, derivative: *const Value, lane_index: usize) RuntimeError!void {
            @setEvalBranchQuota(10_000);
            switch (from_bit_count) {
                inline 8, 16, 32, 64 => |from_bits| {
                    if (from_bits == 8 and from_kind == .Float) return RuntimeError.InvalidSpirV;
                    switch (to_bit_count) {
                        inline 8, 16, 32, 64 => |to_bits| {
                            if (to_bits == 8 and to_kind == .Float) return RuntimeError.InvalidSpirV;

                            const ToT = Value.getPrimitiveFieldType(to_kind, to_bits);
                            const base_from = try Value.readLane(from_kind, from_bits, from, lane_index);
                            const delta_from = try Value.readLane(from_kind, from_bits, derivative, lane_index);
                            const base_to = try castLane(ToT, from_bit_count, from, lane_index);

                            var shifted_from = from.*;
                            try Value.writeLane(from_kind, from_bits, &shifted_from, lane_index, addDerivativeDelta(@TypeOf(base_from), base_from, delta_from));
                            const shifted_to = try castLane(ToT, from_bit_count, &shifted_from, lane_index);
                            try Value.writeLane(to_kind, to_bits, dst, lane_index, subDerivativeDelta(ToT, shifted_to, base_to));
                        },
                        else => return RuntimeError.InvalidSpirV,
                    }
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn addDerivativeDelta(comptime T: type, lhs: T, rhs: T) T {
            return switch (@typeInfo(T)) {
                .int => @addWithOverflow(lhs, rhs)[0],
                else => lhs + rhs,
            };
        }

        fn subDerivativeDelta(comptime T: type, lhs: T, rhs: T) T {
            return switch (@typeInfo(T)) {
                .int => @subWithOverflow(lhs, rhs)[0],
                else => lhs - rhs,
            };
        }

        fn op(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type_word = try rt.it.next();
            const target_type = (try rt.results[target_type_word].getVariant()).Type;
            const dst_id = try rt.it.next();
            const dst_value = try rt.results[dst_id].getValue();

            const src_id = try rt.it.next();
            const src_value = try rt.results[src_id].getValue();

            const from_bits = try src_value.resolveLaneBitWidth();
            const to_bits = try Result.resolveLaneBitWidth(target_type, rt);

            const dst_lane_count = try dst_value.resolveLaneCount();
            const src_lane_count = try src_value.resolveLaneCount();
            if (dst_lane_count != src_lane_count) return RuntimeError.InvalidSpirV;

            for (0..dst_lane_count) |lane_index| {
                try applyLane(from_bits, to_bits, dst_value, src_value, lane_index);
            }

            const src_derivative = rt.derivatives.get(src_id) orelse {
                rt.clearDerivative(allocator, dst_id);
                return;
            };
            var dx = try Value.init(allocator, rt.results, target_type_word, false);
            defer dx.deinit(allocator);
            var dy = try Value.init(allocator, rt.results, target_type_word, false);
            defer dy.deinit(allocator);
            for (0..dst_lane_count) |lane_index| {
                try applyDerivativeLane(from_bits, to_bits, &dx, src_value, &src_derivative.dx, lane_index);
                try applyDerivativeLane(from_bits, to_bits, &dy, src_value, &src_derivative.dy, lane_index);
            }
            try rt.setDerivative(allocator, dst_id, &dx, &dy);
        }
    };
}

fn ImageEngine(comptime Op: ImageOp) type {
    return struct {
        const ImageOperand = struct {
            type_word: SpvWord,
            driver_image: *anyopaque,
            dim: spv.SpvDim,
            arrayed: bool,
        };

        const SampledImageOperand = struct {
            type_word: SpvWord,
            driver_image: *anyopaque,
            driver_sampler: *anyopaque,
            dim: spv.SpvDim,
        };

        fn resolveImageDim(rt: *Runtime, type_word: SpvWord) RuntimeError!spv.SpvDim {
            return switch ((try rt.results[type_word].getConstVariant()).*) {
                .Type => |t| switch (t) {
                    .Image => |i| i.dim,
                    .SampledImage => |i| return resolveImageDim(rt, i.image_type),
                    else => return RuntimeError.InvalidSpirV,
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn resolveImageArrayed(rt: *Runtime, type_word: SpvWord) RuntimeError!bool {
            return switch ((try rt.results[type_word].getConstVariant()).*) {
                .Type => |t| switch (t) {
                    .Image => |i| i.arrayed != 0,
                    .SampledImage => |i| return resolveImageArrayed(rt, i.image_type),
                    else => return RuntimeError.InvalidSpirV,
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn resolveImage(image: *Result, rt: *Runtime) RuntimeError!ImageOperand {
            return switch ((try image.getValue()).*) {
                .Image => |img| .{
                    .type_word = img.type_word,
                    .driver_image = img.driver_image,
                    .dim = try resolveImageDim(rt, img.type_word),
                    .arrayed = try resolveImageArrayed(rt, img.type_word),
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn resolveSampledImage(image: *Result, rt: *Runtime) RuntimeError!SampledImageOperand {
            return switch ((try image.getValue()).*) {
                .SampledImage => |img| blk: {
                    const sampled_image_type = switch ((try rt.results[img.type_word].getConstVariant()).*) {
                        .Type => |t| switch (t) {
                            .SampledImage => |sampled_image| sampled_image,
                            else => return RuntimeError.InvalidSpirV,
                        },
                        else => return RuntimeError.InvalidSpirV,
                    };

                    break :blk .{
                        .type_word = img.type_word,
                        .driver_image = img.driver_image,
                        .driver_sampler = img.driver_sampler,
                        .dim = try resolveImageDim(rt, sampled_image_type.image_type),
                    };
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn resolveImageForQuery(image: *Result, rt: *Runtime) RuntimeError!ImageOperand {
            return switch ((try image.getValue()).*) {
                .Image => try resolveImage(image, rt),
                .SampledImage => |img| blk: {
                    const sampled_image_type = switch ((try rt.results[img.type_word].getConstVariant()).*) {
                        .Type => |t| switch (t) {
                            .SampledImage => |sampled_image| sampled_image,
                            else => return RuntimeError.InvalidSpirV,
                        },
                        else => return RuntimeError.InvalidSpirV,
                    };

                    break :blk .{
                        .type_word = sampled_image_type.image_type,
                        .driver_image = img.driver_image,
                        .dim = try resolveImageDim(rt, sampled_image_type.image_type),
                        .arrayed = try resolveImageArrayed(rt, sampled_image_type.image_type),
                    };
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn readStorageCoordLane(coord: *const Value, lane_index: usize) RuntimeError!i32 {
            return switch (coord.*) {
                .Int => |i| {
                    if (lane_index != 0) return RuntimeError.OutOfBounds;
                    return if (i.is_signed) i.value.sint32 else @intCast(i.value.uint32);
                },
                .Vector => |lanes| {
                    if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
                    return readStorageCoordLane(&lanes[lane_index], 0);
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

        fn readSampleCoordLane(coord: *const Value, lane_index: usize) RuntimeError!f32 {
            return switch (coord.*) {
                .Float => |f| {
                    if (lane_index != 0) return RuntimeError.OutOfBounds;
                    return f.value.float32;
                },
                .Int => |i| {
                    if (lane_index != 0) return RuntimeError.OutOfBounds;
                    return if (i.is_signed) @floatFromInt(i.value.sint32) else @floatFromInt(i.value.uint32);
                },
                .Vector => |lanes| {
                    if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
                    return readSampleCoordLane(&lanes[lane_index], 0);
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
                .Vector4i32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| @floatFromInt(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3i32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| @floatFromInt(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2i32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| @floatFromInt(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector4u32 => |v| switch (lane_index) {
                    inline 0...3 => |idx| @floatFromInt(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector3u32 => |v| switch (lane_index) {
                    inline 0...2 => |idx| @floatFromInt(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                .Vector2u32 => |v| switch (lane_index) {
                    inline 0...1 => |idx| @floatFromInt(v[idx]),
                    else => return RuntimeError.OutOfBounds,
                },
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn readFloatLane(texel: *const Value, lane_index: usize) RuntimeError!f32 {
            return switch (texel.*) {
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

        fn readIntLane(texel: *const Value, lane_index: usize) RuntimeError!u32 {
            return switch (texel.*) {
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

        fn readFloatTexel(texel: *const Value) RuntimeError!Runtime.Vec4(f32) {
            return .{
                .x = try readFloatLane(texel, 0),
                .y = readFloatLane(texel, 1) catch 0.0,
                .z = readFloatLane(texel, 2) catch 0.0,
                .w = readFloatLane(texel, 3) catch 0.0,
            };
        }

        fn readIntTexel(texel: *const Value) RuntimeError!Runtime.Vec4(u32) {
            return .{
                .x = try readIntLane(texel, 0),
                .y = readIntLane(texel, 1) catch 0,
                .z = readIntLane(texel, 2) catch 0,
                .w = readIntLane(texel, 3) catch 0,
            };
        }

        fn imageOperandPresent(image_operands: SpvWord, mask: spv.SpvImageOperandsMask) bool {
            return (image_operands & @intFromEnum(mask)) != 0;
        }

        fn readImageOffset(rt: *Runtime, offset_id: SpvWord) RuntimeError!Runtime.ImageOffset {
            const offset = try rt.results[offset_id].getValue();
            return .{
                .x = try readStorageCoordLane(offset, 0),
                .y = readStorageCoordLane(offset, 1) catch 0,
                .z = readStorageCoordLane(offset, 2) catch 0,
            };
        }

        fn valueLaneCount(value: *const Value) RuntimeError!usize {
            return switch (value.*) {
                .Vector => |lanes| lanes.len,
                .Vector2f32, .Vector2i32, .Vector2u32 => 2,
                .Vector3f32, .Vector3i32, .Vector3u32 => 3,
                .Vector4f32, .Vector4i32, .Vector4u32 => 4,
                .Float, .Int => 1,
                else => RuntimeError.InvalidValueType,
            };
        }

        fn readProjectedSampleCoords(coordinate: *const Value) RuntimeError!struct { x: f32, y: f32, z: f32, w: f32 } {
            const lane_count = try valueLaneCount(coordinate);
            if (lane_count < 2)
                return RuntimeError.InvalidSpirV;

            const q_lane = lane_count - 1;
            const q = try readProjectionDivisor(coordinate);
            return .{
                .x = try readSampleCoordLane(coordinate, 0) / q,
                .y = if (q_lane > 1) (readSampleCoordLane(coordinate, 1) catch 0.0) / q else 0.0,
                .z = if (q_lane > 2) (readSampleCoordLane(coordinate, 2) catch 0.0) / q else 0.0,
                .w = if (q_lane > 3) (readSampleCoordLane(coordinate, 3) catch 0.0) / q else 0.0,
            };
        }

        fn readProjectionDivisor(coordinate: *const Value) RuntimeError!f32 {
            const lane_count = try valueLaneCount(coordinate);
            if (lane_count < 2)
                return RuntimeError.InvalidSpirV;
            return readSampleCoordLane(coordinate, lane_count - 1);
        }

        const ParsedImageOperands = struct {
            bias: f32 = 0.0,
            lod: ?f32 = null,
            image_lod: ?i32 = null,
            grad: ?Runtime.ImageDerivatives = null,
            sample: ?i32 = null,
            offset: Runtime.ImageOffset = .{},
        };

        fn parseImageOperands(rt: *Runtime, image_operands: SpvWord) RuntimeError!ParsedImageOperands {
            var parsed: ParsedImageOperands = .{};

            if (imageOperandPresent(image_operands, .BiasMask)) {
                parsed.bias = try readSampleCoordLane(try rt.results[try rt.it.next()].getValue(), 0);
            }
            if (imageOperandPresent(image_operands, .LodMask)) {
                const lod_value = try rt.results[try rt.it.next()].getValue();
                parsed.lod = try readSampleCoordLane(lod_value, 0);
                parsed.image_lod = readImageQueryLod(lod_value) catch null;
            }
            if (imageOperandPresent(image_operands, .GradMask)) {
                const dx = try rt.results[try rt.it.next()].getValue();
                const dy = try rt.results[try rt.it.next()].getValue();
                parsed.grad = .{
                    .dx = .{
                        .x = try readSampleCoordLane(dx, 0),
                        .y = if (try valueLaneCount(dx) > 1) try readSampleCoordLane(dx, 1) else 0.0,
                        .z = if (try valueLaneCount(dx) > 2) try readSampleCoordLane(dx, 2) else 0.0,
                        .w = if (try valueLaneCount(dx) > 3) try readSampleCoordLane(dx, 3) else 0.0,
                    },
                    .dy = .{
                        .x = try readSampleCoordLane(dy, 0),
                        .y = if (try valueLaneCount(dy) > 1) try readSampleCoordLane(dy, 1) else 0.0,
                        .z = if (try valueLaneCount(dy) > 2) try readSampleCoordLane(dy, 2) else 0.0,
                        .w = if (try valueLaneCount(dy) > 3) try readSampleCoordLane(dy, 3) else 0.0,
                    },
                };
            }
            if (imageOperandPresent(image_operands, .ConstOffsetMask) or imageOperandPresent(image_operands, .OffsetMask)) {
                parsed.offset = try readImageOffset(rt, try rt.it.next());
            }
            if (imageOperandPresent(image_operands, .ConstOffsetsMask)) {
                _ = try rt.it.next();
            }
            if (imageOperandPresent(image_operands, .SampleMask)) {
                const sample_value = try rt.results[try rt.it.next()].getValue();
                parsed.sample = try readStorageCoordLane(sample_value, 0);
            }
            if (imageOperandPresent(image_operands, .MinLodMask)) {
                _ = try rt.it.next();
            }
            if (imageOperandPresent(image_operands, .MakeTexelAvailableMask)) {
                _ = try rt.it.next();
            }
            if (imageOperandPresent(image_operands, .MakeTexelVisibleMask)) {
                _ = try rt.it.next();
            }
            if (imageOperandPresent(image_operands, .OffsetsMask)) {
                _ = try rt.it.next();
            }

            return parsed;
        }

        fn writeFloatTexel(dst: *Value, texel: Runtime.Vec4(f32)) RuntimeError!void {
            switch (dst.*) {
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

        fn writeIntTexel(dst: *Value, texel: Runtime.Vec4(u32)) RuntimeError!void {
            switch (dst.*) {
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

        fn derivativeDelta(comptime T: type, shifted: T, center: T) T {
            return switch (@typeInfo(T)) {
                .int => @subWithOverflow(shifted, center)[0],
                else => shifted - center,
            };
        }

        fn writeValueDelta(rt: *Runtime, target_type_word: SpvWord, dst: *Value, shifted: *const Value, center: *const Value) RuntimeError!void {
            const target_type = (try rt.results[target_type_word].getVariant()).Type;
            const primitive_type = try center.resolvePrimitiveType();
            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
            const lane_count = try Result.resolveLaneCount(target_type);

            switch (primitive_type) {
                inline .Float, .SInt, .UInt => |primitive| switch (lane_bits) {
                    inline 16, 32, 64 => |bits| {
                        const LaneT = Value.getPrimitiveFieldType(primitive, bits);
                        for (0..lane_count) |lane_index| {
                            const shifted_lane = try Value.readLane(primitive, bits, shifted, lane_index);
                            const center_lane = try Value.readLane(primitive, bits, center, lane_index);
                            try Value.writeLane(primitive, bits, dst, lane_index, derivativeDelta(LaneT, shifted_lane, center_lane));
                        }
                    },
                    else => return RuntimeError.UnsupportedSpirV,
                },
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn writeFloatScalar(dst: *Value, value: f32) RuntimeError!void {
            switch (dst.*) {
                .Float => |*f| f.value.float32 = value,
                .Vector => |lanes| {
                    if (lanes.len != 1) return RuntimeError.InvalidValueType;
                    switch (lanes[0]) {
                        .Float => |*f| f.value.float32 = value,
                        else => return RuntimeError.InvalidValueType,
                    }
                },
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn setImageReadDerivative(allocator: std.mem.Allocator, rt: *Runtime, result_type_word: SpvWord, result_id: SpvWord, coordinate_id: SpvWord, dst: *const Value, driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32, lod: ?i32) RuntimeError!void {
            const coord_derivatives = sampleDerivativesFromRuntime(rt, coordinate_id) catch {
                rt.clearDerivative(allocator, result_id);
                return;
            } orelse {
                rt.clearDerivative(allocator, result_id);
                return;
            };

            var dx_read = try Value.init(allocator, rt.results, result_type_word, false);
            defer dx_read.deinit(allocator);
            var dy_read = try Value.init(allocator, rt.results, result_type_word, false);
            defer dy_read.deinit(allocator);

            const dx_x: i32 = @intFromFloat(coord_derivatives.dx.x);
            const dx_y: i32 = @intFromFloat(coord_derivatives.dx.y);
            const dx_z: i32 = @intFromFloat(coord_derivatives.dx.z);
            const dy_x: i32 = @intFromFloat(coord_derivatives.dy.x);
            const dy_y: i32 = @intFromFloat(coord_derivatives.dy.y);
            const dy_z: i32 = @intFromFloat(coord_derivatives.dy.z);

            try readImage(rt, &dx_read, driver_image, dim, x + dx_x, y + dx_y, z + dx_z, lod);
            try readImage(rt, &dy_read, driver_image, dim, x + dy_x, y + dy_y, z + dy_z, lod);

            var dx = try Value.init(allocator, rt.results, result_type_word, false);
            defer dx.deinit(allocator);
            var dy = try Value.init(allocator, rt.results, result_type_word, false);
            defer dy.deinit(allocator);
            try writeValueDelta(rt, result_type_word, &dx, &dx_read, dst);
            try writeValueDelta(rt, result_type_word, &dy, &dy_read, dst);
            try rt.setDerivative(allocator, result_id, &dx, &dy);
        }

        fn readImage(rt: *Runtime, dst: *Value, driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32, lod: ?i32) RuntimeError!void {
            switch (dst.*) {
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                => try writeFloatTexel(dst, try rt.image_api.readImageFloat4(driver_image, dim, x, y, z, lod)),

                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => try writeIntTexel(dst, try rt.image_api.readImageInt4(driver_image, dim, x, y, z, lod)),

                .Vector => |lanes| {
                    if (lanes.len == 0) return RuntimeError.InvalidSpirV;
                    switch (lanes[0]) {
                        .Float => try writeFloatTexel(dst, try rt.image_api.readImageFloat4(driver_image, dim, x, y, z, lod)),
                        .Int => try writeIntTexel(dst, try rt.image_api.readImageInt4(driver_image, dim, x, y, z, lod)),
                        else => return RuntimeError.InvalidValueType,
                    }
                },

                else => return RuntimeError.InvalidValueType,
            }
        }

        fn sampleImageImplicitLod(rt: *Runtime, dst: *Value, driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32, offset: Runtime.ImageOffset) RuntimeError!void {
            switch (dst.*) {
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                => try writeFloatTexel(dst, try rt.image_api.sampleImageFloat4(driver_image, driver_sampler, dim, x, y, z, lod, offset)),
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => try writeIntTexel(dst, try rt.image_api.sampleImageInt4(driver_image, driver_sampler, dim, x, y, z, lod, offset)),

                .Vector => |lanes| {
                    if (lanes.len == 0) return RuntimeError.InvalidSpirV;
                    switch (lanes[0]) {
                        .Float => try writeFloatTexel(dst, try rt.image_api.sampleImageFloat4(driver_image, driver_sampler, dim, x, y, z, lod, offset)),
                        .Int => try writeIntTexel(dst, try rt.image_api.sampleImageInt4(driver_image, driver_sampler, dim, x, y, z, lod, offset)),
                        else => return RuntimeError.InvalidValueType,
                    }
                },

                else => return RuntimeError.InvalidValueType,
            }
        }

        const CubeFaceCoord = struct {
            face: u32,
            u: f32,
            v: f32,
        };

        fn cubeFaceCoordForFace(face: u32, x: f32, y: f32, z: f32) CubeFaceCoord {
            const sc, const tc, const ma = switch (face) {
                0 => .{ -z, -y, @abs(x) },
                1 => .{ z, -y, @abs(x) },
                2 => .{ x, z, @abs(y) },
                3 => .{ x, -z, @abs(y) },
                4 => .{ x, -y, @abs(z) },
                5 => .{ -x, -y, @abs(z) },
                else => .{ 0.0, 0.0, 1.0 },
            };

            const inv_ma = if (ma == 0.0) 0.0 else 1.0 / ma;
            return .{
                .face = face,
                .u = (sc * inv_ma + 1.0) * 0.5,
                .v = (tc * inv_ma + 1.0) * 0.5,
            };
        }

        fn cubeFaceCoord(x: f32, y: f32, z: f32) CubeFaceCoord {
            const ax = @abs(x);
            const ay = @abs(y);
            const az = @abs(z);

            var sc: f32 = 0.0;
            var tc: f32 = 0.0;
            var ma: f32 = 1.0;
            var face: u32 = 0;

            if (ax >= ay and ax >= az) {
                ma = ax;
                if (x >= 0.0) {
                    face = 0;
                    sc = -z;
                    tc = -y;
                } else {
                    face = 1;
                    sc = z;
                    tc = -y;
                }
            } else if (ay >= ax and ay >= az) {
                ma = ay;
                if (y >= 0.0) {
                    face = 2;
                    sc = x;
                    tc = z;
                } else {
                    face = 3;
                    sc = x;
                    tc = -z;
                }
            } else {
                ma = az;
                if (z >= 0.0) {
                    face = 4;
                    sc = x;
                    tc = -y;
                } else {
                    face = 5;
                    sc = -x;
                    tc = -y;
                }
            }

            const inv_ma = if (ma == 0.0) 0.0 else 1.0 / ma;
            return .{
                .face = face,
                .u = (sc * inv_ma + 1.0) * 0.5,
                .v = (tc * inv_ma + 1.0) * 0.5,
            };
        }

        fn projectedSampleDerivatives(rt: *Runtime, coordinate_id: SpvWord, coordinate: *const Value) RuntimeError!?Runtime.ImageDerivatives {
            const coord_derivative = rt.derivatives.get(coordinate_id) orelse return null;
            const lane_count = try valueLaneCount(coordinate);
            if (lane_count < 2)
                return RuntimeError.InvalidSpirV;

            const q_lane = lane_count - 1;
            const q = try readProjectionDivisor(coordinate);
            const inv_q_squared = 1.0 / (q * q);
            const dq_dx = try readSampleCoordLane(&coord_derivative.dx, q_lane);
            const dq_dy = try readSampleCoordLane(&coord_derivative.dy, q_lane);

            const coord_x = try readSampleCoordLane(coordinate, 0);
            const coord_y = readSampleCoordLane(coordinate, 1) catch 0.0;
            const coord_z = readSampleCoordLane(coordinate, 2) catch 0.0;
            const dx_x = try readSampleCoordLane(&coord_derivative.dx, 0);
            const dx_y = readSampleCoordLane(&coord_derivative.dx, 1) catch 0.0;
            const dx_z = readSampleCoordLane(&coord_derivative.dx, 2) catch 0.0;
            const dy_x = try readSampleCoordLane(&coord_derivative.dy, 0);
            const dy_y = readSampleCoordLane(&coord_derivative.dy, 1) catch 0.0;
            const dy_z = readSampleCoordLane(&coord_derivative.dy, 2) catch 0.0;

            return .{
                .dx = .{
                    .x = (dx_x * q - coord_x * dq_dx) * inv_q_squared,
                    .y = if (q_lane > 1) (dx_y * q - coord_y * dq_dx) * inv_q_squared else 0.0,
                    .z = if (q_lane > 2) (dx_z * q - coord_z * dq_dx) * inv_q_squared else 0.0,
                    .w = 0.0,
                },
                .dy = .{
                    .x = (dy_x * q - coord_x * dq_dy) * inv_q_squared,
                    .y = if (q_lane > 1) (dy_y * q - coord_y * dq_dy) * inv_q_squared else 0.0,
                    .z = if (q_lane > 2) (dy_z * q - coord_z * dq_dy) * inv_q_squared else 0.0,
                    .w = 0.0,
                },
            };
        }

        fn sampleDerivativesFromRuntime(rt: *Runtime, coordinate_id: SpvWord) RuntimeError!?Runtime.ImageDerivatives {
            const coord_derivative = rt.derivatives.get(coordinate_id) orelse return null;
            return .{
                .dx = .{
                    .x = try readSampleCoordLane(&coord_derivative.dx, 0),
                    .y = readSampleCoordLane(&coord_derivative.dx, 1) catch 0.0,
                    .z = readSampleCoordLane(&coord_derivative.dx, 2) catch 0.0,
                    .w = readSampleCoordLane(&coord_derivative.dx, 3) catch 0.0,
                },
                .dy = .{
                    .x = try readSampleCoordLane(&coord_derivative.dy, 0),
                    .y = readSampleCoordLane(&coord_derivative.dy, 1) catch 0.0,
                    .z = readSampleCoordLane(&coord_derivative.dy, 2) catch 0.0,
                    .w = readSampleCoordLane(&coord_derivative.dy, 3) catch 0.0,
                },
            };
        }

        fn implicitSampleLod(rt: *Runtime, coordinate_id: SpvWord, coordinate: *const Value, projected: bool, driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, bias: f32) RuntimeError!?f32 {
            const fallback_lod = if (bias != 0.0) bias else null;
            const coord_derivatives = if (projected)
                (try projectedSampleDerivatives(rt, coordinate_id, coordinate)) orelse return fallback_lod
            else
                (try sampleDerivativesFromRuntime(rt, coordinate_id)) orelse return fallback_lod;
            const coord_dx_x = coord_derivatives.dx.x;
            const coord_dx_y = coord_derivatives.dx.y;
            const coord_dx_z = coord_derivatives.dx.z;
            const coord_dy_x = coord_derivatives.dy.x;
            const coord_dy_y = coord_derivatives.dy.y;
            const coord_dy_z = coord_derivatives.dy.z;
            const lod_dim, const derivatives = if (dim == .Cube) blk: {
                const center = cubeFaceCoord(x, y, z);
                const dx = cubeFaceCoordForFace(center.face, x + coord_dx_x, y + coord_dx_y, z + coord_dx_z);
                const dy = cubeFaceCoordForFace(center.face, x + coord_dy_x, y + coord_dy_y, z + coord_dy_z);
                break :blk .{
                    spv.SpvDim.@"2D",
                    Runtime.ImageDerivatives{
                        .dx = .{ .x = dx.u - center.u, .y = dx.v - center.v, .z = 0.0, .w = 0.0 },
                        .dy = .{ .x = dy.u - center.u, .y = dy.v - center.v, .z = 0.0, .w = 0.0 },
                    },
                };
            } else .{
                dim,
                Runtime.ImageDerivatives{
                    .dx = .{ .x = coord_dx_x, .y = coord_dx_y, .z = coord_dx_z, .w = 0.0 },
                    .dy = .{ .x = coord_dy_x, .y = coord_dy_y, .z = coord_dy_z, .w = 0.0 },
                },
            };
            const lod = try rt.image_api.queryImageLod(driver_image, driver_sampler, lod_dim, derivatives);
            return lod.y + bias;
        }

        fn setImplicitSampleDerivative(
            allocator: std.mem.Allocator,
            rt: *Runtime,
            result_type_word: SpvWord,
            result_id: SpvWord,
            coordinate_id: SpvWord,
            coordinate: *const Value,
            projected: bool,
            dst: *const Value,
            driver_image: *anyopaque,
            driver_sampler: *anyopaque,
            dim: spv.SpvDim,
            x: f32,
            y: f32,
            z: f32,
            bias: f32,
            offset: Runtime.ImageOffset,
        ) RuntimeError!void {
            const coord_derivatives = if (projected)
                (try projectedSampleDerivatives(rt, coordinate_id, coordinate)) orelse {
                    rt.clearDerivative(allocator, result_id);
                    return;
                }
            else
                (try sampleDerivativesFromRuntime(rt, coordinate_id)) orelse {
                    rt.clearDerivative(allocator, result_id);
                    return;
                };

            var dx_sample = try Value.init(allocator, rt.results, result_type_word, false);
            defer dx_sample.deinit(allocator);
            var dy_sample = try Value.init(allocator, rt.results, result_type_word, false);
            defer dy_sample.deinit(allocator);

            const coord_dx_x = coord_derivatives.dx.x;
            const coord_dx_y = coord_derivatives.dx.y;
            const coord_dx_z = coord_derivatives.dx.z;
            const coord_dy_x = coord_derivatives.dy.x;
            const coord_dy_y = coord_derivatives.dy.y;
            const coord_dy_z = coord_derivatives.dy.z;
            const sample_lod = try implicitSampleLod(rt, coordinate_id, coordinate, projected, driver_image, driver_sampler, dim, x, y, z, bias);

            try sampleImageImplicitLod(rt, &dx_sample, driver_image, driver_sampler, dim, x + coord_dx_x, y + coord_dx_y, z + coord_dx_z, sample_lod, offset);
            try sampleImageImplicitLod(rt, &dy_sample, driver_image, driver_sampler, dim, x + coord_dy_x, y + coord_dy_y, z + coord_dy_z, sample_lod, offset);

            var dx = try Value.init(allocator, rt.results, result_type_word, false);
            defer dx.deinit(allocator);
            var dy = try Value.init(allocator, rt.results, result_type_word, false);
            defer dy.deinit(allocator);

            try writeValueDelta(rt, result_type_word, &dx, &dx_sample, dst);
            try writeValueDelta(rt, result_type_word, &dy, &dy_sample, dst);

            try rt.setDerivative(allocator, result_id, &dx, &dy);
        }

        fn typeHasFloatLanes(rt: *Runtime, target_type: Result.TypeData) RuntimeError!bool {
            return switch (target_type) {
                .Float,
                .Vector2f32,
                .Vector3f32,
                .Vector4f32,
                => true,
                .Vector => |v| typeHasFloatLanes(rt, (try rt.results[v.components_type_word].getVariant()).Type),
                .Matrix => |m| typeHasFloatLanes(rt, (try rt.results[m.column_type_word].getVariant()).Type),
                else => false,
            };
        }

        fn sampleImageExplicitLod(rt: *Runtime, dst: *Value, driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, lod: ?f32, offset: Runtime.ImageOffset) RuntimeError!void {
            switch (dst.*) {
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                => try writeFloatTexel(dst, try rt.image_api.sampleImageFloat4(driver_image, driver_sampler, dim, x, y, z, lod, offset)),
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => try writeIntTexel(dst, try rt.image_api.sampleImageInt4(driver_image, driver_sampler, dim, x, y, z, lod, offset)),

                .Vector => |lanes| {
                    if (lanes.len == 0) return RuntimeError.InvalidSpirV;
                    switch (lanes[0]) {
                        .Float => try writeFloatTexel(dst, try rt.image_api.sampleImageFloat4(driver_image, driver_sampler, dim, x, y, z, lod, offset)),
                        .Int => try writeIntTexel(dst, try rt.image_api.sampleImageInt4(driver_image, driver_sampler, dim, x, y, z, lod, offset)),
                        else => return RuntimeError.InvalidValueType,
                    }
                },

                else => return RuntimeError.InvalidValueType,
            }
        }

        fn explicitSampleLod(rt: *Runtime, driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, parsed: ParsedImageOperands) RuntimeError!?f32 {
            if (parsed.grad) |derivatives| {
                const lod_dim, const lod_derivatives = if (dim == .Cube) blk: {
                    const center = cubeFaceCoord(x, y, z);
                    const dx = cubeFaceCoordForFace(center.face, x + derivatives.dx.x, y + derivatives.dx.y, z + derivatives.dx.z);
                    const dy = cubeFaceCoordForFace(center.face, x + derivatives.dy.x, y + derivatives.dy.y, z + derivatives.dy.z);
                    break :blk .{
                        spv.SpvDim.@"2D",
                        Runtime.ImageDerivatives{
                            .dx = .{ .x = dx.u - center.u, .y = dx.v - center.v, .z = 0.0, .w = 0.0 },
                            .dy = .{ .x = dy.u - center.u, .y = dy.v - center.v, .z = 0.0, .w = 0.0 },
                        },
                    };
                } else .{ dim, derivatives };
                const lod = try rt.image_api.queryImageLod(driver_image, driver_sampler, lod_dim, lod_derivatives);
                return lod.y;
            }
            return parsed.lod;
        }

        fn sampleImageDref(rt: *Runtime, dst: *Value, driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, w: f32, dref: f32, lod: ?f32, offset: Runtime.ImageOffset) RuntimeError!void {
            try writeFloatScalar(dst, try rt.image_api.sampleImageDref(driver_image, driver_sampler, dim, x, y, z, w, dref, lod, offset));
        }

        fn gatherCoord(index: i32, extent: u32) f32 {
            return (@as(f32, @floatFromInt(index)) + 0.5) / @as(f32, @floatFromInt(extent));
        }

        fn sampleImageGather(rt: *Runtime, dst: *Value, driver_image: *anyopaque, driver_sampler: *anyopaque, dim: spv.SpvDim, x: f32, y: f32, z: f32, component: usize, offset: Runtime.ImageOffset) RuntimeError!void {
            const size = try rt.image_api.queryImageSize(driver_image, dim, false, 0);
            if (size.x == 0) return RuntimeError.InvalidSpirV;

            const width_f: f32 = @floatFromInt(size.x);
            const height = if (size.y == 0) 1 else size.y;
            const height_f: f32 = @floatFromInt(height);
            const base_x: i32 = @intFromFloat(@floor(x * width_f - 0.5));
            const base_y: i32 = @intFromFloat(@floor(y * height_f - 0.5));
            const gather_x = [_]i32{ base_x, base_x + 1, base_x + 1, base_x };
            const gather_y = [_]i32{ base_y + 1, base_y + 1, base_y, base_y };

            switch (dst.*) {
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                => {
                    var result: Runtime.Vec4(f32) = undefined;
                    inline for (0..4) |i| {
                        const texel = try rt.image_api.sampleImageFloat4(
                            driver_image,
                            driver_sampler,
                            dim,
                            gatherCoord(gather_x[i], size.x),
                            gatherCoord(gather_y[i], height),
                            z,
                            0.0,
                            offset,
                        );
                        const values = [_]f32{ texel.x, texel.y, texel.z, texel.w };
                        @field(result, switch (i) {
                            0 => "x",
                            1 => "y",
                            2 => "z",
                            3 => "w",
                            else => unreachable,
                        }) = if (component < values.len) values[component] else return RuntimeError.InvalidSpirV;
                    }
                    try writeFloatTexel(dst, result);
                },
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => {
                    var result: Runtime.Vec4(u32) = undefined;
                    inline for (0..4) |i| {
                        const texel = try rt.image_api.sampleImageInt4(
                            driver_image,
                            driver_sampler,
                            dim,
                            gatherCoord(gather_x[i], size.x),
                            gatherCoord(gather_y[i], height),
                            z,
                            0.0,
                            offset,
                        );
                        const values = [_]u32{ texel.x, texel.y, texel.z, texel.w };
                        @field(result, switch (i) {
                            0 => "x",
                            1 => "y",
                            2 => "z",
                            3 => "w",
                            else => unreachable,
                        }) = if (component < values.len) values[component] else return RuntimeError.InvalidSpirV;
                    }
                    try writeIntTexel(dst, result);
                },
                .Vector => |lanes| {
                    if (lanes.len == 0) return RuntimeError.InvalidSpirV;
                    switch (lanes[0]) {
                        .Float => {
                            var result: Runtime.Vec4(f32) = undefined;
                            inline for (0..4) |i| {
                                const texel = try rt.image_api.sampleImageFloat4(driver_image, driver_sampler, dim, gatherCoord(gather_x[i], size.x), gatherCoord(gather_y[i], height), z, 0.0, offset);
                                const values = [_]f32{ texel.x, texel.y, texel.z, texel.w };
                                @field(result, switch (i) {
                                    0 => "x",
                                    1 => "y",
                                    2 => "z",
                                    3 => "w",
                                    else => unreachable,
                                }) = if (component < values.len) values[component] else return RuntimeError.InvalidSpirV;
                            }
                            try writeFloatTexel(dst, result);
                        },
                        .Int => {
                            var result: Runtime.Vec4(u32) = undefined;
                            inline for (0..4) |i| {
                                const texel = try rt.image_api.sampleImageInt4(driver_image, driver_sampler, dim, gatherCoord(gather_x[i], size.x), gatherCoord(gather_y[i], height), z, 0.0, offset);
                                const values = [_]u32{ texel.x, texel.y, texel.z, texel.w };
                                @field(result, switch (i) {
                                    0 => "x",
                                    1 => "y",
                                    2 => "z",
                                    3 => "w",
                                    else => unreachable,
                                }) = if (component < values.len) values[component] else return RuntimeError.InvalidSpirV;
                            }
                            try writeIntTexel(dst, result);
                        },
                        else => return RuntimeError.InvalidValueType,
                    }
                },
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn writeImage(rt: *Runtime, texel: *const Value, driver_image: *anyopaque, dim: spv.SpvDim, x: i32, y: i32, z: i32) RuntimeError!void {
            switch (texel.*) {
                .Float,
                .Vector4f32,
                .Vector3f32,
                .Vector2f32,
                => try rt.image_api.writeImageFloat4(driver_image, dim, x, y, z, try readFloatTexel(texel)),

                .Int,
                .Vector4i32,
                .Vector3i32,
                .Vector2i32,
                .Vector4u32,
                .Vector3u32,
                .Vector2u32,
                => try rt.image_api.writeImageInt4(driver_image, dim, x, y, z, try readIntTexel(texel)),

                .Vector => |lanes| {
                    if (lanes.len == 0) return RuntimeError.InvalidSpirV;
                    switch (lanes[0]) {
                        .Float => try rt.image_api.writeImageFloat4(driver_image, dim, x, y, z, try readFloatTexel(texel)),
                        .Int => try rt.image_api.writeImageInt4(driver_image, dim, x, y, z, try readIntTexel(texel)),
                        else => return RuntimeError.InvalidValueType,
                    }
                },

                else => return RuntimeError.InvalidValueType,
            }
        }

        fn readImageQueryLod(value: *const Value) RuntimeError!i32 {
            return switch (value.*) {
                .Int => |i| if (i.is_signed) i.value.sint32 else @intCast(i.value.uint32),
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn queryImageSize(rt: *Runtime, dst: *Value, image_operand: ImageOperand, lod: ?i32) RuntimeError!void {
            const size = try rt.image_api.queryImageSize(image_operand.driver_image, image_operand.dim, image_operand.arrayed, lod);
            switch (dst.*) {
                .Int => |*v| v.value.uint32 = size.x,
                .Vector2i32 => |*v| v.* = .{ @bitCast(size.x), @bitCast(size.y) },
                .Vector3i32 => |*v| v.* = .{ @bitCast(size.x), @bitCast(size.y), @bitCast(size.z) },
                .Vector4i32 => |*v| v.* = .{ @bitCast(size.x), @bitCast(size.y), @bitCast(size.z), @bitCast(size.w) },
                .Vector2u32 => |*v| v.* = .{ size.x, size.y },
                .Vector3u32 => |*v| v.* = .{ size.x, size.y, size.z },
                .Vector4u32 => |*v| v.* = .{ size.x, size.y, size.z, size.w },
                .Vector => |lanes| {
                    const values = [_]u32{ size.x, size.y, size.z, size.w };
                    for (lanes, 0..) |*lane, i| {
                        if (i >= values.len) return RuntimeError.InvalidSpirV;
                        switch (lane.*) {
                            .Int => |*int| int.value.uint32 = values[i],
                            else => return RuntimeError.InvalidValueType,
                        }
                    }
                },
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn queryImageSamples(rt: *Runtime, dst: *Value, image_operand: ImageOperand) RuntimeError!void {
            const samples = try rt.image_api.queryImageSamples(image_operand.driver_image);
            switch (dst.*) {
                .Int => |*v| v.value.uint32 = samples,
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn queryImageLevels(rt: *Runtime, dst: *Value, image_operand: ImageOperand) RuntimeError!void {
            const levels = try rt.image_api.queryImageLevels(image_operand.driver_image);
            switch (dst.*) {
                .Int => |*v| v.value.uint32 = levels,
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn queryImageLod(rt: *Runtime, dst: *Value, coordinate_id: SpvWord, image_operand: SampledImageOperand) RuntimeError!void {
            const coord_derivative = rt.derivatives.get(coordinate_id) orelse return RuntimeError.InvalidValueType;
            const coord = try rt.results[coordinate_id].getValue();
            const dim, const derivatives = if (image_operand.dim == .Cube) blk: {
                const x = try readSampleCoordLane(coord, 0);
                const y = try readSampleCoordLane(coord, 1);
                const z = try readSampleCoordLane(coord, 2);
                const center = cubeFaceCoord(x, y, z);
                const dx = cubeFaceCoordForFace(
                    center.face,
                    x + try readSampleCoordLane(&coord_derivative.dx, 0),
                    y + (readSampleCoordLane(&coord_derivative.dx, 1) catch 0.0),
                    z + (readSampleCoordLane(&coord_derivative.dx, 2) catch 0.0),
                );
                const dy = cubeFaceCoordForFace(
                    center.face,
                    x + try readSampleCoordLane(&coord_derivative.dy, 0),
                    y + (readSampleCoordLane(&coord_derivative.dy, 1) catch 0.0),
                    z + (readSampleCoordLane(&coord_derivative.dy, 2) catch 0.0),
                );
                break :blk .{
                    spv.SpvDim.@"2D",
                    Runtime.ImageDerivatives{
                        .dx = .{ .x = dx.u - center.u, .y = dx.v - center.v, .z = 0.0, .w = 0.0 },
                        .dy = .{ .x = dy.u - center.u, .y = dy.v - center.v, .z = 0.0, .w = 0.0 },
                    },
                };
            } else .{
                image_operand.dim,
                Runtime.ImageDerivatives{
                    .dx = .{
                        .x = try readSampleCoordLane(&coord_derivative.dx, 0),
                        .y = readSampleCoordLane(&coord_derivative.dx, 1) catch 0.0,
                        .z = readSampleCoordLane(&coord_derivative.dx, 2) catch 0.0,
                        .w = readSampleCoordLane(&coord_derivative.dx, 3) catch 0.0,
                    },
                    .dy = .{
                        .x = try readSampleCoordLane(&coord_derivative.dy, 0),
                        .y = readSampleCoordLane(&coord_derivative.dy, 1) catch 0.0,
                        .z = readSampleCoordLane(&coord_derivative.dy, 2) catch 0.0,
                        .w = readSampleCoordLane(&coord_derivative.dy, 3) catch 0.0,
                    },
                },
            };
            const lod = try rt.image_api.queryImageLod(image_operand.driver_image, image_operand.driver_sampler, dim, derivatives);

            switch (dst.*) {
                .Vector2f32 => |*v| v.* = .{ lod.x, lod.y },
                .Vector3f32 => |*v| v.* = .{ lod.x, lod.y, lod.z },
                .Vector4f32 => |*v| v.* = .{ lod.x, lod.y, lod.z, lod.w },
                .Vector => |lanes| {
                    const values = [_]f32{ lod.x, lod.y, lod.z, lod.w };
                    for (lanes, 0..) |*lane, i| {
                        if (i >= values.len) return RuntimeError.InvalidSpirV;
                        switch (lane.*) {
                            .Float => |*float| float.value.float32 = values[i],
                            else => return RuntimeError.InvalidValueType,
                        }
                    }
                },
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn op(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
            if (comptime Op == .Resolve) {
                _ = try rt.it.next(); // result type
                const dst = try rt.results[try rt.it.next()].getValue();
                const src = &rt.results[try rt.it.next()];

                const image_operand = try resolveSampledImage(src, rt);

                dst.Image = .{
                    .driver_image = image_operand.driver_image,
                    .type_word = image_operand.type_word,
                };

                return;
            }

            if (comptime Op == .Write) {
                const image = &rt.results[try rt.it.next()];
                const coordinate = try rt.results[try rt.it.next()].getValue();
                const texel = try rt.results[try rt.it.next()].getValue();
                if (rt.helper_invocation)
                    return;

                const image_operand = try resolveImage(image, rt);
                const x = try readStorageCoordLane(coordinate, 0);
                const y = readStorageCoordLane(coordinate, 1) catch 0;
                const z = readStorageCoordLane(coordinate, 2) catch 0;

                return try writeImage(rt, texel, image_operand.driver_image, image_operand.dim, x, y, z);
            }

            const result_type_word = try rt.it.next();
            const result_id = try rt.it.next();
            const image = &rt.results[try rt.it.next()];
            if (comptime Op == .QuerySize or Op == .QuerySizeLod or Op == .QuerySamples or Op == .QueryLevels) {
                const image_operand = try resolveImageForQuery(image, rt);
                const dst = try rt.results[result_id].getValue();
                if (comptime Op == .QuerySamples) {
                    return try queryImageSamples(rt, dst, image_operand);
                }
                if (comptime Op == .QueryLevels) {
                    return try queryImageLevels(rt, dst, image_operand);
                }
                var lod: ?i32 = null;
                if (comptime Op == .QuerySizeLod) {
                    lod = try readImageQueryLod(try rt.results[try rt.it.next()].getValue());
                }
                return try queryImageSize(rt, dst, image_operand, lod);
            }

            const coordinate_id = try rt.it.next();
            const coordinate = try rt.results[coordinate_id].getValue();
            const dst = try rt.results[result_id].getValue();

            switch (Op) {
                .QueryLod => {
                    const sampled_image_operand = try resolveSampledImage(image, rt);
                    return try queryImageLod(rt, dst, coordinate_id, sampled_image_operand);
                },

                .Fetch,
                .Read,
                => {
                    const image_operand = try resolveImage(image, rt);
                    const image_operands = if (word_count > 4) try rt.it.next() else 0;
                    const parsed_operands = try parseImageOperands(rt, image_operands);
                    const x = try readStorageCoordLane(coordinate, 0) + parsed_operands.offset.x;
                    const y = (readStorageCoordLane(coordinate, 1) catch 0) + parsed_operands.offset.y;
                    const z = (readStorageCoordLane(coordinate, 2) catch 0) + parsed_operands.offset.z;

                    const read_z = if (image_operand.arrayed) z else parsed_operands.sample orelse z;
                    try readImage(rt, dst, image_operand.driver_image, image_operand.dim, x, y, read_z, parsed_operands.image_lod);
                    try setImageReadDerivative(allocator, rt, result_type_word, result_id, coordinate_id, dst, image_operand.driver_image, image_operand.dim, x, y, read_z, parsed_operands.image_lod);
                },

                .SampleImplicitLod,
                .SampleProjImplicitLod,
                => {
                    const sampled_image_operand = try resolveSampledImage(image, rt);
                    const coords = if (comptime Op == .SampleProjImplicitLod)
                        try readProjectedSampleCoords(coordinate)
                    else
                        .{
                            .x = try readSampleCoordLane(coordinate, 0),
                            .y = readSampleCoordLane(coordinate, 1) catch 0,
                            .z = readSampleCoordLane(coordinate, 2) catch 0,
                            .w = readSampleCoordLane(coordinate, 3) catch 0,
                        };
                    const image_operands = if (word_count > 4) try rt.it.next() else 0;
                    const parsed_operands = try parseImageOperands(rt, image_operands);
                    const projected = comptime Op == .SampleProjImplicitLod;
                    const lod = try implicitSampleLod(
                        rt,
                        coordinate_id,
                        coordinate,
                        projected,
                        sampled_image_operand.driver_image,
                        sampled_image_operand.driver_sampler,
                        sampled_image_operand.dim,
                        coords.x,
                        coords.y,
                        coords.z,
                        parsed_operands.bias,
                    );

                    try sampleImageImplicitLod(
                        rt,
                        dst,
                        sampled_image_operand.driver_image,
                        sampled_image_operand.driver_sampler,
                        sampled_image_operand.dim,
                        coords.x,
                        coords.y,
                        coords.z,
                        lod,
                        parsed_operands.offset,
                    );
                    try setImplicitSampleDerivative(
                        allocator,
                        rt,
                        result_type_word,
                        result_id,
                        coordinate_id,
                        coordinate,
                        projected,
                        dst,
                        sampled_image_operand.driver_image,
                        sampled_image_operand.driver_sampler,
                        sampled_image_operand.dim,
                        coords.x,
                        coords.y,
                        coords.z,
                        parsed_operands.bias,
                        parsed_operands.offset,
                    );
                },

                .SampleDrefImplicitLod,
                .SampleProjDrefImplicitLod,
                => {
                    const sampled_image_operand = try resolveSampledImage(image, rt);
                    const coords = if (comptime Op == .SampleProjDrefImplicitLod)
                        try readProjectedSampleCoords(coordinate)
                    else
                        .{
                            .x = try readSampleCoordLane(coordinate, 0),
                            .y = readSampleCoordLane(coordinate, 1) catch 0,
                            .z = readSampleCoordLane(coordinate, 2) catch 0,
                            .w = readSampleCoordLane(coordinate, 3) catch 0,
                        };
                    const raw_dref = try readFloatLane(try rt.results[try rt.it.next()].getValue(), 0);
                    const dref = if (comptime Op == .SampleProjDrefImplicitLod)
                        raw_dref / try readProjectionDivisor(coordinate)
                    else
                        raw_dref;
                    const image_operands = if (word_count > 5) try rt.it.next() else 0;
                    const parsed_operands = try parseImageOperands(rt, image_operands);
                    const projected = comptime Op == .SampleProjDrefImplicitLod;
                    const lod = try implicitSampleLod(
                        rt,
                        coordinate_id,
                        coordinate,
                        projected,
                        sampled_image_operand.driver_image,
                        sampled_image_operand.driver_sampler,
                        sampled_image_operand.dim,
                        coords.x,
                        coords.y,
                        coords.z,
                        parsed_operands.bias,
                    );

                    try sampleImageDref(
                        rt,
                        dst,
                        sampled_image_operand.driver_image,
                        sampled_image_operand.driver_sampler,
                        sampled_image_operand.dim,
                        coords.x,
                        coords.y,
                        coords.z,
                        coords.w,
                        dref,
                        lod,
                        parsed_operands.offset,
                    );
                },

                .SampleExplicitLod,
                .SampleProjExplicitLod,
                => {
                    const sampled_image_operand = try resolveSampledImage(image, rt);
                    const coords = if (comptime Op == .SampleProjExplicitLod)
                        try readProjectedSampleCoords(coordinate)
                    else
                        .{
                            .x = try readSampleCoordLane(coordinate, 0),
                            .y = readSampleCoordLane(coordinate, 1) catch 0,
                            .z = readSampleCoordLane(coordinate, 2) catch 0,
                            .w = readSampleCoordLane(coordinate, 3) catch 0,
                        };
                    const image_operands = if (word_count > 4) try rt.it.next() else 0;
                    const parsed_operands = try parseImageOperands(rt, image_operands);

                    try sampleImageExplicitLod(
                        rt,
                        dst,
                        sampled_image_operand.driver_image,
                        sampled_image_operand.driver_sampler,
                        sampled_image_operand.dim,
                        coords.x,
                        coords.y,
                        coords.z,
                        try explicitSampleLod(
                            rt,
                            sampled_image_operand.driver_image,
                            sampled_image_operand.driver_sampler,
                            sampled_image_operand.dim,
                            coords.x,
                            coords.y,
                            coords.z,
                            parsed_operands,
                        ),
                        parsed_operands.offset,
                    );
                },

                .SampleDrefExplicitLod,
                .SampleProjDrefExplicitLod,
                => {
                    const sampled_image_operand = try resolveSampledImage(image, rt);
                    const coords = if (comptime Op == .SampleProjDrefExplicitLod)
                        try readProjectedSampleCoords(coordinate)
                    else
                        .{
                            .x = try readSampleCoordLane(coordinate, 0),
                            .y = readSampleCoordLane(coordinate, 1) catch 0,
                            .z = readSampleCoordLane(coordinate, 2) catch 0,
                            .w = readSampleCoordLane(coordinate, 3) catch 0,
                        };
                    const raw_dref = try readFloatLane(try rt.results[try rt.it.next()].getValue(), 0);
                    const dref = if (comptime Op == .SampleProjDrefExplicitLod)
                        raw_dref / try readProjectionDivisor(coordinate)
                    else
                        raw_dref;
                    const image_operands = if (word_count > 5) try rt.it.next() else 0;
                    const parsed_operands = try parseImageOperands(rt, image_operands);

                    try sampleImageDref(
                        rt,
                        dst,
                        sampled_image_operand.driver_image,
                        sampled_image_operand.driver_sampler,
                        sampled_image_operand.dim,
                        coords.x,
                        coords.y,
                        coords.z,
                        coords.w,
                        dref,
                        try explicitSampleLod(
                            rt,
                            sampled_image_operand.driver_image,
                            sampled_image_operand.driver_sampler,
                            sampled_image_operand.dim,
                            coords.x,
                            coords.y,
                            coords.z,
                            parsed_operands,
                        ),
                        parsed_operands.offset,
                    );
                },

                .Gather => {
                    const sampled_image_operand = try resolveSampledImage(image, rt);
                    const coords = .{
                        .x = try readSampleCoordLane(coordinate, 0),
                        .y = readSampleCoordLane(coordinate, 1) catch 0,
                        .z = readSampleCoordLane(coordinate, 2) catch 0,
                    };
                    const component_value = try rt.results[try rt.it.next()].getValue();
                    const component: usize = @intCast(try readIntLane(component_value, 0));
                    const image_operands = if (word_count > 5) try rt.it.next() else 0;
                    const parsed_operands = try parseImageOperands(rt, image_operands);

                    try sampleImageGather(
                        rt,
                        dst,
                        sampled_image_operand.driver_image,
                        sampled_image_operand.driver_sampler,
                        sampled_image_operand.dim,
                        coords.x,
                        coords.y,
                        coords.z,
                        component,
                        parsed_operands.offset,
                    );
                },

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn resolveImageDimForTexelPointer(rt: *Runtime, type_word: SpvWord) RuntimeError!spv.SpvDim {
    return switch ((try rt.results[type_word].getConstVariant()).*) {
        .Type => |t| switch (t) {
            .Image => |i| i.dim,
            .SampledImage => |i| return resolveImageDimForTexelPointer(rt, i.image_type),
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidSpirV,
    };
}

fn readStorageCoordLaneForTexelPointer(coord: *const Value, lane_index: usize) RuntimeError!i32 {
    return switch (coord.*) {
        .Int => |i| {
            if (lane_index != 0) return RuntimeError.OutOfBounds;
            return if (i.is_signed) i.value.sint32 else @intCast(i.value.uint32);
        },
        .Vector => |lanes| {
            if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
            return readStorageCoordLaneForTexelPointer(&lanes[lane_index], 0);
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

fn opImageTexelPointer(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = word_count;
    const result_type = try rt.it.next();
    const result_id = try rt.it.next();
    const image_id = try rt.it.next();
    const coord = try rt.results[try rt.it.next()].getValue();
    _ = try rt.it.next(); // sample

    const image = switch ((try rt.results[image_id].getValue()).*) {
        .Image => |img| img,
        else => return RuntimeError.InvalidSpirV,
    };
    const dim = try resolveImageDimForTexelPointer(rt, image.type_word);
    const x = try readStorageCoordLaneForTexelPointer(coord, 0);
    const y = readStorageCoordLaneForTexelPointer(coord, 1) catch 0;
    const z = readStorageCoordLaneForTexelPointer(coord, 2) catch 0;
    const texel = try rt.image_api.readImageInt4(image.driver_image, dim, x, y, z, null);

    const pointer_type = switch ((try rt.results[result_type].getConstVariant()).*) {
        .Type => |t| switch (t) {
            .Pointer => |ptr| ptr,
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidSpirV,
    };
    const target_type = switch ((try rt.results[pointer_type.target].getConstVariant()).*) {
        .Type => |t| t,
        else => return RuntimeError.InvalidSpirV,
    };

    const backing = allocator.create(Value) catch return RuntimeError.OutOfMemory;
    errdefer allocator.destroy(backing);
    backing.* = switch (target_type) {
        .Int => |i| .{ .Int = .{
            .bit_count = i.bit_length,
            .is_signed = i.is_signed,
            .value = if (i.is_signed) .{ .sint32 = @bitCast(texel.x) } else .{ .uint32 = texel.x },
        } },
        else => return RuntimeError.InvalidSpirV,
    };
    errdefer backing.deinit(allocator);

    const indexes = allocator.alloc(SpvWord, 0) catch return RuntimeError.OutOfMemory;
    errdefer allocator.free(indexes);

    const new_value: Value = .{ .Pointer = .{
        .ptr = .{ .common = backing },
        .image_texel = .{
            .driver_image = image.driver_image,
            .dim = dim,
            .x = x,
            .y = y,
            .z = z,
        },
        .uniform_backing_value = backing,
        .owns_uniform_backing_value = true,
    } };

    if (rt.results[result_id].variant) |variant| {
        rt.results[result_id].variant = null;
        var old_variant = variant;
        switch (old_variant) {
            .AccessChain => |*a| {
                try a.value.flushPtr(allocator);
                allocator.free(a.indexes);
                a.value.deinit(allocator);
            },
            else => {},
        }
    }

    rt.results[result_id].variant = .{
        .AccessChain = .{
            .target = result_type,
            .base = image_id,
            .indexes = indexes,
            .value = new_value,
        },
    };
}

fn writeImageTexelPointer(rt: *Runtime, ptr: *Value) RuntimeError!void {
    if (std.meta.activeTag(ptr.*) != .Pointer)
        return;
    const image_texel = ptr.Pointer.image_texel orelse return;
    const value = switch (ptr.Pointer.ptr) {
        .common => |v| v,
        else => return RuntimeError.InvalidSpirV,
    };
    const component = switch (value.*) {
        .Int => |i| if (i.is_signed) @as(u32, @bitCast(i.value.sint32)) else i.value.uint32,
        else => return RuntimeError.InvalidValueType,
    };
    try rt.image_api.writeImageInt4(
        image_texel.driver_image,
        image_texel.dim,
        image_texel.x,
        image_texel.y,
        image_texel.z,
        .{ .x = component, .y = 0, .z = 0, .w = 0 },
    );
}

fn AtomicEngine(comptime Op: AtomicOp) type {
    return struct {
        fn apply(old: u32, value: u32, comparator: u32) u32 {
            return switch (Op) {
                .Add => old +% value,
                .And => old & value,
                .CompareExchange => if (old == comparator) value else old,
                .Decrement => old -% 1,
                .Exchange => value,
                .Increment => old +% 1,
                .MaxSigned => @bitCast(@max(@as(i32, @bitCast(old)), @as(i32, @bitCast(value)))),
                .MaxUnsigned => @max(old, value),
                .MinSigned => @bitCast(@min(@as(i32, @bitCast(old)), @as(i32, @bitCast(value)))),
                .MinUnsigned => @min(old, value),
                .Or => old | value,
                .Sub => old -% value,
                .Xor => old ^ value,
            };
        }

        fn readU32(value: *const Value) RuntimeError!u32 {
            return switch (value.*) {
                .Int => |i| if (i.is_signed) @bitCast(i.value.sint32) else i.value.uint32,
                .Pointer => |p| switch (p.ptr) {
                    .common => |v| readU32(v),
                    .u32_ptr => |ptr| ptr.*,
                    .i32_ptr => |ptr| @bitCast(ptr.*),
                    else => return RuntimeError.InvalidValueType,
                },
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn writeU32(value: *Value, bits: u32) RuntimeError!void {
            switch (value.*) {
                .Int => |*i| {
                    if (i.is_signed) {
                        i.value.sint32 = @bitCast(bits);
                    } else {
                        i.value.uint32 = bits;
                    }
                },
                .Pointer => |p| switch (p.ptr) {
                    .common => |v| try writeU32(v, bits),
                    .u32_ptr => |ptr| ptr.* = bits,
                    .i32_ptr => |ptr| ptr.* = @bitCast(bits),
                    else => return RuntimeError.InvalidValueType,
                },
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn op(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            _ = try rt.it.next(); // result type
            const dst = try rt.results[try rt.it.next()].getValue();
            const ptr = try rt.results[try rt.it.next()].getValue();
            _ = try rt.it.next(); // scope
            _ = try rt.it.next(); // semantics

            if (comptime Op == .CompareExchange) {
                _ = try rt.it.next(); // unequal semantics
            }

            const value: u32 = switch (Op) {
                .Decrement, .Increment => 0,
                else => try readU32(try rt.results[try rt.it.next()].getValue()),
            };
            const comparator: u32 = if (comptime Op == .CompareExchange)
                try readU32(try rt.results[try rt.it.next()].getValue())
            else
                0;

            const old = try readU32(ptr);
            const new = apply(old, value, comparator);
            if (!rt.helper_invocation)
                try writeU32(ptr, new);
            try writeU32(dst, old);
            if (!rt.helper_invocation) {
                try writeImageTexelPointer(rt, ptr);
                try ptr.flushPtr(allocator);
            }
        }
    };
}

fn readMatrixLane(comptime bits: u32, matrix: *const Value, column_index: usize, row_index: usize) RuntimeError!Value.getPrimitiveFieldType(.Float, bits) {
    const columns = switch (matrix.*) {
        .Matrix => |columns| columns,
        else => return RuntimeError.InvalidSpirV,
    };
    if (column_index >= columns.len) return RuntimeError.OutOfBounds;
    return Value.readLane(.Float, bits, &columns[column_index], row_index);
}

fn opOuterProduct(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
    const dst = try rt.results[try rt.it.next()].getValue();
    const lhs = try rt.results[try rt.it.next()].getValue();
    const rhs = try rt.results[try rt.it.next()].getValue();

    const dst_columns = switch (dst.*) {
        .Matrix => |columns| columns,
        else => return RuntimeError.InvalidSpirV,
    };
    const lhs_lanes = try lhs.resolveLaneCount();
    const rhs_lanes = try rhs.resolveLaneCount();
    if (dst_columns.len != rhs_lanes) return RuntimeError.InvalidSpirV;

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            for (dst_columns, 0..) |*dst_column, column_index| {
                if (try dst_column.resolveLaneCount() != lhs_lanes) return RuntimeError.InvalidSpirV;
                const rhs_lane = try Value.readLane(.Float, bits, rhs, column_index);
                for (0..lhs_lanes) |row_index| {
                    const lhs_lane = try Value.readLane(.Float, bits, lhs, row_index);
                    try Value.writeLane(.Float, bits, dst_column, row_index, lhs_lane * rhs_lane);
                }
            }
        },
        else => return RuntimeError.UnsupportedSpirV,
    }
}

fn opTranspose(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
    const dst = try rt.results[try rt.it.next()].getValue();
    const src = try rt.results[try rt.it.next()].getValue();

    const dst_columns = switch (dst.*) {
        .Matrix => |columns| columns,
        else => return RuntimeError.InvalidSpirV,
    };
    const src_columns = switch (src.*) {
        .Matrix => |columns| columns,
        else => return RuntimeError.InvalidSpirV,
    };
    if (dst_columns.len == 0 or src_columns.len == 0) return RuntimeError.InvalidSpirV;

    const dst_rows = try dst_columns[0].resolveLaneCount();
    if (dst_rows != src_columns.len) return RuntimeError.InvalidSpirV;

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            for (dst_columns, 0..) |*dst_column, dst_column_index| {
                const src_row_index = dst_column_index;
                for (0..dst_rows) |dst_row_index| {
                    const src_column_index = dst_row_index;
                    const value = try readMatrixLane(bits, src, src_column_index, src_row_index);
                    try Value.writeLane(.Float, bits, dst_column, dst_row_index, value);
                }
            }
        },
        else => return RuntimeError.UnsupportedSpirV,
    }
}

fn MathEngine(comptime T: PrimitiveType, comptime Op: MathOp, comptime IsAtomic: bool) type {
    return struct {
        fn operation(comptime TT: type, op1: TT, op2: TT) RuntimeError!TT {
            const is_int = @typeInfo(TT) == .int or (@typeInfo(TT) == .vector and @typeInfo(std.meta.Child(TT)) == .int);
            const op2_is_zero = if (@typeInfo(TT) == .vector) std.simd.countElementsWithValue(op2, 0) != 0 else op2 == 0;
            const zero: TT = switch (@typeInfo(TT)) {
                .vector => @splat(0),
                else => 0,
            };

            if (@typeInfo(TT) == .vector) {
                switch (Op) {
                    .Div, .Mod, .Rem => {
                        var result: TT = undefined;
                        inline for (0..@typeInfo(TT).vector.len) |i| {
                            result[i] = if (op2[i] == 0)
                                0
                            else switch (Op) {
                                .Div => if (comptime is_int) @divTrunc(op1[i], op2[i]) else op1[i] / op2[i],
                                .Mod => @mod(op1[i], op2[i]),
                                .Rem => @rem(op1[i], op2[i]),
                                else => unreachable,
                            };
                        }
                        return result;
                    },
                    else => {},
                }
            }

            return switch (Op) {
                .Add => if (comptime is_int) @addWithOverflow(op1, op2)[0] else op1 + op2,
                .Sub => if (comptime is_int) @subWithOverflow(op1, op2)[0] else op1 - op2,
                .Mul,
                .MatrixTimesMatrix,
                .MatrixTimesScalar,
                .MatrixTimesVector,
                .VectorTimesScalar,
                .VectorTimesMatrix,
                => if (comptime is_int) @mulWithOverflow(op1, op2)[0] else op1 * op2,
                .Div => blk: {
                    if (comptime is_int) {
                        if (op2_is_zero) return zero;
                    }
                    break :blk if (comptime is_int) @divTrunc(op1, op2) else op1 / op2;
                },
                .Mod => if (op2_is_zero) zero else @mod(op1, op2),
                .Rem => if (op2_is_zero) zero else @rem(op1, op2),
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn applyScalarRaw(comptime BitCount: SpvWord, l: *const Value, r: *const Value) RuntimeError!Value.getPrimitiveFieldType(T, BitCount) {
            const ScalarT = Value.getPrimitiveFieldType(T, BitCount);
            const l_field = try Value.getPrimitiveFieldConst(T, BitCount, l);
            const r_field = try Value.getPrimitiveFieldConst(T, BitCount, r);
            return try operation(ScalarT, l_field.*, r_field.*);
        }

        fn applyScalar(bit_count: SpvWord, d: *Value, l: *const Value, r: *const Value) RuntimeError!void {
            switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| {
                    if (comptime bits == 8 and T == .Float) return RuntimeError.UnsupportedSpirV;
                    const d_field = try Value.getPrimitiveField(T, bits, d);
                    d_field.* = try applyScalarRaw(bits, l, r);
                },
                else => return RuntimeError.UnsupportedSpirV,
            }
        }

        inline fn applyVectorTimesScalarFloat(comptime bit_count: SpvWord, d: []Value, l: []const Value, r_v: *const Value) RuntimeError!void {
            for (d, l) |*d_v, l_v| {
                switch (bit_count) {
                    inline 16 => d_v.Float.value.float16 = l_v.Float.value.float16 * r_v.Float.value.float16,
                    inline 32 => d_v.Float.value.float32 = l_v.Float.value.float32 * r_v.Float.value.float32,
                    inline 64 => d_v.Float.value.float64 = l_v.Float.value.float64 * r_v.Float.value.float64,
                    else => return RuntimeError.UnsupportedSpirV,
                }
            }
        }

        inline fn matrixRows(matrix: *const Value) RuntimeError!usize {
            const columns = switch (matrix.*) {
                .Matrix => |columns| columns,
                else => return RuntimeError.InvalidSpirV,
            };
            if (columns.len == 0) return RuntimeError.InvalidSpirV;
            return try columns[0].resolveLaneCount();
        }

        fn applyMatrixTimesVectorFloat(comptime bits: SpvWord, dst_vec: []Value, matrix: *const Value, vector: *const Value) RuntimeError!void {
            const columns = switch (matrix.*) {
                .Matrix => |columns| columns,
                else => return RuntimeError.InvalidSpirV,
            };
            const rows = try matrixRows(matrix);
            if (dst_vec.len != rows or try vector.resolveLaneCount() != columns.len) return RuntimeError.InvalidSpirV;

            const FloatT = Value.getPrimitiveFieldType(.Float, bits);
            for (dst_vec, 0..) |*dst_lane, row_index| {
                var sum: FloatT = 0;
                for (columns, 0..) |*column, column_index| {
                    const l = try Value.readLane(.Float, bits, column, row_index);
                    const r = try Value.readLane(.Float, bits, vector, column_index);
                    sum += l * r;
                }
                try Value.writeLane(.Float, bits, dst_lane, 0, sum);
            }
        }

        fn applyVectorTimesMatrixFloat(comptime bits: SpvWord, dst_vec: []Value, vector: *const Value, matrix: *const Value) RuntimeError!void {
            const columns = switch (matrix.*) {
                .Matrix => |columns| columns,
                else => return RuntimeError.InvalidSpirV,
            };
            if (dst_vec.len != columns.len) return RuntimeError.InvalidSpirV;
            const rows = try matrixRows(matrix);
            if (try vector.resolveLaneCount() != rows) return RuntimeError.InvalidSpirV;

            const FloatT = Value.getPrimitiveFieldType(.Float, bits);
            for (dst_vec, columns, 0..) |*dst_lane, *column, column_index| {
                _ = column_index;
                var sum: FloatT = 0;
                for (0..rows) |row_index| {
                    const l = try Value.readLane(.Float, bits, vector, row_index);
                    const r = try Value.readLane(.Float, bits, column, row_index);
                    sum += l * r;
                }
                try Value.writeLane(.Float, bits, dst_lane, 0, sum);
            }
        }

        fn applyMatrixTimesMatrixFloat(comptime bits: SpvWord, dst_matrix: []Value, lhs_matrix: *const Value, rhs_matrix: *const Value) RuntimeError!void {
            const lhs_columns = switch (lhs_matrix.*) {
                .Matrix => |columns| columns,
                else => return RuntimeError.InvalidSpirV,
            };
            const rhs_columns = switch (rhs_matrix.*) {
                .Matrix => |columns| columns,
                else => return RuntimeError.InvalidSpirV,
            };
            if (dst_matrix.len != rhs_columns.len) return RuntimeError.InvalidSpirV;

            const rows = try matrixRows(lhs_matrix);
            if (lhs_columns.len != try matrixRows(rhs_matrix)) return RuntimeError.InvalidSpirV;

            const FloatT = Value.getPrimitiveFieldType(.Float, bits);
            for (dst_matrix, rhs_columns) |*dst_column, *rhs_column| {
                if (try dst_column.resolveLaneCount() != rows) return RuntimeError.InvalidSpirV;

                for (0..rows) |row_index| {
                    var sum: FloatT = 0;
                    for (lhs_columns, 0..) |*lhs_column, inner_index| {
                        const l = try Value.readLane(.Float, bits, lhs_column, row_index);
                        const r = try Value.readLane(.Float, bits, rhs_column, inner_index);
                        sum += l * r;
                    }
                    try Value.writeLane(.Float, bits, dst_column, row_index, sum);
                }
            }
        }

        inline fn applySIMDVector(comptime ElemT: type, comptime N: usize, d: *@Vector(N, ElemT), l: @Vector(N, ElemT), r: @Vector(N, ElemT)) RuntimeError!void {
            d.* = try operation(@Vector(N, ElemT), l, r);
        }

        fn applySIMDVectorf32(comptime N: usize, d: *@Vector(N, f32), l: *const Value, r: *const Value) RuntimeError!void {
            switch (Op) {
                .MatrixTimesVector => {
                    const columns = switch (l.*) {
                        .Matrix => |columns| columns,
                        else => return RuntimeError.InvalidSpirV,
                    };
                    if (try r.resolveLaneCount() != columns.len) return RuntimeError.InvalidSpirV;

                    inline for (0..N) |row_index| {
                        d[row_index] = 0;
                        for (columns, 0..) |*column, column_index| {
                            d[row_index] += try Value.readLane(.Float, 32, column, row_index) *
                                try Value.readLane(.Float, 32, r, column_index);
                        }
                    }
                },
                .VectorTimesMatrix => {
                    const columns = switch (r.*) {
                        .Matrix => |columns| columns,
                        else => return RuntimeError.InvalidSpirV,
                    };
                    if (columns.len != N) return RuntimeError.InvalidSpirV;
                    const rows = try matrixRows(r);
                    if (try l.resolveLaneCount() != rows) return RuntimeError.InvalidSpirV;

                    inline for (0..N) |column_index| {
                        d[column_index] = 0;
                        for (0..rows) |row_index| {
                            d[column_index] += try Value.readLane(.Float, 32, l, row_index) *
                                try Value.readLane(.Float, 32, &columns[column_index], row_index);
                        }
                    }
                },
                else => try applyDirectSIMDVectorf32(N, d, l.getVectorSpecialization(N, f32), r),
            }
        }

        fn applyDirectSIMDVectorf32(comptime N: usize, d: *@Vector(N, f32), l: @Vector(N, f32), r: *const Value) RuntimeError!void {
            switch (Op) {
                .VectorTimesScalar,
                .MatrixTimesScalar,
                => d.* = l * @as(@Vector(N, f32), @splat(r.Float.value.float32)),
                else => try applySIMDVector(f32, N, d, l, r.getVectorSpecialization(N, f32)),
            }
        }

        fn operationSingle(comptime TT: type, ope: TT) RuntimeError!TT {
            return switch (Op) {
                .Negate => if (@typeInfo(TT) == .int) @subWithOverflow(@as(TT, 0), ope)[0] else -ope,
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn applyScalarSingle(bit_count: SpvWord, d: *Value, v: *Value) RuntimeError!void {
            switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| {
                    if (bits == 8 and T == .Float) return RuntimeError.InvalidSpirV;

                    const ScalarT = Value.getPrimitiveFieldType(T, bits);
                    const d_field = try Value.getPrimitiveField(T, bits, d);
                    const v_field = try Value.getPrimitiveField(T, bits, v);
                    d_field.* = try operationSingle(ScalarT, v_field.*);
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn derivativeAdd(comptime ScalarT: type, lhs: ScalarT, rhs: ScalarT) ScalarT {
            return switch (@typeInfo(ScalarT)) {
                .int => @addWithOverflow(lhs, rhs)[0],
                else => lhs + rhs,
            };
        }

        fn derivativeSub(comptime ScalarT: type, lhs: ScalarT, rhs: ScalarT) ScalarT {
            return switch (@typeInfo(ScalarT)) {
                .int => @subWithOverflow(lhs, rhs)[0],
                else => lhs - rhs,
            };
        }

        inline fn applySIMDVectorSingle(comptime ElemT: type, comptime N: usize, d: *@Vector(N, ElemT), v: *const @Vector(N, ElemT)) RuntimeError!void {
            inline for (0..N) |i| {
                d[i] = try operationSingle(ElemT, v[i]);
            }
        }

        fn propagateDerivative(
            allocator: std.mem.Allocator,
            rt: *Runtime,
            target_type_word: SpvWord,
            target_type: Result.TypeData,
            dst_id: SpvWord,
            lhs_id: SpvWord,
            rhs_id: SpvWord,
            lhs: *const Value,
            rhs: *const Value,
        ) RuntimeError!void {
            switch (comptime Op) {
                .Add, .Sub, .Mul, .Div, .Mod, .VectorTimesScalar => {},
                else => {
                    rt.clearDerivative(allocator, dst_id);
                    return;
                },
            }

            const lhs_derivative = rt.derivatives.get(lhs_id);
            const rhs_derivative = rt.derivatives.get(rhs_id);
            if (lhs_derivative == null and rhs_derivative == null) {
                rt.clearDerivative(allocator, dst_id);
                return;
            }

            var dx = try Value.init(allocator, rt.results, target_type_word, false);
            defer dx.deinit(allocator);
            var dy = try Value.init(allocator, rt.results, target_type_word, false);
            defer dy.deinit(allocator);

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
            const lane_count = try Result.resolveLaneCount(target_type);

            switch (lane_bits) {
                inline 16, 32, 64 => |bits| {
                    const ScalarT = Value.getPrimitiveFieldType(T, bits);
                    for (0..lane_count) |lane_index| {
                        const l = try Value.readLane(T, bits, lhs, lane_index);
                        const r = try Value.readLane(T, bits, rhs, lane_index);

                        const ldx: ScalarT = if (lhs_derivative) |derivative|
                            try Value.readLane(T, bits, &derivative.dx, lane_index)
                        else
                            @as(ScalarT, 0);
                        const ldy: ScalarT = if (lhs_derivative) |derivative|
                            try Value.readLane(T, bits, &derivative.dy, lane_index)
                        else
                            @as(ScalarT, 0);
                        const rdy: ScalarT = if (rhs_derivative) |derivative|
                            try Value.readLane(T, bits, &derivative.dy, lane_index)
                        else
                            @as(ScalarT, 0);
                        const rdx: ScalarT = if (rhs_derivative) |derivative|
                            try Value.readLane(T, bits, &derivative.dx, lane_index)
                        else
                            @as(ScalarT, 0);

                        const base = try operation(ScalarT, l, r);
                        const dx_lane = derivativeSub(ScalarT, try operation(ScalarT, derivativeAdd(ScalarT, l, ldx), derivativeAdd(ScalarT, r, rdx)), base);
                        const dy_lane = derivativeSub(ScalarT, try operation(ScalarT, derivativeAdd(ScalarT, l, ldy), derivativeAdd(ScalarT, r, rdy)), base);

                        try Value.writeLane(T, bits, &dx, lane_index, dx_lane);
                        try Value.writeLane(T, bits, &dy, lane_index, dy_lane);
                    }
                },
                else => return RuntimeError.UnsupportedSpirV,
            }

            try rt.setDerivative(allocator, dst_id, &dx, &dy);
        }

        fn op(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type_word = try rt.it.next();
            const target_type = (try rt.results[target_type_word].getVariant()).Type;
            const dst_id = try rt.it.next();
            const dst = try rt.results[dst_id].getValue();
            const lhs_id = try rt.it.next();
            const lhs = try rt.results[lhs_id].getValue();

            var arena = std.heap.ArenaAllocator.init(allocator);
            defer arena.deinit();

            var lhs_save: ?Value = null;

            if (comptime IsAtomic) {
                _ = rt.it.skip(); // scope
                _ = rt.it.skip(); // semantic
                lhs_save = try lhs.dupe(arena.allocator());
            }

            const rhs_id = try rt.it.next();
            const rhs = try rt.results[rhs_id].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

            const vectorRoutines = struct {
                fn routines(dst2: *Value, lhs2: *const Value, rhs2: *const Value, lane_bits2: SpvWord) RuntimeError!void {
                    switch (dst2.*) {
                        .Vector => |dst_vec| switch (Op) {
                            .VectorTimesScalar, .MatrixTimesScalar => switch (lane_bits2) {
                                inline 16, 32, 64 => |bits_count| try applyVectorTimesScalarFloat(bits_count, dst_vec, lhs2.Vector, rhs2),
                                else => return RuntimeError.UnsupportedSpirV,
                            },
                            .MatrixTimesVector => switch (lane_bits2) {
                                inline 16, 32, 64 => |bits_count| try applyMatrixTimesVectorFloat(bits_count, dst_vec, lhs2, rhs2),
                                else => return RuntimeError.UnsupportedSpirV,
                            },
                            .VectorTimesMatrix => switch (lane_bits2) {
                                inline 16, 32, 64 => |bits_count| try applyVectorTimesMatrixFloat(bits_count, dst_vec, lhs2, rhs2),
                                else => return RuntimeError.UnsupportedSpirV,
                            },
                            else => for (dst_vec, lhs2.Vector, rhs2.Vector) |*d_lane, *l_lane, *r_lane| {
                                try applyScalar(lane_bits2, d_lane, l_lane, r_lane);
                            },
                        },

                        .Vector4f32 => |*d| try applySIMDVectorf32(4, d, lhs2, rhs2),
                        .Vector3f32 => |*d| try applySIMDVectorf32(3, d, lhs2, rhs2),
                        .Vector2f32 => |*d| try applySIMDVectorf32(2, d, lhs2, rhs2),

                        .Vector4i32 => |*d| try applySIMDVector(i32, 4, d, lhs2.Vector4i32, rhs2.Vector4i32),
                        .Vector3i32 => |*d| try applySIMDVector(i32, 3, d, lhs2.Vector3i32, rhs2.Vector3i32),
                        .Vector2i32 => |*d| try applySIMDVector(i32, 2, d, lhs2.Vector2i32, rhs2.Vector2i32),

                        .Vector4u32 => |*d| try applySIMDVector(u32, 4, d, lhs2.Vector4u32, rhs2.Vector4u32),
                        .Vector3u32 => |*d| try applySIMDVector(u32, 3, d, lhs2.Vector3u32, rhs2.Vector3u32),
                        .Vector2u32 => |*d| try applySIMDVector(u32, 2, d, lhs2.Vector2u32, rhs2.Vector2u32),

                        else => return RuntimeError.InvalidValueType,
                    }
                }
            }.routines;

            switch (dst.*) {
                .Int, .Float => try applyScalar(lane_bits, dst, lhs, rhs),

                .Matrix => |dst_m| switch (Op) {
                    .MatrixTimesMatrix => switch (lane_bits) {
                        inline 16, 32, 64 => |bits_count| try applyMatrixTimesMatrixFloat(bits_count, dst_m, lhs, rhs),
                        else => return RuntimeError.UnsupportedSpirV,
                    },
                    .MatrixTimesScalar => {
                        for (dst_m, lhs.Matrix) |*dst_vec, *lhs_vec| {
                            try vectorRoutines(dst_vec, lhs_vec, rhs, lane_bits);
                        }
                    },
                    else => return RuntimeError.ToDo,
                },

                else => try vectorRoutines(dst, lhs, rhs, lane_bits),
            }
            if (comptime IsAtomic) {
                try copyValue(lhs, dst);
                try copyValue(dst, &lhs_save.?);
                try lhs.flushPtr(allocator);
            }

            try propagateDerivative(allocator, rt, target_type_word, target_type, dst_id, lhs_id, rhs_id, lhs, rhs);
        }

        fn opSingle(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
            const dst = try rt.results[try rt.it.next()].getValue();
            const val = try rt.results[try rt.it.next()].getValue();

            const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

            switch (dst.*) {
                .Int, .Float => try applyScalarSingle(lane_bits, dst, val),

                .Vector => |dst_vec| for (dst_vec, val.Vector) |*d_lane, *v_lane| {
                    try applyScalarSingle(lane_bits, d_lane, v_lane);
                },

                .Vector4f32 => |*d| try applySIMDVectorSingle(f32, 4, d, &val.Vector4f32),
                .Vector3f32 => |*d| try applySIMDVectorSingle(f32, 3, d, &val.Vector3f32),
                .Vector2f32 => |*d| try applySIMDVectorSingle(f32, 2, d, &val.Vector2f32),

                .Vector4i32 => |*d| try applySIMDVectorSingle(i32, 4, d, &val.Vector4i32),
                .Vector3i32 => |*d| try applySIMDVectorSingle(i32, 3, d, &val.Vector3i32),
                .Vector2i32 => |*d| try applySIMDVectorSingle(i32, 2, d, &val.Vector2i32),

                .Vector4u32 => |*d| try applySIMDVectorSingle(u32, 4, d, &val.Vector4u32),
                .Vector3u32 => |*d| try applySIMDVectorSingle(u32, 3, d, &val.Vector3u32),
                .Vector2u32 => |*d| try applySIMDVectorSingle(u32, 2, d, &val.Vector2u32),

                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn addDecoration(allocator: std.mem.Allocator, rt: *Runtime, target: SpvWord, decoration_type: spv.SpvDecoration, member: ?SpvWord) RuntimeError!void {
    var decoration = rt.results[target].decorations.addOne(allocator) catch return RuntimeError.OutOfMemory;
    decoration.rtype = decoration_type;
    decoration.literal_1 = 0;
    decoration.literal_2 = null;
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

fn setupAtomic(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.reflection_infos.has_atomics = true;
    try autoSetupConstant(allocator, word_count, rt);
}

fn setupAtomicStore(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.reflection_infos.has_atomics = true;
}

fn opArrayLength(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result type
    const id = try rt.it.next();
    const structure = try rt.results[try rt.it.next()].getValue();
    const member_index = try rt.it.next();

    const structure_value = switch (structure.*) {
        .Pointer => |p| switch (p.ptr) {
            .common => |value| value,
            else => return RuntimeError.InvalidValueType,
        },
        else => structure,
    };

    const length: u32 = switch (structure_value.*) {
        .Structure => |s| blk: {
            if (member_index >= s.values.len) return RuntimeError.OutOfBounds;
            break :blk switch (s.values[member_index]) {
                .RuntimeArray => |arr| @as(u32, @intCast(arr.getLen())),
                else => return RuntimeError.InvalidValueType,
            };
        },
        .RuntimeArray => |arr| @intCast(arr.getLen()),
        else => return RuntimeError.InvalidValueType,
    };

    try Value.writeLane(.UInt, 32, try rt.results[id].getValue(), 0, length);
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

    var new_value = try Value.initUnresolved(allocator, rt.results, var_type, false);
    errdefer new_value.deinit(allocator);

    if (rt.results[id].variant) |variant| {
        rt.results[id].variant = null;
        var old_variant = variant;
        switch (old_variant) {
            .AccessChain => |*a| {
                try a.value.flushPtr(allocator);
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
            .value = new_value,
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

        inline fn readWindow(dst_v: *Value, window: []const u8, matrix_stride: ?SpvWord, matrix_row_major: bool) RuntimeError!void {
            _ = if (matrix_stride) |stride| blk: {
                if (matrix_row_major and dst_v.isVector())
                    break :blk try dst_v.writeVectorWithStride(window, stride);
                break :blk try dst_v.writeWithMatrixLayout(window, stride, matrix_row_major);
            } else try dst_v.write(window);
        }
    };

    if (std.meta.activeTag(dst.*) == .Pointer) {
        const dst_ptr = dst.Pointer;
        switch (dst_ptr.ptr) {
            .common => |dst_val_ptr| {
                switch (src.*) {
                    .Pointer => |src_ptr| switch (src_ptr.ptr) {
                        .common => |src_val_ptr| {
                            if (src_ptr.uniform_slice_window) |window| {
                                try helpers.readWindow(dst_val_ptr, window, src_ptr.matrix_stride, src_ptr.matrix_row_major);
                            } else {
                                try copyValue(dst_val_ptr, src_val_ptr);
                            }
                        },
                        .f32_ptr,
                        .i32_ptr,
                        .u32_ptr,
                        => {
                            if (src_ptr.uniform_slice_window) |window| {
                                try helpers.readWindow(dst_val_ptr, window, src_ptr.matrix_stride, src_ptr.matrix_row_major);
                            } else {
                                dst_val_ptr.* = src.*;
                            }
                        },
                    },
                    else => try copyValue(dst_val_ptr, src),
                }
                if (dst_ptr.uniform_slice_window) |window| {
                    _ = if (dst_ptr.matrix_stride) |matrix_stride| blk: {
                        if (dst_ptr.matrix_row_major and dst_val_ptr.isVector())
                            break :blk try dst_val_ptr.readVectorWithStride(window, matrix_stride);
                        break :blk try dst_val_ptr.readWithMatrixLayout(window, matrix_stride, dst_ptr.matrix_row_major);
                    } else try dst_val_ptr.read(window);
                }
                return;
            },
            .f32_ptr => |dst_f32_ptr| {
                try helpers.writeF32(dst_f32_ptr, src);
                if (dst_ptr.uniform_slice_window) |window| {
                    if (window.len < @sizeOf(f32)) return RuntimeError.OutOfBounds;
                    @memcpy(window[0..@sizeOf(f32)], std.mem.asBytes(dst_f32_ptr));
                }
                return;
            },
            .i32_ptr => |dst_i32_ptr| {
                try helpers.writeI32(dst_i32_ptr, src);
                if (dst_ptr.uniform_slice_window) |window| {
                    if (window.len < @sizeOf(i32)) return RuntimeError.OutOfBounds;
                    @memcpy(window[0..@sizeOf(i32)], std.mem.asBytes(dst_i32_ptr));
                }
                return;
            },
            .u32_ptr => |dst_u32_ptr| {
                try helpers.writeU32(dst_u32_ptr, src);
                if (dst_ptr.uniform_slice_window) |window| {
                    if (window.len < @sizeOf(u32)) return RuntimeError.OutOfBounds;
                    @memcpy(window[0..@sizeOf(u32)], std.mem.asBytes(dst_u32_ptr));
                }
                return;
            },
        }
    }

    switch (src.*) {
        .Vector, .Matrix => |src_slice| {
            if (dst.* == .RuntimeArray) {
                const size = try src.getPlainMemorySize();
                if (size > dst.RuntimeArray.data.len) return RuntimeError.OutOfBounds;
                _ = try src.read(dst.RuntimeArray.data[0..size]);
                return;
            }
            const dst_slice = helpers.getDstSlice(dst) orelse return RuntimeError.InvalidSpirV;
            try helpers.copySlice(dst_slice, src_slice);
        },
        .Array => |a| {
            if (dst.* == .RuntimeArray) {
                const size = try src.getPlainMemorySize();
                if (size > dst.RuntimeArray.data.len) return RuntimeError.OutOfBounds;
                _ = try src.read(dst.RuntimeArray.data[0..size]);
                return;
            }
            const dst_slice = helpers.getDstSlice(dst) orelse return RuntimeError.InvalidSpirV;
            try helpers.copySlice(dst_slice, a.values);
        },
        .Structure => |s| {
            if (s.external_data) |src_data| {
                if (dst.* == .Structure) {
                    if (dst.Structure.external_data) |dst_data| {
                        if (src_data.len > dst_data.len) return RuntimeError.OutOfBounds;
                        @memcpy(dst_data[0..src_data.len], src_data);
                        _ = try dst.write(dst_data[0..src_data.len]);
                        return;
                    }
                }
            }
            if (dst.* == .RuntimeArray) {
                const size = try src.getPlainMemorySize();
                if (size > dst.RuntimeArray.data.len) return RuntimeError.OutOfBounds;
                _ = try src.read(dst.RuntimeArray.data[0..size]);
                return;
            }
            if (dst.* == .Structure) {
                @memcpy(@constCast(dst.Structure.offsets), s.offsets);
                @memcpy(@constCast(dst.Structure.matrix_strides), s.matrix_strides);
                @memcpy(@constCast(dst.Structure.row_major), s.row_major);
            }
            const dst_slice = helpers.getDstSlice(dst) orelse return RuntimeError.InvalidSpirV;
            try helpers.copySlice(dst_slice, s.values);
            if (dst.* == .Structure) {
                if (dst.Structure.external_data) |dst_data| {
                    _ = try dst.read(dst_data);
                }
            }
        },
        .Pointer => |ptr| switch (ptr.ptr) {
            .common => |src_val_ptr| {
                if (ptr.uniform_slice_window) |window| {
                    try helpers.readWindow(dst, window, ptr.matrix_stride, ptr.matrix_row_major);
                } else {
                    try copyValue(dst, src_val_ptr);
                }
            },
            .f32_ptr => |src_f32_ptr| {
                if (ptr.uniform_slice_window) |window| {
                    try helpers.readWindow(dst, window, ptr.matrix_stride, ptr.matrix_row_major);
                } else {
                    try helpers.readF32(dst, src_f32_ptr);
                }
            },
            .i32_ptr => |src_i32_ptr| {
                if (ptr.uniform_slice_window) |window| {
                    try helpers.readWindow(dst, window, ptr.matrix_stride, ptr.matrix_row_major);
                } else {
                    try helpers.readI32(dst, src_i32_ptr);
                }
            },
            .u32_ptr => |src_u32_ptr| {
                if (ptr.uniform_slice_window) |window| {
                    try helpers.readWindow(dst, window, ptr.matrix_stride, ptr.matrix_row_major);
                } else {
                    try helpers.readU32(dst, src_u32_ptr);
                }
            },
        },
        .RuntimeArray => |src_arr| switch (dst.*) {
            .Array => {
                const size = try dst.getPlainMemorySize();
                if (size > src_arr.data.len) return RuntimeError.OutOfBounds;
                _ = try dst.write(src_arr.data[0..size]);
            },
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

fn intValueToIndex(i: @TypeOf(@as(Value, undefined).Int)) RuntimeError!usize {
    return switch (i.bit_count) {
        8 => if (i.is_signed) std.math.cast(usize, i.value.sint8) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint8),
        16 => if (i.is_signed) std.math.cast(usize, i.value.sint16) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint16),
        32 => if (i.is_signed) std.math.cast(usize, i.value.sint32) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint32),
        64 => if (i.is_signed) std.math.cast(usize, i.value.sint64) orelse RuntimeError.OutOfBounds else std.math.cast(usize, i.value.uint64) orelse RuntimeError.OutOfBounds,
        else => RuntimeError.InvalidSpirV,
    };
}

fn opAccessChain(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();
    const base_id = try rt.it.next();

    const base = &rt.results[base_id];
    var value_ptr = try base.getValue();

    const index_count: usize = @intCast(word_count - 3);

    const indexes, const free_responsability = blk: {
        if (rt.results[id].variant) |variant| {
            rt.results[id].variant = null;
            var old_variant = variant;
            switch (old_variant) {
                .AccessChain => |*a| {
                    try a.value.flushPtr(allocator);
                    a.value.deinit(allocator);
                    if (a.indexes.len == index_count)
                        break :blk .{ a.indexes, false };
                    allocator.free(a.indexes);
                },
                .Constant => |*constant| {
                    constant.value.deinit(allocator);
                },
                .Variable => |*variable| {
                    variable.value.deinit(allocator);
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
                    const F32Pointer = struct {
                        ptr: *f32,
                        backing: ?*Value,
                        owns_backing: bool,
                    };
                    const I32Pointer = struct {
                        ptr: *i32,
                        backing: ?*Value,
                        owns_backing: bool,
                    };
                    const U32Pointer = struct {
                        ptr: *u32,
                        backing: ?*Value,
                        owns_backing: bool,
                    };

                    fn destroyBacking(gpa: std.mem.Allocator, backing: ?*Value, owns_backing: bool) void {
                        if (!owns_backing) return;
                        if (backing) |value| {
                            value.deinit(gpa);
                            gpa.destroy(value);
                        }
                    }

                    fn robustF32Pointer(gpa: std.mem.Allocator, ptr: ?*f32, window: ?[]u8, descriptor_backed: bool, backing: ?*Value, owns_backing: bool) RuntimeError!F32Pointer {
                        if (window != null or !descriptor_backed) return .{ .ptr = ptr orelse return RuntimeError.InvalidSpirV, .backing = backing, .owns_backing = owns_backing };

                        destroyBacking(gpa, backing, owns_backing);
                        const value = gpa.create(Value) catch return RuntimeError.OutOfMemory;
                        value.* = .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = 0 } } };
                        return .{ .ptr = &value.Float.value.float32, .backing = value, .owns_backing = true };
                    }

                    fn robustI32Pointer(gpa: std.mem.Allocator, ptr: ?*i32, window: ?[]u8, descriptor_backed: bool, backing: ?*Value, owns_backing: bool) RuntimeError!I32Pointer {
                        if (window != null or !descriptor_backed) return .{ .ptr = ptr orelse return RuntimeError.InvalidSpirV, .backing = backing, .owns_backing = owns_backing };

                        destroyBacking(gpa, backing, owns_backing);
                        const value = gpa.create(Value) catch return RuntimeError.OutOfMemory;
                        value.* = .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = 0 } } };
                        return .{ .ptr = &value.Int.value.sint32, .backing = value, .owns_backing = true };
                    }

                    fn robustU32Pointer(gpa: std.mem.Allocator, ptr: ?*u32, window: ?[]u8, descriptor_backed: bool, backing: ?*Value, owns_backing: bool) RuntimeError!U32Pointer {
                        if (window != null or !descriptor_backed) return .{ .ptr = ptr orelse return RuntimeError.InvalidSpirV, .backing = backing, .owns_backing = owns_backing };

                        destroyBacking(gpa, backing, owns_backing);
                        const value = gpa.create(Value) catch return RuntimeError.OutOfMemory;
                        value.* = .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = 0 } } };
                        return .{ .ptr = &value.Int.value.uint32, .backing = value, .owns_backing = true };
                    }

                    fn advanceWindow(window: ?[]u8, offset: usize) RuntimeError!?[]u8 {
                        if (window) |w| {
                            if (offset > w.len) return null;
                            return w[offset..];
                        }
                        return null;
                    }

                    fn advanceWindowSized(window: ?[]u8, offset: usize, size: usize) RuntimeError!?[]u8 {
                        if (window) |w| {
                            if (offset > w.len or size > w.len - offset) return null;
                            return w[offset .. offset + size];
                        }
                        return null;
                    }

                    fn laneOffset(matrix_stride: ?SpvWord, matrix_row_major: bool, lane_index: usize, lane_size: usize) usize {
                        if (matrix_row_major) {
                            if (matrix_stride) |stride| {
                                return lane_index * @as(usize, @intCast(stride));
                            }
                        }
                        return lane_index * lane_size;
                    }
                };

                var uniform_slice_window: ?[]u8 = null;
                var uniform_root_window: ?[]u8 = null;
                var uniform_window_offset: usize = 0;
                var uniform_backing_value: ?*Value = null;
                var owns_uniform_backing_value = false;
                var matrix_stride: ?SpvWord = null;
                var matrix_row_major = false;
                var descriptor_backed = false;

                if (index_count == 0 and std.meta.activeTag(value_ptr.*) == .Pointer) {
                    break :blk try value_ptr.dupe(allocator);
                }

                if (std.meta.activeTag(value_ptr.*) == .Pointer) {
                    const ptr = value_ptr.Pointer;
                    uniform_slice_window = ptr.uniform_slice_window;
                    uniform_root_window = ptr.uniform_root_window orelse ptr.uniform_slice_window;
                    uniform_window_offset = ptr.uniform_window_offset;
                    uniform_backing_value = ptr.uniform_backing_value;
                    owns_uniform_backing_value = false;
                    matrix_stride = ptr.matrix_stride;
                    matrix_row_major = ptr.matrix_row_major;
                    descriptor_backed = uniform_slice_window != null or uniform_backing_value != null;
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
                        .FunctionParameter => |p| p.value_ptr orelse return RuntimeError.InvalidSpirV,
                        else => return RuntimeError.InvalidSpirV,
                    };

                    switch (member_value.*) {
                        .Int => |i| {
                            if (std.meta.activeTag(value_ptr.*) == .Pointer) {
                                const ptr = value_ptr.Pointer;
                                uniform_slice_window = ptr.uniform_slice_window;
                                uniform_root_window = ptr.uniform_root_window orelse ptr.uniform_slice_window;
                                uniform_window_offset = ptr.uniform_window_offset;
                                uniform_backing_value = ptr.uniform_backing_value;
                                owns_uniform_backing_value = false;
                                matrix_stride = ptr.matrix_stride;
                                matrix_row_major = ptr.matrix_row_major;
                                descriptor_backed = uniform_slice_window != null or uniform_backing_value != null;
                                switch (ptr.ptr) {
                                    .common => |common| value_ptr = common,
                                    else => return RuntimeError.InvalidSpirV,
                                }
                            }

                            const component_index = try intValueToIndex(i);

                            switch (value_ptr.*) {
                                .Vector, .Matrix => |v| {
                                    if (component_index >= v.len) return RuntimeError.OutOfBounds;
                                    const is_matrix = std.meta.activeTag(value_ptr.*) == .Matrix;
                                    const offset = if (!is_matrix and matrix_stride != null and matrix_row_major)
                                        component_index * @as(usize, @intCast(matrix_stride.?))
                                    else if (is_matrix and matrix_stride != null and matrix_row_major) row_major_offset_blk: {
                                        const column = &v[component_index];
                                        const lane_count: usize = @intCast(try column.resolveLaneCount());
                                        if (lane_count == 0) return RuntimeError.OutOfBounds;
                                        break :row_major_offset_blk component_index * @divExact(try column.getPlainMemorySize(), lane_count);
                                    } else if (is_matrix and matrix_stride != null)
                                        component_index * @as(usize, @intCast(matrix_stride.?))
                                    else plain_offset_blk: {
                                        var plain_offset: usize = 0;
                                        for (v[0..component_index]) |*element| {
                                            plain_offset += try element.getPlainMemorySize();
                                        }
                                        break :plain_offset_blk plain_offset;
                                    };
                                    uniform_slice_window = try helpers.advanceWindow(uniform_slice_window, offset);
                                    uniform_window_offset += offset;
                                    value_ptr = &v[component_index];
                                    if (is_matrix and matrix_row_major) {
                                        // Keep stride for the selected column vector; its lanes are separated by MatrixStride.
                                    } else if (std.meta.activeTag(value_ptr.*) != .Matrix) {
                                        matrix_stride = null;
                                        matrix_row_major = false;
                                    }
                                },
                                .Array => |a| {
                                    if (component_index >= a.values.len)
                                        return RuntimeError.OutOfBounds;
                                    const element_offset = component_index * a.stride;
                                    uniform_slice_window = try helpers.advanceWindow(uniform_slice_window, element_offset);
                                    uniform_window_offset += element_offset;
                                    value_ptr = &a.values[component_index];
                                    switch (value_ptr.*) {
                                        .Array, .Matrix => {},
                                        else => {
                                            matrix_stride = null;
                                            matrix_row_major = false;
                                        },
                                    }
                                },
                                .Structure => |s| {
                                    if (component_index >= s.values.len) return RuntimeError.OutOfBounds;
                                    var end_offset: usize = 0;
                                    for (s.values[0..component_index], 0..) |*field, field_index| {
                                        const field_offset: usize = @intCast(s.offsets[field_index] orelse end_offset);
                                        end_offset = @max(end_offset, field_offset + try field.getPlainMemorySize());
                                    }
                                    const member_offset: usize = @intCast(s.offsets[component_index] orelse end_offset);

                                    if (uniform_slice_window != null) {
                                        descriptor_backed = true;
                                        uniform_slice_window = try helpers.advanceWindow(uniform_slice_window, member_offset);
                                        uniform_window_offset += member_offset;
                                    } else if (s.external_data) |data| {
                                        descriptor_backed = true;
                                        uniform_slice_window = try helpers.advanceWindow(data, member_offset);
                                        uniform_root_window = data;
                                        uniform_window_offset = member_offset;
                                    }

                                    value_ptr = &s.values[component_index];
                                    matrix_stride = s.matrix_strides[component_index];
                                    matrix_row_major = s.row_major[component_index];
                                    if (uniform_slice_window) |window| {
                                        if (value_ptr.* == .RuntimeArray) {
                                            value_ptr.RuntimeArray.data = window;
                                            if (matrix_stride) |stride| {
                                                value_ptr.RuntimeArray.matrix_stride = stride;
                                                value_ptr.RuntimeArray.row_major = matrix_row_major;
                                            }
                                        }
                                    }
                                },
                                .RuntimeArray => |*arr| {
                                    const backing = try arr.createRobustValueFromIndex(allocator, rt.results, component_index);
                                    errdefer {
                                        backing.deinit(allocator);
                                        allocator.destroy(backing);
                                    }

                                    if (owns_uniform_backing_value) if (uniform_backing_value) |old_backing| {
                                        old_backing.deinit(allocator);
                                        allocator.destroy(old_backing);
                                    };

                                    value_ptr = backing;
                                    uniform_backing_value = backing;
                                    owns_uniform_backing_value = true;
                                    descriptor_backed = true;
                                    if (arr.getRobustOffsetOfIndex(component_index)) |element_offset| {
                                        uniform_slice_window = arr.data[element_offset..];
                                        uniform_root_window = arr.data;
                                        uniform_window_offset = element_offset;
                                    } else {
                                        uniform_slice_window = null;
                                    }
                                    if (arr.matrix_stride) |stride| {
                                        matrix_stride = stride;
                                        matrix_row_major = arr.row_major;
                                    }
                                    switch (value_ptr.*) {
                                        .Array, .Matrix => {},
                                        else => {
                                            matrix_stride = null;
                                            matrix_row_major = false;
                                        },
                                    }
                                },
                                .Vector4f32 => |*v| switch (component_index) {
                                    inline 0...3 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(f32)), @sizeOf(f32));
                                        const ptr = try helpers.robustF32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .f32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustF32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .f32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                },
                                .Vector3f32 => |*v| switch (component_index) {
                                    inline 0...2 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(f32)), @sizeOf(f32));
                                        const ptr = try helpers.robustF32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .f32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustF32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .f32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                },
                                .Vector2f32 => |*v| switch (component_index) {
                                    inline 0...1 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(f32)), @sizeOf(f32));
                                        const ptr = try helpers.robustF32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .f32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustF32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .f32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                },
                                .Vector4i32 => |*v| switch (component_index) {
                                    inline 0...3 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(i32)), @sizeOf(i32));
                                        const ptr = try helpers.robustI32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .i32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustI32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .i32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                },
                                .Vector3i32 => |*v| switch (component_index) {
                                    inline 0...2 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(i32)), @sizeOf(i32));
                                        const ptr = try helpers.robustI32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .i32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustI32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .i32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                },
                                .Vector2i32 => |*v| switch (component_index) {
                                    inline 0...1 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(i32)), @sizeOf(i32));
                                        const ptr = try helpers.robustI32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .i32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustI32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .i32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                },
                                .Vector4u32 => |*v| switch (component_index) {
                                    inline 0...3 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(u32)), @sizeOf(u32));
                                        const ptr = try helpers.robustU32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .u32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustU32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .u32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                },
                                .Vector3u32 => |*v| switch (component_index) {
                                    inline 0...2 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(u32)), @sizeOf(u32));
                                        const ptr = try helpers.robustU32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .u32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustU32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .u32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                },
                                .Vector2u32 => |*v| switch (component_index) {
                                    inline 0...1 => |idx| {
                                        const lane_window = try helpers.advanceWindowSized(uniform_slice_window, helpers.laneOffset(matrix_stride, matrix_row_major, idx, @sizeOf(u32)), @sizeOf(u32));
                                        const ptr = try helpers.robustU32Pointer(allocator, &v[idx], lane_window, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .u32_ptr = ptr.ptr }, .uniform_slice_window = lane_window, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
                                    else => {
                                        const ptr = try helpers.robustU32Pointer(allocator, null, null, descriptor_backed, uniform_backing_value, owns_uniform_backing_value);
                                        break :blk .{ .Pointer = .{ .ptr = .{ .u32_ptr = ptr.ptr }, .uniform_slice_window = null, .uniform_backing_value = ptr.backing, .owns_uniform_backing_value = ptr.owns_backing, .matrix_stride = null } };
                                    },
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
                        .uniform_root_window = uniform_root_window,
                        .uniform_window_offset = uniform_window_offset,
                        .uniform_backing_value = uniform_backing_value,
                        .owns_uniform_backing_value = owns_uniform_backing_value,
                        .matrix_stride = matrix_stride,
                        .matrix_row_major = matrix_row_major,
                    },
                };
            },
        },
    };

    if (index_count == 1) {
        const base_derivative = rt.derivatives.get(base_id) orelse {
            rt.clearDerivative(allocator, id);
            return;
        };
        const index_value = switch ((try rt.results[indexes[0]].getVariant()).*) {
            .Constant => |c| &c.value,
            .Variable => |v| &v.value,
            .FunctionParameter => |p| p.value_ptr orelse return RuntimeError.InvalidSpirV,
            else => return RuntimeError.InvalidSpirV,
        };
        const lane_index: usize = switch (index_value.*) {
            .Int => |int| @intCast(int.value.uint32),
            else => return RuntimeError.InvalidSpirV,
        };
        const base_value = try base.getValue();
        const base_lane_count = try base_value.resolveLaneCount();
        const target_type_word = switch ((try rt.results[var_type].getVariant()).*) {
            .Type => |t| switch (t) {
                .Pointer => |p| p.target,
                else => return RuntimeError.InvalidSpirV,
            },
            else => return RuntimeError.InvalidSpirV,
        };
        const target_type = (try rt.results[target_type_word].getVariant()).Type;
        const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);

        var dx = try Value.init(allocator, rt.results, target_type_word, false);
        defer dx.deinit(allocator);
        var dy = try Value.init(allocator, rt.results, target_type_word, false);
        defer dy.deinit(allocator);

        const index_derivative = rt.derivatives.get(indexes[0]);
        switch (lane_bits) {
            inline 16, 32, 64 => |bits| {
                const writeAxis = struct {
                    fn run(
                        comptime primitive: PrimitiveType,
                        dst: *Value,
                        base_v: *const Value,
                        derivative_axis: *const Value,
                        index_derivative_axis: ?*const Value,
                        center_lane_index: usize,
                        source_lane_count: usize,
                    ) RuntimeError!void {
                        const LaneT = Value.getPrimitiveFieldType(primitive, bits);
                        const index_delta = if (index_derivative_axis) |idx_deriv|
                            try readIndexDelta(idx_deriv)
                        else
                            0;
                        const shifted_lane_index_signed = @as(isize, @intCast(center_lane_index)) + index_delta;
                        if (shifted_lane_index_signed < 0 or shifted_lane_index_signed >= @as(isize, @intCast(source_lane_count))) {
                            try Value.writeLane(primitive, bits, dst, 0, @as(LaneT, 0));
                            return;
                        }
                        const shifted_lane_index: usize = @intCast(shifted_lane_index_signed);
                        const shifted_base = try Value.readLane(primitive, bits, base_v, shifted_lane_index);
                        const shifted_derivative = try Value.readLane(primitive, bits, derivative_axis, shifted_lane_index);
                        const shifted = addFiniteDifferenceDelta(LaneT, shifted_base, shifted_derivative);
                        const center = try Value.readLane(primitive, bits, base_v, center_lane_index);
                        try Value.writeLane(primitive, bits, dst, 0, finiteDifferenceDelta(LaneT, shifted, center));
                    }
                }.run;

                switch (try base_value.resolvePrimitiveType()) {
                    inline .Float, .SInt, .UInt => |primitive| {
                        try writeAxis(primitive, &dx, base_value, &base_derivative.dx, if (index_derivative) |d| &d.dx else null, lane_index, base_lane_count);
                        try writeAxis(primitive, &dy, base_value, &base_derivative.dy, if (index_derivative) |d| &d.dy else null, lane_index, base_lane_count);
                    },
                    else => return RuntimeError.InvalidValueType,
                }
            },
            else => return RuntimeError.UnsupportedSpirV,
        }

        try rt.setDerivative(allocator, id, &dx, &dy);
    } else {
        rt.clearDerivative(allocator, id);
    }
}
fn opAtomicStore(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const ptr_id = try rt.it.next();
    _ = rt.it.skip(); // scope
    _ = rt.it.skip(); // semantic
    const val_id = try rt.it.next();
    if (rt.helper_invocation)
        return;
    try copyValue(try rt.results[ptr_id].getValue(), try rt.results[val_id].getValue());
}

fn opBitcast(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const to_id = try rt.it.next();
    const to_value = try rt.results[to_id].getValue();
    const from_id = try rt.it.next();
    const from_value = try rt.results[from_id].getValue();

    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();
    const local_allocator = arena.allocator();

    const size = try to_value.getPlainMemorySize();
    const bytes = local_allocator.alloc(u8, size) catch return RuntimeError.OutOfMemory;
    _ = try from_value.read(bytes);
    _ = try to_value.write(bytes);
    try rt.copyDerivative(allocator, to_id, from_id);
}

fn opBranch(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    try rt.snapshotPhiValues(allocator);
    rt.previous_label = rt.current_label;
    _ = rt.it.jumpToSourceLocation(switch ((try rt.results[id].getVariant()).*) {
        .Label => |l| l.source_location,
        else => return RuntimeError.InvalidSpirV,
    });
}

fn opBranchConditional(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const cond_value = try rt.results[try rt.it.next()].getValue();
    const true_branch = switch ((try rt.results[try rt.it.next()].getVariant()).*) {
        .Label => |l| l.source_location,
        else => return RuntimeError.InvalidSpirV,
    };
    const false_branch = switch ((try rt.results[try rt.it.next()].getVariant()).*) {
        .Label => |l| l.source_location,
        else => return RuntimeError.InvalidSpirV,
    };
    try rt.snapshotPhiValues(allocator);
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

fn opControlBarrierSetup(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.reflection_infos.has_control_barriers = true;
}

fn opControlBarrier(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip(); // execution scope
    _ = rt.it.skip(); // memory scope
    _ = rt.it.skip(); // memory semantics
    return RuntimeError.Barrier;
}

fn opCompositeConstruct(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    @setEvalBranchQuota(10_000);
    const target_type_word = try rt.it.next();
    const target_type = (try rt.results[target_type_word].getVariant()).Type;
    const id = try rt.it.next();

    const index_count: usize = @intCast(word_count - 2);
    const operand_ids = allocator.alloc(SpvWord, index_count) catch return RuntimeError.OutOfMemory;
    defer allocator.free(operand_ids);
    for (operand_ids) |*operand_id| {
        operand_id.* = try rt.it.next();
    }

    try rt.refreshResultValueLayout(id);
    const value = &(try rt.results[id].getVariant()).Constant.value;
    if (value.getCompositeDataOrNull()) |target| {
        for (target[0..index_count], operand_ids) |*elem, operand_id| {
            try copyValue(elem, try rt.results[operand_id].getValue());
        }
        rt.clearDerivative(allocator, id);
        return;
    }

    const primitive_type: PrimitiveType = switch (target_type) {
        .Float, .Vector, .Vector2f32, .Vector3f32, .Vector4f32 => .Float,
        .Int, .Vector2i32, .Vector3i32, .Vector4i32 => .SInt,
        .Vector2u32, .Vector3u32, .Vector4u32 => .UInt,
        else => return RuntimeError.InvalidValueType,
    };

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const target_lane_count = try value.resolveLaneCount();

    var dx: ?Value = try Value.init(allocator, rt.results, target_type_word, false);
    defer if (dx) |*dx_value| dx_value.deinit(allocator);
    var dy: ?Value = try Value.init(allocator, rt.results, target_type_word, false);
    defer if (dy) |*dy_value| dy_value.deinit(allocator);

    var has_derivative = false;
    var target_lane_index: usize = 0;
    for (operand_ids) |operand_id| {
        const operand_value = try rt.results[operand_id].getValue();
        const operand_lane_count = try operand_value.resolveLaneCount();
        const derivative = rt.derivatives.get(operand_id);

        for (0..operand_lane_count) |operand_lane_index| {
            if (target_lane_index >= target_lane_count) return RuntimeError.InvalidSpirV;

            switch (primitive_type) {
                .Float => switch (lane_bits) {
                    inline 16, 32, 64 => |bits| {
                        {
                            const lane = try Value.readLane(.Float, bits, operand_value, operand_lane_index);
                            try Value.writeLane(.Float, bits, value, target_lane_index, lane);

                            const FloatT = Value.getPrimitiveFieldType(.Float, bits);
                            const dx_lane: FloatT = if (derivative) |d| blk: {
                                has_derivative = true;
                                break :blk try Value.readLane(.Float, bits, &d.dx, operand_lane_index);
                            } else 0;
                            const dy_lane: FloatT = if (derivative) |d| blk: {
                                has_derivative = true;
                                break :blk try Value.readLane(.Float, bits, &d.dy, operand_lane_index);
                            } else 0;
                            try Value.writeLane(.Float, bits, &dx.?, target_lane_index, dx_lane);
                            try Value.writeLane(.Float, bits, &dy.?, target_lane_index, dy_lane);
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .SInt => switch (lane_bits) {
                    inline 8, 16, 32, 64 => |bits| {
                        {
                            const lane = try Value.readLane(.SInt, bits, operand_value, operand_lane_index);
                            try Value.writeLane(.SInt, bits, value, target_lane_index, lane);

                            const IntT = Value.getPrimitiveFieldType(.SInt, bits);
                            const dx_lane: IntT = if (derivative) |d| blk: {
                                has_derivative = true;
                                break :blk try Value.readLane(.SInt, bits, &d.dx, operand_lane_index);
                            } else 0;
                            const dy_lane: IntT = if (derivative) |d| blk: {
                                has_derivative = true;
                                break :blk try Value.readLane(.SInt, bits, &d.dy, operand_lane_index);
                            } else 0;
                            try Value.writeLane(.SInt, bits, &dx.?, target_lane_index, dx_lane);
                            try Value.writeLane(.SInt, bits, &dy.?, target_lane_index, dy_lane);
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                .UInt => switch (lane_bits) {
                    inline 8, 16, 32, 64 => |bits| {
                        {
                            const lane = try Value.readLane(.UInt, bits, operand_value, operand_lane_index);
                            try Value.writeLane(.UInt, bits, value, target_lane_index, lane);

                            const IntT = Value.getPrimitiveFieldType(.UInt, bits);
                            const dx_lane: IntT = if (derivative) |d| blk: {
                                has_derivative = true;
                                break :blk try Value.readLane(.UInt, bits, &d.dx, operand_lane_index);
                            } else 0;
                            const dy_lane: IntT = if (derivative) |d| blk: {
                                has_derivative = true;
                                break :blk try Value.readLane(.UInt, bits, &d.dy, operand_lane_index);
                            } else 0;
                            try Value.writeLane(.UInt, bits, &dx.?, target_lane_index, dx_lane);
                            try Value.writeLane(.UInt, bits, &dy.?, target_lane_index, dy_lane);
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                },
                else => return RuntimeError.InvalidValueType,
            }

            target_lane_index += 1;
        }
    }

    if (target_lane_index != target_lane_count) return RuntimeError.InvalidSpirV;

    if (has_derivative) {
        try rt.setDerivative(allocator, id, &dx.?, &dy.?);
    } else {
        rt.clearDerivative(allocator, id);
    }
}

fn extractCompositeValue(allocator: std.mem.Allocator, results: []Result, value: *const Value, member_ids: []const SpvWord) RuntimeError!Value {
    var composite = value.*;
    for (member_ids) |member_id_word| {
        const member_id: usize = @intCast(member_id_word);
        if (composite.getCompositeDataOrNull()) |v| {
            composite = v[member_id];
            continue;
        }
        switch (composite) {
            .RuntimeArray => |arr| composite = try arr.createLocalValueFromIndex(allocator, results, member_id_word),
            .Vector4f32 => |v| return .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (member_id) {
                inline 0...3 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            .Vector3f32 => |v| return .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (member_id) {
                inline 0...2 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            .Vector2f32 => |v| return .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (member_id) {
                inline 0...1 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            .Vector4i32 => |v| return .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (member_id) {
                inline 0...3 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            .Vector3i32 => |v| return .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (member_id) {
                inline 0...2 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            .Vector2i32 => |v| return .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (member_id) {
                inline 0...1 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            .Vector4u32 => |v| return .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (member_id) {
                inline 0...3 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            .Vector3u32 => |v| return .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (member_id) {
                inline 0...2 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            .Vector2u32 => |v| return .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (member_id) {
                inline 0...1 => |idx| v[idx],
                else => return RuntimeError.OutOfBounds,
            } } } },
            else => return RuntimeError.InvalidValueType,
        }
    }
    return composite.dupe(allocator);
}

fn opCompositeExtract(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const res_type = try rt.it.next();
    const id = try rt.it.next();
    const composite_id = try rt.it.next();
    const index_count: usize = @intCast(word_count - 3);

    const member_ids = allocator.alloc(SpvWord, index_count) catch return RuntimeError.OutOfMemory;
    defer allocator.free(member_ids);
    for (member_ids) |*member_id| {
        member_id.* = try rt.it.next();
    }

    const value = try extractCompositeValue(allocator, rt.results, try rt.results[composite_id].getValue(), member_ids);
    rt.results[id].variant = .{
        .Constant = .{
            .type_word = res_type,
            .type = switch ((try rt.results[res_type].getVariant()).*) {
                .Type => |t| @as(Result.Type, t),
                else => return RuntimeError.InvalidSpirV,
            },
            .value = value,
        },
    };

    if (rt.derivatives.get(composite_id)) |derivative| {
        var dx = try extractCompositeValue(allocator, rt.results, &derivative.dx, member_ids);
        defer dx.deinit(allocator);
        var dy = try extractCompositeValue(allocator, rt.results, &derivative.dy, member_ids);
        defer dy.deinit(allocator);
        try rt.setDerivative(allocator, id, &dx, &dy);
    } else {
        rt.clearDerivative(allocator, id);
    }
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

                    var elem_value = try arr.createLocalValueFromIndex(alloc, results, index);
                    defer elem_value.deinit(alloc);
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

fn opConstantFalse(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    switch (target.variant.?.Constant.value) {
        .Bool => |*b| b.* = false,
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opConstantTrue(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    switch (target.variant.?.Constant.value) {
        .Bool => |*b| b.* = true,
        else => return RuntimeError.InvalidSpirV,
    }
}

fn zeroValue(value: *Value) RuntimeError!void {
    switch (value.*) {
        .Void => {},
        .Bool => |*b| b.* = false,
        .Int => |*i| i.value.uint64 = 0,
        .Float => |*f| f.value.float64 = 0,
        .Vector => |lanes| for (lanes) |*lane| try zeroValue(lane),
        .Vector4f32 => |*v| v.* = .{ 0, 0, 0, 0 },
        .Vector3f32 => |*v| v.* = .{ 0, 0, 0 },
        .Vector2f32 => |*v| v.* = .{ 0, 0 },
        .Vector4i32 => |*v| v.* = .{ 0, 0, 0, 0 },
        .Vector3i32 => |*v| v.* = .{ 0, 0, 0 },
        .Vector2i32 => |*v| v.* = .{ 0, 0 },
        .Vector4u32 => |*v| v.* = .{ 0, 0, 0, 0 },
        .Vector3u32 => |*v| v.* = .{ 0, 0, 0 },
        .Vector2u32 => |*v| v.* = .{ 0, 0 },
        .Matrix => |columns| for (columns) |*column| try zeroValue(column),
        .Array => |*a| for (a.values) |*element| try zeroValue(element),
        .RuntimeArray => |*arr| @memset(arr.data, 0),
        .Structure => |*s| for (s.values) |*field| try zeroValue(field),
        .Image, .Sampler, .SampledImage, .Pointer => {},
        .Function => unreachable,
    }
}

fn opConstantNull(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    try zeroValue(&target.variant.?.Constant.value);
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

fn opUnreachable(_: std.mem.Allocator, _: SpvWord, _: *Runtime) RuntimeError!void {
    return RuntimeError.Unreachable;
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
                _ = try (try result.getValue()).write(data);
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
                _ = try (try target.getValue()).write(data);
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
                _ = try (try target.getValue()).write(data);
            }
        }
    }
}

fn opSwitch(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    if (word_count < 2)
        return RuntimeError.InvalidSpirV;

    const selector = try rt.results[try rt.it.next()].getValue();
    const default_target = try rt.it.next();

    const SelectorData = struct {
        value: u64,
        literal_width: SpvWord,
    };
    const selector_data: SelectorData = switch (selector.*) {
        .Int => |i| switch (i.bit_count) {
            8 => .{ .value = @as(u64, i.value.uint8), .literal_width = @as(SpvWord, 1) },
            16 => .{ .value = @as(u64, i.value.uint16), .literal_width = @as(SpvWord, 1) },
            32 => .{ .value = @as(u64, i.value.uint32), .literal_width = @as(SpvWord, 1) },
            64 => .{ .value = i.value.uint64, .literal_width = @as(SpvWord, 2) },
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidValueType,
    };
    const selector_value = selector_data.value;
    const literal_width = selector_data.literal_width;

    var target = default_target;
    var remaining = word_count - 2;
    while (remaining != 0) {
        if (remaining < literal_width + 1)
            return RuntimeError.InvalidSpirV;

        const literal = if (literal_width == 2) blk: {
            const low = @as(u64, try rt.it.next());
            const high = @as(u64, try rt.it.next());
            break :blk (high << 32) | low;
        } else try rt.it.next();
        const literal_target = try rt.it.next();

        if (literal == selector_value)
            target = literal_target;

        remaining -= literal_width + 1;
    }

    rt.previous_label = rt.current_label;
    _ = rt.it.jumpToSourceLocation(switch ((try rt.results[target].getVariant()).*) {
        .Label => |l| l.source_location,
        else => return RuntimeError.InvalidSpirV,
    });
}

fn opSpecConstantOp(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    if (word_count < 3)
        return RuntimeError.InvalidSpirV;

    const helpers = struct {
        fn readUInt(value: *const Value) RuntimeError!u64 {
            return switch (value.*) {
                .Int => |i| switch (i.bit_count) {
                    8 => i.value.uint8,
                    16 => i.value.uint16,
                    32 => i.value.uint32,
                    64 => i.value.uint64,
                    else => return RuntimeError.InvalidSpirV,
                },
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn readSInt(value: *const Value) RuntimeError!i64 {
            return switch (value.*) {
                .Int => |i| switch (i.bit_count) {
                    8 => i.value.sint8,
                    16 => i.value.sint16,
                    32 => i.value.sint32,
                    64 => i.value.sint64,
                    else => return RuntimeError.InvalidSpirV,
                },
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn readBool(value: *const Value) RuntimeError!bool {
            return switch (value.*) {
                .Bool => |b| b,
                else => return RuntimeError.InvalidValueType,
            };
        }

        fn writeUInt(dst: *Value, raw: u64) RuntimeError!void {
            switch (dst.*) {
                .Int => |*i| switch (i.bit_count) {
                    8 => i.value.uint8 = @truncate(raw),
                    16 => i.value.uint16 = @truncate(raw),
                    32 => i.value.uint32 = @truncate(raw),
                    64 => i.value.uint64 = raw,
                    else => return RuntimeError.InvalidSpirV,
                },
                else => return RuntimeError.InvalidValueType,
            }
        }

        fn signedBinary(lhs_u: u64, rhs_u: u64, bit_count: usize, comptime op: enum { div, rem, mod }) RuntimeError!u64 {
            if (rhs_u == 0) return 0;

            return switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| blk: {
                    const SInt = std.meta.Int(.signed, bits);
                    const UInt = std.meta.Int(.unsigned, bits);
                    const lhs: SInt = @bitCast(@as(UInt, @truncate(lhs_u)));
                    const rhs: SInt = @bitCast(@as(UInt, @truncate(rhs_u)));

                    if (lhs == std.math.minInt(SInt) and rhs == -1)
                        break :blk @as(u64, @as(UInt, @bitCast(lhs)));

                    const result = switch (op) {
                        .div => @divTrunc(lhs, rhs),
                        .rem => @rem(lhs, rhs),
                        .mod => @mod(lhs, rhs),
                    };
                    break :blk @as(u64, @as(UInt, @bitCast(result)));
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn convertLane(
            comptime from_kind: PrimitiveType,
            comptime to_kind: PrimitiveType,
            comptime to_bits: SpvWord,
            from_bits: SpvWord,
            dst: *Value,
            src: *const Value,
            lane_index: usize,
        ) RuntimeError!void {
            const ToT = Value.getPrimitiveFieldType(to_kind, to_bits);
            switch (from_bits) {
                inline 8, 16, 32, 64 => |bits| {
                    if (bits == 8 and from_kind == .Float) return RuntimeError.InvalidSpirV;
                    const from = try Value.readLane(from_kind, bits, src, lane_index);
                    try Value.writeLane(to_kind, to_bits, dst, lane_index, std.math.lossyCast(ToT, from));
                },
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn convertValue(
            comptime from_kind: PrimitiveType,
            comptime to_kind: PrimitiveType,
            target_type: Result.TypeData,
            runtime: *Runtime,
            dst: *Value,
            src_result: *Result,
        ) RuntimeError!void {
            const src_type_word = try src_result.getValueTypeWord();
            const src_value = try src_result.getValue();
            const from_bits = try Result.resolveLaneBitWidth((try runtime.results[src_type_word].getVariant()).Type, runtime);
            const to_bits = try Result.resolveLaneBitWidth(target_type, runtime);

            const dst_lane_count = try dst.resolveLaneCount();
            const src_lane_count = try src_value.resolveLaneCount();
            if (dst_lane_count != src_lane_count) return RuntimeError.InvalidSpirV;

            for (0..dst_lane_count) |lane_index| {
                switch (to_bits) {
                    inline 8, 16, 32, 64 => |bits| {
                        if (comptime to_kind == .Float and bits == 8) {
                            return RuntimeError.InvalidSpirV;
                        } else {
                            try convertLane(from_kind, to_kind, bits, from_bits, dst, src_value, lane_index);
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                }
            }
        }

        fn shiftLeftLogical(value: u64, amount: u64, bit_count: usize) RuntimeError!u64 {
            return switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| blk: {
                    if (amount >= bits) break :blk 0;

                    const UInt = std.meta.Int(.unsigned, bits);
                    const shift: std.math.Log2Int(UInt) = @intCast(amount);
                    const result = @as(UInt, @truncate(value)) << shift;
                    break :blk @as(u64, result);
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn shiftRightLogical(value: u64, amount: u64, bit_count: usize) RuntimeError!u64 {
            return switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| blk: {
                    if (amount >= bits) break :blk 0;

                    const UInt = std.meta.Int(.unsigned, bits);
                    const shift: std.math.Log2Int(UInt) = @intCast(amount);
                    const result = @as(UInt, @truncate(value)) >> shift;
                    break :blk @as(u64, result);
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn shiftRightArithmetic(value: *const Value, amount: u64, bit_count: usize) RuntimeError!u64 {
            return switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| blk: {
                    const SInt = std.meta.Int(.signed, bits);
                    const UInt = std.meta.Int(.unsigned, bits);
                    const lhs: SInt = @bitCast(@as(UInt, @truncate(try readUInt(value))));
                    if (amount >= bits) {
                        break :blk @as(u64, @as(UInt, @bitCast(if (lhs < 0) @as(SInt, -1) else @as(SInt, 0))));
                    }

                    const shift: std.math.Log2Int(SInt) = @intCast(amount);
                    break :blk @as(u64, @as(UInt, @bitCast(lhs >> shift)));
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn bitNot(value: u64, bit_count: usize) RuntimeError!u64 {
            return switch (bit_count) {
                inline 8, 16, 32, 64 => |bits| blk: {
                    const UInt = std.meta.Int(.unsigned, bits);
                    break :blk @as(u64, ~@as(UInt, @truncate(value)));
                },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn readVectorLane(alloc: std.mem.Allocator, src: *const Value, lane_index: usize) RuntimeError!Value {
            if (src.getCompositeDataOrNull()) |lanes| {
                if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
                return lanes[lane_index].dupe(alloc);
            }

            return switch (try src.resolvePrimitiveType()) {
                .Float => .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = try Value.readLane(.Float, 32, src, lane_index) } } },
                .SInt => .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = try Value.readLane(.SInt, 32, src, lane_index) } } },
                .UInt => .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = try Value.readLane(.UInt, 32, src, lane_index) } } },
                else => return RuntimeError.InvalidSpirV,
            };
        }

        fn writeVectorLane(dst: *Value, lane_index: usize, lane: *const Value) RuntimeError!void {
            if (dst.getCompositeDataOrNull()) |lanes| {
                if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
                try copyValue(&lanes[lane_index], lane);
                return;
            }

            switch (try dst.resolvePrimitiveType()) {
                .Float => try Value.writeLane(.Float, 32, dst, lane_index, try Value.readLane(.Float, 32, lane, 0)),
                .SInt => try Value.writeLane(.SInt, 32, dst, lane_index, try Value.readLane(.SInt, 32, lane, 0)),
                .UInt => try Value.writeLane(.UInt, 32, dst, lane_index, try Value.readLane(.UInt, 32, lane, 0)),
                else => return RuntimeError.InvalidSpirV,
            }
        }

        fn insertAt(current: *Value, object_value: *const Value, indices: []const SpvWord) RuntimeError!void {
            if (indices.len == 0) {
                try copyValue(current, object_value);
                return;
            }

            const index = indices[0];
            if (current.getCompositeDataOrNull()) |children| {
                if (index >= children.len) return RuntimeError.OutOfBounds;
                return insertAt(&children[index], object_value, indices[1..]);
            }

            if (indices.len != 1) return RuntimeError.OutOfBounds;
            try writeVectorLane(current, index, object_value);
        }

        fn extractAt(alloc: std.mem.Allocator, composite: *const Value, indices: []const SpvWord) RuntimeError!Value {
            if (indices.len == 0) return composite.dupe(alloc);

            const index = indices[0];
            if (composite.getCompositeDataOrNull()) |children| {
                if (index >= children.len) return RuntimeError.OutOfBounds;
                return extractAt(alloc, &children[index], indices[1..]);
            }

            if (indices.len != 1) return RuntimeError.OutOfBounds;
            return readVectorLane(alloc, composite, index);
        }

        fn readIndices(rt_iter: anytype, op_word_count: SpvWord, base_words: SpvWord) RuntimeError![16]SpvWord {
            var indices: [16]SpvWord = undefined;
            const index_count: usize = @intCast(op_word_count - base_words);
            if (index_count > indices.len) return RuntimeError.OutOfBounds;
            for (indices[0..index_count]) |*index| index.* = try rt_iter.next();
            return indices;
        }

        fn specCompositeInsert(rt_iter: anytype, op_word_count: SpvWord, target_value: *Value, object_value: *const Value, composite: *const Value) RuntimeError!void {
            try copyValue(target_value, composite);

            const indices = try readIndices(rt_iter, op_word_count, 5);
            const index_count: usize = @intCast(op_word_count - 5);
            try insertAt(target_value, object_value, indices[0..index_count]);
        }

        fn specCompositeExtract(alloc: std.mem.Allocator, rt_iter: anytype, op_word_count: SpvWord, target_value: *Value, composite: *const Value) RuntimeError!void {
            const indices = try readIndices(rt_iter, op_word_count, 4);
            const index_count: usize = @intCast(op_word_count - 4);
            var extracted = try extractAt(alloc, composite, indices[0..index_count]);
            defer extracted.deinit(alloc);
            try copyValue(target_value, &extracted);
        }

        fn specVectorShuffle(alloc: std.mem.Allocator, rt_iter: anytype, target_value: *Value, vector_1: *const Value, vector_2: *const Value) RuntimeError!void {
            const vector_1_lanes: usize = @intCast(try vector_1.resolveLaneCount());
            const dst_lanes: usize = @intCast(try target_value.resolveLaneCount());

            for (0..dst_lanes) |lane_index| {
                const component = try rt_iter.next();
                if (component == 0xFFFFFFFF) continue;

                const source = if (component < vector_1_lanes) vector_1 else vector_2;
                const source_lane: usize = @intCast(if (component < vector_1_lanes) component else component - vector_1_lanes);
                var lane = try readVectorLane(alloc, source, source_lane);
                defer lane.deinit(alloc);
                try writeVectorLane(target_value, lane_index, &lane);
            }
        }
    };

    const target = try setupConstant(allocator, rt);
    const inner_op = try rt.it.nextAs(spv.SpvOp);
    const target_value = try target.getValue();
    const target_type = switch ((try rt.results[target.variant.?.Constant.type_word].getVariant()).*) {
        .Type => |t| t,
        else => return RuntimeError.InvalidSpirV,
    };

    switch (target_value.*) {
        .Int => |dst| {
            const bit_count = dst.bit_count;

            const result = switch (inner_op) {
                .Not, .SNegate => blk: {
                    if (word_count != 4)
                        return RuntimeError.InvalidSpirV;

                    const operand = try rt.results[try rt.it.next()].getValue();
                    const operand_u = try helpers.readUInt(operand);
                    break :blk switch (inner_op) {
                        .Not => try helpers.bitNot(operand_u, bit_count),
                        .SNegate => @subWithOverflow(@as(u64, 0), operand_u)[0],
                        else => unreachable,
                    };
                },
                .Select => blk: {
                    if (word_count != 6)
                        return RuntimeError.InvalidSpirV;

                    const condition = try rt.results[try rt.it.next()].getValue();
                    const true_value = try rt.results[try rt.it.next()].getValue();
                    const false_value = try rt.results[try rt.it.next()].getValue();
                    break :blk try helpers.readUInt(if (try helpers.readBool(condition)) true_value else false_value);
                },
                .SConvert => blk: {
                    if (word_count != 4)
                        return RuntimeError.InvalidSpirV;

                    try helpers.convertValue(.SInt, .SInt, target_type, rt, target_value, &rt.results[try rt.it.next()]);
                    break :blk try helpers.readUInt(target_value);
                },
                .UConvert => blk: {
                    if (word_count != 4)
                        return RuntimeError.InvalidSpirV;

                    try helpers.convertValue(.UInt, .UInt, target_type, rt, target_value, &rt.results[try rt.it.next()]);
                    break :blk try helpers.readUInt(target_value);
                },
                .CompositeExtract => blk: {
                    const composite = try rt.results[try rt.it.next()].getValue();
                    try helpers.specCompositeExtract(allocator, &rt.it, word_count, target_value, composite);
                    break :blk try helpers.readUInt(target_value);
                },
                else => blk: {
                    if (word_count != 5)
                        return RuntimeError.InvalidSpirV;

                    const lhs_value = try rt.results[try rt.it.next()].getValue();
                    const rhs_value = try rt.results[try rt.it.next()].getValue();
                    const lhs_u = try helpers.readUInt(lhs_value);
                    const rhs_u = try helpers.readUInt(rhs_value);

                    break :blk switch (inner_op) {
                        .IAdd => @addWithOverflow(lhs_u, rhs_u)[0],
                        .ISub => @subWithOverflow(lhs_u, rhs_u)[0],
                        .IMul => @mulWithOverflow(lhs_u, rhs_u)[0],

                        .UDiv => if (rhs_u != 0) @divTrunc(lhs_u, rhs_u) else 0,
                        .UMod => if (rhs_u != 0) @mod(lhs_u, rhs_u) else 0,
                        .SDiv => try helpers.signedBinary(lhs_u, rhs_u, bit_count, .div),
                        .SRem => try helpers.signedBinary(lhs_u, rhs_u, bit_count, .rem),
                        .SMod => try helpers.signedBinary(lhs_u, rhs_u, bit_count, .mod),

                        .BitwiseAnd => lhs_u & rhs_u,
                        .BitwiseOr => lhs_u | rhs_u,
                        .BitwiseXor => lhs_u ^ rhs_u,
                        .ShiftLeftLogical => try helpers.shiftLeftLogical(lhs_u, rhs_u, bit_count),
                        .ShiftRightLogical => try helpers.shiftRightLogical(lhs_u, rhs_u, bit_count),
                        .ShiftRightArithmetic => try helpers.shiftRightArithmetic(lhs_value, rhs_u, bit_count),

                        else => return RuntimeError.UnsupportedSpirV,
                    };
                },
            };

            try helpers.writeUInt(target_value, result);
        },
        .Array, .Matrix, .Structure => {
            switch (inner_op) {
                .CompositeInsert => {
                    const object = try rt.results[try rt.it.next()].getValue();
                    const composite = try rt.results[try rt.it.next()].getValue();
                    try helpers.specCompositeInsert(&rt.it, word_count, target_value, object, composite);
                },
                else => return RuntimeError.UnsupportedSpirV,
            }
        },
        .Bool => |*dst| {
            const result = switch (inner_op) {
                .LogicalNot => blk: {
                    if (word_count != 4)
                        return RuntimeError.InvalidSpirV;

                    const operand = try rt.results[try rt.it.next()].getValue();
                    break :blk !(try helpers.readBool(operand));
                },
                else => blk: {
                    if (word_count != 5)
                        return RuntimeError.InvalidSpirV;

                    const lhs_value = try rt.results[try rt.it.next()].getValue();
                    const rhs_value = try rt.results[try rt.it.next()].getValue();

                    break :blk switch (inner_op) {
                        .IEqual => (try helpers.readUInt(lhs_value)) == (try helpers.readUInt(rhs_value)),
                        .INotEqual => (try helpers.readUInt(lhs_value)) != (try helpers.readUInt(rhs_value)),
                        .UGreaterThan => (try helpers.readUInt(lhs_value)) > (try helpers.readUInt(rhs_value)),
                        .UGreaterThanEqual => (try helpers.readUInt(lhs_value)) >= (try helpers.readUInt(rhs_value)),
                        .ULessThan => (try helpers.readUInt(lhs_value)) < (try helpers.readUInt(rhs_value)),
                        .ULessThanEqual => (try helpers.readUInt(lhs_value)) <= (try helpers.readUInt(rhs_value)),
                        .SGreaterThan => (try helpers.readSInt(lhs_value)) > (try helpers.readSInt(rhs_value)),
                        .SGreaterThanEqual => (try helpers.readSInt(lhs_value)) >= (try helpers.readSInt(rhs_value)),
                        .SLessThan => (try helpers.readSInt(lhs_value)) < (try helpers.readSInt(rhs_value)),
                        .SLessThanEqual => (try helpers.readSInt(lhs_value)) <= (try helpers.readSInt(rhs_value)),
                        .LogicalAnd => (try helpers.readBool(lhs_value)) and (try helpers.readBool(rhs_value)),
                        .LogicalOr => (try helpers.readBool(lhs_value)) or (try helpers.readBool(rhs_value)),
                        .LogicalEqual => (try helpers.readBool(lhs_value)) == (try helpers.readBool(rhs_value)),
                        .LogicalNotEqual => (try helpers.readBool(lhs_value)) != (try helpers.readBool(rhs_value)),
                        else => return RuntimeError.UnsupportedSpirV,
                    };
                },
            };

            dst.* = result;
        },
        .Float,
        .Vector,
        .Vector2f32,
        .Vector3f32,
        .Vector4f32,
        => {
            switch (inner_op) {
                .QuantizeToF16 => {
                    if (word_count != 4)
                        return RuntimeError.UnsupportedSpirV;

                    const operand = try rt.results[try rt.it.next()].getValue();
                    try quantizeToF16Value(target_type, rt, target_value, operand);
                },
                .FConvert => {
                    if (word_count != 4)
                        return RuntimeError.InvalidSpirV;

                    try helpers.convertValue(.Float, .Float, target_type, rt, target_value, &rt.results[try rt.it.next()]);
                },
                .CompositeInsert => {
                    const object = try rt.results[try rt.it.next()].getValue();
                    const composite = try rt.results[try rt.it.next()].getValue();
                    try helpers.specCompositeInsert(&rt.it, word_count, target_value, object, composite);
                },
                .CompositeExtract => {
                    const composite = try rt.results[try rt.it.next()].getValue();
                    try helpers.specCompositeExtract(allocator, &rt.it, word_count, target_value, composite);
                },
                .VectorShuffle => {
                    const vector_1 = try rt.results[try rt.it.next()].getValue();
                    const vector_2 = try rt.results[try rt.it.next()].getValue();
                    try helpers.specVectorShuffle(allocator, &rt.it, target_value, vector_1, vector_2);
                },
                else => return RuntimeError.UnsupportedSpirV,
            }
        },
        .Vector2i32,
        .Vector3i32,
        .Vector4i32,
        .Vector2u32,
        .Vector3u32,
        .Vector4u32,
        => {
            switch (inner_op) {
                .CompositeInsert => {
                    const object = try rt.results[try rt.it.next()].getValue();
                    const composite = try rt.results[try rt.it.next()].getValue();
                    try helpers.specCompositeInsert(&rt.it, word_count, target_value, object, composite);
                },
                .CompositeExtract => {
                    const composite = try rt.results[try rt.it.next()].getValue();
                    try helpers.specCompositeExtract(allocator, &rt.it, word_count, target_value, composite);
                },
                .VectorShuffle => {
                    const vector_1 = try rt.results[try rt.it.next()].getValue();
                    const vector_2 = try rt.results[try rt.it.next()].getValue();
                    try helpers.specVectorShuffle(allocator, &rt.it, target_value, vector_1, vector_2);
                },
                else => return RuntimeError.UnsupportedSpirV,
            }
        },
        else => return RuntimeError.UnsupportedSpirV,
    }
}

fn opCopyMemory(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try rt.it.next();
    const source = try rt.it.next();
    try copyValue(try rt.results[target].getValue(), try rt.results[source].getValue());
    try rt.copyDerivative(allocator, target, source);
}

fn opCopyObject(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result type
    const target = try rt.it.next();
    const source = try rt.it.next();
    const target_value = try rt.results[target].getValue();
    const source_value = try rt.results[source].getValue();
    if (source_value.* == .Pointer) {
        target_value.* = source_value.*;
        target_value.Pointer.owns_uniform_backing_value = false;
        return;
    }
    try copyValue(target_value, source_value);
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

fn opDerivativeSetup(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    try autoSetupConstant(allocator, word_count, rt);
    rt.mod.reflection_infos.needs_derivatives = true;
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

fn opFwidth(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
    const id = try rt.it.next();
    const operand = try rt.it.next();

    const dst = try rt.results[id].getValue();
    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);
    const derivative = rt.derivatives.get(operand);

    switch (lane_bits) {
        inline 16, 32, 64 => |bits| {
            const FloatT = Value.getPrimitiveFieldType(.Float, bits);
            for (0..lane_count) |lane_index| {
                const dx: FloatT = if (derivative) |d|
                    try Value.readLane(.Float, bits, &d.dx, lane_index)
                else
                    1;
                const dy: FloatT = if (derivative) |d|
                    try Value.readLane(.Float, bits, &d.dy, lane_index)
                else
                    0;
                try Value.writeLane(.Float, bits, dst, lane_index, @as(FloatT, @abs(dx) + @abs(dy)));
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }

    rt.clearDerivative(allocator, id);
}

fn DerivativeEngine(comptime axis: enum { x, y }) type {
    return struct {
        fn op(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const result_type_word = try rt.it.next();
            const id = try rt.it.next();
            const operand = try rt.it.next();

            const derivative = rt.derivatives.get(operand) orelse {
                const target_type = (try rt.results[result_type_word].getVariant()).Type;
                const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
                const lane_count = try Result.resolveLaneCount(target_type);
                const dst = try rt.results[id].getValue();

                switch (lane_bits) {
                    inline 16, 32, 64 => |bits| {
                        const FloatT = Value.getPrimitiveFieldType(.Float, bits);
                        for (0..lane_count) |lane_index| {
                            try Value.writeLane(.Float, bits, dst, lane_index, @as(FloatT, 0));
                        }
                    },
                    else => return RuntimeError.InvalidSpirV,
                }

                rt.clearDerivative(allocator, id);
                return;
            };
            const src = switch (axis) {
                .x => &derivative.dx,
                .y => &derivative.dy,
            };
            try copyValue(try rt.results[id].getValue(), src);
            try rt.copyDerivative(allocator, id, operand);
        }
    };
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
            rt.mod.reflection_infos.local_size_x = try rt.it.next();
            rt.mod.reflection_infos.local_size_y = try rt.it.next();
            rt.mod.reflection_infos.local_size_z = try rt.it.next();
        },
        .Invocations => rt.mod.reflection_infos.geometry_invocations = try rt.it.next(),
        .OutputVertices => rt.mod.reflection_infos.geometry_output_count = try rt.it.next(),
        .InputPoints,
        .InputLines,
        .Triangles,
        .InputLinesAdjacency,
        .InputTrianglesAdjacency,
        => rt.mod.reflection_infos.geometry_input = @intFromEnum(mode),
        .OutputPoints,
        .OutputLineStrip,
        .OutputTriangleStrip,
        => rt.mod.reflection_infos.geometry_output = @intFromEnum(mode),
        .EarlyFragmentTests => rt.mod.reflection_infos.early_fragment_tests = true,
        else => {},
    }
}

fn opExtInst(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = try rt.it.next();
    const id = try rt.it.next();
    const set = try rt.it.next();
    const inst = try rt.it.next();

    switch ((try rt.results[set].getVariant()).*) {
        .Extension => |ext| {
            if (inst >= ext.dispatcher.len) return RuntimeError.UnsupportedSpirV;
            const pfn = ext.dispatcher[inst] orelse return RuntimeError.UnsupportedSpirV;
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
    const existing_params = if (rt.results[id].variant) |variant| switch (variant) {
        .Function => |function| function.params,
        else => return RuntimeError.InvalidSpirV,
    } else null;

    rt.results[id].variant = .{
        .Function = .{
            .source_location = source_location,
            .return_type = return_type,
            .function_type = function_type_id,
            .params = existing_params orelse params: {
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
    rt.function_stack.items[rt.function_stack.items.len - 1].current_label = rt.current_label;
    rt.function_stack.items[rt.function_stack.items.len - 1].previous_label = rt.previous_label;
    const source_location = (try func.getVariant()).Function.source_location;
    rt.function_stack.append(allocator, .{
        .source_location = source_location,
        .result = func,
        .ret = ret,
        .current_label = null,
        .previous_label = null,
    }) catch return RuntimeError.OutOfMemory;
    if (!rt.it.jumpToSourceLocation(source_location)) return RuntimeError.InvalidSpirV;
    rt.current_function = func;
    rt.current_parameter_index = 0;
}

fn opFunctionEnd(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.current_function = null;
}

fn opFunctionParameter(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();

    const target = &rt.results[id];
    if (rt.function_stack.items.len != 0) {
        if (target.variant) |*variant| switch (variant.*) {
            .FunctionParameter => |parameter| {
                if (parameter.value_ptr != null) {
                    (try (rt.current_function orelse return RuntimeError.InvalidSpirV).getVariant()).Function.params[rt.current_parameter_index] = id;
                    rt.current_parameter_index += 1;
                    return;
                }
            },
            else => {},
        };
    }
    const value_ptr = if (target.variant) |*variant| switch (variant.*) {
        .FunctionParameter => |parameter| parameter.value_ptr,
        else => null,
    } else null;

    const resolved = rt.results[var_type].resolveType(rt.results);
    target.variant = .{
        .FunctionParameter = .{
            .type_word = var_type,
            .type = switch ((try resolved.getConstVariant()).*) {
                .Type => |t| @as(Result.Type, t),
                else => return RuntimeError.InvalidSpirV,
            },
            .value_ptr = value_ptr,
        },
    };
    (try (rt.current_function orelse return RuntimeError.InvalidSpirV).getVariant()).Function.params[rt.current_parameter_index] = id;
    rt.current_parameter_index += 1;
}

fn opLabel(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.current_label = id;
    if (rt.function_stack.items.len != 0) {
        rt.function_stack.items[rt.function_stack.items.len - 1].current_label = id;
    }
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

fn finiteDifferenceDelta(comptime T: type, shifted: T, center: T) T {
    return switch (@typeInfo(T)) {
        .int => @subWithOverflow(shifted, center)[0],
        else => shifted - center,
    };
}

fn writeFiniteDifferenceValue(rt: *Runtime, target_type_word: SpvWord, dst: *Value, shifted: *const Value, center: *const Value) RuntimeError!void {
    const target_type = (try rt.results[target_type_word].getVariant()).Type;
    const primitive_type = try center.resolvePrimitiveType();
    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_count = try Result.resolveLaneCount(target_type);

    switch (primitive_type) {
        inline .Float, .SInt, .UInt => |primitive| switch (lane_bits) {
            inline 16, 32, 64 => |bits| {
                const LaneT = Value.getPrimitiveFieldType(primitive, bits);
                for (0..lane_count) |lane_index| {
                    const shifted_lane = try Value.readLane(primitive, bits, shifted, lane_index);
                    const center_lane = try Value.readLane(primitive, bits, center, lane_index);
                    try Value.writeLane(primitive, bits, dst, lane_index, finiteDifferenceDelta(LaneT, shifted_lane, center_lane));
                }
            },
            else => return RuntimeError.UnsupportedSpirV,
        },
        else => return RuntimeError.InvalidValueType,
    }
}

fn readIndexDelta(value: *const Value) RuntimeError!isize {
    return switch (value.*) {
        .Int => |int| switch (int.bit_count) {
            8 => if (int.is_signed) int.value.sint8 else @as(i8, @bitCast(int.value.uint8)),
            16 => if (int.is_signed) int.value.sint16 else @as(i16, @bitCast(int.value.uint16)),
            32 => if (int.is_signed) int.value.sint32 else @as(i32, @bitCast(int.value.uint32)),
            64 => if (int.is_signed) std.math.cast(isize, int.value.sint64) orelse return RuntimeError.OutOfBounds else std.math.cast(isize, @as(i64, @bitCast(int.value.uint64))) orelse return RuntimeError.OutOfBounds,
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidValueType,
    };
}

fn addFiniteDifferenceDelta(comptime T: type, base_value: T, delta: T) T {
    return switch (@typeInfo(T)) {
        .int => @addWithOverflow(base_value, delta)[0],
        else => base_value + delta,
    };
}

fn shiftedWindow(root_window: []const u8, current_offset: usize, stride: usize, delta: isize) ?[]const u8 {
    const byte_delta = std.math.mul(isize, @as(isize, @intCast(stride)), delta) catch return null;
    const shifted_offset = @as(isize, @intCast(current_offset)) + byte_delta;
    if (shifted_offset < 0) return null;
    const offset: usize = @intCast(shifted_offset);
    if (offset > root_window.len) return null;
    return root_window[offset..];
}

fn setDescriptorLoadDerivative(allocator: std.mem.Allocator, rt: *Runtime, result_type_word: SpvWord, result_id: SpvWord, ptr_id: SpvWord, center: *const Value) RuntimeError!bool {
    const access_chain = switch ((try rt.results[ptr_id].getConstVariant()).*) {
        .AccessChain => |a| a,
        else => return false,
    };
    const ptr = switch (access_chain.value) {
        .Pointer => |p| p,
        else => return false,
    };
    const window = ptr.uniform_slice_window orelse return false;
    const root_window = ptr.uniform_root_window orelse window;
    const current_offset = ptr.uniform_window_offset;

    var index_derivative: ?Runtime.Derivative = null;
    for (access_chain.indexes) |index_id| {
        if (rt.derivatives.get(index_id)) |derivative|
            index_derivative = derivative;
    }
    const derivative = index_derivative orelse return false;

    const stride = try center.getPlainMemorySize();
    var dx_shifted = try Value.init(allocator, rt.results, result_type_word, false);
    defer dx_shifted.deinit(allocator);
    var dy_shifted = try Value.init(allocator, rt.results, result_type_word, false);
    defer dy_shifted.deinit(allocator);

    if (shiftedWindow(root_window, current_offset, stride, try readIndexDelta(&derivative.dx))) |dx_window| {
        _ = try dx_shifted.write(dx_window);
    }
    if (shiftedWindow(root_window, current_offset, stride, try readIndexDelta(&derivative.dy))) |dy_window| {
        _ = try dy_shifted.write(dy_window);
    }

    var dx = try Value.init(allocator, rt.results, result_type_word, false);
    defer dx.deinit(allocator);
    var dy = try Value.init(allocator, rt.results, result_type_word, false);
    defer dy.deinit(allocator);
    try writeFiniteDifferenceValue(rt, result_type_word, &dx, &dx_shifted, center);
    try writeFiniteDifferenceValue(rt, result_type_word, &dy, &dy_shifted, center);
    try rt.setDerivative(allocator, result_id, &dx, &dy);
    return true;
}

fn opDemoteToHelperInvocation(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.helper_invocation = true;
    const helper_invocation = true;
    rt.writeBuiltIn(allocator, std.mem.asBytes(&helper_invocation), .HelperInvocation) catch |err| switch (err) {
        RuntimeError.NotFound => {},
        else => return err,
    };
}

fn opIsHelperInvocationEXT(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next();
    const id = try rt.it.next();
    const value = try rt.results[id].getValue();
    switch (value.*) {
        .Bool => |*b| b.* = rt.helper_invocation,
        else => return RuntimeError.InvalidSpirV,
    }
}

fn opLoad(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const result_type_word = try rt.it.next();
    const id = try rt.it.next();
    const ptr_id = try rt.it.next();
    try rt.refreshResultValueLayout(id);
    const dst = try rt.results[id].getValue();
    try copyValue(dst, try rt.results[ptr_id].getValue());
    if (try setDescriptorLoadDerivative(allocator, rt, result_type_word, id, ptr_id, dst))
        return;
    try rt.copyDerivative(allocator, id, ptr_id);
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
                    .members_matrix_strides = undefined,
                    .members_row_major = undefined,
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

fn opMemoryBarrier(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip(); // memory scope
    _ = rt.it.skip(); // memory semantics
}

fn opName(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    var result = &rt.results[id];
    result.name = try readStringN(allocator, &rt.it, word_count - 1);
}

fn opPhi(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result type
    const id = try rt.it.next();

    const predecessor = rt.previous_label orelse return RuntimeError.InvalidSpirV;
    const pair_count = @divExact(word_count - 2, 2);

    for (0..pair_count) |_| {
        const value_id = try rt.it.next();
        const parent_label_id = try rt.it.next();

        if (parent_label_id == predecessor) {
            const value = rt.getPhiValueSnapshot(value_id) orelse try rt.results[value_id].getValue();
            try copyValue(try rt.results[id].getValue(), value);
            try rt.copyDerivative(allocator, id, value_id);
            return;
        }
    }
    return RuntimeError.InvalidSpirV;
}

fn quantizeF32ToF16(value: f32) f32 {
    const quantized = @as(f32, @floatCast(@as(f16, @floatCast(value))));
    if (quantized != 0.0 and @abs(quantized) < 0x1.0p-14) {
        const sign = @as(u32, @bitCast(quantized)) & 0x8000_0000;
        return @as(f32, @bitCast(sign));
    }
    return quantized;
}

fn quantizeToF16Value(target_type: Result.TypeData, rt: *Runtime, dst: *Value, src: *const Value) RuntimeError!void {
    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    if (lane_bits != 32)
        return RuntimeError.InvalidSpirV;

    const lane_count = try Result.resolveLaneCount(target_type);
    for (0..lane_count) |lane_index| {
        try Value.writeLane(.Float, 32, dst, lane_index, quantizeF32ToF16(try Value.readLane(.Float, 32, src, lane_index)));
    }
}

fn opQuantizeToF16(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const target_type = (try rt.results[try rt.it.next()].getVariant()).Type;
    const id = try rt.it.next();
    const src = try rt.results[try rt.it.next()].getValue();
    const dst = try rt.results[id].getValue();

    try quantizeToF16Value(target_type, rt, dst, src);
}

fn opReturn(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.function_stack.pop();
    if (rt.function_stack.getLastOrNull()) |function| {
        _ = rt.it.jumpToSourceLocation(function.source_location);
        rt.current_function = function.result;
        rt.current_label = function.current_label;
        rt.previous_label = function.previous_label;
    } else {
        rt.current_function = null;
        rt.current_label = null;
        rt.previous_label = null;
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
        rt.current_label = function.current_label;
        rt.previous_label = function.previous_label;
    } else {
        rt.current_function = null;
        rt.current_label = null;
        rt.previous_label = null;
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
        for (targets.*, 0..) |*t, lane_index| {
            const condition = try readVectorLaneAsValue(cond_val, lane_index);
            if (condition != .Bool)
                return RuntimeError.InvalidValueType;

            const selected = try readVectorLaneAsValue(if (condition.Bool) obj1_val else obj2_val, lane_index);
            try copyValue(t, &selected);
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

fn opStore(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const ptr_id = try rt.it.next();
    const val_id = try rt.it.next();
    if (rt.helper_invocation)
        return;
    try copyValue(try rt.results[ptr_id].getValue(), try rt.results[val_id].getValue());
    try rt.copyDerivative(allocator, ptr_id, val_id);
}

fn opTypeArray(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    var target = &rt.results[id];
    const components_type_word = try rt.it.next();
    const components_type_data = &((try rt.results[components_type_word].getVariant()).*).Type;
    const length_word = try rt.it.next();
    target.variant = .{
        .Type = .{
            .Array = .{
                .components_type_word = components_type_word,
                .components_type = switch ((try rt.results[components_type_word].getVariant()).*) {
                    .Type => |t| @as(Result.Type, t),
                    else => return RuntimeError.InvalidSpirV,
                },
                .member_count = try arrayMemberCount(rt, length_word),
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

fn arrayMemberCount(rt: *Runtime, length_word: SpvWord) RuntimeError!SpvWord {
    for (rt.results[length_word].decorations.items) |decoration| {
        if (decoration.rtype != .SpecId)
            continue;

        if (rt.specialization_constants.get(decoration.literal_1)) |data| {
            if (data.len < @sizeOf(u32))
                return RuntimeError.OutOfBounds;

            return @intCast(std.mem.bytesToValue(u32, data[0..@sizeOf(u32)]));
        }
    }

    return switch ((try rt.results[length_word].getValue()).*) {
        .Int => |i| if (!i.is_signed) @intCast(i.value.uint64) else switch (i.bit_count) {
            8 => @intCast(i.value.sint8),
            16 => @intCast(i.value.sint16),
            32 => @intCast(i.value.sint32),
            64 => @intCast(i.value.sint64),
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidSpirV,
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

fn opTypeSampledImage(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.results[id].variant = .{
        .Type = .{
            .SampledImage = .{
                .image_type = try rt.it.next(),
            },
        },
    };
}

fn opTypeSampler(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.results[id].variant = .{
        .Type = .{
            .Sampler = .{},
        },
    };
}

fn opSampledImage(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const type_word = try rt.it.next();
    const dst = try rt.results[try rt.it.next()].getValue();
    const image = try rt.results[try rt.it.next()].getValue();
    const sampler = try rt.results[try rt.it.next()].getValue();

    dst.* = .{
        .SampledImage = .{
            .type_word = type_word,
            .driver_image = switch (image.*) {
                .Image => |img| img.driver_image,
                else => return RuntimeError.InvalidSpirV,
            },
            .driver_sampler = switch (sampler.*) {
                .Sampler => |s| s.driver_sampler,
                else => return RuntimeError.InvalidSpirV,
            },
        },
    };
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
    const members_matrix_strides = allocator.alloc(?SpvWord, word_count - 1) catch return RuntimeError.OutOfMemory;
    @memset(members_matrix_strides, null);
    const members_row_major = allocator.alloc(bool, word_count - 1) catch return RuntimeError.OutOfMemory;
    @memset(members_row_major, false);

    if (rt.results[id].variant) |*variant| {
        switch (variant.*) {
            .Type => |*t| switch (t.*) {
                .Structure => |*s| {
                    s.members_type_word = members_type_word;
                    s.members_offsets = members_offsets;
                    s.members_matrix_strides = members_matrix_strides;
                    s.members_row_major = members_row_major;
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
                    .members_matrix_strides = members_matrix_strides,
                    .members_row_major = members_row_major,
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
        .PushConstant,
        .Workgroup,
    };

    const is_externally_visible = std.mem.containsAtLeastScalar(spv.SpvStorageClass, &externally_visible_data_storages, 1, storage_class);
    const use_external_storage = is_externally_visible and (storage_class == .Workgroup or resolved_type != .Array);

    var initial_value = try Value.init(allocator, rt.results, resolved_word, use_external_storage);
    errdefer initial_value.deinit(allocator);
    if (initializer) |initializer_id| {
        try copyValue(&initial_value, try rt.results[initializer_id].getValue());
    }

    if (target.variant) |*variant| {
        switch (variant.*) {
            .Variable => |*variable| {
                variable.storage_class = storage_class;
                variable.type_word = resolved_word;
                variable.type = resolved_type;
                try copyValue(&variable.value, &initial_value);
                initial_value.deinit(allocator);
                return;
            },
            else => {},
        }
    }

    target.variant = .{
        .Variable = .{
            .storage_class = storage_class,
            .type_word = resolved_word,
            .type = resolved_type,
            .value = initial_value,
        },
    };
}

fn readDynamicVectorIndex(index_value: *const Value) RuntimeError!usize {
    return switch (index_value.*) {
        .Int => |i| switch (i.bit_count) {
            8 => if (i.is_signed) std.math.cast(usize, i.value.sint8) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint8),
            16 => if (i.is_signed) std.math.cast(usize, i.value.sint16) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint16),
            32 => if (i.is_signed) std.math.cast(usize, i.value.sint32) orelse RuntimeError.OutOfBounds else @as(usize, i.value.uint32),
            64 => if (i.is_signed) std.math.cast(usize, i.value.sint64) orelse RuntimeError.OutOfBounds else std.math.cast(usize, i.value.uint64) orelse RuntimeError.OutOfBounds,
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.InvalidSpirV,
    };
}

fn readVectorLaneAsValue(src: *const Value, lane_index: usize) RuntimeError!Value {
    return switch (src.*) {
        .Vector => |lanes| blk: {
            if (lane_index >= lanes.len) return RuntimeError.OutOfBounds;
            break :blk lanes[lane_index];
        },

        .Vector2f32 => |lanes| .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (lane_index) {
            inline 0...1 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },
        .Vector3f32 => |lanes| .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (lane_index) {
            inline 0...2 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },
        .Vector4f32 => |lanes| .{ .Float = .{ .bit_count = 32, .value = .{ .float32 = switch (lane_index) {
            inline 0...3 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },

        .Vector2i32 => |lanes| .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (lane_index) {
            inline 0...1 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },
        .Vector3i32 => |lanes| .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (lane_index) {
            inline 0...2 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },
        .Vector4i32 => |lanes| .{ .Int = .{ .bit_count = 32, .is_signed = true, .value = .{ .sint32 = switch (lane_index) {
            inline 0...3 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },

        .Vector2u32 => |lanes| .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (lane_index) {
            inline 0...1 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },
        .Vector3u32 => |lanes| .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (lane_index) {
            inline 0...2 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },
        .Vector4u32 => |lanes| .{ .Int = .{ .bit_count = 32, .is_signed = false, .value = .{ .uint32 = switch (lane_index) {
            inline 0...3 => |idx| lanes[idx],
            else => return RuntimeError.OutOfBounds,
        } } } },

        else => return RuntimeError.InvalidSpirV,
    };
}

fn opVectorExtractDynamic(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try rt.it.next(); // result type
    const result_id = try rt.it.next();
    const vector_id = try rt.it.next();
    const vector = try rt.results[vector_id].getValue();
    const index = try readDynamicVectorIndex(try rt.results[try rt.it.next()].getValue());

    const lane_value = try readVectorLaneAsValue(vector, index);
    try copyValue(try rt.results[result_id].getValue(), &lane_value);

    if (rt.derivatives.get(vector_id)) |derivative| {
        const dx_lane = try readVectorLaneAsValue(&derivative.dx, index);
        const dy_lane = try readVectorLaneAsValue(&derivative.dy, index);
        try rt.setDerivative(allocator, result_id, &dx_lane, &dy_lane);
    } else {
        rt.clearDerivative(allocator, result_id);
    }
}

fn opVectorInsertDynamic(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const result_type_word = try rt.it.next();
    const result_id = try rt.it.next();
    const vector_id = try rt.it.next();
    const component_id = try rt.it.next();
    const index = try readDynamicVectorIndex(try rt.results[try rt.it.next()].getValue());

    const target = try rt.results[result_id].getValue();
    try copyValue(target, try rt.results[vector_id].getValue());

    const target_type = switch ((try rt.results[result_type_word].getVariant()).*) {
        .Type => |t| t,
        else => return RuntimeError.InvalidSpirV,
    };
    const component = try rt.results[component_id].getValue();

    const lane_bits = try Result.resolveLaneBitWidth(target_type, rt);
    const lane_kind: PrimitiveType = switch (target_type) {
        .Float,
        .Vector2f32,
        .Vector3f32,
        .Vector4f32,
        => .Float,
        .Int => |i| if (i.is_signed) .SInt else .UInt,
        .Vector => |v| switch ((try rt.results[v.components_type_word].getVariant()).*) {
            .Type => |t| switch (t) {
                .Float => .Float,
                .Int => |i| if (i.is_signed) .SInt else .UInt,
                else => return RuntimeError.InvalidSpirV,
            },
            else => return RuntimeError.InvalidSpirV,
        },
        .Vector2i32,
        .Vector3i32,
        .Vector4i32,
        => .SInt,
        .Vector2u32,
        .Vector3u32,
        .Vector4u32,
        => .UInt,
        else => return RuntimeError.InvalidSpirV,
    };

    switch (lane_bits) {
        inline 32 => |bits| switch (lane_kind) {
            inline .Float, .SInt, .UInt => |kind| try Value.writeLane(kind, bits, target, index, try Value.readLane(kind, bits, component, 0)),
            else => return RuntimeError.InvalidSpirV,
        },
        else => return RuntimeError.UnsupportedSpirV,
    }
}

fn opVectorShuffle(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const result_type_word = try rt.it.next();

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
    const vector_1_derivative = rt.derivatives.get(vector_1_id);
    const vector_2_derivative = rt.derivatives.get(vector_2_id);
    var has_derivative = false;

    var dx = try Value.init(allocator, rt.results, result_type_word, false);
    defer dx.deinit(allocator);
    var dy = try Value.init(allocator, rt.results, result_type_word, false);
    defer dy.deinit(allocator);

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

        const derivative = if (selector < vector_1_lane_count)
            vector_1_derivative
        else
            vector_2_derivative;

        if (derivative) |d| {
            const src_lane_index = if (selector < vector_1_lane_count)
                selector
            else
                selector - vector_1_lane_count;
            try Impl.writeLane(&dx, lane_index, try Impl.readLane(&d.dx, src_lane_index));
            try Impl.writeLane(&dy, lane_index, try Impl.readLane(&d.dy, src_lane_index));
            has_derivative = true;
        }
    }

    if (has_derivative) {
        try rt.setDerivative(allocator, result_id, &dx, &dy);
    } else {
        rt.clearDerivative(allocator, result_id);
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
