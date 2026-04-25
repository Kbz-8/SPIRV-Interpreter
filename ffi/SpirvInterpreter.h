/*
	Copyright (C) 2026 kbz_8 (contact@kbz8.me)
	This file is part of the "SPIR-V Interpreter - FFI C Bindings" project

	MIT License

	Copyright (c) 2026 kbz_8

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
*/

#ifndef SPIRV_INTERPRETER_H
#define SPIRV_INTERPRETER_H

#ifndef SPV_API
	#define SPV_API extern
#endif /* SPV_API */

#ifdef __cplusplus
extern "C"
{
#endif

#ifndef spirv_H
	typedef enum SpvBuiltIn_ SpvBuiltIn;
#endif /* spirv_H */

typedef int SpvBool;
typedef unsigned char SpvByte;
typedef unsigned long SpvWord;
typedef unsigned long SpvSize;

typedef enum
{
	SPV_RESULT_SUCCESS = 0,
	SPV_RESULT_DIVISION_BY_ZERO = -1,
	SPV_RESULT_INVALID_ENTRY_POINT = -2,
	SPV_RESULT_INVALID_SPIRV = -3,
	SPV_RESULT_INVALID_VALUE_TYPE = -4,
	SPV_RESULT_KILLED = -5,
	SPV_RESULT_NOT_FOUND = -6,
	SPV_RESULT_OUT_OF_MEMORY = -7,
	SPV_RESULT_OUT_OF_BOUNDS = -8,
	SPV_RESULT_TODO = -9,
	SPV_RESULT_UNREACHABLE = -10,
	SPV_RESULT_UNSUPPORTED_SPIRV = -11,
	SPV_RESULT_UNSUPPORTED_EXTENSION = -12,
	SPV_RESULT_UNSUPPORTED_ENDIANNESS = -13,
	SPV_RESULT_INVALID_MAGIC = -14,
	SPV_RESULT_UNKNOWN = -15,
} SpvResult;

typedef struct
{
	SpvBool use_simd_vectors_specializations;
} SpvModuleOptions;

typedef struct
{
	SpvWord id;
	SpvSize offset;
	SpvSize size;
} SpvRuntimeSpecializationEntry;

typedef void* SpvModule;
typedef void* SpvRuntime;

SPV_API SpvResult SpvInitModule(SpvModule* module, const SpvWord* source, SpvSize source_len, SpvModuleOptions options);
SPV_API void SpvDeinitModule(SpvModule module);

SPV_API SpvResult SpvInitRuntime(SpvRuntime* runtime, SpvModule module);
SPV_API void SpvDeinitRuntime(SpvRuntime runtime);

SPV_API SpvResult SpvFlushDescriptorSets(SpvRuntime runtime);

SPV_API SpvResult SpvAddSpecializationInfo(SpvRuntime runtime, SpvRuntimeSpecializationEntry entry, const SpvByte* data, SpvSize data_size);

SPV_API SpvResult SpvGetResultByName(SpvRuntime runtime, const char* name, SpvWord* result);
SPV_API SpvResult SpvGetEntryPointByName(SpvRuntime runtime, const char* name, SpvWord* result);
SPV_API SpvResult SpvCallEntryPoint(SpvRuntime runtime, SpvWord entry_point_index);

SPV_API SpvResult SpvReadOutput(SpvRuntime runtime, SpvByte* output, SpvSize output_size, SpvWord result);

SPV_API SpvResult SpvWriteInput(SpvRuntime runtime, const SpvByte* input, SpvSize input_size, SpvWord result);
SPV_API SpvResult SpvWriteBuiltIn(SpvRuntime runtime, const SpvByte* input, SpvSize input_size, SpvBuiltIn builtin);
SPV_API SpvResult SpvWriteDescriptorSet(SpvRuntime runtime, const SpvByte* input, SpvSize input_size, SpvWord set, SpvWord binding, SpvWord descriptor_index);

#ifndef spirv_H
	enum SpvBuiltIn_
	{
		SpvBuiltInPosition = 0,
		SpvBuiltInPointSize = 1,
		SpvBuiltInClipDistance = 3,
		SpvBuiltInCullDistance = 4,
		SpvBuiltInVertexId = 5,
		SpvBuiltInInstanceId = 6,
		SpvBuiltInPrimitiveId = 7,
		SpvBuiltInInvocationId = 8,
		SpvBuiltInLayer = 9,
		SpvBuiltInViewportIndex = 10,
		SpvBuiltInTessLevelOuter = 11,
		SpvBuiltInTessLevelInner = 12,
		SpvBuiltInTessCoord = 13,
		SpvBuiltInPatchVertices = 14,
		SpvBuiltInFragCoord = 15,
		SpvBuiltInPointCoord = 16,
		SpvBuiltInFrontFacing = 17,
		SpvBuiltInSampleId = 18,
		SpvBuiltInSamplePosition = 19,
		SpvBuiltInSampleMask = 20,
		SpvBuiltInFragDepth = 22,
		SpvBuiltInHelperInvocation = 23,
		SpvBuiltInNumWorkgroups = 24,
		SpvBuiltInWorkgroupSize = 25,
		SpvBuiltInWorkgroupId = 26,
		SpvBuiltInLocalInvocationId = 27,
		SpvBuiltInGlobalInvocationId = 28,
		SpvBuiltInLocalInvocationIndex = 29,
		SpvBuiltInWorkDim = 30,
		SpvBuiltInGlobalSize = 31,
		SpvBuiltInEnqueuedWorkgroupSize = 32,
		SpvBuiltInGlobalOffset = 33,
		SpvBuiltInGlobalLinearId = 34,
		SpvBuiltInSubgroupSize = 36,
		SpvBuiltInSubgroupMaxSize = 37,
		SpvBuiltInNumSubgroups = 38,
		SpvBuiltInNumEnqueuedSubgroups = 39,
		SpvBuiltInSubgroupId = 40,
		SpvBuiltInSubgroupLocalInvocationId = 41,
		SpvBuiltInVertexIndex = 42,
		SpvBuiltInInstanceIndex = 43,
		SpvBuiltInCoreIDARM = 4160,
		SpvBuiltInCoreCountARM = 4161,
		SpvBuiltInCoreMaxIDARM = 4162,
		SpvBuiltInWarpIDARM = 4163,
		SpvBuiltInWarpMaxIDARM = 4164,
		SpvBuiltInSubgroupEqMask = 4416,
		SpvBuiltInSubgroupEqMaskKHR = 4416,
		SpvBuiltInSubgroupGeMask = 4417,
		SpvBuiltInSubgroupGeMaskKHR = 4417,
		SpvBuiltInSubgroupGtMask = 4418,
		SpvBuiltInSubgroupGtMaskKHR = 4418,
		SpvBuiltInSubgroupLeMask = 4419,
		SpvBuiltInSubgroupLeMaskKHR = 4419,
		SpvBuiltInSubgroupLtMask = 4420,
		SpvBuiltInSubgroupLtMaskKHR = 4420,
		SpvBuiltInBaseVertex = 4424,
		SpvBuiltInBaseInstance = 4425,
		SpvBuiltInDrawIndex = 4426,
		SpvBuiltInPrimitiveShadingRateKHR = 4432,
		SpvBuiltInDeviceIndex = 4438,
		SpvBuiltInViewIndex = 4440,
		SpvBuiltInShadingRateKHR = 4444,
		SpvBuiltInTileOffsetQCOM = 4492,
		SpvBuiltInTileDimensionQCOM = 4493,
		SpvBuiltInTileApronSizeQCOM = 4494,
		SpvBuiltInBaryCoordNoPerspAMD = 4992,
		SpvBuiltInBaryCoordNoPerspCentroidAMD = 4993,
		SpvBuiltInBaryCoordNoPerspSampleAMD = 4994,
		SpvBuiltInBaryCoordSmoothAMD = 4995,
		SpvBuiltInBaryCoordSmoothCentroidAMD = 4996,
		SpvBuiltInBaryCoordSmoothSampleAMD = 4997,
		SpvBuiltInBaryCoordPullModelAMD = 4998,
		SpvBuiltInFragStencilRefEXT = 5014,
		SpvBuiltInRemainingRecursionLevelsAMDX = 5021,
		SpvBuiltInShaderIndexAMDX = 5073,
		SpvBuiltInSamplerHeapEXT = 5122,
		SpvBuiltInResourceHeapEXT = 5123,
		SpvBuiltInViewportMaskNV = 5253,
		SpvBuiltInSecondaryPositionNV = 5257,
		SpvBuiltInSecondaryViewportMaskNV = 5258,
		SpvBuiltInPositionPerViewNV = 5261,
		SpvBuiltInViewportMaskPerViewNV = 5262,
		SpvBuiltInFullyCoveredEXT = 5264,
		SpvBuiltInTaskCountNV = 5274,
		SpvBuiltInPrimitiveCountNV = 5275,
		SpvBuiltInPrimitiveIndicesNV = 5276,
		SpvBuiltInClipDistancePerViewNV = 5277,
		SpvBuiltInCullDistancePerViewNV = 5278,
		SpvBuiltInLayerPerViewNV = 5279,
		SpvBuiltInMeshViewCountNV = 5280,
		SpvBuiltInMeshViewIndicesNV = 5281,
		SpvBuiltInBaryCoordKHR = 5286,
		SpvBuiltInBaryCoordNV = 5286,
		SpvBuiltInBaryCoordNoPerspKHR = 5287,
		SpvBuiltInBaryCoordNoPerspNV = 5287,
		SpvBuiltInFragSizeEXT = 5292,
		SpvBuiltInFragmentSizeNV = 5292,
		SpvBuiltInFragInvocationCountEXT = 5293,
		SpvBuiltInInvocationsPerPixelNV = 5293,
		SpvBuiltInPrimitivePointIndicesEXT = 5294,
		SpvBuiltInPrimitiveLineIndicesEXT = 5295,
		SpvBuiltInPrimitiveTriangleIndicesEXT = 5296,
		SpvBuiltInCullPrimitiveEXT = 5299,
		SpvBuiltInLaunchIdKHR = 5319,
		SpvBuiltInLaunchIdNV = 5319,
		SpvBuiltInLaunchSizeKHR = 5320,
		SpvBuiltInLaunchSizeNV = 5320,
		SpvBuiltInWorldRayOriginKHR = 5321,
		SpvBuiltInWorldRayOriginNV = 5321,
		SpvBuiltInWorldRayDirectionKHR = 5322,
		SpvBuiltInWorldRayDirectionNV = 5322,
		SpvBuiltInObjectRayOriginKHR = 5323,
		SpvBuiltInObjectRayOriginNV = 5323,
		SpvBuiltInObjectRayDirectionKHR = 5324,
		SpvBuiltInObjectRayDirectionNV = 5324,
		SpvBuiltInRayTminKHR = 5325,
		SpvBuiltInRayTminNV = 5325,
		SpvBuiltInRayTmaxKHR = 5326,
		SpvBuiltInRayTmaxNV = 5326,
		SpvBuiltInInstanceCustomIndexKHR = 5327,
		SpvBuiltInInstanceCustomIndexNV = 5327,
		SpvBuiltInObjectToWorldKHR = 5330,
		SpvBuiltInObjectToWorldNV = 5330,
		SpvBuiltInWorldToObjectKHR = 5331,
		SpvBuiltInWorldToObjectNV = 5331,
		SpvBuiltInHitTNV = 5332,
		SpvBuiltInHitKindKHR = 5333,
		SpvBuiltInHitKindNV = 5333,
		SpvBuiltInCurrentRayTimeNV = 5334,
		SpvBuiltInHitTriangleVertexPositionsKHR = 5335,
		SpvBuiltInHitMicroTriangleVertexPositionsNV = 5337,
		SpvBuiltInHitMicroTriangleVertexBarycentricsNV = 5344,
		SpvBuiltInIncomingRayFlagsKHR = 5351,
		SpvBuiltInIncomingRayFlagsNV = 5351,
		SpvBuiltInRayGeometryIndexKHR = 5352,
		SpvBuiltInHitIsSphereNV = 5359,
		SpvBuiltInHitIsLSSNV = 5360,
		SpvBuiltInHitSpherePositionNV = 5361,
		SpvBuiltInWarpsPerSMNV = 5374,
		SpvBuiltInSMCountNV = 5375,
		SpvBuiltInWarpIDNV = 5376,
		SpvBuiltInSMIDNV = 5377,
		SpvBuiltInHitLSSPositionsNV = 5396,
		SpvBuiltInHitKindFrontFacingMicroTriangleNV = 5405,
		SpvBuiltInHitKindBackFacingMicroTriangleNV = 5406,
		SpvBuiltInHitSphereRadiusNV = 5420,
		SpvBuiltInHitLSSRadiiNV = 5421,
		SpvBuiltInClusterIDNV = 5436,
		SpvBuiltInCullMaskKHR = 6021,
		SpvBuiltInMax = 0x7fffffff,
	};
#endif /* spirv_H */

#ifdef __cplusplus
}
#endif

#endif /* SPIRV_INTERPRETER_H */
