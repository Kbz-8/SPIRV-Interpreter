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

// DUMB INDEV OPCODES TODO :
//     OpVariable           X
//     OpAccessChain        X
//     OpStore              X
//     OpLoad               X
//     OpCompositeExtract   X
//     OpReturn             X
//     OpFunctionEnd        X

pub const OpCodeFunc = *const fn (std.mem.Allocator, SpvWord, *Runtime) RuntimeError!void;

pub const SetupDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeFunc).init(.{
        .Capability = opCapability,
        .Constant = opConstant,
        .Decorate = opDecorate,
        .EntryPoint = opEntryPoint,
        .ExecutionMode = opExecutionMode,
        .MemberDecorate = opDecorateMember,
        .MemberName = opMemberName,
        .MemoryModel = opMemoryModel,
        .Name = opName,
        .Source = opSource,
        .SourceExtension = opSourceExtension,
        .TypeBool = opTypeBool,
        .TypeFloat = opTypeFloat,
        .TypeFunction = opTypeFunction,
        .TypeInt = opTypeInt,
        .TypeMatrix = opTypeMatrix,
        .TypePointer = opTypePointer,
        .TypeStruct = opTypeStruct,
        .TypeVector = opTypeVector,
        .TypeVoid = opTypeVoid,
        .Variable = opVariable,
        .Function = opFunction,
        .Label = opLabel,
        .CompositeConstruct = opCompositeConstruct,
    });
};

fn opCapability(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.capabilities.insert(try rt.it.nextAs(spv.SpvCapability));
}

fn addDecoration(allocator: std.mem.Allocator, rt: *Runtime, target: SpvWord, decoration_type: spv.SpvDecoration, member: ?SpvWord) RuntimeError!void {
    var decoration = rt.mod.results.items[target].decorations.addOne(allocator) catch return RuntimeError.OutOfMemory;
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

fn opMemberName(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const memb = try rt.it.next();

    var result = &rt.mod.results.items[id];

    if (memb + 1 > result.member_names.items.len) {
        _ = result.member_names.resize(allocator, memb + 1) catch return RuntimeError.OutOfMemory;
    }

    const slen = word_count - 2;
    result.member_names.items[memb] = try readStringN(allocator, &rt.it, slen);
}

fn opMemoryModel(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.addressing = try rt.it.nextAs(spv.SpvAddressingModel);
    rt.mod.memory_model = try rt.it.nextAs(spv.SpvMemoryModel);
}

fn opName(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    if (id >= rt.mod.results.items.len) return RuntimeError.InvalidSpirV;
    var result = &rt.mod.results.items[id];
    result.* = Result.init();
    result.name = try readStringN(allocator, &rt.it, word_count - 1);
}

fn opSource(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    var file = rt.mod.files.addOne(allocator) catch return RuntimeError.OutOfMemory;
    file.lang = try rt.it.nextAs(spv.SpvSourceLanguage);
    file.lang_version = try rt.it.next();
    if (word_count > 2) {
        const id = try rt.it.next();
        if (id >= rt.mod.results.items.len) return RuntimeError.InvalidSpirV;
        if (rt.mod.results.items[id].name) |name| {
            file.file_name = name;
        }
    }
    if (word_count > 3) {
        const id = try rt.it.next();
        if (id >= rt.mod.results.items.len) return RuntimeError.InvalidSpirV;
        if (rt.mod.results.items[id].name) |name| {
            file.source = name;
        }
    }
}

fn opSourceExtension(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.extensions.append(allocator, try readStringN(allocator, &rt.it, word_count)) catch return RuntimeError.OutOfMemory;
}

fn opTypeVoid(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Type = .{
            .Void = .{},
        },
    };
}

fn opTypeBool(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Type = .{
            .Bool = .{},
        },
    };
}

fn opTypeInt(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Type = .{
            .Int = .{
                .bit_length = try rt.it.next(),
                .is_signed = if (try rt.it.next() != 0) true else false,
            },
        },
    };
}

fn opTypeFloat(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Type = .{
            .Float = .{
                .bit_length = try rt.it.next(),
            },
        },
    };
}

fn opTypeVector(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Type = .{
            .Vector = .{
                .components_type = switch (rt.mod.results.items[try rt.it.next()].variant.?) {
                    .Type => |t| @as(Result.Type, t),
                    else => return RuntimeError.InvalidSpirV,
                },
                .member_count = try rt.it.next(),
            },
        },
    };
}

fn opTypeMatrix(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Type = .{
            .Matrix = .{
                .column_type = switch (rt.mod.results.items[try rt.it.next()].variant.?) {
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
    rt.mod.results.items[id].variant = .{
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
    rt.mod.results.items[id].variant = .{
        .Type = .{
            .Structure = .{
                .members = blk: {
                    const members = allocator.alloc(SpvWord, word_count - 1) catch return RuntimeError.OutOfMemory;
                    errdefer allocator.free(members);
                    for (members) |*member| {
                        member.* = try rt.it.next();
                    }
                    break :blk members;
                },
            },
        },
    };
}

fn opTypeFunction(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Type = .{
            .Function = .{
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

fn opConstant(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Constant = value: {
            const resolved = rt.mod.results.items[var_type].resolveType(rt.mod.results.items);
            if (resolved.variant) |variant| {
                switch (variant) {
                    .Type => |t| switch (t) {
                        .Int => {
                            break :value if (word_count - 2 != 1) .{
                                .Int = .{
                                    .uint64 = @as(u64, try rt.it.next()) | (@as(u64, try rt.it.next()) >> 32),
                                },
                            } else .{
                                .Int = .{
                                    .uint32 = try rt.it.next(),
                                },
                            };
                        },
                        .Float => {
                            break :value if (word_count - 2 != 1) .{
                                .Float = .{
                                    .float64 = @bitCast(@as(u64, try rt.it.next()) | (@as(u64, try rt.it.next()) >> 32)),
                                },
                            } else .{
                                .Float = .{
                                    .float32 = @bitCast(try rt.it.next()),
                                },
                            };
                        },
                        else => {}, // Will fallback on error,
                    },
                    else => {}, // Will fallback on error
                }
            }
            return RuntimeError.InvalidSpirV;
        },
    };
}

fn opVariable(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();
    const storage_class = try rt.it.nextAs(spv.SpvStorageClass);
    const initializer: ?SpvWord = if (word_count >= 4) try rt.it.next() else null;
    _ = initializer;

    rt.mod.results.items[id].variant = .{
        .Variable = .{
            .storage_class = storage_class,
            .value = value: {
                const resolved = rt.mod.results.items[var_type].resolveType(rt.mod.results.items);
                _ = resolved;
                break :value undefined;
            },
        },
    };
}

fn opFunction(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const return_type = try rt.it.next();
    const id = try rt.it.next();
    _ = rt.it.skip(); // Skip function control
    const function_type_id = try rt.it.next();

    rt.mod.results.items[id].variant = .{
        .Function = .{
            .return_type = return_type,
            .function_type = function_type_id,
            .params = params: {
                if (rt.mod.results.items[function_type_id].variant) |variant| {
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

    rt.current_function = &rt.mod.results.items[id];
}

fn opLabel(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results.items[id].variant = .{
        .Label = .{},
    };
}

fn opCompositeConstruct(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt;
}

fn readString(allocator: std.mem.Allocator, it: *WordIterator) RuntimeError![]const u8 {
    var str: std.ArrayList(u8) = .empty;
    while (it.nextOrNull()) |word| {
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate(word & 0x000000FF);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0x0000FF00) >> 8);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0x00FF0000) >> 16);
        (str.addOne(allocator) catch return RuntimeError.OutOfMemory).* = @truncate((word & 0xFF000000) >> 24);
        if (str.getLast() == 0) {
            break;
        }
    }
    return str.toOwnedSlice(allocator);
}

fn readStringN(allocator: std.mem.Allocator, it: *WordIterator, n: usize) RuntimeError![]const u8 {
    var str = std.ArrayList(u8).initCapacity(allocator, n * 4) catch return RuntimeError.OutOfMemory;
    for (0..n) |_| {
        if (it.nextOrNull()) |word| {
            str.addOneAssumeCapacity().* = @truncate(word & 0x000000FF);
            str.addOneAssumeCapacity().* = @truncate((word & 0x0000FF00) >> 8);
            str.addOneAssumeCapacity().* = @truncate((word & 0x00FF0000) >> 16);
            str.addOneAssumeCapacity().* = @truncate((word & 0xFF000000) >> 24);
            if (str.getLast() == 0) {
                break;
            }
        }
    }
    return str.toOwnedSlice(allocator);
}
