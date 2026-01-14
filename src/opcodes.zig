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

const MathType = enum {
    Float,
    SInt,
    UInt,
};

pub const OpCodeFunc = *const fn (std.mem.Allocator, SpvWord, *Runtime) RuntimeError!void;

pub const SetupDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeFunc).init(.{
        .Capability = opCapability,
        .CompositeConstruct = autoSetupConstant,
        .Constant = opConstant,
        .Decorate = opDecorate,
        .EntryPoint = opEntryPoint,
        .ExecutionMode = opExecutionMode,
        .FMul = autoSetupConstant,
        .Function = opFunction,
        .FunctionEnd = opFunctionEnd,
        .IMul = autoSetupConstant,
        .Label = opLabel,
        .Load = autoSetupConstant,
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
    });
};

pub const RuntimeDispatcher = block: {
    @setEvalBranchQuota(65535);
    break :block std.EnumMap(spv.SpvOp, OpCodeFunc).init(.{
        .AccessChain = opAccessChain,
        .CompositeConstruct = opCompositeConstruct,
        .CompositeExtract = opCompositeExtract,
        .FMul = maths(.Float).opMul,
        .IMul = maths(.SInt).opMul,
        .Load = opLoad,
        .Return = opReturn,
        .Store = opStore,
    });
};

fn opCapability(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.mod.capabilities.insert(try rt.it.nextAs(spv.SpvCapability));
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

fn opTypeVoid(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Void = .{},
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

fn opTypeVector(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    const components_type_word = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Vector = .{
                .components_type_word = components_type_word,
                .components_type = switch (rt.mod.results[components_type_word].variant orelse return RuntimeError.InvalidSpirV) {
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
    const column_type_word = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Type = .{
            .Matrix = .{
                .column_type_word = column_type_word,
                .column_type = switch (rt.mod.results[column_type_word].variant orelse return RuntimeError.InvalidSpirV) {
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

fn opConstant(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const target = try setupConstant(allocator, rt);
    // No check on null and sizes, absolute trust in this shit
    switch (target.variant.?.Constant) {
        .Int => |*i| {
            if (word_count - 2 != 1) {
                i.uint64 = @as(u64, try rt.it.next()) | (@as(u64, try rt.it.next()) >> 32);
            } else {
                i.uint32 = try rt.it.next();
            }
        },
        .Float => |*f| {
            if (word_count - 2 != 1) {
                f.float64 = @bitCast(@as(u64, try rt.it.next()) | (@as(u64, try rt.it.next()) >> 32));
            } else {
                f.float32 = @bitCast(try rt.it.next());
            }
        },
        else => return RuntimeError.InvalidSpirV,
    }
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
            .value = try Result.initValue(allocator, member_count, rt.mod.results, resolved),
        },
    };

    _ = initializer;
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

fn opLabel(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const id = try rt.it.next();
    rt.mod.results[id].variant = .{
        .Label = .{
            .source_location = rt.it.emitSourceLocation() - 2, // Original label location
        },
    };
}

fn opCompositeConstruct(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const id = try rt.it.next();

    const index_count = word_count - 2;
    const target = (rt.results[id].variant orelse return RuntimeError.InvalidSpirV).Constant.getCompositeDataOrNull() orelse return RuntimeError.InvalidSpirV;
    for (target[0..index_count]) |*elem| {
        const value = (rt.results[try rt.it.next()].variant orelse return RuntimeError.InvalidSpirV).Constant;
        elem.* = value;
    }
}

fn opCompositeExtract(allocator: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const id = try rt.it.next();
    const composite_id = try rt.it.next();

    const index_count = word_count - 3;
    var composite = (rt.results[composite_id].variant orelse return RuntimeError.InvalidSpirV).Constant;
    for (0..index_count) |_| {
        const member_id = try rt.it.next();
        composite = (composite.getCompositeDataOrNull() orelse return RuntimeError.InvalidSpirV)[member_id];
    }
    rt.results[id].variant = .{
        .Constant = try composite.dupe(allocator),
    };
}

fn opFunctionEnd(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    rt.current_function = null;
}

fn opAccessChain(_: std.mem.Allocator, word_count: SpvWord, rt: *Runtime) RuntimeError!void {
    const var_type = try rt.it.next();
    const id = try rt.it.next();
    const base_id = try rt.it.next();

    const base = &rt.results[base_id];
    var value_ptr = try base.getValue();

    const index_count = word_count - 3;
    for (0..index_count) |_| {
        const member = &rt.results[try rt.it.next()];
        const member_value = switch (member.variant orelse return RuntimeError.InvalidSpirV) {
            .Constant => |c| &c,
            .Variable => |v| &v.value,
            else => return RuntimeError.InvalidSpirV,
        };
        switch (member_value.*) {
            .Int => |i| {
                switch (value_ptr.*) {
                    .Vector => |v| {
                        if (i.uint32 > v.len) return RuntimeError.InvalidSpirV;
                        value_ptr = &v[i.uint32];
                    },
                    .Matrix => |m| {
                        if (i.uint32 > m.len) return RuntimeError.InvalidSpirV;
                        value_ptr = &m[i.uint32];
                    },
                    .Array => |_| return RuntimeError.ToDo,
                    .Structure => |s| {
                        if (i.uint32 > s.len) return RuntimeError.InvalidSpirV;
                        value_ptr = &s[i.uint32];
                    },
                    else => return RuntimeError.InvalidSpirV,
                }
            },
            else => return RuntimeError.InvalidSpirV,
        }
    }

    rt.results[id].variant = .{
        .AccessChain = .{
            .target = var_type,
            .value = value_ptr.*,
        },
    };
}

fn opStore(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    const ptr_id = try rt.it.next();
    const val_id = try rt.it.next();
    copyValue(
        switch (rt.results[ptr_id].variant orelse return RuntimeError.InvalidSpirV) {
            .Variable => |*v| &v.value,
            .Constant => |*c| c,
            .AccessChain => |*a| &a.value,
            else => return RuntimeError.InvalidSpirV,
        },
        switch (rt.results[val_id].variant orelse return RuntimeError.InvalidSpirV) {
            .Variable => |v| &v.value,
            .Constant => |c| &c,
            .AccessChain => |a| &a.value,
            else => return RuntimeError.InvalidSpirV,
        },
    );
}

fn opLoad(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = rt.it.skip();
    const id = try rt.it.next();
    const ptr_id = try rt.it.next();
    copyValue(
        switch (rt.results[id].variant orelse return RuntimeError.InvalidSpirV) {
            .Variable => |*v| &v.value,
            .Constant => |*c| c,
            .AccessChain => |*a| &a.value,
            else => return RuntimeError.InvalidSpirV,
        },
        switch (rt.results[ptr_id].variant orelse return RuntimeError.InvalidSpirV) {
            .Variable => |v| &v.value,
            .Constant => |c| &c,
            .AccessChain => |a| &a.value,
            else => return RuntimeError.InvalidSpirV,
        },
    );
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

fn maths(comptime T: MathType) type {
    return struct {
        fn opMul(_: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
            const target_type = (rt.results[try rt.it.next()].variant orelse return RuntimeError.InvalidSpirV).Type;
            const value = try rt.results[try rt.it.next()].getValue();
            const op1_value = try rt.results[try rt.it.next()].getValue();
            const op2_value = try rt.results[try rt.it.next()].getValue();

            const size = sw: switch (target_type) {
                .Vector => |v| continue :sw (rt.results[v.components_type_word].variant orelse return RuntimeError.InvalidSpirV).Type,
                .Float => |f| if (T == .Float) f.bit_length else return RuntimeError.InvalidSpirV,
                .Int => |i| if (T == .SInt or T == .UInt) i.bit_length else return RuntimeError.InvalidSpirV,
                else => return RuntimeError.InvalidSpirV,
            };

            const operator = struct {
                fn process(bit_count: SpvWord, v: *Result.Value, op1_v: *const Result.Value, op2_v: *const Result.Value) RuntimeError!void {
                    switch (T) {
                        .Float => switch (bit_count) {
                            16 => v.Float.float16 = op1_v.Float.float16 * op2_v.Float.float16,
                            32 => v.Float.float32 = op1_v.Float.float32 * op2_v.Float.float32,
                            64 => v.Float.float64 = op1_v.Float.float64 * op2_v.Float.float64,
                            else => return RuntimeError.InvalidSpirV,
                        },
                        .SInt => switch (bit_count) {
                            8 => v.Int.sint8 = @mulWithOverflow(op1_v.Int.sint8, op2_v.Int.sint8)[0],
                            16 => v.Int.sint16 = @mulWithOverflow(op1_v.Int.sint16, op2_v.Int.sint16)[0],
                            32 => v.Int.sint32 = @mulWithOverflow(op1_v.Int.sint32, op2_v.Int.sint32)[0],
                            64 => v.Int.sint64 = @mulWithOverflow(op1_v.Int.sint64, op2_v.Int.sint64)[0],
                            else => return RuntimeError.InvalidSpirV,
                        },
                        .UInt => switch (bit_count) {
                            8 => v.Int.uint8 = @mulWithOverflow(op1_v.Int.uint8, op2_v.Int.uint8)[0],
                            16 => v.Int.uint16 = @mulWithOverflow(op1_v.Int.uint16, op2_v.Int.uint16)[0],
                            32 => v.Int.uint32 = @mulWithOverflow(op1_v.Int.uint32, op2_v.Int.uint32)[0],
                            64 => v.Int.uint64 = @mulWithOverflow(op1_v.Int.uint64, op2_v.Int.uint64)[0],
                            else => return RuntimeError.InvalidSpirV,
                        },
                    }
                }
            };

            switch (value.*) {
                .Float => if (T == .Float) try operator.process(size, value, op1_value, op2_value) else return RuntimeError.InvalidSpirV,
                .Int => if (T == .SInt or T == .UInt) try operator.process(size, value, op1_value, op2_value) else return RuntimeError.InvalidSpirV,
                .Vector => |vec| for (vec, op1_value.Vector, op2_value.Vector) |*val, op1_v, op2_v| try operator.process(size, val, &op1_v, &op2_v),
                else => return RuntimeError.InvalidSpirV,
            }
        }
    };
}

fn setupConstant(allocator: std.mem.Allocator, rt: *Runtime) RuntimeError!*Result {
    const res_type = try rt.it.next();
    const id = try rt.it.next();
    const target = &rt.mod.results[id];

    const resolved = rt.mod.results[res_type].resolveType(rt.mod.results);
    const member_count = resolved.getMemberCounts();
    if (member_count == 0) {
        return RuntimeError.InvalidSpirV;
    }
    target.variant = .{ .Constant = try Result.initValue(allocator, member_count, rt.mod.results, resolved) };
    return target;
}

fn autoSetupConstant(allocator: std.mem.Allocator, _: SpvWord, rt: *Runtime) RuntimeError!void {
    _ = try setupConstant(allocator, rt);
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
