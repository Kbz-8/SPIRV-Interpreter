const std = @import("std");
const root = @import("root.zig");
const zm = @import("zmath");
const compileNzsl = root.compileNzsl;
const case = root.case;

const Operations = enum {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
};

// Tests all mathematical operation on all NZSL supported primitive types
test "Maths primitives" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, f64, i32, u32 };
    var operations = std.EnumMap(Operations, u8).init(.{
        .Add = '+',
        .Sub = '-',
        .Mul = '*',
        .Div = '/',
        .Mod = '%',
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (types) |T| {
            const base: T = case.random(T);
            const ratio: T = case.random(T);
            const expected = switch (op.key) {
                .Add => if (@typeInfo(T) == .int) @addWithOverflow(base, ratio)[0] else base + ratio,
                .Sub => if (@typeInfo(T) == .int) @subWithOverflow(base, ratio)[0] else base - ratio,
                .Mul => if (@typeInfo(T) == .int) @mulWithOverflow(base, ratio)[0] else base * ratio,
                .Div => if (@typeInfo(T) == .int) @divTrunc(base, ratio) else base / ratio,
                .Mod => @mod(base, ratio),
            };

            const shader = try std.fmt.allocPrint(
                allocator,
                \\ [nzsl_version("1.1")]
                \\ [feature(float64)]
                \\ module;
                \\ 
                \\ struct FragOut
                \\ {{
                \\     [location(0)] color: vec4[{s}]
                \\ }}
                \\
                \\ [entry(frag)]
                \\ fn main() -> FragOut
                \\ {{
                \\     let ratio: {s} = {d};
                \\     let base: {s} = {d};
                \\     let color = base {c} ratio;
                \\
                \\     let output: FragOut;
                \\     output.color = vec4[{s}](color, color, color, color);
                \\     return output;
                \\ }}
            ,
                .{
                    @typeName(T),
                    @typeName(T),
                    ratio,
                    @typeName(T),
                    base,
                    op.value.*,
                    @typeName(T),
                },
            );
            defer allocator.free(shader);
            const code = try compileNzsl(allocator, shader);
            defer allocator.free(code);
            try case.expect(.{
                .source = code,
                .expected_outputs = &.{
                    std.mem.asBytes(&[_]T{ expected, expected, expected, expected }),
                },
            });
        }
    }
}

// Tests all mathematical operation on vec2/3/4 with all NZSL supported primitive types
test "Maths vectors" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, f64, i32, u32 };
    var operations = std.EnumMap(Operations, u8).init(.{
        .Add = '+',
        .Sub = '-',
        .Mul = '*',
        .Div = '/',
        .Mod = '%',
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (2..5) |L| {
            inline for (types) |T| {
                const base_color: case.Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };
                const ratio: case.Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };
                const expected = switch (op.key) {
                    .Add => if (@typeInfo(T) == .int) @addWithOverflow(base_color.val, ratio.val)[0] else base_color.val + ratio.val,
                    .Sub => if (@typeInfo(T) == .int) @subWithOverflow(base_color.val, ratio.val)[0] else base_color.val - ratio.val,
                    .Mul => if (@typeInfo(T) == .int) @mulWithOverflow(base_color.val, ratio.val)[0] else base_color.val * ratio.val,
                    .Div => if (@typeInfo(T) == .int) @divTrunc(base_color.val, ratio.val) else base_color.val / ratio.val,
                    .Mod => @mod(base_color.val, ratio.val),
                };

                const shader = try std.fmt.allocPrint(
                    allocator,
                    \\ [nzsl_version("1.1")]
                    \\ [feature(float64)]
                    \\ module;
                    \\ 
                    \\ struct FragOut
                    \\ {{
                    \\     [location(0)] color: vec{d}[{s}]
                    \\ }}
                    \\
                    \\ [entry(frag)]
                    \\ fn main() -> FragOut
                    \\ {{
                    \\     let ratio = vec{d}[{s}]({f});
                    \\
                    \\     let output: FragOut;
                    \\     output.color = vec{d}[{s}]({f}) {c} ratio;
                    \\     return output;
                    \\ }}
                ,
                    .{
                        L,
                        @typeName(T),
                        L,
                        @typeName(T),
                        ratio,
                        L,
                        @typeName(T),
                        base_color,
                        op.value.*,
                    },
                );
                defer allocator.free(shader);
                const code = try compileNzsl(allocator, shader);
                defer allocator.free(code);
                try case.expect(.{
                    .source = code,
                    .expected_outputs = &.{
                        std.mem.asBytes(&@as([L]T, expected)),
                    },
                });
            }
        }
    }
}

// Tests all mathematical operation on vec2/3/4 with scalars with all NZSL supported primitive types
test "Maths vectors with scalars" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, f64, i32, u32 };
    var operations = std.EnumMap(Operations, u8).init(.{
        .Mul = '*',
        .Div = '/',
        .Mod = '%',
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (2..5) |L| {
            inline for (types) |T| {
                const base_color: case.Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };
                const ratio = case.random(T);
                const splat_ratio = @as(@Vector(L, T), @splat(ratio));
                const expected = switch (op.key) {
                    .Mul => if (@typeInfo(T) == .int) @mulWithOverflow(base_color.val, splat_ratio)[0] else base_color.val * splat_ratio,
                    .Div => if (@typeInfo(T) == .int) @divTrunc(base_color.val, splat_ratio) else base_color.val / splat_ratio,
                    .Mod => @mod(base_color.val, splat_ratio),
                    else => unreachable,
                };

                const shader = try std.fmt.allocPrint(
                    allocator,
                    \\ [nzsl_version("1.1")]
                    \\ [feature(float64)]
                    \\ module;
                    \\ 
                    \\ struct FragOut
                    \\ {{
                    \\     [location(0)] color: vec{d}[{s}]
                    \\ }}
                    \\
                    \\ [entry(frag)]
                    \\ fn main() -> FragOut
                    \\ {{
                    \\     let output: FragOut;
                    \\     output.color = vec{d}[{s}]({f}) {c} {d};
                    \\     return output;
                    \\ }}
                ,
                    .{
                        L,
                        @typeName(T),
                        L,
                        @typeName(T),
                        base_color,
                        op.value.*,
                        ratio,
                    },
                );
                defer allocator.free(shader);
                const code = try compileNzsl(allocator, shader);
                defer allocator.free(code);
                try case.expect(.{
                    .source = code,
                    .expected_outputs = &.{
                        std.mem.asBytes(&@as([L]T, expected)),
                    },
                });
            }
        }
    }
}

// Tests all mathematical operation on mat3/4 with all NZSL supported primitive types
test "Maths matrices" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, f64 };
    var operations = std.EnumMap(Operations, u8).init(.{
        .Add = '+',
        .Sub = '-',
        .Mul = '*',
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (3..5) |L| {
            inline for (types) |T| {
                const base: case.Mat(L, T) = .{ .val = case.random([L][L]T) };
                const ratio: case.Mat(L, T) = .{ .val = case.random([L][L]T) };
                var expected: case.Mat(L, T) = undefined;
                for (expected.val[0..], base.val[0..], ratio.val[0..]) |*ec, bc, rc| {
                    for (ec[0..], bc[0..], rc[0..]) |*e, b, r| {
                        e.* = switch (op.key) {
                            .Add => b + r,
                            .Sub => b - r,
                            .Mul => b * r,
                            else => unreachable,
                        };
                    }
                }

                const shader = try std.fmt.allocPrint(
                    allocator,
                    \\ [nzsl_version("1.1")]
                    \\ [feature(float64)]
                    \\ module;
                    \\ 
                    \\ struct FragOut
                    \\ {{
                    \\     [location(0)] value: mat{d}[{s}]
                    \\ }}
                    \\
                    \\ [entry(frag)]
                    \\ fn main() -> FragOut
                    \\ {{
                    \\     let output: FragOut;
                    \\     output.value = mat{d}[{s}]({f}) {c} mat{d}[{s}]({f});
                    \\     return output;
                    \\ }}
                ,
                    .{
                        L,
                        @typeName(T),
                        L,
                        @typeName(T),
                        base,
                        op.value.*,
                        L,
                        @typeName(T),
                        ratio,
                    },
                );
                defer allocator.free(shader);
                const code = try compileNzsl(allocator, shader);
                defer allocator.free(code);
                try case.expect(.{
                    .source = code,
                    .expected_outputs = &.{
                        std.mem.asBytes(&expected),
                    },
                });
            }
        }
    }
}

// Tests all mathematical operation on mat3/4 with all NZSL supported vectors
test "Maths matrices with vectors" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, f64 };

    inline for (3..5) |L| {
        inline for (types) |T| {
            const base: case.Mat(L, T) = .{ .val = case.random([L][L]T) };
            const ratio: case.Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };
            var expected: @Vector(L, T) = undefined;

            expected[0] = (base.val[0][0] * ratio.val[0]) + (base.val[0][1] * ratio.val[1]) + (base.val[0][2] * ratio.val[2]) + if (L == 4) (base.val[0][3] * ratio.val[3]) else 0.0;
            expected[1] = (base.val[1][0] * ratio.val[0]) + (base.val[1][1] * ratio.val[1]) + (base.val[1][2] * ratio.val[2]) + if (L == 4) (base.val[1][3] * ratio.val[3]) else 0.0;
            expected[2] = (base.val[2][0] * ratio.val[0]) + (base.val[2][1] * ratio.val[1]) + (base.val[2][2] * ratio.val[2]) + if (L == 4) (base.val[2][3] * ratio.val[3]) else 0.0;
            if (L == 4)
                expected[3] = (base.val[3][0] * ratio.val[0]) + (base.val[3][1] * ratio.val[1]) + (base.val[3][2] * ratio.val[2]) + (base.val[3][3] * ratio.val[3]);

            const shader = try std.fmt.allocPrint(
                allocator,
                \\ [nzsl_version("1.1")]
                \\ [feature(float64)]
                \\ module;
                \\ 
                \\ struct FragOut
                \\ {{
                \\     [location(0)] value: vec{d}[{s}]
                \\ }}
                \\
                \\ [entry(frag)]
                \\ fn main() -> FragOut
                \\ {{
                \\     let output: FragOut;
                \\     output.value = mat{d}[{s}]({f}) * vec{d}[{s}]({f});
                \\     return output;
                \\ }}
            ,
                .{
                    L,
                    @typeName(T),
                    L,
                    @typeName(T),
                    base,
                    L,
                    @typeName(T),
                    ratio,
                },
            );
            defer allocator.free(shader);
            const code = try compileNzsl(allocator, shader);
            defer allocator.free(code);
            try case.expect(.{
                .source = code,
                .expected_outputs = &.{
                    std.mem.asBytes(&@as([L]T, expected)),
                },
            });
        }
    }
}
