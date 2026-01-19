const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

const Operations = enum {
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    ShiftRightArithmetic,
};

test "Bitwise primitives" {
    const allocator = std.testing.allocator;
    const types = [_]type{ i32, u32 };
    var operations = std.EnumMap(Operations, []const u8).init(.{
        .BitwiseAnd = "&",
        .BitwiseOr = "|",
        .BitwiseXor = "^",
        .ShiftLeft = "<<",
        .ShiftRight = ">>",
        .ShiftRightArithmetic = ">>",
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (types) |T| {
            const op1: T = case.random(T);
            const op2: T = @mod(case.random(T), @bitSizeOf(T));
            const expected = switch (op.key) {
                .BitwiseAnd => op1 & op2,
                .BitwiseOr => op1 | op2,
                .BitwiseXor => op1 ^ op2,
                .ShiftLeft => op1 << @intCast(op2),
                .ShiftRight, .ShiftRightArithmetic => op1 >> @intCast(op2),
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
                \\     let op1: {s} = {d};
                \\     let op2: {s} = {d};
                \\     let color = op1 {s} op2;
                \\
                \\     let output: FragOut;
                \\     output.color = vec4[{s}](color, color, color, color);
                \\     return output;
                \\ }}
            ,
                .{
                    @typeName(T),
                    @typeName(T),
                    op1,
                    @typeName(T),
                    op2,
                    op.value.*,
                    @typeName(T),
                },
            );
            defer allocator.free(shader);
            const code = try compileNzsl(allocator, shader);
            defer allocator.free(code);
            try case.expectOutput(T, 4, code, "color", &.{ expected, expected, expected, expected });
        }
    }
}

test "Bitwise vectors" {
    const allocator = std.testing.allocator;
    const types = [_]type{ i32, u32 };
    var operations = std.EnumMap(Operations, []const u8).init(.{
        .BitwiseAnd = "&",
        .BitwiseOr = "|",
        .BitwiseXor = "^",
        .ShiftLeft = "<<",
        .ShiftRight = ">>",
        .ShiftRightArithmetic = ">>",
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (2..5) |L| {
            inline for (types) |T| {
                const op1: case.Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };
                var op2: case.Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };
                for (0..L) |i| op2.val[i] = @mod(op2.val[i], @bitSizeOf(T));
                const expected = switch (op.key) {
                    .BitwiseAnd => op1.val & op2.val,
                    .BitwiseOr => op1.val | op2.val,
                    .BitwiseXor => op1.val ^ op2.val,
                    .ShiftLeft => op1.val << @intCast(op2.val),
                    .ShiftRight, .ShiftRightArithmetic => op1.val >> @intCast(op2.val),
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
                    \\     let op1 = vec{d}[{s}]({f});
                    \\     let op2 = vec{d}[{s}]({f});
                    \\
                    \\     let output: FragOut;
                    \\     output.color = op1 {s} op2;
                    \\     return output;
                    \\ }}
                ,
                    .{
                        L,
                        @typeName(T),
                        L,
                        @typeName(T),
                        op1,
                        L,
                        @typeName(T),
                        op2,
                        op.value.*,
                    },
                );
                defer allocator.free(shader);
                const code = try compileNzsl(allocator, shader);
                defer allocator.free(code);
                try case.expectOutput(T, L, code, "color", &@as([L]T, expected));
            }
        }
    }
}
