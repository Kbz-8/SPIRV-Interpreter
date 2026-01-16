const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

const Operations = enum {
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
};

test "Simple branching" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, f64, i32, u32 };
    var operations = std.EnumMap(Operations, []const u8).init(.{
        .Equal = "==",
        .NotEqual = "!=",
        .Greater = ">",
        .GreaterEqual = ">=",
        .Less = "<",
        .LessEqual = "<=",
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (types) |T| {
            const values = [_][2]T{
                [2]T{ std.math.lossyCast(T, 0), std.math.lossyCast(T, 9) },
                [2]T{ std.math.lossyCast(T, 1), std.math.lossyCast(T, 8) },
                [2]T{ std.math.lossyCast(T, 2), std.math.lossyCast(T, 7) },
                [2]T{ std.math.lossyCast(T, 3), std.math.lossyCast(T, 6) },
                [2]T{ std.math.lossyCast(T, 4), std.math.lossyCast(T, 5) },
                [2]T{ std.math.lossyCast(T, 5), std.math.lossyCast(T, 4) },
                [2]T{ std.math.lossyCast(T, 6), std.math.lossyCast(T, 3) },
                [2]T{ std.math.lossyCast(T, 7), std.math.lossyCast(T, 2) },
                [2]T{ std.math.lossyCast(T, 8), std.math.lossyCast(T, 1) },
                [2]T{ std.math.lossyCast(T, 9), std.math.lossyCast(T, 0) },
                [2]T{ std.math.lossyCast(T, 0), std.math.lossyCast(T, 0) },
            };
            for (values) |v| {
                const op1: T = v[0];
                const op2: T = v[1];
                const expected = switch (op.key) {
                    .Equal => if (op1 == op2) op1 else op2,
                    .NotEqual => if (op1 != op2) op1 else op2,
                    .Greater => if (op1 > op2) op1 else op2,
                    .GreaterEqual => if (op1 >= op2) op1 else op2,
                    .Less => if (op1 < op2) op1 else op2,
                    .LessEqual => if (op1 <= op2) op1 else op2,
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
                    \\     let op1 = {s}({d});
                    \\     let op2 = {s}({d});
                    \\     let color: {s};
                    \\     if (op1 {s} op2)
                    \\         color = op1;
                    \\     else
                    \\         color = op2;
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
                        @typeName(T),
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
}
