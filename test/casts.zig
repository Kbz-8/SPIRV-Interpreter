const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Primitives casts" {
    const allocator = std.testing.allocator;
    const types = [_][2]type{
        [2]type{ f32, u32 },
        [2]type{ f32, i32 },
        [2]type{ u32, f32 },
        [2]type{ i32, f32 },
        [2]type{ f32, f64 },
        [2]type{ f64, f32 },
        [2]type{ f64, u32 },
        [2]type{ f64, i32 },
        [2]type{ u32, f64 },
        [2]type{ i32, f64 },
    };

    inline for (types) |T| {
        const base = case.random(T[0]);
        const expected = std.math.lossyCast(T[1], base);

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
            \\     let base = {s}({d});
            \\     let color = {s}(base);
            \\
            \\     let output: FragOut;
            \\     output.color = vec4[{s}](color, color, color, color);
            \\     return output;
            \\ }}
        ,
            .{
                @typeName(T[1]),
                @typeName(T[0]),
                base,
                @typeName(T[1]),
                @typeName(T[1]),
            },
        );
        defer allocator.free(shader);
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);
        try case.expectOutput(T[1], 4, code, "color", &.{ expected, expected, expected, expected });
    }
}

test "Primitives bitcasts" {
    const allocator = std.testing.allocator;
    const types = [_][2]type{
        [2]type{ u32, i32 },
        [2]type{ i32, u32 },
    };

    inline for (types) |T| {
        const base = case.random(T[0]);
        const expected = @as(T[1], @bitCast(base));

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
            \\     let base = {s}({d});
            \\     let color = {s}(base);
            \\
            \\     let output: FragOut;
            \\     output.color = vec4[{s}](color, color, color, color);
            \\     return output;
            \\ }}
        ,
            .{
                @typeName(T[1]),
                @typeName(T[0]),
                base,
                @typeName(T[1]),
                @typeName(T[1]),
            },
        );
        defer allocator.free(shader);
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);
        try case.expectOutput(T[1], 4, code, "color", &.{ expected, expected, expected, expected });
    }
}
