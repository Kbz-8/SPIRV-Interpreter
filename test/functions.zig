const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Simple function calls" {
    const allocator = std.testing.allocator;
    const types = [_]type{ i32, u32, f32, f64 };

    inline for (types) |T| {
        const n = case.random(T);

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
            \\ fn value() -> {s}
            \\ {{
            \\     return {d};
            \\ }}
            \\
            \\ [entry(frag)]
            \\ fn main() -> FragOut
            \\ {{
            \\     let output: FragOut;
            \\     output.color = vec4[{s}](value(), value(), value(), value());
            \\     return output;
            \\ }}
        ,
            .{
                @typeName(T),
                @typeName(T),
                n,
                @typeName(T),
            },
        );
        defer allocator.free(shader);
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);
        try case.expectOutput(T, 4, code, "color", &.{ n, n, n, n });
    }
}

test "Nested function calls" {
    const allocator = std.testing.allocator;
    const types = [_]type{ i32, u32, f32, f64 };

    inline for (types) |T| {
        const n = case.random(T);

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
            \\ fn deepValue() -> {s}
            \\ {{
            \\     return {d};
            \\ }}
            \\
            \\ fn value() -> {s}
            \\ {{
            \\     return deepValue();
            \\ }}
            \\
            \\ [entry(frag)]
            \\ fn main() -> FragOut
            \\ {{
            \\     let output: FragOut;
            \\     output.color = vec4[{s}](value(), value(), value(), value());
            \\     return output;
            \\ }}
        ,
            .{
                @typeName(T),
                @typeName(T),
                n,
                @typeName(T),
                @typeName(T),
            },
        );
        defer allocator.free(shader);
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);
        try case.expectOutput(T, 4, code, "color", &.{ n, n, n, n });
    }
}
