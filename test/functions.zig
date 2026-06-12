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
        try case.expect(.{
            .source = code,
            .expected_outputs = &.{
                std.mem.asBytes(&[_]T{ n, n, n, n }),
            },
        });
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
        try case.expect(.{
            .source = code,
            .expected_outputs = &.{
                std.mem.asBytes(&[_]T{ n, n, n, n }),
            },
        });
    }
}

test "Function params" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ fn affine(value: f32, scale: f32, bias: f32) -> f32
        \\ {
        \\     return value * scale + bias;
        \\ }
        \\
        \\ fn combine(a: vec2[f32], b: vec2[f32]) -> vec4[f32]
        \\ {
        \\     let left = affine(a.x, b.x, b.y);
        \\     let right = affine(a.y, b.y, b.x);
        \\     return vec4[f32](left, right, left + right, left - right);
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let output: FragOut;
        \\     output.color = combine(vec2[f32](2.0, 3.0), vec2[f32](4.0, 5.0));
        \\     return output;
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    try case.expect(.{
        .source = code,
        .expected_outputs = &.{
            std.mem.asBytes(&[_]f32{ 13.0, 19.0, 32.0, -6.0 }),
        },
    });
}

test "Struct logic" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct Pair
        \\ {
        \\     a: f32,
        \\     b: f32
        \\ }
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[f32]
        \\ }
        \\
        \\ fn eval(pair: Pair) -> vec2[f32]
        \\ {
        \\     return vec2[f32](pair.a + pair.b, pair.a * pair.b);
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let pair: Pair;
        \\     pair.a = 3.0;
        \\     pair.b = 4.0;
        \\     let v = eval(pair);
        \\     let output: FragOut;
        \\     output.color = vec4[f32](v.x, v.y, v.y - v.x, v.x + v.y);
        \\     return output;
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    try case.expect(.{
        .source = code,
        .expected_outputs = &.{
            std.mem.asBytes(&[_]f32{ 7.0, 12.0, 5.0, 19.0 }),
        },
    });
}
