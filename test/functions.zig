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

test "Recursive function calls" {
    const allocator = std.testing.allocator;
    const types = [_]type{ i32, u32, f32, f64 };

    inline for (types) |T| {
        const iterations = 10;

        const fib = struct {
            fn onacci(n: T) T {
                if (n <= 0) return n;
                return onacci(n - 1) + onacci(n - 2);
            }
        };
        const expected = fib.onacci(iterations);

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
            \\ fn fibonacci(n: {s}) -> {s}
            \\ {{
            \\     if (n <= {s}(1)) return n;
            \\     return fibonacci(n - {s}(1)) + fibonacci(n - {s}(2));
            \\ }}
            \\
            \\ [entry(frag)]
            \\ fn main() -> FragOut
            \\ {{
            \\     let output: FragOut;
            \\     output.color = vec4[{s}](fibonacci({d}), fibonacci({d}), fibonacci({d}), fibonacci({d}));
            \\     return output;
            \\ }}
        ,
            .{
                @typeName(T),
                @typeName(T),
                @typeName(T),
                @typeName(T),
                @typeName(T),
                @typeName(T),
                @typeName(T),
                iterations,
                iterations,
                iterations,
                iterations,
            },
        );
        defer allocator.free(shader);
        std.debug.print("{s}\n\n", .{shader});
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);
        try case.expectOutput(T, 4, code, "color", &.{ expected, expected, expected, expected });
    }
}
