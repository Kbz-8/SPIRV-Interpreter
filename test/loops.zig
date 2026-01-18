const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Simple while loop" {
    const allocator = std.testing.allocator;
    const base = @mod(case.random(f32), 5.0);
    const iterations = 5;

    var expected = base;
    for (1..iterations) |i| {
        expected *= @floatFromInt(i);
    }

    const shader = try std.fmt.allocPrint(
        allocator,
        \\ [nzsl_version("1.1")]
        \\ [feature(float64)]
        \\ module;
        \\ 
        \\ struct FragOut
        \\ {{
        \\     [location(0)] color: vec4[f32]
        \\ }}
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {{
        \\     let value = f32({d});
        \\     let i = 1;
        \\     while (i < {d})
        \\     {{
        \\         value *= f32(i);
        \\         i += 1;
        \\     }}
        \\     let output: FragOut;
        \\     output.color = vec4[f32](value, value, value, value);
        \\     return output;
        \\ }}
    ,
        .{
            base,
            iterations,
        },
    );
    defer allocator.free(shader);
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);
    try case.expectOutput(f32, 4, code, "color", &.{ expected, expected, expected, expected });
}
