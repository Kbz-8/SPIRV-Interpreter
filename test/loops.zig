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
    try case.expect(.{
        .source = code,
        .expected_outputs = &.{
            std.mem.asBytes(&[_]f32{ expected, expected, expected, expected }),
        },
    });
}

test "For filter" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[u32]
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let even_sum: u32 = 0;
        \\     let odd_sum: u32 = 0;
        \\     let product: u32 = 1;
        \\     for i in u32(1) -> u32(8)
        \\     {
        \\         if ((i % u32(2)) == u32(0))
        \\         {
        \\             even_sum += i;
        \\             product *= i;
        \\         }
        \\         else
        \\             odd_sum += i;
        \\     }
        \\
        \\     let output: FragOut;
        \\     output.color = vec4[u32](even_sum, odd_sum, product, even_sum + odd_sum);
        \\     return output;
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    try case.expect(.{
        .source = code,
        .expected_outputs = &.{
            std.mem.asBytes(&[_]u32{ 12, 16, 48, 28 }),
        },
    });
}
