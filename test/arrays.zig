const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Simple array" {
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
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let value = array[f32](4.0, 3.0, 2.0, 1.0);
        \\     let output: FragOut;
        \\     output.color = vec4[f32](value[0], value[1], value[2], value[3]);
        \\     return output;
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    try case.expect(.{
        .source = code,
        .expected_outputs = &.{
            std.mem.asBytes(&[_]f32{ 4, 3, 2, 1 }),
        },
    });
}

test "Array fold" {
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
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let values = array[f32](1.0, 2.0, 3.0, 4.0);
        \\     let sum = 0.0;
        \\     let weighted = 0.0;
        \\     for i in u32(0) -> values.Size()
        \\     {
        \\         sum += values[i];
        \\         weighted += values[i] * f32(i + 1);
        \\     }
        \\
        \\     let output: FragOut;
        \\     output.color = vec4[f32](sum, weighted, values[2], f32(values.Size()));
        \\     return output;
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    try case.expect(.{
        .source = code,
        .expected_outputs = &.{
            std.mem.asBytes(&[_]f32{ 10.0, 30.0, 3.0, 4.0 }),
        },
    });
}
