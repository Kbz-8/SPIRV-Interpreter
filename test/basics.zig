const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Simple fragment shader" {
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
        \\     let output: FragOut;
        \\     output.color = vec4[f32](4.0, 3.0, 2.0, 1.0);
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
