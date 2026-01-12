const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "FMul vec4[f32]" {
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
        \\     let ratio = vec4[f32](2.0, 2.0, 8.0, 0.25);
        \\
        \\     let output: FragOut;
        \\     output.color = vec4[f32](4.0, 3.0, 2.0, 1.0) * ratio;
        \\     return output;
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    try case.expectOutput(f32, code, "color", &.{ 8, 6, 16, 0.25 });
}

test "IMul vec4[i32]" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\ 
        \\ struct FragOut
        \\ {
        \\     [location(0)] color: vec4[i32]
        \\ }
        \\
        \\ [entry(frag)]
        \\ fn main() -> FragOut
        \\ {
        \\     let ratio = vec4[i32](2, 2, 8, 25);
        \\
        \\     let output: FragOut;
        \\     output.color = vec4[i32](4, 3, 2, 1) * ratio;
        \\     return output;
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    try case.expectOutput(i32, code, "color", &.{ 8, 6, 16, 25 });
}
