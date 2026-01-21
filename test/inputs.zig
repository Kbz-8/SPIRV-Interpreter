const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Inputs" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f64, f32, i32, u32 };

    inline for (2..5) |L| {
        inline for (types) |T| {
            const input: case.Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };

            const shader = try std.fmt.allocPrint(
                allocator,
                \\ [nzsl_version("1.1")]
                \\ [feature(float64)]
                \\ module;
                \\
                \\ struct FragIn
                \\ {{
                \\     [location(0)] pos: vec{d}[{s}]
                \\ }}
                \\ 
                \\ struct FragOut
                \\ {{
                \\     [location(0)] color: vec{d}[{s}]
                \\ }}
                \\
                \\ [entry(frag)]
                \\ fn main(input: FragIn) -> FragOut
                \\ {{
                \\     let output: FragOut;
                \\     output.color = input.pos;
                \\     return output;
                \\ }}
            ,
                .{
                    L,
                    @typeName(T),
                    L,
                    @typeName(T),
                },
            );
            defer allocator.free(shader);
            const code = try compileNzsl(allocator, shader);
            defer allocator.free(code);
            try case.expectOutputWithInput(T, L, code, "color", &@as([L]T, input.val), "pos", &@as([L]T, input.val));
        }
    }
}
