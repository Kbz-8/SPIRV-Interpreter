const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Mul vec4" {
    const allocator = std.testing.allocator;
    const types = [_]type{
        f32,
        //f64,
        i32,
        u32,
    };

    inline for (types) |T| {
        const base_color = case.random(@Vector(4, T));
        const ratio = case.random(@Vector(4, T));
        const expected = switch (@typeInfo(T)) {
            .float => base_color * ratio,
            .int => @mulWithOverflow(base_color, ratio)[0],
            else => unreachable,
        };

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
            \\     let ratio = vec4[{s}]({d}, {d}, {d}, {d});
            \\
            \\     let output: FragOut;
            \\     output.color = vec4[{s}]({d}, {d}, {d}, {d}) * ratio;
            \\     return output;
            \\ }}
        ,
            .{
                @typeName(T),
                @typeName(T),
                ratio[0],
                ratio[1],
                ratio[2],
                ratio[3],
                @typeName(T),
                base_color[0],
                base_color[1],
                base_color[2],
                base_color[3],
            },
        );
        defer allocator.free(shader);
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);
        try case.expectOutput(T, 4, code, "color", &@as([4]T, expected));
    }
}
