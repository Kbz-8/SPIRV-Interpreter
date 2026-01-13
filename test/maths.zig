const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Mul vec4" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, i32 };
    inline for (types) |T| {
        const prng: std.Random.DefaultPrng = .init(@intCast(std.time.microTimestamp()));

        const base_color: [4]T = undefined;
        std.Random.shuffle(prng, T, base_color);
        const ratio: [4]T = undefined;
        std.Random.shuffle(prng, T, ratio);

        const expected = [4]T{
            base_color[0] * ratio[0],
            base_color[1] * ratio[1],
            base_color[2] * ratio[2],
            base_color[3] * ratio[3],
        };

        const shader = try std.fmt.allocPrint(
            allocator,
            \\ [nzsl_version("1.1")]
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
        );
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);

        try case.expectOutput(f32, code, "color", &expected);
    }
}
