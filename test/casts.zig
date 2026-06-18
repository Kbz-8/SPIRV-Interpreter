const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Primitives casts" {
    const allocator = std.testing.allocator;
    const types = [_][2]type{
        [2]type{ f32, u32 },
        [2]type{ f32, i32 },
        [2]type{ u32, f32 },
        [2]type{ i32, f32 },
        [2]type{ f32, f64 },
        [2]type{ f64, f32 },
        [2]type{ f64, u32 },
        [2]type{ f64, i32 },
        [2]type{ u32, f64 },
        [2]type{ i32, f64 },
    };

    inline for (types) |T| {
        const base = case.random(T[0]);
        const expected = std.math.lossyCast(T[1], base);

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
            \\     let base = {s}({d});
            \\     let color = {s}(base);
            \\
            \\     let output: FragOut;
            \\     output.color = vec4[{s}](color, color, color, color);
            \\     return output;
            \\ }}
        ,
            .{
                @typeName(T[1]),
                @typeName(T[0]),
                base,
                @typeName(T[1]),
                @typeName(T[1]),
            },
        );
        defer allocator.free(shader);
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);
        try case.expect(.{
            .source = code,
            .expected_outputs = &.{
                std.mem.asBytes(&[_]T[1]{ expected, expected, expected, expected }),
            },
        });
    }
}

test "Vector float casts" {
    const allocator = std.testing.allocator;
    const cases = [_]struct {
        len: usize,
        source_type: []const u8,
        target_type: []const u8,
        values: []const f64,
    }{
        .{ .len = 2, .source_type = "f32", .target_type = "f64", .values = &.{ 1.25, -2.5 } },
        .{ .len = 3, .source_type = "f32", .target_type = "f64", .values = &.{ 1.25, -2.5, 3.75 } },
        .{ .len = 4, .source_type = "f32", .target_type = "f64", .values = &.{ 1.25, -2.5, 3.75, -4.5 } },
        .{ .len = 2, .source_type = "f64", .target_type = "f32", .values = &.{ 1.25, -2.5 } },
        .{ .len = 3, .source_type = "f64", .target_type = "f32", .values = &.{ 1.25, -2.5, 3.75 } },
        .{ .len = 4, .source_type = "f64", .target_type = "f32", .values = &.{ 1.25, -2.5, 3.75, -4.5 } },
    };

    inline for (cases) |c| {
        const constructor_values = try std.fmt.allocPrint(
            allocator,
            switch (c.len) {
                2 => "{d}, {d}",
                3 => "{d}, {d}, {d}",
                4 => "{d}, {d}, {d}, {d}",
                else => unreachable,
            },
            switch (c.len) {
                2 => .{ c.values[0], c.values[1] },
                3 => .{ c.values[0], c.values[1], c.values[2] },
                4 => .{ c.values[0], c.values[1], c.values[2], c.values[3] },
                else => unreachable,
            },
        );
        defer allocator.free(constructor_values);

        const output_values = switch (c.len) {
            2 => "casted.x, casted.y, 0.0, 1.0",
            3 => "casted.x, casted.y, casted.z, 1.0",
            4 => "casted.x, casted.y, casted.z, casted.w",
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
            \\     let base = vec{d}[{s}]({s});
            \\     let casted = vec{d}[{s}](base);
            \\
            \\     let output: FragOut;
            \\     output.color = vec4[{s}]({s});
            \\     return output;
            \\ }}
        ,
            .{
                c.target_type,
                c.len,
                c.source_type,
                constructor_values,
                c.len,
                c.target_type,
                c.target_type,
                output_values,
            },
        );
        defer allocator.free(shader);

        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);

        if (std.mem.eql(u8, c.target_type, "f32")) {
            var expected = [_]f32{ 0.0, 0.0, 0.0, 1.0 };
            for (c.values, 0..) |value, i| expected[i] = @floatCast(value);
            try case.expect(.{
                .source = code,
                .expected_outputs = &.{
                    std.mem.asBytes(&expected),
                },
            });
        } else {
            var expected = [_]f64{ 0.0, 0.0, 0.0, 1.0 };
            for (c.values, 0..) |value, i| expected[i] = @floatCast(value);
            try case.expect(.{
                .source = code,
                .expected_outputs = &.{
                    std.mem.asBytes(&expected),
                },
            });
        }
    }
}

test "Primitives bitcasts" {
    const allocator = std.testing.allocator;
    const types = [_][2]type{
        [2]type{ u32, i32 },
        [2]type{ i32, u32 },
    };

    inline for (types) |T| {
        const base = case.random(T[0]);
        const expected = @as(T[1], @bitCast(base));

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
            \\     let base = {s}({d});
            \\     let color = {s}(base);
            \\
            \\     let output: FragOut;
            \\     output.color = vec4[{s}](color, color, color, color);
            \\     return output;
            \\ }}
        ,
            .{
                @typeName(T[1]),
                @typeName(T[0]),
                base,
                @typeName(T[1]),
                @typeName(T[1]),
            },
        );
        defer allocator.free(shader);
        const code = try compileNzsl(allocator, shader);
        defer allocator.free(code);
        try case.expect(.{
            .source = code,
            .expected_outputs = &.{
                std.mem.asBytes(&[_]T[1]{ expected, expected, expected, expected }),
            },
        });
    }
}

test "Cast chain" {
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
        \\     let v = vec4[f32](1.25, 2.75, -3.25, 4.5);
        \\     let a = vec4[i32](v);
        \\     let output: FragOut;
        \\     output.color = vec4[i32](a.x, a.y, a.z, a.w);
        \\     return output;
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    try case.expect(.{
        .source = code,
        .expected_outputs = &.{
            std.mem.asBytes(&[_]i32{ 1, 2, -3, 4 }),
        },
    });
}
