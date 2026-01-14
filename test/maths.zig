const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

const Operations = enum {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
};

fn Vec(comptime len: usize, comptime T: type) type {
    return struct {
        const Self = @This();
        val: @Vector(len, T),
        pub fn format(self: *const Self, w: *std.Io.Writer) std.Io.Writer.Error!void {
            inline for (0..len) |i| {
                try w.print("{d}", .{self.val[i]});
                if (i < len - 1) try w.writeAll(", ");
            }
        }
    };
}

// Tests all mathematical operation on all NZSL supported primitive types
test "Maths primitives" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, f64, i32, u32 };
    var operations = std.EnumMap(Operations, u8).init(.{
        .Add = '+',
        .Sub = '-',
        .Mul = '*',
        .Div = '/',
        .Mod = '%',
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (types) |T| {
            const base: T = case.random(T);
            const ratio: T = case.random(T);
            const expected = switch (op.key) {
                .Add => if (@typeInfo(T) == .int) @addWithOverflow(base, ratio)[0] else base + ratio,
                .Sub => if (@typeInfo(T) == .int) @subWithOverflow(base, ratio)[0] else base - ratio,
                .Mul => if (@typeInfo(T) == .int) @mulWithOverflow(base, ratio)[0] else base * ratio,
                .Div => if (@typeInfo(T) == .int) @divTrunc(base, ratio) else base / ratio,
                .Mod => @mod(base, ratio),
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
                \\     let ratio: {s} = {d};
                \\     let base: {s} = {d};
                \\     let color = base {c} ratio;
                \\
                \\     let output: FragOut;
                \\     output.color = vec4[{s}](color, color, color, color);
                \\     return output;
                \\ }}
            ,
                .{
                    @typeName(T),
                    @typeName(T),
                    ratio,
                    @typeName(T),
                    base,
                    op.value.*,
                    @typeName(T),
                },
            );
            defer allocator.free(shader);
            const code = try compileNzsl(allocator, shader);
            defer allocator.free(code);
            try case.expectOutput(T, 4, code, "color", &.{ expected, expected, expected, expected });
        }
    }
}

// Tests all mathematical operation on vec2/3/4 with all NZSL supported primitive types
test "Maths vectors" {
    const allocator = std.testing.allocator;
    const types = [_]type{ f32, f64, i32, u32 };
    var operations = std.EnumMap(Operations, u8).init(.{
        .Add = '+',
        .Sub = '-',
        .Mul = '*',
        .Div = '/',
        .Mod = '%',
    });

    var it = operations.iterator();
    while (it.next()) |op| {
        inline for (2..5) |L| {
            inline for (types) |T| {
                const base_color: Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };
                const ratio: Vec(L, T) = .{ .val = case.random(@Vector(L, T)) };
                const expected = switch (op.key) {
                    .Add => if (@typeInfo(T) == .int) @addWithOverflow(base_color.val, ratio.val)[0] else base_color.val + ratio.val,
                    .Sub => if (@typeInfo(T) == .int) @subWithOverflow(base_color.val, ratio.val)[0] else base_color.val - ratio.val,
                    .Mul => if (@typeInfo(T) == .int) @mulWithOverflow(base_color.val, ratio.val)[0] else base_color.val * ratio.val,
                    .Div => if (@typeInfo(T) == .int) @divTrunc(base_color.val, ratio.val) else base_color.val / ratio.val,
                    .Mod => @mod(base_color.val, ratio.val),
                };

                const shader = try std.fmt.allocPrint(
                    allocator,
                    \\ [nzsl_version("1.1")]
                    \\ [feature(float64)]
                    \\ module;
                    \\ 
                    \\ struct FragOut
                    \\ {{
                    \\     [location(0)] color: vec{d}[{s}]
                    \\ }}
                    \\
                    \\ [entry(frag)]
                    \\ fn main() -> FragOut
                    \\ {{
                    \\     let ratio = vec{d}[{s}]({f});
                    \\
                    \\     let output: FragOut;
                    \\     output.color = vec{d}[{s}]({f}) {c} ratio;
                    \\     return output;
                    \\ }}
                ,
                    .{
                        L,
                        @typeName(T),
                        L,
                        @typeName(T),
                        ratio,
                        L,
                        @typeName(T),
                        base_color,
                        op.value.*,
                    },
                );
                defer allocator.free(shader);
                const code = try compileNzsl(allocator, shader);
                defer allocator.free(code);
                try case.expectOutput(T, L, code, "color", &@as([L]T, expected));
            }
        }
    }
}
