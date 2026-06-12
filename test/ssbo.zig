const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "SSBO read" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\ 
        \\ [layout(std430)]
        \\ struct SSBO
        \\ {
        \\     data: dyn_array[u32]
        \\ }
        \\ 
        \\ external
        \\ {
        \\     [set(0), binding(0)] ssbo: storage[SSBO],
        \\ }
        \\ 
        \\ [entry(compute)]
        \\ [workgroup(16, 1, 1)]
        \\ fn main()
        \\ {
        \\     for i in 0 -> 256
        \\     {
        \\         ssbo.data[i] = u32(i);
        \\     }
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    var ssbo = [_]u32{0} ** 256;

    var expected = [_]u32{0} ** 256;
    for (expected[0..], 0..) |*val, i| {
        val.* = @intCast(i);
    }

    try case.expect(.{
        .source = code,
        .descriptor_sets = &.{
            // Set 0
            &.{
                // Binding 0
                std.mem.asBytes(&ssbo),
            },
        },
        .expected_descriptor_sets = &.{
            // Set 0
            &.{
                // Binding 0
                std.mem.asBytes(&expected),
            },
        },
    });
}

test "SSBO write" {
    const allocator = std.testing.allocator;
    const shader =
        \\ [nzsl_version("1.1")]
        \\ module;
        \\
        \\ [layout(std430)]
        \\ struct SSBO
        \\ {
        \\     data: dyn_array[u32]
        \\ }
        \\
        \\ external
        \\ {
        \\     [set(0), binding(0)] ssbo: storage[SSBO],
        \\ }
        \\
        \\ [entry(compute)]
        \\ [workgroup(1, 1, 1)]
        \\ fn main()
        \\ {
        \\     for i in u32(2) -> u32(8)
        \\     {
        \\         ssbo.data[i] = ssbo.data[i - u32(1)] + ssbo.data[i - u32(2)];
        \\     }
        \\ }
    ;
    const code = try compileNzsl(allocator, shader);
    defer allocator.free(code);

    var ssbo = [_]u32{ 1, 1, 0, 0, 0, 0, 0, 0 };
    const expected = [_]u32{ 1, 1, 2, 3, 5, 8, 13, 21 };

    try case.expect(.{
        .source = code,
        .descriptor_sets = &.{
            &.{
                std.mem.asBytes(&ssbo),
            },
        },
        .expected_descriptor_sets = &.{
            &.{
                std.mem.asBytes(&expected),
            },
        },
    });
}
