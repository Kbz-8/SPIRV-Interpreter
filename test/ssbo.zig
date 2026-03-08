const std = @import("std");
const root = @import("root.zig");
const compileNzsl = root.compileNzsl;
const case = root.case;

test "Simple SSBO" {
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
