const std = @import("std");
const gpu = std.gpu;

extern var frag_color: @Vector(4, f32) addrspace(.output);

export fn main() callconv(.spirv_fragment) void {
    gpu.location(&frag_color, 0);
    frag_color = .{ 1.0, 1.0, 1.0, 1.0 };
}
