//! A small footprint SPIR-V interpreter to execute SPIR-V shaders on the CPU. It is designed to be used with multiple runtimes concurrently.
//!
//! ```zig
//! const std = @import("std");
//! const spv = @import("spv");
//!
//! const shader_source = @embedFile("shader.spv");
//!
//! pub fn main() !void {
//!     {
//!         var gpa: std.heap.DebugAllocator(.{}) = .init;
//!         defer _ = gpa.deinit();
//!
//!         const allocator = gpa.allocator();
//!
//!         var module = try spv.Module.init(allocator, @ptrCast(@alignCast(shader_source)), .{});
//!         defer module.deinit(allocator);
//!
//!         var rt = try spv.Runtime.init(allocator, &module);
//!         defer rt.deinit(allocator);
//!
//!         try rt.callEntryPoint(allocator, try rt.getEntryPointByName("main"));
//!         var output: [4]f32 = undefined;
//!         try rt.readOutput(std.mem.asBytes(output[0..output.len]), try rt.getResultByName("color"));
//!         std.log.info("Output: Vec4{any}", .{output});
//!     }
//!     std.log.info("Successfully executed", .{});
//! }
//! ```

const std = @import("std");

pub const Image = @import("Image.zig");
pub const Module = @import("Module.zig");
pub const Runtime = @import("Runtime.zig");

const opcodes = @import("opcodes.zig");
pub const spv = @import("spv.zig");

pub const SpvVoid = spv.SpvVoid;
pub const SpvByte = spv.SpvByte;
pub const SpvWord = spv.SpvWord;
pub const SpvBool = spv.SpvBool;

pub const Vec4f32 = @Vector(4, f32);
pub const Vec3f32 = @Vector(3, f32);
pub const Vec2f32 = @Vector(2, f32);

pub const Vec4i32 = @Vector(4, i32);
pub const Vec3i32 = @Vector(3, i32);
pub const Vec2i32 = @Vector(2, i32);

pub const Vec4u32 = @Vector(4, u32);
pub const Vec3u32 = @Vector(3, u32);
pub const Vec2u32 = @Vector(2, u32);

pub const GLSL_std_450 = @import("GLSL_std_450/opcodes.zig");

/// Maximum number of input locations per module
pub const SPIRV_MAX_INPUT_LOCATIONS: usize = 32;

/// Maximum number of output locations per module
pub const SPIRV_MAX_OUTPUT_LOCATIONS: usize = 32;

/// Maximum number of descriptor set per module
pub const SPIRV_MAX_SET: usize = 32;

/// Maximum number of bindings per descriptor set
pub const SPIRV_MAX_SET_BINDINGS: usize = 32;
