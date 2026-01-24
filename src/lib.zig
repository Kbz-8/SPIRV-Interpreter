//! A small footprint SPIR-V interpreter with zero dependencies to execute SPIR-V shaders on the CPU. It is designed to be used with multiple runtimes concurrently.
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
//!         try rt.readOutput(f32, output[0..output.len], try rt.getResultByName("color"));
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
const spv = @import("spv.zig");

pub const SpvVoid = spv.SpvVoid;
pub const SpvByte = spv.SpvByte;
pub const SpvWord = spv.SpvWord;
pub const SpvBool = spv.SpvBool;

pub const GLSL_std_450 = @import("GLSL_std_450/opcodes.zig");
