const std = @import("std");

const Interpreter = @import("Interpreter.zig");
const Module = @import("Module.zig");

const Self = @This();

ctx: *Interpreter,
owner: *Module,
