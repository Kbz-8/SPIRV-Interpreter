const std = @import("std");
const spv = @import("spv.zig");

const SpvVoid = spv.SpvVoid;
const SpvByte = spv.SpvByte;
const SpvWord = spv.SpvWord;
const SpvBool = spv.SpvBool;

width: u32,
height: u32,
depth: u32,
layers: u32,
levels: u32,
