const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const pretty = b.dependency("pretty", .{ .target = target, .optimize = optimize });
    mod.addImport("pretty", pretty.module("pretty"));

    const lib = b.addLibrary(.{
        .name = "spirv_interpreter",
        .root_module = mod,
        .linkage = .dynamic,
        //.use_llvm = true,
    });
    const lib_install = b.addInstallArtifact(lib, .{});

    // Zig example setup

    const example_exe = b.addExecutable(.{
        .name = "spirv_interpreter_example",
        .root_module = b.createModule(.{
            .root_source_file = b.path("example/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "spv", .module = mod },
                .{ .name = "pretty", .module = pretty.module("pretty") },
            },
        }),
    });

    const sdl3 = b.lazyDependency("sdl3", .{}) orelse return;
    example_exe.root_module.addImport("sdl3", sdl3.module("sdl3"));

    const example_install = b.addInstallArtifact(example_exe, .{});
    example_install.step.dependOn(&lib_install.step);

    const run_example = b.addRunArtifact(example_exe);
    run_example.step.dependOn(&example_install.step);

    const run_example_step = b.step("example", "Run the example");
    run_example_step.dependOn(&run_example.step);

    const compile_shader_cmd = b.addSystemCommand(&[_][]const u8{ "nzslc", "example/shader.nzsl", "--compile=spv,spv-dis", "-o", "example" });
    const compile_shader_step = b.step("example-shader", "Compiles example's shader");
    compile_shader_step.dependOn(&compile_shader_cmd.step);

    // Zig unit tests setup

    const nzsl = b.lazyDependency("NZSL", .{}) orelse return;
    const test_mod = b.createModule(.{
        .root_source_file = b.path("test/root.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "spv", .module = mod },
            .{ .name = "nzsl", .module = nzsl.module("nzigsl") },
        },
    });
    const lib_tests = b.addTest(.{
        .root_module = test_mod,
        .test_runner = .{ .path = b.path("test/test_runner.zig"), .mode = .simple },
    });
    const run_tests = b.addRunArtifact(lib_tests);
    const test_step = b.step("test", "Run Zig unit tests");
    test_step.dependOn(&run_tests.step);

    // Docs generation

    const autodoc_test = b.addObject(.{
        .name = "lib",
        .root_module = mod,
    });
    const install_docs = b.addInstallDirectory(.{
        .source_dir = autodoc_test.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Build and install the documentation");
    docs_step.dependOn(&install_docs.step);
}
