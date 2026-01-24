const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_llvm = b.option(bool, "use-llvm", "use llvm") orelse (b.release_mode != .off);

    const mod = b.addModule("spv", .{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const zmath = b.dependency("zmath", .{});
    mod.addImport("zmath", zmath.module("root"));

    const pretty = b.dependency("pretty", .{ .target = target, .optimize = optimize });
    mod.addImport("pretty", pretty.module("pretty"));

    var it = b.user_input_options.iterator();
    while (it.next()) |entry| {
        std.debug.print("{s} - {s} {any}", .{ entry.key_ptr.*, entry.value_ptr.name, entry.value_ptr.used });
    }

    const lib = b.addLibrary(.{
        .name = "spirv_interpreter",
        .root_module = mod,
        .linkage = .dynamic,
        .use_llvm = use_llvm,
    });
    const lib_install = b.addInstallArtifact(lib, .{});

    // Zig example setup

    const no_example = b.option(bool, "no-example", "skips example dependencies fetch") orelse false;

    if (!no_example) {
        const sdl3 = b.lazyDependency("sdl3", .{ .target = target, .optimize = optimize }) orelse return;
        const example_exe = b.addExecutable(.{
            .name = "spirv_interpreter_example",
            .root_module = b.createModule(.{
                .root_source_file = b.path("example/main.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "spv", .module = mod },
                    .{ .name = "sdl3", .module = sdl3.module("sdl3") },
                    //.{ .name = "pretty", .module = pretty.module("pretty") },
                },
            }),
            .use_llvm = use_llvm,
        });

        const example_install = b.addInstallArtifact(example_exe, .{});
        example_install.step.dependOn(&lib_install.step);

        const run_example = b.addRunArtifact(example_exe);
        run_example.step.dependOn(&example_install.step);

        const run_example_step = b.step("example", "Run the example");
        run_example_step.dependOn(&run_example.step);

        const compile_shader_cmd = b.addSystemCommand(&[_][]const u8{ "nzslc", "example/shader.nzsl", "--compile=spv,spv-dis", "-o", "example" });
        const compile_shader_step = b.step("example-shader", "Compiles example's shader (needs nzslc installed)");
        compile_shader_step.dependOn(&compile_shader_cmd.step);
    }

    // Zig sandbox setup

    const sandbox_exe = b.addExecutable(.{
        .name = "spirv_interpreter_sandbpx",
        .root_module = b.createModule(.{
            .root_source_file = b.path("sandbox/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "spv", .module = mod },
                //.{ .name = "pretty", .module = pretty.module("pretty") },
            },
        }),
        .use_llvm = use_llvm,
    });

    const sandbox_install = b.addInstallArtifact(sandbox_exe, .{});
    sandbox_install.step.dependOn(&lib_install.step);

    const run_sandbox = b.addRunArtifact(sandbox_exe);
    run_sandbox.step.dependOn(&sandbox_install.step);

    const run_sandbox_step = b.step("sandbox", "Run the sandbox");
    run_sandbox_step.dependOn(&run_sandbox.step);

    const compile_shader_cmd = b.addSystemCommand(&[_][]const u8{ "nzslc", "sandbox/shader.nzsl", "--compile=spv,spv-dis", "-o", "sandbox" });
    const compile_shader_step = b.step("sandbox-shader", "Compiles sandbox's shader (needs nzslc installed)");
    compile_shader_step.dependOn(&compile_shader_cmd.step);

    // Zig unit tests setup

    const no_test = b.option(bool, "no-test", "skips unit test dependencies fetch") orelse false;

    if (!no_test) {
        const nzsl = b.lazyDependency("NZSL", .{ .target = target, .optimize = optimize }) orelse return;
        const lib_tests = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("test/root.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "spv", .module = mod },
                    .{ .name = "nzsl", .module = nzsl.module("nzigsl") },
                    .{ .name = "zmath", .module = zmath.module("root") },
                },
            }),
            .test_runner = .{ .path = b.path("test/test_runner.zig"), .mode = .simple },
        });
        const run_tests = b.addRunArtifact(lib_tests);
        const test_step = b.step("test", "Run Zig unit tests");
        test_step.dependOn(&run_tests.step);
    }

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
