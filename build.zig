const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const spirv_headers = b.dependency("spirv_headers", .{});
    mod.addSystemIncludePath(spirv_headers.path("include/spirv/unified1"));

    const lib = b.addLibrary(.{
        .name = "spirv_interpreter",
        .root_module = mod,
        .linkage = .dynamic,
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
            },
        }),
    });

    const spirv_target = b.resolveTargetQuery(.{
        .cpu_arch = .spirv32,
        .cpu_model = .{ .explicit = &std.Target.spirv.cpu.vulkan_v1_2 },
        .os_tag = .vulkan,
        .ofmt = .spirv,
    });

    const shader = b.addObject(.{
        .name = "shader.zig",
        .root_module = b.createModule(.{
            .root_source_file = b.path("example/shader.zig"),
            .target = spirv_target,
        }),
        .use_llvm = false,
    });

    example_exe.root_module.addAnonymousImport("shader", .{ .root_source_file = shader.getEmittedBin() });

    const example_install = b.addInstallArtifact(example_exe, .{});
    example_install.step.dependOn(&lib_install.step);

    const run_example = b.addRunArtifact(example_exe);
    run_example.step.dependOn(&example_install.step);

    const run_example_step = b.step("example", "Run the basic example");
    run_example_step.dependOn(&run_example.step);

    // Zig unit tests setup

    const lib_tests = b.addTest(.{ .root_module = mod });
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
