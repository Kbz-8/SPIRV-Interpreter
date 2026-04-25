const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const use_llvm = b.option(bool, "use-llvm", "Use LLVM backend") orelse (b.release_mode != .off);

    const spv_mod = b.addModule("spv", .{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const zmath = b.dependency("zmath", .{});
    spv_mod.addImport("zmath", zmath.module("root"));

    const pretty = b.dependency("pretty", .{
        .target = target,
        .optimize = optimize,
    });
    spv_mod.addImport("pretty", pretty.module("pretty"));

    const spv_lib = b.addLibrary(.{
        .name = "spirv_interpreter",
        .root_module = spv_mod,
        .linkage = .dynamic,
        .use_llvm = use_llvm,
    });

    const install_spv_lib = b.addInstallArtifact(spv_lib, .{});

    addSandbox(b, target, optimize, use_llvm, spv_mod, &install_spv_lib.step);
    addExample(b, target, optimize, use_llvm, spv_mod, &install_spv_lib.step);
    addZigTests(b, target, optimize, spv_mod, zmath);
    addCffi(b, target, optimize, use_llvm, spv_mod);
    addDocs(b, spv_mod);
}

fn addExample(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    use_llvm: bool,
    spv_mod: *std.Build.Module,
    install_spv_lib_step: *std.Build.Step,
) void {
    const no_example = b.option(bool, "no-example", "Skip example build") orelse false;
    if (!no_example and false) {
        const sdl3 = b.lazyDependency("sdl3", .{
            .target = target,
            .optimize = optimize,
        }) orelse return;

        const exe = b.addExecutable(.{
            .name = "spirv_interpreter_example",
            .root_module = b.createModule(.{
                .root_source_file = b.path("example/main.zig"),
                .target = target,
                .optimize = optimize,
                .imports = &.{
                    .{ .name = "spv", .module = spv_mod },
                    .{ .name = "sdl3", .module = sdl3.module("sdl3") },
                },
            }),
            .use_llvm = use_llvm,
        });

        const install_exe = b.addInstallArtifact(exe, .{});
        install_exe.step.dependOn(install_spv_lib_step);

        const run_exe = b.addRunArtifact(exe);
        run_exe.step.dependOn(&install_exe.step);

        const run_step = b.step("example", "Run the example");
        run_step.dependOn(&run_exe.step);

        addShaderCompileStep(
            b,
            "example-shader",
            "Compile example shader using nzslc",
            "example/shader.nzsl",
            "example",
        );
    }
}

fn addSandbox(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    use_llvm: bool,
    spv_mod: *std.Build.Module,
    install_spv_lib_step: *std.Build.Step,
) void {
    const exe = b.addExecutable(.{
        .name = "spirv_interpreter_sandbox",
        .root_module = b.createModule(.{
            .root_source_file = b.path("sandbox/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "spv", .module = spv_mod },
            },
        }),
        .use_llvm = use_llvm,
    });

    const install_exe = b.addInstallArtifact(exe, .{});
    install_exe.step.dependOn(install_spv_lib_step);

    const run_exe = b.addRunArtifact(exe);
    run_exe.step.dependOn(&install_exe.step);

    const run_step = b.step("sandbox", "Run the sandbox");
    run_step.dependOn(&run_exe.step);

    addShaderCompileStep(
        b,
        "sandbox-shader",
        "Compile sandbox shader using nzslc",
        "sandbox/shader.nzsl",
        "sandbox",
    );
}

fn addShaderCompileStep(
    b: *std.Build,
    step_name: []const u8,
    description: []const u8,
    shader_path: []const u8,
    output_dir: []const u8,
) void {
    const cmd = b.addSystemCommand(&.{
        "nzslc",
        shader_path,
        "--compile=spv,spv-dis",
        "-o",
        output_dir,
    });

    const step = b.step(step_name, description);
    step.dependOn(&cmd.step);
}

fn addZigTests(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    spv_mod: *std.Build.Module,
    zmath: *std.Build.Dependency,
) void {
    const no_test = b.option(bool, "no-test", "Skip unit test dependencies fetch") orelse false;
    if (no_test) return;

    const nzsl = b.lazyDependency("NZSL", .{
        .target = target,
        .optimize = optimize,
    }) orelse return;

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/root.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "spv", .module = spv_mod },
                .{ .name = "nzsl", .module = nzsl.module("nzigsl") },
                .{ .name = "zmath", .module = zmath.module("root") },
            },
        }),
        .test_runner = .{
            .path = b.path("test/test_runner.zig"),
            .mode = .simple,
        },
    });

    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run Zig unit tests");
    test_step.dependOn(&run_tests.step);
}

fn addCffi(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    use_llvm: bool,
    spv_mod: *std.Build.Module,
) void {
    const static_c_ffi = b.option(bool, "ffi-build-static", "Build C FFI statically") orelse true;

    const c_ffi_mod = b.addModule("c_ffi_spv", .{
        .root_source_file = b.path("ffi/ffi.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
        .imports = &.{
            .{ .name = "spv", .module = spv_mod },
        },
    });

    const install_header = b.addInstallHeaderFile(
        b.path("ffi/SpirvInterpreter.h"),
        "SpirvInterpreter.h",
    );

    const c_ffi_lib = b.addLibrary(.{
        .name = "spirv_interpreter_c_ffi",
        .root_module = c_ffi_mod,
        .linkage = if (static_c_ffi) .static else .dynamic,
        .use_llvm = use_llvm,
    });

    const install_lib = b.addInstallArtifact(c_ffi_lib, .{});

    const ffi_step = b.step("ffi-c", "Build C FFI");
    ffi_step.dependOn(&install_lib.step);
    ffi_step.dependOn(&install_header.step);

    const c_test = b.addExecutable(.{
        .name = "c_test",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .use_llvm = use_llvm,
    });

    c_test.root_module.addCSourceFile(.{ .file = b.path("test_c/main.c") });
    c_test.root_module.linkLibrary(c_ffi_lib);
    c_test.root_module.addSystemIncludePath(b.path("ffi"));

    const install_c_test = b.addInstallArtifact(c_test, .{});
    install_c_test.step.dependOn(&install_lib.step);

    const run_c_test = b.addRunArtifact(c_test);
    run_c_test.step.dependOn(&install_c_test.step);

    const test_c_step = b.step("test-c", "Run C test");
    test_c_step.dependOn(&run_c_test.step);
}

fn addDocs(b: *std.Build, spv_mod: *std.Build.Module) void {
    const autodoc_obj = b.addObject(.{
        .name = "lib",
        .root_module = spv_mod,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = autodoc_obj.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "docs",
    });

    const docs_step = b.step("docs", "Build and install documentation");
    docs_step.dependOn(&install_docs.step);
}
