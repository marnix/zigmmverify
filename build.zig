const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();
    const target = b.standardTargetOptions(.{});

    var exe = b.addExecutable("zigmmverify", "src/main.zig");
    exe.setBuildMode(mode);
    exe.setTarget(target);

    // separately testing parts that are not yet used from main.zig
    exe.step.dependOn(&(b.addTest("src/prove.zig")).step);

    exe.step.dependOn(&(b.addTest("src/main.zig")).step);

    b.default_step.dependOn(&exe.step);

    exe.install();
}
