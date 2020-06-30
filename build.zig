const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    var exe = b.addExecutable("zigmmverify", "src/main.zig");
    exe.setBuildMode(mode);

    exe.step.dependOn(&(b.addTest("src/main.zig")).step);

    b.default_step.dependOn(&exe.step);

    exe.install();
}
