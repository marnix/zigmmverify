const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    var exe = b.addExecutable("zigmmverify", "src/main.zig");
    exe.setBuildMode(mode);

    var tst = b.addTest("src/main.zig");

    tst.step.dependOn(&exe.step);
    b.default_step.dependOn(&tst.step);

    exe.install();
}
