const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const mode = b.standardReleaseOptions();

    var exe = b.addExecutable("zigmmverify", "src/main.zig");
    exe.setBuildMode(mode);

    var tst1 = b.addTest("src/mm.zig");
    // var tst2 = b.addTest("src/main.zig");

    // TODO: Add back when all parses successfully
    exe.step.dependOn(&tst1.step);
    // exe.step.dependOn(&tst2.step);
    b.default_step.dependOn(&exe.step);

    exe.install();
}
