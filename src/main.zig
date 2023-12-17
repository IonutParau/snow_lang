const std = @import("std");

const libsnow = @import("libsnow.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();

    try writer.print("Hello there! This will be an interpreter soon", .{});
}

test "libsnow" {
    std.testing.refAllDeclsRecursive(libsnow);
}
