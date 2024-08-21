const std = @import("std");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var string = std.ArrayList(u8).init(allocator);
    defer string.deinit();

    for (args[1..]) |arg| {
        _ = try string.appendSlice(arg);
        _ = try string.append(' ');
    }
    const stdout = std.io.getStdOut();
    _ = try stdout.write(string.items);
    _ = try stdout.write("\n");
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
