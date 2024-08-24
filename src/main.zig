const std = @import("std");
const lib = @import("lib.zig");

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

    const result = try lib.interpret(allocator, string.items);
    try std.io.getStdOut().writer().print("{d}\n", .{result});
}

//
// test
//

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "tokenize" {
    var tokens = try lib.scan(std.testing.allocator, "12 + (3*4)");
    defer tokens.deinit();

    try std.testing.expectEqual(tokens.items[0], lib.Token{ .number = 12 });
    try std.testing.expectEqual(tokens.items[1], lib.Token{ .symbol = '+' });
    try std.testing.expectEqual(tokens.items[2], lib.Token{ .symbol = '(' });
    try std.testing.expectEqual(tokens.items[3], lib.Token{ .number = 3 });
    try std.testing.expectEqual(tokens.items[4], lib.Token{ .symbol = '*' });
    try std.testing.expectEqual(tokens.items[5], lib.Token{ .number = 4 });
    try std.testing.expectEqual(tokens.items[6], lib.Token{ .symbol = ')' });
}

test "eval" {
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "12 + (3*4)"), 24.0);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "-5 + (3*(1+1))"), 1.0);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "(3*3*3-1+1)"), 27.0);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "(3*3*3-1+1)"), 27.0);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "((1)+1)"), 2.0);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "-(-1)"), 1.0);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "1.5+1.5"), 3.0);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "-1 / 2"), -0.5);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "PI"), lib.PI);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "min(1,2)"), 1);
    try std.testing.expectEqual(try lib.interpret(std.testing.allocator, "max(1,2)"), 2);

    try std.testing.expectError(lib.ParseError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "1+2*"));
    try std.testing.expectError(lib.ParseError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "1+"));
    try std.testing.expectError(lib.ParseError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "1("));
    try std.testing.expectError(lib.ScanError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "max(1,2"));
    try std.testing.expectError(lib.ScanError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "max(1"));
    try std.testing.expectError(lib.ScanError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "1."));
    try std.testing.expectError(lib.ScanError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "max("));
    try std.testing.expectError(lib.ScanError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "2*"));
    try std.testing.expectError(lib.ScanError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "max(1,2*"));

    try std.testing.expectError(lib.ParseError.UnexpectedToken, lib.interpret(std.testing.allocator, "()"));
    try std.testing.expectError(lib.ParseError.UnexpectedToken, lib.interpret(std.testing.allocator, "1+2*)"));
    try std.testing.expectError(lib.ParseError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "1("));
    try std.testing.expectError(lib.ScanError.UnexpectedTerminate, lib.interpret(std.testing.allocator, "1."));
    try std.testing.expectError(lib.ScanError.UnexpectedSymbol, lib.interpret(std.testing.allocator, "1. + 1"));
}
