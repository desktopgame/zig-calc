const std = @import("std");

//
// Stream
//

const StreamError = error{
    OutOfRange,
};

pub fn Stream(comptime T: type) type {
    return struct {
        source: []const T,
        pos: u32,

        pub fn get(self: *Self) ?T {
            if (self.pos >= self.source.len) {
                return null;
            }
            const next = self.source[self.pos];
            self.pos += 1;
            return next;
        }

        pub fn unget(self: *Self) StreamError!void {
            if (self.pos > 0) {
                self.pos -= 1;
            } else {
                return StreamError.OutOfRange;
            }
        }

        pub fn consume(self: *Self) StreamError!void {
            if (self.pos < self.source.len) {
                self.pos += 1;
            } else {
                return StreamError.OutOfRange;
            }
        }

        pub fn peek(self: *Self) ?T {
            if (self.pos < self.source.len) {
                return self.source[self.pos];
            }
            return null;
        }

        pub fn ready(self: *Self) bool {
            return self.pos < self.source.len;
        }

        pub fn skip(self: *Self, pred: fn (T) bool) void {
            while (self.get()) |c| {
                if (!pred(c)) {
                    self.unget() catch unreachable;
                    break;
                }
            }
        }
        const Self = @This();
    };
}

//
// Scanner
//

const ScannerError = error{UnexpectedSymbol} || std.mem.Allocator.Error;

const Scanner = Stream(u8);

const Token = union(enum) { number: i32, symbol: u8 };

fn scan(allocator: std.mem.Allocator, source: []const u8) ScannerError!std.ArrayList(Token) {
    var tokens = std.ArrayList(Token).init(allocator);
    errdefer tokens.deinit();

    var scanner = Scanner{
        .source = source,
        .pos = 0,
    };
    while (scanner.ready()) {
        scanner.skip(std.ascii.isWhitespace);

        const ahead = scanner.peek() orelse break;
        if (std.ascii.isDigit(ahead)) {
            const start = scanner.pos;
            while (scanner.get()) |c| {
                if (!std.ascii.isDigit(c)) {
                    scanner.unget() catch unreachable;
                    break;
                }
            }
            try tokens.append(Token{ .number = std.fmt.parseInt(i32, scanner.source[start..scanner.pos], 10) catch unreachable });
        } else if (ahead == '+') {
            scanner.consume() catch unreachable;
            try tokens.append(Token{ .symbol = '+' });
        } else if (ahead == '-') {
            scanner.consume() catch unreachable;
            try tokens.append(Token{ .symbol = '-' });
        } else if (ahead == '*') {
            scanner.consume() catch unreachable;
            try tokens.append(Token{ .symbol = '*' });
        } else if (ahead == '/') {
            scanner.consume() catch unreachable;
            try tokens.append(Token{ .symbol = '/' });
        } else if (ahead == '%') {
            scanner.consume() catch unreachable;
            try tokens.append(Token{ .symbol = '%' });
        } else if (ahead == '(') {
            scanner.consume() catch unreachable;
            try tokens.append(Token{ .symbol = '(' });
        } else if (ahead == ')') {
            scanner.consume() catch unreachable;
            try tokens.append(Token{ .symbol = ')' });
        } else {
            return ScannerError.UnexpectedSymbol;
        }
    }
    return tokens;
}

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

test "tokenize" {
    var tokens = try scan(std.testing.allocator, "12 + (3*4)");
    defer tokens.deinit();

    try std.testing.expectEqual(tokens.items[0], Token{ .number = 12 });
    try std.testing.expectEqual(tokens.items[1], Token{ .symbol = '+' });
    try std.testing.expectEqual(tokens.items[2], Token{ .symbol = '(' });
    try std.testing.expectEqual(tokens.items[3], Token{ .number = 3 });
    try std.testing.expectEqual(tokens.items[4], Token{ .symbol = '*' });
    try std.testing.expectEqual(tokens.items[5], Token{ .number = 4 });
    try std.testing.expectEqual(tokens.items[6], Token{ .symbol = ')' });
}
