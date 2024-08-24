const std = @import("std");
const Stream = @import("stream.zig").Stream;

pub const ScanError = error{ UnexpectedSymbol, UnexpectedTerminate } || std.mem.Allocator.Error;

pub const Scanner = Stream(u8);

pub const Token = union(enum) { number: f32, identifier: []const u8, symbol: u8 };

pub fn scan(allocator: std.mem.Allocator, source: []const u8) ScanError!std.ArrayList(Token) {
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
            if (scanner.get()) |d| {
                if (d == '.') {
                    const dotPos = scanner.pos;
                    while (scanner.get()) |c| {
                        if (!std.ascii.isDigit(c)) {
                            scanner.unget() catch unreachable;
                            break;
                        }
                    }
                    if (scanner.pos == dotPos) {
                        if (scanner.ready()) {
                            return ScanError.UnexpectedSymbol;
                        } else {
                            return ScanError.UnexpectedTerminate;
                        }
                    }
                } else {
                    scanner.unget() catch unreachable;
                }
            }
            try tokens.append(Token{ .number = std.fmt.parseFloat(f32, scanner.source[start..scanner.pos]) catch unreachable });
        } else if (std.ascii.isAlphabetic(ahead)) {
            const start = scanner.pos;
            while (scanner.get()) |c| {
                if (!std.ascii.isAlphabetic(c)) {
                    scanner.unget() catch unreachable;
                    break;
                }
            }
            try tokens.append(Token{ .identifier = scanner.source[start..scanner.pos] });
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
        } else if (ahead == ',') {
            scanner.consume() catch unreachable;
            try tokens.append(Token{ .symbol = ',' });
        } else {
            return ScanError.UnexpectedSymbol;
        }
    }
    return tokens;
}
