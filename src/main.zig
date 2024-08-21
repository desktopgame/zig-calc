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

//
// Parser
//

const ParseError = error{ UnexpectedToken, UnexpectedTerminate } || std.mem.Allocator.Error;

const Parser = Stream(Token);

const Node = union(enum) {
    number: i32,
    binaryOperator: struct { leftArg: *Node, rightArg: *Node, symbol: u8 },
    unaryOperator: struct {
        arg: *Node,
        symbol: u8,
    },

    pub fn init(allocator: std.mem.Allocator) std.mem.Allocator.Error!*Self {
        return try allocator.create(Self);
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .binaryOperator => |binOp| {
                binOp.leftArg.deinit(allocator);
                binOp.rightArg.deinit(allocator);
            },
            .unaryOperator => |uOp| {
                uOp.arg.deinit(allocator);
            },
            else => {},
        }
        allocator.destroy(self);
    }

    pub fn eval(self: *Self) i32 {
        switch (self.*) {
            .number => |num| {
                return num;
            },
            .binaryOperator => |binOp| {
                const left = binOp.leftArg.eval();
                const right = binOp.rightArg.eval();
                switch (binOp.symbol) {
                    '+' => {
                        return left + right;
                    },
                    '-' => {
                        return left - right;
                    },
                    '*' => {
                        return left * right;
                    },
                    '/' => {
                        return @divFloor(left, right);
                    },
                    '%' => {
                        return @mod(left, right);
                    },
                    else => unreachable,
                }
            },
            .unaryOperator => |uOp| {
                const arg = uOp.arg.eval();
                switch (uOp.symbol) {
                    '-' => {
                        return -arg;
                    },
                    else => unreachable,
                }
            },
        }
    }
    const Self = @This();
};

fn parse(allocator: std.mem.Allocator, source: []const Token) ParseError!*Node {
    var p = Parser{
        .source = source,
        .pos = 0,
    };
    return parseExpr(allocator, &p);
}

fn parseExpr(allocator: std.mem.Allocator, p: *Parser) ParseError!*Node {
    return parseAddSub(allocator, p);
}

fn parseAddSub(allocator: std.mem.Allocator, p: *Parser) ParseError!*Node {
    var left = try parseMulDiv(allocator, p);
    while (p.get()) |token| {
        switch (token) {
            .number => {
                return ParseError.UnexpectedToken;
            },
            .symbol => |sym| {
                switch (sym) {
                    '+', '-' => {
                        const node = try Node.init(allocator);
                        errdefer node.deinit(allocator);
                        node.* = .{ .binaryOperator = .{ .leftArg = left, .rightArg = try parseMulDiv(allocator, p), .symbol = sym } };
                        left = node;
                    },
                    else => {
                        p.unget() catch unreachable;
                        return left;
                    },
                }
            },
        }
    }
    return left;
}

fn parseMulDiv(allocator: std.mem.Allocator, p: *Parser) ParseError!*Node {
    var left = try parseNegative(allocator, p);
    while (p.get()) |token| {
        switch (token) {
            .number => {
                return ParseError.UnexpectedToken;
            },
            .symbol => |sym| {
                switch (sym) {
                    '*', '/', '%' => {
                        const node = try Node.init(allocator);
                        errdefer node.deinit(allocator);
                        node.* = .{ .binaryOperator = .{ .leftArg = left, .rightArg = try parseNegative(allocator, p), .symbol = sym } };
                        left = node;
                    },
                    else => {
                        p.unget() catch unreachable;
                        return left;
                    },
                }
            },
        }
    }
    return left;
}

fn parseNegative(allocator: std.mem.Allocator, p: *Parser) ParseError!*Node {
    const ahead = p.get() orelse return ParseError.UnexpectedTerminate;
    switch (ahead) {
        .symbol => |sym| {
            if (sym == '-') {
                const negativeNode = try Node.init(allocator);
                errdefer negativeNode.deinit(allocator);
                negativeNode.* = .{ .unaryOperator = .{ .arg = try parsePrimary(allocator, p), .symbol = '-' } };
                return negativeNode;
            } else {
                p.unget() catch unreachable;
                return parsePrimary(allocator, p);
            }
        },
        else => {
            p.unget() catch unreachable;
            return parsePrimary(allocator, p);
        },
    }
}

fn parsePrimary(allocator: std.mem.Allocator, p: *Parser) ParseError!*Node {
    const ahead = p.get() orelse return ParseError.UnexpectedTerminate;
    switch (ahead) {
        .number => |num| {
            const prim = try Node.init(allocator);
            errdefer prim.deinit(allocator);
            prim.* = .{ .number = num };
            return prim;
        },
        .symbol => |sym| {
            if (sym == '(') {
                const node = parseExpr(allocator, p);
                const close = p.get() orelse return ParseError.UnexpectedTerminate;
                switch (close) {
                    .number => {
                        return ParseError.UnexpectedToken;
                    },
                    .symbol => |symClose| {
                        if (symClose != ')') {
                            return ParseError.UnexpectedToken;
                        }
                    },
                }
                return node;
            } else {
                return ParseError.UnexpectedToken;
            }
        },
    }
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

test "parse1" {
    var tokens = try scan(std.testing.allocator, "12 + (3*4)");
    defer tokens.deinit();

    var node = try parse(std.testing.allocator, tokens.items);
    defer node.deinit(std.testing.allocator);

    try std.testing.expectEqual(node.eval(), 24);
}

test "parse2" {
    var tokens = try scan(std.testing.allocator, "-5 + (3*(1+1))");
    defer tokens.deinit();

    var node = try parse(std.testing.allocator, tokens.items);
    defer node.deinit(std.testing.allocator);

    try std.testing.expectEqual(node.eval(), 1);
}

test "parse3" {
    var tokens = try scan(std.testing.allocator, "(3*3*3-1)");
    defer tokens.deinit();

    var node = try parse(std.testing.allocator, tokens.items);
    defer node.deinit(std.testing.allocator);

    try std.testing.expectEqual(node.eval(), 26);
}

test "parse4" {
    var tokens = try scan(std.testing.allocator, "(3*3*3-1+1)");
    defer tokens.deinit();

    var node = try parse(std.testing.allocator, tokens.items);
    defer node.deinit(std.testing.allocator);

    try std.testing.expectEqual(node.eval(), 27);
}
