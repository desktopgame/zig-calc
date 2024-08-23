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

const ScanError = error{ UnexpectedSymbol, UnexpectedTerminate } || std.mem.Allocator.Error;

const Scanner = Stream(u8);

const Token = union(enum) { number: f32, identifier: []const u8, symbol: u8 };

fn scan(allocator: std.mem.Allocator, source: []const u8) ScanError!std.ArrayList(Token) {
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
        } else {
            return ScanError.UnexpectedSymbol;
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
    number: f32,
    variable: []const u8,
    call: struct {
        f: *Node,
        args: std.ArrayList(*Node),
    },
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

    const Self = @This();
};

fn parse(allocator: std.mem.Allocator, source: []const Token) ParseError!*Node {
    var p = Parser{
        .source = source,
        .pos = 0,
    };
    const node = try parseExpr(allocator, &p);
    errdefer node.deinit(allocator);

    if (p.ready()) {
        return ParseError.UnexpectedToken;
    }
    return node;
}

fn parseExprOpt(allocator: std.mem.Allocator, p: *Parser) ?*Node {
    if (parseExpr(allocator, p)) |node| {
        return node;
    } else |_| {
        return null;
    }
}

fn parseExpr(allocator: std.mem.Allocator, p: *Parser) ParseError!*Node {
    return try parseAddSub(allocator, p);
}

fn parseAddSub(allocator: std.mem.Allocator, p: *Parser) ParseError!*Node {
    var left = try parseMulDiv(allocator, p);
    errdefer left.deinit(allocator);
    while (p.get()) |token| {
        switch (token) {
            .number => {
                return ParseError.UnexpectedToken;
            },
            .identifier => {
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
    errdefer left.deinit(allocator);
    while (p.get()) |token| {
        switch (token) {
            .number => {
                return ParseError.UnexpectedToken;
            },
            .identifier => {
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
                negativeNode.* = .{ .unaryOperator = .{ .arg = try parseCall(allocator, p), .symbol = '-' } };
                return negativeNode;
            } else {
                p.unget() catch unreachable;
                return try parseCall(allocator, p);
            }
        },
        else => {
            p.unget() catch unreachable;
            return try parseCall(allocator, p);
        },
    }
}

fn parseCall(allocator: std.mem.Allocator, p: *Parser) ParseError!*Node {
    var expr = try parsePrimary(allocator, p);
    errdefer expr.deinit(allocator);
    while (p.get()) |token| {
        switch (token) {
            .symbol => |open| {
                if (open == '(') {
                    var args = std.ArrayList(*Node).init(allocator);
                    errdefer args.deinit();
                    while (true) {
                        const argOpt = parseExprOpt(allocator, p);
                        if (argOpt == null) {
                            break;
                        }
                        try args.append(argOpt.?);
                        if (p.get()) |camma| {
                            switch (camma) {
                                .symbol => |separateSym| {
                                    if (separateSym == ',') {} else if (separateSym == ')') {
                                        p.unget() catch unreachable;
                                    } else {
                                        return ParseError.UnexpectedToken;
                                    }
                                },
                                else => {
                                    return ParseError.UnexpectedToken;
                                },
                            }
                        } else {
                            return ParseError.UnexpectedTerminate;
                        }
                    }
                    if (p.get()) |closeToken| {
                        switch (closeToken) {
                            .symbol => |closeSym| {
                                if (closeSym != ')') {
                                    return ParseError.UnexpectedToken;
                                }
                            },
                            else => {
                                return ParseError.UnexpectedToken;
                            },
                        }
                    } else {
                        return ParseError.UnexpectedTerminate;
                    }
                    const call = try Node.init(allocator);
                    call.* = .{ .call = .{ .f = expr, .args = args } };
                    expr = call;
                } else {
                    p.unget() catch unreachable;
                    return expr;
                }
            },
            else => {
                p.unget() catch unreachable;
                return expr;
            },
        }
    }
    return expr;
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
        .identifier => |ident| {
            const variable = try Node.init(allocator);
            errdefer variable.deinit(allocator);
            variable.* = .{ .variable = ident };
            return variable;
        },
        .symbol => |sym| {
            if (sym == '(') {
                const node = try parseExpr(allocator, p);
                errdefer node.deinit(allocator);
                const close = p.get() orelse return ParseError.UnexpectedTerminate;
                switch (close) {
                    .number => {
                        return ParseError.UnexpectedToken;
                    },
                    .identifier => {
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

//
// Eval
//

const FunctionError = error{ InvalidArgumentCount, InvalidArgumentType };

const Value = union(enum) {
    number: f32,
    f: *fn ([]const Value) FunctionError!Value,
};

const EvalError = error{ UndefinedIdentifier, CannotOperation } || FunctionError || std.mem.Allocator.Error;

const InterpretError = error{ValueIsFunction} || ScanError || ParseError || EvalError;

fn eval(allocator: std.mem.Allocator, node: *Node) EvalError!Value {
    switch (node.*) {
        .number => |num| {
            return .{ .number = num };
        },
        .variable => |v| {
            if (global(v)) |globalVar| {
                return globalVar;
            }
            return EvalError.UndefinedIdentifier;
        },
        .call => |c| {
            const fv = try eval(allocator, c.f);
            switch (fv) {
                .f => |f| {
                    var argv = std.ArrayList(Value).init(allocator);
                    defer argv.deinit();

                    for (c.args.items) |arg| {
                        try argv.append(try eval(allocator, arg));
                    }
                    return try f(argv.items);
                },
                else => {
                    return EvalError.CannotOperation;
                },
            }
        },
        .binaryOperator => |binOp| {
            const left = literal(try eval(allocator, binOp.leftArg)) orelse return EvalError.CannotOperation;
            const right = literal(try eval(allocator, binOp.rightArg)) orelse return EvalError.CannotOperation;
            switch (binOp.symbol) {
                '+' => {
                    return .{ .number = left + right };
                },
                '-' => {
                    return .{ .number = left - right };
                },
                '*' => {
                    return .{ .number = left * right };
                },
                '/' => {
                    return .{ .number = left / right };
                },
                '%' => {
                    return .{ .number = @mod(left, right) };
                },
                else => unreachable,
            }
        },
        .unaryOperator => |uOp| {
            const arg = literal(try eval(allocator, uOp.arg)) orelse return EvalError.CannotOperation;
            switch (uOp.symbol) {
                '-' => {
                    return .{ .number = -arg };
                },
                else => unreachable,
            }
        },
    }
}

fn literal(value: Value) ?f32 {
    switch (value) {
        .number => |num| {
            return num;
        },
        else => {
            return null;
        },
    }
}

fn interpret(allocator: std.mem.Allocator, source: []const u8) InterpretError!f32 {
    var tokens = try scan(allocator, source);
    defer tokens.deinit();

    var node = try parse(allocator, tokens.items);
    defer node.deinit(allocator);

    switch (try eval(allocator, node)) {
        .f => |_| {
            return InterpretError.ValueIsFunction;
        },
        .number => |num| {
            return num;
        },
    }
}

//
// builtins
//

const PI: f32 = 3.14;

fn global(name: []const u8) ?Value {
    if (std.mem.eql(u8, name, "PI")) {
        return .{ .number = PI };
    }
    return null;
}

//
// main
//

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

    const result = try interpret(allocator, string.items);
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

test "eval" {
    try std.testing.expectEqual(try interpret(std.testing.allocator, "12 + (3*4)"), 24.0);
    try std.testing.expectEqual(try interpret(std.testing.allocator, "-5 + (3*(1+1))"), 1.0);
    try std.testing.expectEqual(try interpret(std.testing.allocator, "(3*3*3-1+1)"), 27.0);
    try std.testing.expectEqual(try interpret(std.testing.allocator, "(3*3*3-1+1)"), 27.0);
    try std.testing.expectEqual(try interpret(std.testing.allocator, "((1)+1)"), 2.0);
    try std.testing.expectEqual(try interpret(std.testing.allocator, "-(-1)"), 1.0);
    try std.testing.expectEqual(try interpret(std.testing.allocator, "1.5+1.5"), 3.0);
    try std.testing.expectEqual(try interpret(std.testing.allocator, "-1 / 2"), -0.5);
    try std.testing.expectEqual(try interpret(std.testing.allocator, "PI"), PI);

    try std.testing.expectError(ParseError.UnexpectedTerminate, interpret(std.testing.allocator, "1+2*"));
    try std.testing.expectError(ParseError.UnexpectedTerminate, interpret(std.testing.allocator, "1+"));
    try std.testing.expectError(ParseError.UnexpectedToken, interpret(std.testing.allocator, "()"));
    try std.testing.expectError(ParseError.UnexpectedToken, interpret(std.testing.allocator, "1+2*)"));
    try std.testing.expectError(ParseError.UnexpectedTerminate, interpret(std.testing.allocator, "1("));
    try std.testing.expectError(ScanError.UnexpectedTerminate, interpret(std.testing.allocator, "1."));
    try std.testing.expectError(ScanError.UnexpectedSymbol, interpret(std.testing.allocator, "1. + 1"));
}
