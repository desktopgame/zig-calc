const std = @import("std");
const Stream = @import("stream.zig").Stream;
const Token = @import("scan.zig").Token;

pub const ParseError = error{ UnexpectedToken, UnexpectedTerminate } || std.mem.Allocator.Error;

pub const Parser = Stream(Token);

pub const Node = union(enum) {
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
            .call => |c| {
                c.f.deinit(allocator);
                for (c.args.items) |item| {
                    item.deinit(allocator);
                }
                c.args.deinit();
            },
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

pub fn parse(allocator: std.mem.Allocator, source: []const Token) ParseError!*Node {
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
                        const right = try parseMulDiv(allocator, p);
                        errdefer right.deinit(allocator);
                        const node = try Node.init(allocator);
                        errdefer node.deinit(allocator);
                        node.* = .{ .binaryOperator = .{ .leftArg = left, .rightArg = right, .symbol = sym } };
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
                        const right = try parseNegative(allocator, p);
                        errdefer right.deinit(allocator);
                        const node = try Node.init(allocator);
                        errdefer node.deinit(allocator);
                        node.* = .{ .binaryOperator = .{ .leftArg = left, .rightArg = right, .symbol = sym } };
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
                const arg = try parseCall(allocator, p);
                errdefer arg.deinit(allocator);
                const negativeNode = try Node.init(allocator);
                errdefer negativeNode.deinit(allocator);
                negativeNode.* = .{ .unaryOperator = .{ .arg = arg, .symbol = '-' } };
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
                    const start = p.pos;
                    var args = std.ArrayList(*Node).init(allocator);
                    var lastError: ?ParseError = null;
                    while (true) {
                        const arg = parseExpr(allocator, p) catch |err| {
                            p.pos = start;
                            lastError = err;
                            break;
                        };
                        errdefer {
                            for (args.items) |item| {
                                item.deinit(allocator);
                            }
                            args.deinit();
                        }
                        try args.append(arg);
                        if (p.get()) |camma| {
                            switch (camma) {
                                .symbol => |separateSym| {
                                    if (separateSym == ',') {} else if (separateSym == ')') {
                                        p.unget() catch unreachable;
                                        break;
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
                    errdefer {
                        for (args.items) |item| {
                            item.deinit(allocator);
                        }
                        args.deinit();
                    }
                    if (p.get()) |closeToken| {
                        switch (closeToken) {
                            .symbol => |closeSym| {
                                if (closeSym != ')') {
                                    return lastError orelse ParseError.UnexpectedToken;
                                }
                            },
                            else => {
                                return lastError orelse ParseError.UnexpectedToken;
                            },
                        }
                    } else {
                        return ParseError.UnexpectedTerminate;
                    }
                    const call = try Node.init(allocator);
                    errdefer call.deinit(allocator);
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
