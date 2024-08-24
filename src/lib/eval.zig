const std = @import("std");

const _scan = @import("scan.zig");
const ScanError = _scan.ScanError;
const scan = _scan.scan;

const _parse = @import("parse.zig");
const ParseError = _parse.ParseError;
const Node = _parse.Node;
const parse = _parse.parse;

const _builtin = @import("builtin.zig");
const global = _builtin.global;

pub const FunctionError = error{ InvalidArgumentCount, InvalidArgumentType };

pub const EvalError = error{ UndefinedIdentifier, CannotOperation } || FunctionError || std.mem.Allocator.Error;

pub const InterpretError = error{ValueIsFunction} || ScanError || ParseError || EvalError;

pub const Value = union(enum) {
    number: f32,
    f: *const fn ([]const Value) FunctionError!Value,
};

//
// eval
//

pub fn eval(allocator: std.mem.Allocator, node: *Node) EvalError!Value {
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

//
// interpret
//

pub fn interpret(allocator: std.mem.Allocator, source: []const u8) InterpretError!f32 {
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
