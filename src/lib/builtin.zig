const std = @import("std");
const _eval = @import("eval.zig");
const Value = _eval.Value;
const FunctionError = _eval.FunctionError;

pub fn global(name: []const u8) ?Value {
    if (std.mem.eql(u8, name, "PI")) {
        return .{ .number = PI };
    } else if (std.mem.eql(u8, name, "min")) {
        return .{ .f = &blt_min };
    } else if (std.mem.eql(u8, name, "max")) {
        return .{ .f = &blt_max };
    } else if (std.mem.eql(u8, name, "sum")) {
        return .{ .f = &blt_sum };
    } else if (std.mem.eql(u8, name, "avg")) {
        return .{ .f = &blt_avg };
    }
    return null;
}

//
// builtin variables
//

pub const PI: f32 = 3.14;

//
// builtin functions
//

pub fn blt_min(args: []const Value) FunctionError!Value {
    var min: f32 = std.math.floatMax(f32);
    for (args) |arg| {
        const farg = try shouldBeLiteral(arg);
        if (farg < min) {
            min = farg;
        }
    }
    return .{ .number = min };
}

pub fn blt_max(args: []const Value) FunctionError!Value {
    var max: f32 = std.math.floatMin(f32);
    for (args) |arg| {
        const farg = try shouldBeLiteral(arg);
        if (farg > max) {
            max = farg;
        }
    }
    return .{ .number = max };
}

pub fn blt_sum(args: []const Value) FunctionError!Value {
    var total: f32 = 0;
    for (args) |arg| {
        total += try shouldBeLiteral(arg);
    }
    return .{ .number = total };
}

pub fn blt_avg(args: []const Value) FunctionError!Value {
    const sum = try shouldBeLiteral(try blt_sum(args));
    return .{ .number = sum / @as(f32, @floatFromInt(@as(i32, @intCast(args.len)))) };
}

//
// helper
//

fn shouldBeLiteral(v: Value) FunctionError!f32 {
    switch (v) {
        .f => |_| {
            return FunctionError.InvalidArgumentType;
        },
        .number => |num| {
            return num;
        },
    }
}

fn shouldBeFunction(v: Value) FunctionError!fn (Value) Value {
    switch (v) {
        .number => |_| {
            return FunctionError.InvalidArgumentType;
        },
        .f => |f| {
            return f;
        },
    }
}
