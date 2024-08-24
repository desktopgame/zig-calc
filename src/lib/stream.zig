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
