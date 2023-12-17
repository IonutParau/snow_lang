const std = @import("std");

const lexer = @import("lexer.zig");

pub const SnowError = error{
    // Catch-all "parser did oopsie" error
    SyntaxError,
    // Catch-all "interpreter did oopsie" error
    RuntimeError,
    // Lexer found a character it is unable to process
    BadChar,
    // Lexer found there is an unfinished string
    UnfinishedString,
    // An escape in a string literal was just bad
    MalformedEscape,
    // A number with ASCII characters in it
    MalformedNumber,
} || std.mem.Allocator.Error;

pub const SnowErrorStore = struct {
    msg: ?[]const u8,
    msg_allocator: ?std.mem.Allocator,
    error_source: lexer.SnowSource,

    const Self = @This();

    pub fn empty() Self {
        const source = lexer.SnowSource.init("");

        return .{
            .msg = null,
            .msg_allocator = null,
            .error_source = source,
        };
    }

    pub fn init(msg: []const u8, msg_allocator: ?std.mem.Allocator, error_source: lexer.SnowSource) Self {
        return .{
            .msg = msg,
            .msg_allocator = msg_allocator,
            .error_source = error_source,
        };
    }

    pub fn noAlloc(msg: []const u8, error_source: lexer.SnowSource) Self {
        return Self.init(msg, null, error_source);
    }

    pub fn fmt(comptime format: []const u8, args: anytype, allocator: std.mem.Allocator, error_source: lexer.SnowSource) !Self {
        const msg = try std.fmt.allocPrint(allocator, format, args);

        return .{
            .msg = msg,
            .msg_allocator = allocator,
            .error_source = error_source,
        };
    }

    pub fn initCopyStr(msg: []const u8, msg_allocator: std.mem.Allocator, error_source: lexer.SnowSource) !Self {
        const msgcpy = try msg_allocator.alloc(u8, msg.len);
        return Self.init(msgcpy, msg_allocator, error_source);
    }

    // Dumps into StdErr
    pub fn dump(self: *Self) void {
        if (self.msg == null) return;
        const stderr = std.io.getStdErr();
        const writer = stderr.writer();

        // Ignore errors
        writer.print("{s}\nAt: {s}:{}:{}", .{ self.msg.?, self.error_source.file, self.error_source.line, self.error_source.column }) catch unreachable;
    }

    pub fn dumpTesting(self: *Self) void {
        if (self.msg == null) return;

        std.debug.print("{s}\nAt: {s}:{}:{}", .{ self.msg.?, self.error_source.file, self.error_source.line, self.error_source.column });
    }

    pub fn deinit(self: *Self) void {
        if (self.msg_allocator) |allocator| {
            if (self.msg) |msg| {
                allocator.free(msg);
            }
        }
    }
};
