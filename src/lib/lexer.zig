const std = @import("std");
const errors = @import("errors.zig");

pub const SnowSource = struct {
    file: []const u8,
    column: usize,
    line: usize,

    pub fn init(file: []const u8) SnowSource {
        return .{
            .file = file,
            .column = 1,
            .line = 1,
        };
    }

    pub fn copy(self: SnowSource) SnowSource {
        return self;
    }

    pub fn next(self: *SnowSource) void {
        self.column += 1;
    }

    pub fn nextN(self: *SnowSource, n: usize) void {
        self.column += n;
    }

    pub fn skip(self: *SnowSource, c: u8) void {
        if (c == '\n') {
            self.line += 1;
        } else {
            self.column += 1;
        }
    }
};

// the void variants may seem useless.
// Remeber: This has a tag!
pub const SnowTokenType = union(enum) {
    stringLiteral: []const u8,
    numberLiteral: f64,
    identifier: []const u8,
    bar, // |
    openParen, // (
    closeParen, // )
    openBracket, // [
    closeBracket, // ]
    openCurly, // {
    closeCurly, // }
    plus, // +
    minus, // -
    asterisk, // *
    slash, // /
    semicolon, // ;
    dot, // .
    doubleDot, // ..
    tripleDot, // ...
    intDivision, // //
    comma, // ,
    caret, // ^
    mod, // %
    greater, // >
    less, // <
    greaterThan, // >=
    lessThan, // <=
    assign, // =
    equals, // ==
    notEquals, // !=
    colon, // :

    // Keyword
    funKeyword,
    structKeyword,
    forKeyword,
    inKeyword,
    isKeyword,
    matchKeyword,
    ifKeyword,
    elseKeyword,
    returnKeyword,
    whileKeyword,
    doKeyword,
    localKeyword,
    tryKeyword,
    catchKeyword,
    continueKeyword,
    breakKeyword,
    andKeyword,
    orKeyword,
    notKeyword,
    trueKeyword,
    falseKeyword,
    nullKeyword,
    thenKeyword,

    pub fn eq(self: SnowTokenType, other: SnowTokenType) bool {
        const typeInfo: std.builtin.Type = @typeInfo(SnowTokenType);
        const tag = typeInfo.Union.tag_type.?;

        if (@as(tag, self) != @as(tag, other)) {
            return false;
        }

        return switch (self) {
            .stringLiteral => |str| std.mem.eql(u8, str, other.stringLiteral),
            .numberLiteral => |n| n == other.numberLiteral,
            .identifier => |str| std.mem.eql(u8, str, other.identifier),
            else => true,
        };
    }
};

pub const SnowToken = struct {
    source: SnowSource,
    kind: SnowTokenType,

    pub fn init(kind: SnowTokenType, source: SnowSource) SnowToken {
        return .{
            .kind = kind,
            .source = source,
        };
    }
};

const Allocator = std.mem.Allocator;

pub const SnowLexer = struct {
    file: []const u8,
    code: []const u8,
    source: SnowSource,
    char_idx: usize,
    error_store: *errors.SnowErrorStore,
    allocator: Allocator,

    pub fn init(file: []const u8, code: []const u8, error_store: *errors.SnowErrorStore, allocator: Allocator) SnowLexer {
        return .{
            .file = file,
            .code = code,
            .source = SnowSource.init(file),
            .char_idx = 0,
            .error_store = error_store,
            .allocator = allocator,
        };
    }

    pub fn done(self: *const SnowLexer) bool {
        return self.char_idx >= self.code.len;
    }

    pub fn peek(self: *const SnowLexer) errors.SnowError!?SnowToken {
        var peeker = self.*;
        return peeker.next();
    }

    pub fn next(self: *SnowLexer) errors.SnowError!?SnowToken {
        const SymbolPair = struct {
            kind: SnowTokenType,
            text: []const u8,
        };

        // Whitespace be gone
        while (true) {
            if (self.char_idx >= self.code.len) {
                return null; // We are done
            }

            const c = self.code[self.char_idx];
            if (std.ascii.isWhitespace(c)) {
                self.source.skip(c);
                self.char_idx += 1;
                continue;
            }

            break;
        }

        const symbols = [_]SymbolPair{
            .{ .kind = .plus, .text = "+" },
            .{ .kind = .minus, .text = "-" },
            .{ .kind = .tripleDot, .text = "..." },
            .{ .kind = .doubleDot, .text = ".." },
            .{ .kind = .dot, .text = "." },
            .{ .kind = .caret, .text = "^" },
            .{ .kind = .bar, .text = "|" },
            .{ .kind = .intDivision, .text = "//" },
            .{ .kind = .slash, .text = "/" },
            .{ .kind = .asterisk, .text = "*" },
            .{ .kind = .openParen, .text = "(" },
            .{ .kind = .closeParen, .text = ")" },
            .{ .kind = .openBracket, .text = "[" },
            .{ .kind = .closeBracket, .text = "]" },
            .{ .kind = .openCurly, .text = "{" },
            .{ .kind = .closeCurly, .text = "}" },
            .{ .kind = .comma, .text = "," },
            .{ .kind = .semicolon, .text = ";" },
            .{ .kind = .greaterThan, .text = ">=" },
            .{ .kind = .lessThan, .text = "<=" },
            .{ .kind = .greater, .text = ">" },
            .{ .kind = .less, .text = "<" },
            .{ .kind = .equals, .text = "==" },
            .{ .kind = .notEquals, .text = "!=" },
            .{ .kind = .assign, .text = "=" },
            .{ .kind = .colon, .text = ":" },
        };
        inline for (symbols) |symbol| {
            const i = self.char_idx + symbol.text.len;
            if (i <= self.code.len) {
                var text = self.code[self.char_idx..(self.char_idx + symbol.text.len)];
                if (std.mem.eql(u8, text, symbol.text)) {
                    const source = self.source.copy();
                    self.source.nextN(symbol.text.len);
                    self.char_idx += symbol.text.len;

                    return SnowToken.init(symbol.kind, source);
                }
            }
        }

        const c = self.code[self.char_idx];
        if (std.ascii.isDigit(c)) {
            const start = self.source;
            var hasFrac = false;
            self.source.next();
            var n: []const u8 = self.code[self.char_idx..];
            n.len = 1;
            self.char_idx += 1;

            while (true) {
                if (self.char_idx >= self.code.len) {
                    break; // we're done
                }

                const digit = self.code[self.char_idx];
                if (std.ascii.isDigit(digit)) {
                    n.len += 1;
                    self.source.next();
                    self.char_idx += 1;
                } else if (digit == '.' and !hasFrac) {
                    hasFrac = true;
                    n.len += 1;
                    self.source.next();
                    self.char_idx += 1;
                    self.source.next();
                } else if (std.ascii.isAlphabetic(digit)) {
                    // Malformed number!
                    n.len += 1;
                    self.error_store.* = try errors.SnowErrorStore.fmt("Malformed Number: {s} | If this is meant to be an identifier, consider putting a _ at the start", .{n}, self.allocator, self.source);
                    return errors.SnowError.MalformedNumber;
                } else {
                    break;
                }
            }

            // I pinky promise Zig, this is perfectly safe
            const a = std.fmt.parseFloat(f64, n) catch unreachable;
            return SnowToken.init(.{ .numberLiteral = a }, start);
        }

        if (c == '_' or std.ascii.isAlphabetic(c)) {
            const start = self.source;
            var i: []const u8 = self.code[self.char_idx..];
            i.len = 1;
            self.char_idx += 1;

            while (true) {
                if (self.char_idx >= self.code.len) {
                    break; // we're done
                }

                const ch = self.code[self.char_idx];
                if (std.ascii.isAlphanumeric(ch) or ch == '_') {
                    self.char_idx += 1;
                    self.source.skip(ch);
                    i.len += 1;
                    continue;
                }

                break;
            }
            // Comptime black magic to automate keyword
            // Looks for xKeyword and checks if the identifier is x, and if so returns the keyword.
            // This is fucking black magic
            const info: std.builtin.Type = @typeInfo(SnowTokenType);
            const tag = info.Union.tag_type.?;
            const e: std.builtin.Type.Enum = @typeInfo(tag).Enum;

            inline for (e.fields) |field| {
                if (comptime std.mem.indexOf(u8, field.name, "Keyword")) |idx| {
                    const keyword = field.name[0..idx];
                    if (std.mem.eql(u8, i, keyword)) {
                        const kind: tag = @enumFromInt(field.value);
                        return SnowToken.init(@as(SnowTokenType, kind), start);
                    }
                }
            }

            return SnowToken.init(.{ .identifier = i }, start);
        }

        if (c == '\'' or c == '"') {
            const strChar = c;
            const start = self.source;
            var str = self.code[self.char_idx..];
            str.len = 1;
            self.char_idx += 1;
            var is_escaping = false;

            while (true) {
                if (self.char_idx == self.code.len) {
                    // We reached the end of the file...
                    // This means a string literal was not closed
                    if (is_escaping) {
                        // We should report this as a malformed escape
                        self.error_store.* = try errors.SnowErrorStore.fmt("Malformed Escape: \\ expected at least 1 character after it", .{}, self.allocator, self.source);
                        return errors.SnowError.MalformedEscape;
                    }

                    self.error_store.* = try errors.SnowErrorStore.fmt("Unfinished String: String started at {s}:{}:{} was never finished", .{ start.file, start.line, start.column }, self.allocator, self.source);
                    return errors.SnowError.UnfinishedString;
                }

                const ch = self.code[self.char_idx];
                if (is_escaping) {
                    // Special escapes
                    if (ch == 'x') {
                        if (self.char_idx + 2 < self.code.len) {
                            if (std.ascii.isHex(self.code[self.char_idx + 1]) and std.ascii.isHex(self.code[self.char_idx + 2])) {
                                str.len += 3;
                                self.char_idx += 3;
                                self.source.nextN(3);
                                continue;
                            }
                        }

                        self.error_store.* = try errors.SnowErrorStore.fmt("Malformed Escape: \\x expects 2 hex digits after it", .{}, self.allocator, self.source);
                    }

                    // Generic escape
                    is_escaping = false;
                    str.len += 1;
                    self.char_idx += 1;
                    self.source.skip(ch);
                    continue;
                }

                if (ch == '\\') {
                    is_escaping = true;
                    str.len += 1;
                    self.char_idx += 1;
                    self.source.skip(ch);
                    continue;
                }

                if (ch == strChar) {
                    self.char_idx += 1;
                    self.source.skip(ch);
                    break;
                }

                self.char_idx += 1;
                self.source.skip(ch);
                str.len += 1;
            }

            str = str[1..];
            return SnowToken.init(.{ .stringLiteral = str }, start);
        }

        self.error_store.* = try errors.SnowErrorStore.fmt("Bad Character: {c}", .{c}, self.allocator, self.source);
        return errors.SnowError.BadChar;
    }
};

const testing = std.testing;

test "Lexing" {
    const code = "( ) [ ] { } + - * / // ^ > < >= <= == != = . .. ... , | ; 5 5.3 5.32 x hello fun in is local while if else do try catch return continue break and or not true false null struct then 'hello there'";
    var error_store = errors.SnowErrorStore.empty();
    errdefer error_store.deinit();

    errdefer error_store.dumpTesting();

    var lexer = SnowLexer.init("test.snow", code, &error_store, testing.allocator);

    const types = [_]SnowTokenType{
        .openParen,
        .closeParen,
        .openBracket,
        .closeBracket,
        .openCurly,
        .closeCurly,
        .plus,
        .minus,
        .asterisk,
        .slash,
        .intDivision,
        .caret,
        .greater,
        .less,
        .greaterThan,
        .lessThan,
        .equals,
        .notEquals,
        .assign,
        .dot,
        .doubleDot,
        .tripleDot,
        .comma,
        .bar,
        .semicolon,
        .{ .numberLiteral = 5 },
        .{ .numberLiteral = 5.3 },
        .{ .numberLiteral = 5.32 },
        .{ .identifier = "x" },
        .{ .identifier = "hello" },
        .funKeyword,
        .inKeyword,
        .isKeyword,
        .localKeyword,
        .whileKeyword,
        .ifKeyword,
        .elseKeyword,
        .doKeyword,
        .tryKeyword,
        .catchKeyword,
        .returnKeyword,
        .continueKeyword,
        .breakKeyword,
        .andKeyword,
        .orKeyword,
        .notKeyword,
        .trueKeyword,
        .falseKeyword,
        .nullKeyword,
        .structKeyword,
        .thenKeyword,
        .{ .stringLiteral = "hello there" },
    };

    var lexed = std.ArrayList(SnowTokenType).init(testing.allocator);
    defer lexed.deinit();

    while (true) {
        const token = try lexer.next();
        try lexed.append((token orelse break).kind);
    }

    for (types, 0..) |t, i| {
        const l = lexed.items[i];

        try testing.expect(t.eq(l));
    }
}
