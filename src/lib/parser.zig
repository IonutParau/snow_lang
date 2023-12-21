const std = @import("std");

const Allocator = std.mem.Allocator;

const lexing = @import("lexer.zig");
const Lexer = lexing.SnowLexer;
const Token = lexing.SnowToken;
const TokenKind = lexing.SnowTokenType;
const TokenKindInfo: std.builtin.Type = @typeInfo(TokenKind);
const TokenTag = TokenKindInfo.Union.tag_type.?;
const Source = lexing.SnowSource;
const errors = @import("errors.zig");
const Error = errors.SnowError;
const ErrorStore = errors.SnowErrorStore;

pub const StatementNode = struct {
    statement: Statement,
    source: Source,
};

pub const Statement = union(enum) {
    assign: struct {
        expr: *ExpressionNode,
        to: *ExpressionNode,
    },
    ifStatement: struct {
        condition: *ExpressionNode,
        body: []StatementNode,
        fallback: ?[]StatementNode,
    },
    whileLoop: struct {
        condition: *ExpressionNode,
        body: []StatementNode,
    },
    matchStatement: []struct {
        pattern: *PatternNode,
        body: *StatementNode,
    },
    codeblock: []StatementNode,
    returnStatement: ?*ExpressionNode,
    defineLocal: struct {
        name: []const u8,
        val: ?*ExpressionNode,
    },
    defineFunction: struct {
        name: []const u8,
        args: []const []const u8,
        body: []StatementNode,
        local: bool,
    },
    call: struct {
        callee: *ExpressionNode,
        args: []ExpressionNode,
    },
};

pub const PatternNode = struct {
    pattern: Pattern,
    source: Source,
};

pub const Pattern = union(enum) {
    variable: []const u8,
    field: struct {
        field: []const u8,
        of: *PatternNode,
    },
    wildcard,
    defineLocal: []const u8,
    number: f64,
    string: []const u8,
    list: []union(enum) {
        pattern: *PatternNode,
        repeating,
        namedRepeat: []const u8,
    },
    tuple: []PatternNode,
};

pub const ExpressionNode = struct {
    expression: Expression,
    source: Source,
};

pub const Expression = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    null,
    tuple: []ExpressionNode,
    list: []ExpressionNode,
    structLiteral: []struct {
        field: []const u8,
        value: *ExpressionNode,
    },
    tableLiteral: []struct {
        field: *ExpressionNode,
        value: *ExpressionNode,
    },
    variable: []const u8,
    ifExpr: struct {
        condition: *ExpressionNode,
        body: *ExpressionNode,
        fallback: *ExpressionNode,
    },
    matchExpr: []struct {
        pattern: *PatternNode,
        response: *ExpressionNode,
    },
    codeblock: []ExpressionNode,
    returnExpr: ?*ExpressionNode,
    shortLambda: struct {
        args: []const []const u8,
        expr: *ExpressionNode,
    },
    longLambda: struct {
        args: []const []const u8,
        body: []ExpressionNode,
    },
    call: struct {
        callee: *ExpressionNode,
        args: []ExpressionNode,
    },
    field: struct {
        expr: *ExpressionNode,
        name: []const u8,
    },
    index: struct {
        expr: *ExpressionNode,
        idx: *ExpressionNode,
    },
    opcode: struct {
        a: *ExpressionNode,
        b: *ExpressionNode,
        op: enum { Add, Sub, Mult, Div, IntDiv, Power, Mod },
    },
};

fn ValueToSExpression(allocator: Allocator, node: anytype) ![]const u8 {
    const t: std.builtin.Type = @typeInfo(@TypeOf(node));
    switch (t) {
        .Int, .Float => {
            return std.fmt.allocPrint(allocator, "{}", .{node});
        },
        .Enum => {
            return std.fmt.allocPrint(allocator, "({s} {s})", .{ @typeName(@TypeOf(node)), @tagName(node) });
        },
        .Union => |u| {
            const typeName = @typeName(@TypeOf(node));
            const tagName = @tagName(node);

            inline for (u.fields) |field| {
                if (comptime std.mem.eql(u8, tagName, field.name)) {
                    const v = @field(node, field.name);
                    const m = try ValueToSExpression(allocator, v);
                    defer allocator.free(m);
                    return std.fmt.allocPrint(allocator, "({s} {s} {s})", .{ typeName, tagName, m });
                }
            }
        },
        .Struct => |s| {
            const typeName = @typeName(@TypeOf(node));
            var m = std.fmt.allocPrint("({s}", .{typeName});

            inline for (s.fields) |field| {
                const v = @field(node, field.name);
                const vm = ValueToSExpression(allocator, v);
                defer allocator.free(vm);

                m = try std.fmt.allocPrint(" ({s} {s})", .{ field.name, vm });
            }

            m = try std.fmt.allocPrint("{s})", .{m});
            return m;
        },
        .Pointer => |p| {
            switch (p.size) {
                .One => return ValueToSExpression(node.*),
                else => return std.fmt.allocPrint("{s}", .{node}),
            }
        },
        else => @compileError("ValueToSExpression does not support" ++ @typeName(@TypeOf(node))),
    }
}

pub const Parser = struct {
    error_store: *ErrorStore,
    lexer: Lexer,
    allocator: Allocator,

    const Self = @This();

    pub fn init(file: []const u8, code: []const u8, allocator: Allocator, error_store: *ErrorStore) Self {
        var lexer = Lexer.init(file, code, error_store, allocator);

        return .{
            .error_store = error_store,
            .lexer = lexer,
            .allocator = allocator,
        };
    }

    pub fn alloc(self: *Self, x: anytype) !*@TypeOf(x) {
        var p = try self.allocator.alloc(@TypeOf(x));
        p.* = x;
        return p;
    }

    pub fn parseStatement(self: *Self) Error!StatementNode {
        var token = try self.nextToken();

        if (token.kind.eq(.localKeyword)) {
            // Name
            const name = try self.expectNextToken(.identifier, "Syntax Error: Expected identifier");
            const local_name = try self.allocator.dupe(u8, name.kind.identifier);
            const t = try self.peekToken();
            if (t.kind.eq(.assign)) {
                _ = try self.nextToken();
                var expr = try self.parseExpressionOps(0);
                var e = try self.allocator.create(ExpressionNode);
                e.* = expr;

                return StatementNode{ .statement = .{ .defineLocal = .{ .name = local_name, .val = e } }, .source = token.source };
            }

            return StatementNode{ .statement = .{ .defineLocal = .{ .name = local_name, .val = null } }, .source = token.source };
        }

        self.error_store.* = try ErrorStore.fmt("Syntax Error: Expected statement", .{}, self.allocator, self.lexer.source);
        return Error.SyntaxError;
    }

    pub fn isValidAssignee(expr: ExpressionNode) bool {
        return switch (expr.expression) {
            .tuple => |t| {
                for (t) |e| {
                    if (!Self.isValidAssignee(e)) {
                        return false;
                    }
                }
                return true;
            },
            .list => |t| {
                for (t) |e| {
                    if (!Self.isValidAssignee(e)) {
                        return false;
                    }
                }
                return true;
            },
            .variable => true,
            .field => true,
            .index => true,
            else => false,
        };
    }

    pub fn checkDone(self: *Self, token: ?Token) Error!Token {
        if (token) |t| {
            return t;
        }

        self.error_store.* = try ErrorStore.fmt("Syntax Error: Code ends abruptly", .{}, self.allocator, self.lexer.source);
        return Error.SyntaxError;
    }

    fn nextToken(self: *Self) Error!Token {
        return self.checkDone(try self.lexer.next());
    }

    fn expectNextToken(self: *Self, kind: TokenTag, comptime msg: []const u8) Error!Token {
        const token = try self.nextToken();

        if (@as(TokenTag, token.kind) == kind) {
            return token;
        }

        self.error_store.* = try ErrorStore.fmt(msg, .{}, self.allocator, token.source);
        return Error.SyntaxError;
    }

    fn peekToken(self: *Self) Error!Token {
        return self.checkDone(try self.lexer.peek());
    }

    fn expectPeekToken(self: *Self, kind: TokenTag, comptime msg: []const u8) Error!Token {
        const token = try self.peekToken();

        if (@as(TokenTag, token.kind) == kind) {
            return token;
        }

        self.error_store.* = try ErrorStore.fmt(msg, .{}, self.allocator, token.source);
        return Error.SyntaxError;
    }

    pub fn parseExpression(self: *Self) Error!ExpressionNode {
        const token = try self.nextToken();
        const tag = @as(TokenTag, token.kind);

        if (tag == .numberLiteral) {
            return ExpressionNode{ .expression = .{ .number = token.kind.numberLiteral }, .source = token.source };
        }

        if (tag == .stringLiteral) {
            const str = try self.allocator.dupe(u8, token.kind.stringLiteral);
            return ExpressionNode{ .expression = .{ .string = str }, .source = token.source };
        }

        if (tag == .identifier) {
            const ident = try self.allocator.dupe(u8, token.kind.identifier);
            var node = ExpressionNode{ .expression = .{ .variable = ident }, .source = token.source };

            while (!self.lexer.done()) {
                const t = try self.peekToken();
                switch (t.kind) {
                    .dot => {
                        // Field access
                        _ = try self.nextToken();
                        const field = try self.expectNextToken(.identifier, "Syntax Error: Expected identifier");
                        const v = try self.allocator.create(ExpressionNode);
                        v.* = node;
                        node = ExpressionNode{ .expression = .{ .field = .{ .expr = v, .name = field.kind.identifier } }, .source = t.source };
                    },
                    .openBracket => {
                        // Potential index

                        // Indexing must be on the same line
                        // This is to disambiguate between
                        // local v = name[hello]
                        // And
                        // local v = name
                        // [hello] = v
                        // This could also be fixed with Automatic Semicolon Insertion, but I'm lazy
                        if (t.source.line != node.source.line) {
                            break;
                        }

                        _ = try self.nextToken();
                        const e = try self.parseExpressionOps(0);
                        const ep = try self.allocator.create(ExpressionNode);
                        ep.* = e;
                        const v = try self.allocator.create(ExpressionNode);
                        v.* = node;
                        node = ExpressionNode{ .expression = .{ .index = .{ .expr = v, .idx = ep } }, .source = t.source };
                    },
                    .openParen => {
                        // Potential call

                        // Calls must be on the same line too
                        // This is to diambiguate between
                        // local v = foo(bar)
                        // And
                        // local v = foo
                        // (bar) = v
                        if (t.source.line != node.source.line) {
                            break;
                        }

                        _ = try self.nextToken();
                        const ct = try self.peekToken();
                        if (ct.kind == .closeParen) {
                            _ = try self.nextToken();
                            const a = try self.allocator.alloc(ExpressionNode, 0);
                            const v = try self.allocator.create(ExpressionNode);
                            v.* = node;
                            node = ExpressionNode{ .expression = .{ .call = .{ .callee = v, .args = a } }, .source = t.source };
                        } else {
                            var l = std.ArrayList(ExpressionNode).init(self.allocator);
                            errdefer l.deinit();
                            const e = try self.parseExpressionOps(0);
                            try l.append(e);

                            while (true) {
                                const nt = try self.nextToken();
                                if (nt.kind == .comma) {
                                    if ((try self.peekToken()).kind == .closeParen) {
                                        _ = try self.nextToken();
                                        break; // Trailing comma is a life saver
                                    }
                                    const ne = try self.parseExpressionOps(0);
                                    try l.append(ne);
                                } else if (nt.kind == .closeParen) {
                                    break; // Call done
                                }
                            }

                            const v = try self.allocator.create(ExpressionNode);
                            v.* = node;
                            node = ExpressionNode{ .expression = .{ .call = .{ .callee = v, .args = l.items } }, .source = t.source };
                        }
                    },
                    else => break, // Just give up otherwise
                }
            }

            return node;
        }

        if (tag == .trueKeyword or tag == .falseKeyword) {
            return ExpressionNode{ .expression = .{ .boolean = (tag == .trueKeyword) }, .source = token.source };
        }

        if (tag == .nullKeyword) {
            return ExpressionNode{ .expression = .null, .source = token.source };
        }

        if (tag == .openParen) {
            const expr = self.parseExpressionOps(0);
            const firstComma = try self.nextToken();

            if (firstComma.kind.eq(.closeParen)) {
                return expr;
            } else if (firstComma.kind.eq(.comma)) {
                // We go full tuple mode
                var tuple = std.ArrayList(ExpressionNode).init(self.allocator);
                errdefer tuple.deinit();

                while (true) {
                    const e = try self.parseExpressionOps(0);
                    try tuple.append(e);
                    const t = try self.nextToken();
                    if (t.kind.eq(.comma)) {
                        continue;
                    } else if (t.kind.eq(.closeParen)) {
                        break;
                    } else {
                        self.error_store.* = try ErrorStore.fmt("Syntax Error: Expected ) or ,", .{}, self.allocator, token.source);
                        return Error.SyntaxError;
                    }
                }

                return ExpressionNode{ .expression = .{ .tuple = tuple.items }, .source = token.source };
            } else {
                self.error_store.* = try ErrorStore.fmt("Syntax Error: Expected ) or ,", .{}, self.allocator, token.source);
                return Error.SyntaxError;
            }
        }

        if (tag == .openBracket) {
            var l = std.ArrayList(ExpressionNode).init(self.allocator);
            errdefer l.deinit();

            if ((try self.peekToken()).kind.eq(.closeBracket)) {
                _ = try self.nextToken();
            } else while (true) {
                const e = try self.parseExpressionOps(0);
                try l.append(e);

                const t = try self.nextToken();
                if (t.kind.eq(.comma)) {
                    continue;
                } else if (t.kind.eq(.closeParen)) {
                    break;
                } else {
                    self.error_store.* = try ErrorStore.fmt("Syntax Error: Expected ) or ,", .{}, self.allocator, token.source);
                    return Error.SyntaxError;
                }
            }

            return ExpressionNode{ .expression = .{ .list = l.items }, .source = token.source };
        }

        self.error_store.* = try ErrorStore.fmt("Syntax Error: Expected expression", .{}, self.allocator, token.source);
        return Error.SyntaxError;
    }

    pub fn parseExpressionOps(self: *Self, bias: usize) Error!ExpressionNode {
        _ = bias;
        return self.parseExpression();
    }

    pub fn parsePattern(self: *Self) Error!PatternNode {
        _ = self;
        std.debug.panic("Not implemented yet, sorry", .{});
    }

    pub fn parse(self: *Self) Error![]StatementNode {
        var asts = std.ArrayList(StatementNode).init(self.allocator);
        errdefer asts.deinit();

        while (!self.done()) {
            try asts.append(try self.parseStatement());
        }

        return asts.items;
    }

    pub fn done(self: *const Self) bool {
        return self.lexer.done();
    }
};

test "Parser can parse basic expressions" {
    var error_store: ErrorStore = undefined;
    errdefer error_store.dumpTesting();

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var parser = Parser.init("test.snow", "57 true false null \"hello there\" x", arena.allocator(), &error_store);
    const a = try parser.parseExpression();
    const b = try parser.parseExpression();
    const c = try parser.parseExpression();
    const d = try parser.parseExpression();
    const e = try parser.parseExpression();
    const f = try parser.parseExpression();

    try std.testing.expect(a.expression.number == 57);
    try std.testing.expect(b.expression.boolean == true);
    try std.testing.expect(c.expression.boolean == false);
    try std.testing.expectEqualStrings("null", @tagName(d.expression));
    try std.testing.expect(std.mem.eql(u8, e.expression.string, "hello there"));
    try std.testing.expect(std.mem.eql(u8, f.expression.variable, "x"));

    try std.testing.expect(parser.done()); // Parser should say it is done
}
