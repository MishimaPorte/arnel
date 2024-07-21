const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const Tuple = @import("std").meta.Tuple;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = @import("std").mem.Allocator;
const log = @import("std").log;
const mem = @import("std").mem;
const io = @import("std").io;
const fs = @import("std").fs;
const UsizeList = @import("std").ArrayList(usize);
const ArrayList = @import("std").ArrayList;
const testing = @import("std").testing;
const Trait = @import("std").meta.TrailerFlags;
const FIFO = @import("std").fifo.LinearFifo;

pub const NodeType = enum(u8) {
    PLUS,
    UNARY_PLUS,
    MULT,
    DIV,
    MINUS,
    UNARY_MINUS,
    LITERAL_INT,
    LITERAL_STR,
    NOTHING,

    IDENT,

    INDEX,
    CALL,

    EXPR_STMT,
    PAREN_EXPR,

    POSTFIX_BANG,

    EOF,
};

fn SizedNode(num_of_children: comptime_int) type {
    return struct {
        header: NodeHeader,
        // should not be accessed directly as zig does not guarantee struct layouts
        // we use child(n) and setChild(n, value) to access this array's contents.
        // header can be accessed after a cast to a Node (SizedNode of size 0).
        children: [num_of_children]NodeChild = undefined,

        pub const NodeHeader = packed struct {
            type: NodeType,
            num_children: u8 = num_of_children,
            _: u48 = undefined,
        };

        pub const NodeChild = struct {
            data: usize,
            pub fn as(self: @This(), t: type) *t {
                return @ptrFromInt(self.data);
            }
        };

        pub fn asGeneric(self: *@This()) *Node {
            return @ptrCast(self);
        }

        pub fn child(self: *@This(), t: type, n: usize) t {
            if (@sizeOf(t) != @sizeOf(usize)) @compileError("child of a node can only be a pointer-sized type");
            const val: *t = @ptrFromInt(@intFromPtr(self) + @sizeOf(@TypeOf(self.header)) + n * @sizeOf(NodeChild));
            return val.*;
        }
        pub fn setChild(self: *@This(), n: usize, val: anytype) !void {
            if (@sizeOf(@TypeOf(val)) != @sizeOf(usize)) @compileError("child of a node can only be a pointer-sized type");
            if (n >= self.header.num_children) return ASTError.NodeHasInsufficientChildNumber;

            const val_ptr: *@TypeOf(val) = @ptrFromInt(@intFromPtr(self) + @sizeOf(@TypeOf(self.header)) + n * @sizeOf(NodeChild));
            val_ptr.* = val;
        }

        pub fn print(self: *@This(), writer: fs.File.Writer, n: usize, ns: *UsizeList) !void {
            const s = self.asGeneric();
            if (self.header.num_children != 0) try ns.append(n);

            if (n != 0) {
                for (0..n - 1) |x| {
                    if (mem.count(usize, ns.items, &[1]usize{x}) != 0) {
                        try writer.print("|    ", .{});
                    } else {
                        try writer.print("     ", .{});
                    }
                }
                try writer.print("|--->", .{});
            }
            try writer.print("node {any}, {d} children\n", .{ s.header.type, s.header.num_children });
            if (s.header.type == .CALL) {
                try s.child(*Node, 0).print(writer, n + 1, ns);
                const args_count = s.child(usize, 1);

                if (args_count == 0) {
                    _ = ns.pop();
                    return;
                }
                const args_slice = s.child([*]*Node, 2)[0..args_count];
                for (args_slice, 0..) |ch, i| {
                    if (i == args_slice.len - 1) _ = ns.pop();
                    try ch.print(writer, n + 1, ns);
                }
            } else for (0..s.header.num_children) |i| {
                if (i == s.header.num_children - 1) _ = ns.pop();
                switch (s.header.type) {
                    .LITERAL_INT, .LITERAL_STR, .IDENT => {
                        const tok = s.child(*Token, i);
                        for (0..n) |x| {
                            if (mem.containsAtLeast(usize, ns.items, 1, &[1]usize{x})) {
                                try writer.print("|    ", .{});
                            } else {
                                try writer.print("     ", .{});
                            }
                        }
                        try writer.print("|--->", .{});
                        try writer.print("{s}\n", .{tok.text});
                    },
                    else => {
                        try s.child(*Node, i).print(writer, n + 1, ns);
                    },
                }
            }
        }
    };
}

pub fn newNode(allocator: Allocator, num_of_children: comptime_int, nt: NodeType) !*Node {
    const node: *Node = @ptrCast(try allocator.create(SizedNode(num_of_children)));
    node.*.header.num_children = num_of_children;
    node.*.header.type = nt;
    return node;
}

pub const Node = SizedNode(0);

pub const ASTError = error{
    BadToken,
    NodeHasInsufficientChildNumber,
};

pub const Parser = struct {
    l: Lexer,
    a: Allocator,
    peekables: FIFO(*Node, .Dynamic),

    pub fn init(parser: *Parser, buf: []u8, allocator: Allocator) void {
        parser.l = .{
            .buf = buf,
            .fifo = FIFO(*Token, .Dynamic).init(allocator),
            .a = allocator,
        };
        parser.a = allocator;
    }

    const ParseError = error{
        UnexpectedSemicolon,
        NoSemicolonAtTheEndOfStmt,
        UnexpectedEOF,
        UnexpectedNode,
        UnexpectedToken,
        NeedClosingParen,
        NeedClosingBracket,
        CommaSeparatesFunctionArgs,
    };

    fn awaitT(self: *Parser, comptime tt: TokenType) !bool {
        const tok = try self.l.peek(0);
        return tok.type == tt;
    }

    fn parseExpression(self: *Parser, prec: u8) !*Node {
        var tok = try self.l.next();
        var lhs = switch (tok.type) {
            .IDENT => b: {
                const lit = try newNode(self.a, 1, .IDENT);
                const lit_tok = try self.a.create(Token);
                lit_tok.* = tok.*;
                try lit.setChild(0, lit_tok);
                break :b lit.asGeneric();
            },
            .INT => b: {
                const lit = try newNode(self.a, 1, .LITERAL_INT);
                const lit_tok = try self.a.create(Token);
                lit_tok.* = tok.*;
                try lit.setChild(0, lit_tok);
                break :b lit.asGeneric();
            },
            .PLUS => b: {
                const lprec = TokenType.prefixPrec(.PLUS);
                const ch = try self.parseExpression(lprec);
                const expr = try newNode(self.a, 1, .UNARY_PLUS);
                try expr.setChild(0, ch);
                break :b expr.asGeneric();
            },
            .MINUS => b: {
                const lprec = TokenType.prefixPrec(.MINUS);
                const ch = try self.parseExpression(lprec);
                const expr = try newNode(self.a, 1, .UNARY_MINUS);
                try expr.setChild(0, ch);
                break :b expr.asGeneric();
            },
            .PARENL => b: {
                const prexpr = try self.parseExpression(0);
                const n = try self.l.next();
                if (n.type != .PARENR) {
                    return ParseError.NeedClosingParen;
                }
                const expr = try newNode(self.a, 1, .PAREN_EXPR);
                try expr.setChild(0, prexpr);
                break :b expr.asGeneric();
            },
            else => return ParseError.UnexpectedToken,
        };

        while (true) {
            tok = try self.l.peek(0);
            switch (tok.type) {
                .SLASH => {
                    const precs = TokenType.infixPrecs(.SLASH);
                    if (precs.left < prec) break;
                    _ = try self.l.next();
                    const rhs = try self.parseExpression(precs.right);

                    const expr = try newNode(self.a, 2, .DIV);
                    try expr.setChild(0, lhs.asGeneric());
                    try expr.setChild(1, rhs.asGeneric());
                    lhs = expr.asGeneric();
                },
                .STAR => {
                    const precs = TokenType.infixPrecs(.STAR);
                    if (precs.left < prec) break;
                    _ = try self.l.next();
                    const rhs = try self.parseExpression(precs.right);

                    const expr = try newNode(self.a, 2, .MULT);
                    try expr.setChild(0, lhs.asGeneric());
                    try expr.setChild(1, rhs.asGeneric());
                    lhs = expr.asGeneric();
                },
                .PLUS => {
                    const precs = TokenType.infixPrecs(.PLUS);
                    if (precs.left < prec) break;
                    _ = try self.l.next();
                    const rhs = try self.parseExpression(precs.right);

                    const expr = try newNode(self.a, 2, .PLUS);
                    try expr.setChild(0, lhs.asGeneric());
                    try expr.setChild(1, rhs.asGeneric());
                    lhs = expr.asGeneric();
                },
                .MINUS => {
                    const precs = TokenType.infixPrecs(.MINUS);
                    if (precs.left < prec) break;
                    _ = try self.l.next();
                    const rhs = try self.parseExpression(precs.right);

                    const expr = try newNode(self.a, 2, .MINUS);
                    try expr.setChild(0, lhs.asGeneric());
                    try expr.setChild(1, rhs.asGeneric());
                    lhs = expr.asGeneric();
                },
                .BANG => {
                    const rprec = TokenType.postfixPrec(.BANG);
                    if (rprec >= prec) {
                        _ = try self.l.next();
                        const expr = try newNode(self.a, 1, .POSTFIX_BANG);
                        try expr.setChild(0, lhs);
                        lhs = expr.asGeneric();
                    } else break;
                },
                .BRACKETL => {
                    const rprec = TokenType.postfixPrec(.BRACKETL);
                    if (rprec >= prec) {
                        _ = try self.l.next();
                        const expr = try newNode(self.a, 2, .INDEX);
                        try expr.setChild(0, lhs);
                        const prexpr = try self.parseExpression(0);
                        const n = try self.l.next();
                        if (n.type != .BRACKETR) {
                            return ParseError.NeedClosingBracket;
                        }
                        try expr.setChild(1, prexpr);
                        lhs = expr.asGeneric();
                    } else break;
                },
                .PARENL => {
                    const rprec = TokenType.postfixPrec(.PARENL);
                    if (rprec >= prec) {
                        _ = try self.l.next();
                        // called expression, num of args and a pointer to an array of them
                        const expr = try newNode(self.a, 3, .CALL);
                        var args = ArrayList(*Node).init(self.a);

                        while (true) {
                            var n = try self.l.peek(0);
                            if (n.type == .PARENR) {
                                _ = try self.l.next();
                                break;
                            }
                            const arg = try self.parseExpression(0);
                            try args.append(arg);
                            n = try self.l.peek(0);
                            if (n.type == .PARENR) {
                                _ = try self.l.next();
                                break;
                            }
                            if (n.type != .COMMA) {
                                return ParseError.CommaSeparatesFunctionArgs;
                            }
                            _ = try self.l.next();
                        }
                        try expr.setChild(0, lhs);
                        try expr.setChild(1, args.items.len);
                        if (args.items.len != 0) {
                            try expr.setChild(2, args.items.ptr);
                        } else {
                            args.deinit();
                        }

                        lhs = expr.asGeneric();
                    } else break;
                },
                .EOF, .SEMICOLON, .PARENR, .BRACKETR, .COMMA => break,
                else => {
                    return ParseError.UnexpectedToken;
                },
            }
        }

        return lhs;
    }

    pub fn ok(self: *Parser) !bool {
        const tok = try self.l.peek(0);
        return tok.type != .EOF;
    }

    pub fn parseStmt(self: *Parser) !*Node {
        var tok = try self.l.peek(0);
        const expr = switch (tok.type) {
            else => b: {
                const expr = try self.parseExpression(0);
                const stmt = try newNode(self.a, 1, .EXPR_STMT);
                try stmt.setChild(0, expr.asGeneric());
                break :b stmt;
            },
        };

        tok = try self.l.peek(0);
        if (tok.type != .SEMICOLON) {
            return ParseError.NoSemicolonAtTheEndOfStmt;
        }
        _ = try self.l.next();
        while (true) {
            tok = try self.l.peek(0);
            if (tok.type == .SEMICOLON) {
                _ = try self.l.next();
            } else {
                break;
            }
        }

        return expr;
    }
};
