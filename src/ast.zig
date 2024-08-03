const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;
const TokId = @import("lexer.zig").TokId;
const Tuple = @import("std").meta.Tuple;
const Lexer = @import("lexer.zig").Lexer;
const Allocator = @import("std").mem.Allocator;
const log = @import("std").log;
const mem = @import("std").mem;
const io = @import("std").io;
const fs = @import("std").fs;
const UsizeList = @import("std").ArrayList(usize);
const NodeList = @import("std").ArrayList(NodeId);
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

    IDENT,
    STRING_LITERAL,

    TYPE,
    INDEX,
    CALL,

    RETURN,

    EXPR_STMT,
    PAREN_EXPR,

    POSTFIX_BANG,
    // Two children - stmt_ptr and stmt_amount
    FUNC_DECL,
    PARAM_DECL,
    /// Variadic nodes have runtime-known number of children.
    VARIADIC,

    EOF,

    pub fn getChildNumber(self: NodeType) usize {
        return switch (self) {
            .EXPR_STMT, .UNARY_PLUS, .TYPE, .UNARY_MINUS, .LITERAL_INT, .POSTFIX_BANG, .IDENT, .STRING_LITERAL, .RETURN => 1,

            .PAREN_EXPR, .PLUS, .PARAM_DECL, .MULT, .DIV, .MINUS, .INDEX => 2,

            .FUNC_DECL => 3,

            .CALL => 3,
            .VARIADIC => @panic("variadics should be handled separately!"),

            .EOF => 0,
        };
    }
};

pub const ASTError = error{
    BadToken,
    NodeHasInsufficientChildNumber,
};

pub fn printNode(self: NodeId, storage: *NodeArena, tokens: *Lexer, writer: fs.File.Writer, n: usize, ns: *UsizeList) !void {
    const s = storage.get(0, self.handle);
    const num_children = self.type.getChildNumber();
    if (num_children != 0) try ns.append(n);

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
    try writer.print("node[{d}] {any}, {d} children\n", .{ n, self.type, num_children });
    if (self.type == .CALL) {
        try printNode(s.child(NodeId, 0), storage, tokens, writer, n + 1, ns);
        const args_count = s.child(usize, 1);

        if (args_count == 0) {
            _ = ns.pop();
            return;
        }
        const args_slice = s.child([*]NodeId, 2)[0..args_count];
        for (args_slice, 0..) |ch, i| {
            if (i == args_slice.len - 1) _ = ns.pop();
            try printNode(ch, storage, tokens, writer, n + 1, ns);
        }
    } else if (self.type == .FUNC_DECL) {
        const name = s.child(TokId, 0);
        const name_token = try tokens.getToken(name.handle);
        for (0..n) |x| {
            if (mem.containsAtLeast(usize, ns.items, 1, &[1]usize{x})) {
                try writer.print("|    ", .{});
            } else {
                try writer.print("     ", .{});
            }
        }
        try writer.print("|--->", .{});
        try writer.print("{s}\n", .{name_token.text});
        const args = s.child(NodeId, 1);
        const args_len = s.child(usize, 2);
        const args_node = storage.get(0, args.handle);
        for (0..args_len) |i| {
            try printNode(args_node.child(NodeId, i), storage, tokens, writer, n + 1, ns);
        }

        const return_type = s.child(NodeId, 3);
        try printNode(return_type, storage, tokens, writer, n + 1, ns);

        const stmts = s.child(NodeId, 4);
        const stmts_len = s.child(usize, 5);
        const stmts_node = storage.get(0, stmts.handle);
        for (0..stmts_len) |i| {
            if (i == stmts_len - 1) _ = ns.pop();
            try printNode(stmts_node.child(NodeId, i), storage, tokens, writer, n + 1, ns);
        }
    } else for (0..num_children) |i| {
        if (i == num_children - 1) _ = ns.pop();
        switch (self.type) {
            .LITERAL_INT, .STRING_LITERAL, .TYPE, .IDENT => {
                const tokid = s.child(TokId, i);
                const tok = try tokens.getToken(tokid.handle);
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
            .PARAM_DECL => if (i == 0) {
                const tokid = s.child(TokId, i);
                const tok = try tokens.getToken(tokid.handle);
                for (0..n) |x| {
                    if (mem.containsAtLeast(usize, ns.items, 1, &[1]usize{x})) {
                        try writer.print("|    ", .{});
                    } else {
                        try writer.print("     ", .{});
                    }
                }
                try writer.print("|--->", .{});
                try writer.print("{s}\n", .{tok.text});
            } else try printNode(s.child(NodeId, i), storage, tokens, writer, n + 1, ns),
            else => {
                try printNode(s.child(NodeId, i), storage, tokens, writer, n + 1, ns);
            },
        }
    }
}

/// NodeId consists of node's type (1 byte), number of node's children
/// (a single byte too, but seems unlikely to be needed since
/// we never need a dynamically-sized nodes, it seems),
/// and a u48 pointer into a dynamically-allocated array of node's children contents.
pub const NodeId = packed struct(usize) {
    type: NodeType = undefined,
    num_children: u8,
    /// A pointer into a separately-allocated memory buffer.
    /// math.maxInt(u48) is a niche value, indicating no content has been allocated.
    /// The storage must return a valid zero-sized type for that niche.
    handle: u48,

    pub fn deinit(self: NodeId, parser: *Parser) void {
        switch (self.type) {
            .FUNC_DECL => {
                const cont = parser.nodes.get(5, self.handle);
                const params = cont.child([*]NodeId, 0);
                const params_len = cont.child(usize, 5);
                NodeList.fromOwnedSlice(parser.a, params[0..params_len]).deinit();
                const stmts = cont.child([*]NodeId, 3);
                const stmts_len = cont.child(usize, 6);
                NodeList.fromOwnedSlice(parser.a, stmts[0..stmts_len]).deinit();
            },
            else => return,
        }
    }
};

/// This struct wraps an array of node's children. It is stored in a separate memory buffer,
/// and the location is pointed to by the lesser 48 bits of the nodeId.
fn SizedNode(num_of_children: comptime_int) type {
    return struct {
        // should not be accessed directly as zig does not guarantee struct layouts
        // we use child(n) and setChild(n, value) to access this array's contents.
        // header can be accessed after a cast to a Node (SizedNode of size 0).
        children: [num_of_children]NodeChild = undefined,

        pub const NodeChild = struct {
            data: usize,

            pub fn as(self: @This(), t: type) *t {
                return @ptrFromInt(self.data);
            }

            pub fn asNode(self: @This()) NodeId {
                return @bitCast(self.data);
            }
        };

        pub fn asGeneric(self: *@This()) *Node {
            return @ptrCast(self);
        }

        pub fn child(self: *@This(), t: type, n: usize) t {
            if (@sizeOf(t) != @sizeOf(usize)) @compileError("child of a node can only be a pointer-sized type");
            const addr: *t = @ptrFromInt(@intFromPtr(&@as(*SizedNode(1), @ptrCast(self)).children[0]) + n * @sizeOf(usize));
            return addr.*;
        }

        pub fn setChild(self: *@This(), n: usize, val: anytype) !void {
            if (@sizeOf(@TypeOf(val)) != @sizeOf(usize))
                @compileError("child of a node can only be a pointer-sized type");

            if (n >= num_of_children) return ASTError.NodeHasInsufficientChildNumber;
            self.children[n].data = if (@typeInfo(@TypeOf(val)) == .Pointer)
                @intFromPtr(val)
            else
                @bitCast(val);
        }
    };
}

pub const Node = SizedNode(0);

const NodeArena = struct {
    const debug = false;
    const Buffer = @import("std").ArrayList(usize);
    const math = @import("std").math;
    const NodeArenaError = error{
        TooManyItems,
        BadHandle,
    };

    memory: Buffer,

    pub fn init(allocator: Allocator) NodeArena {
        return .{
            .memory = Buffer.init(allocator),
        };
    }

    pub fn deinit(self: *NodeArena) void {
        self.memory.deinit();
    }

    /// Since the compiler is purely non-multithreaded, we can do this
    /// sort of thing without ever needing to store the state (i.e. the fact that we
    /// are inside an arena list creation faze) anywhere.
    pub const NodeArenaList = struct {
        arena: *NodeArena,
        handle: u48,
        len: usize,

        const print = @import("std").debug.print;

        pub fn append(self: *NodeArenaList, val: anytype) !void {
            if (@sizeOf(@TypeOf(val)) > @sizeOf(usize))
                @compileError("only types of usize size are permitted inside the arena.");
            print("handle: {d}, len: {d}, mem: {d}\nthing: {any}\n", .{
                self.handle,
                self.len,
                self.arena.memory.items.len,
                val,
            });
            try self.arena.memory.append(@bitCast(val));

            self.len += 1;
        }

        pub fn appendSlice(self: *NodeArenaList, T: type, val: []T) !void {
            if (@sizeOf(@TypeOf(val)) != @sizeOf(usize))
                @compileError("only types of usize size are permitted inside the arena.");
            try self.arena.memory.appendSlice(val);
            self.len += val.len;
            print("handle: {d}, len: {d}, mem: {d}\nthing: {any}\n", .{
                self.handle,
                self.len,
                self.arena.memory.items.len,
                val,
            });
            try self.arena.memory.append(@bitCast(val));
        }
    };

    pub fn createList(self: *NodeArena) NodeArenaList {
        return .{
            .arena = self,
            .len = 0,
            .handle = @intCast(self.memory.items.len),
        };
    }

    pub fn get(self: *NodeArena, comptime num_of_children: usize, handle: u48) *SizedNode(num_of_children) {
        if (debug) {
            if (handle >= math.maxInt(u48))
                @panic("bad handle!");
            if (handle >= self.memory.items.len)
                @panic("bad handle!");
        }

        return @ptrCast(&self.memory.items[handle]);
    }

    /// Gets an element by a handle in case it being not a Node.
    pub fn getT(self: *NodeArena, T: type, handle: u48) T {
        if (debug) {
            if (handle >= math.maxInt(u48))
                @panic("bad handle!");
            if (handle >= self.memory.items.len)
                @panic("bad handle!");
        }

        if (@typeInfo(T) == .Pointer)
            return @intFromPtr(self.memory.items[handle])
        else
            return @bitCast(self.memory.items[handle]);
    }

    pub fn createFromSlice(self: *NodeArena, T: type, slice: []T) !NodeId {
        if (slice.len == 0 or @sizeOf(T) == 0) return .{
            .num_children = 0,
            .handle = math.maxInt(u48),
        };
        const handle = self.memory.items.len;
        if (handle > math.maxInt(u48))
            return NodeArenaError.TooManyItems;
        const mem_ptr = self.memory.addManyAsSlice(slice.len) catch unreachable;
        @memcpy(mem_ptr, @as([*]usize, @ptrCast(slice.ptr))[0..slice.len]);
        return .{
            .handle = @intCast(handle),
            .num_children = 0,
            .type = .VARIADIC,
        };
    }

    pub fn create(self: *NodeArena, comptime num_of_children: usize) !NodeId {
        if (num_of_children == 0) return .{
            .num_children = 0,
            .handle = math.maxInt(u48),
        };
        const handle = self.memory.items.len;
        if (handle > math.maxInt(u48))
            return NodeArenaError.TooManyItems;
        try self.memory.addManyAsArray(num_of_children);
        return .{
            .handle = handle,
            .num_children = num_of_children,
        };
    }

    pub fn createT(self: *NodeArena, comptime num_of_children: usize, comptime t: NodeType) !NodeId {
        if (num_of_children == 0) return .{
            .num_children = 0,
            .type = t,
            .handle = math.maxInt(u48),
        };
        const handle = self.memory.items.len;
        if (handle > math.maxInt(u48))
            return NodeArenaError.TooManyItems;
        _ = try self.memory.addManyAsArray(num_of_children);
        return .{
            .handle = @intCast(handle),
            .type = t,
            .num_children = num_of_children,
        };
    }
};

pub const Parser = struct {
    l: Lexer,
    a: Allocator,
    /// it is a plain memory buffer of max length math.maxInt(u48),
    /// so handles to it may be u48 in size.
    nodes: NodeArena,

    pub fn init(buf: []const u8, allocator: Allocator) Parser {
        return .{
            .l = Lexer.init(allocator, buf),
            .a = allocator,
            .nodes = NodeArena.init(allocator),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.l.deinit();
        self.nodes.deinit();
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

    fn awaitTDiscard(self: *Parser, comptime tt: TokenType) !void {
        const tokid = try self.l.next();
        if (tokid.type != tt)
            return ASTError.BadToken;
    }

    fn awaitT(self: *Parser, comptime tt: TokenType) !TokId {
        const tokid = try self.l.next();
        if (tokid.type != tt)
            return ASTError.BadToken;
        return tokid;
    }

    fn parseExpression(self: *Parser, prec: u8) !NodeId {
        var tokid = try self.l.next();
        var lhs = switch (tokid.type) {
            .IDENT => b: {
                const lit = try self.nodes.createT(1, .IDENT);
                const node = self.nodes.get(1, lit.handle);
                try node.setChild(0, tokid);
                break :b lit;
            },
            .STRING => b: {
                const lit = try self.nodes.createT(1, .STRING_LITERAL);
                const node = self.nodes.get(1, lit.handle);
                try node.setChild(0, tokid);
                break :b lit;
            },
            .INT => b: {
                const lit = try self.nodes.createT(1, .LITERAL_INT);
                const node = self.nodes.get(1, lit.handle);
                try node.setChild(0, tokid);
                break :b lit;
            },
            .PLUS => b: {
                const lprec = TokenType.prefixPrec(.PLUS);
                const ch = try self.parseExpression(lprec);
                const expr = try self.nodes.createT(1, .UNARY_PLUS);
                const node = self.nodes.get(1, expr.handle);
                try node.setChild(0, ch);
                break :b expr;
            },
            .MINUS => b: {
                const lprec = TokenType.prefixPrec(.MINUS);
                const ch = try self.parseExpression(lprec);
                const expr = try self.nodes.createT(1, .UNARY_MINUS);
                const node = self.nodes.get(1, expr.handle);
                try node.setChild(0, ch);
                break :b expr;
            },
            .PARENL => b: {
                const prexpr = try self.parseExpression(0);
                const n = try self.l.next();
                if (n.type != .PARENR) {
                    return ParseError.NeedClosingParen;
                }
                const expr = try self.nodes.createT(1, .UNARY_MINUS);
                const node = self.nodes.get(1, expr.handle);
                try node.setChild(0, prexpr);
                break :b expr;
            },
            else => return ParseError.UnexpectedToken,
        };

        while (true) {
            tokid = try self.l.peek(0);
            switch (tokid.type) {
                .SLASH => {
                    const precs = TokenType.infixPrecs(.SLASH);
                    if (precs.left < prec) break;
                    _ = try self.l.next();
                    const rhs = try self.parseExpression(precs.right);

                    const expr = try self.nodes.createT(2, .DIV);
                    const node = self.nodes.get(2, expr.handle);
                    try node.setChild(0, lhs);
                    try node.setChild(1, rhs);
                    lhs = expr;
                },
                .STAR => {
                    const precs = TokenType.infixPrecs(.STAR);
                    if (precs.left < prec) break;
                    _ = try self.l.next();
                    const rhs = try self.parseExpression(precs.right);

                    const expr = try self.nodes.createT(2, .MULT);
                    const node = self.nodes.get(2, expr.handle);
                    try node.setChild(0, lhs);
                    try node.setChild(1, rhs);
                    lhs = expr;
                },
                .PLUS => {
                    const precs = TokenType.infixPrecs(.PLUS);
                    if (precs.left < prec) break;
                    _ = try self.l.next();
                    const rhs = try self.parseExpression(precs.right);

                    const expr = try self.nodes.createT(2, .PLUS);
                    const node = self.nodes.get(2, expr.handle);
                    try node.setChild(0, lhs);
                    try node.setChild(1, rhs);
                    lhs = expr;
                },
                .MINUS => {
                    const precs = TokenType.infixPrecs(.MINUS);
                    if (precs.left < prec) break;
                    _ = try self.l.next();
                    const rhs = try self.parseExpression(precs.right);

                    const expr = try self.nodes.createT(2, .MINUS);
                    const node = self.nodes.get(2, expr.handle);
                    try node.setChild(0, lhs);
                    try node.setChild(1, rhs);
                    lhs = expr;
                },
                .BANG => {
                    const rprec = TokenType.postfixPrec(.BANG);
                    if (rprec >= prec) {
                        _ = try self.l.next();
                        const expr = try self.nodes.createT(1, .POSTFIX_BANG);
                        const node = self.nodes.get(2, expr.handle);
                        try node.setChild(0, lhs);
                        lhs = expr;
                    } else break;
                },
                .BRACKETL => {
                    const rprec = TokenType.postfixPrec(.BRACKETL);
                    if (rprec >= prec) {
                        _ = try self.l.next();
                        const expr = try self.nodes.createT(1, .POSTFIX_BANG);
                        const node = self.nodes.get(2, expr.handle);
                        try node.setChild(0, lhs);
                        const prexpr = try self.parseExpression(0);
                        const n = try self.l.next();
                        if (n.type != .BRACKETR) {
                            return ParseError.NeedClosingBracket;
                        }
                        try node.setChild(1, prexpr);
                        lhs = expr;
                    } else break;
                },
                .PARENL => {
                    const rprec = TokenType.postfixPrec(.PARENL);
                    if (rprec >= prec) {
                        _ = try self.l.next();
                        // called expression, num of args and a pointer to an array of them
                        const expr = try self.nodes.createT(3, .CALL);
                        const node = self.nodes.get(2, expr.handle);
                        var args = NodeList.init(self.a);

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
                        try node.setChild(0, lhs);
                        try node.setChild(1, args.items.len);
                        if (args.items.len != 0) {
                            try node.setChild(2, args.items.ptr);
                        } else {
                            args.deinit();
                        }

                        lhs = expr;
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
        const tokid = try self.l.peek(0);
        return tokid.type != .EOF;
    }

    pub fn parseStmt(self: *Parser) !NodeId {
        var tokid = try self.l.peek(0);
        const expr = switch (tokid.type) {
            .RETURN => b: {
                _ = try self.l.next();
                const expr = try self.parseExpression(0);
                const stmt = try self.nodes.createT(1, .RETURN);
                const node = self.nodes.get(1, stmt.handle);
                try node.setChild(0, expr);
                break :b stmt;
            },
            else => b: {
                const expr = try self.parseExpression(0);
                const stmt = try self.nodes.createT(1, .EXPR_STMT);
                const node = self.nodes.get(1, stmt.handle);
                try node.setChild(0, expr);
                break :b stmt;
            },
        };

        tokid = try self.l.peek(0);
        if (tokid.type != .SEMICOLON) {
            return ParseError.NoSemicolonAtTheEndOfStmt;
        }
        _ = try self.l.next();
        while (true) {
            tokid = try self.l.peek(0);
            if (tokid.type == .SEMICOLON) {
                _ = try self.l.next();
            } else {
                break;
            }
        }

        return expr;
    }

    /// Only single-name types are supported for now.
    pub fn parseType(self: *Parser) !NodeId {
        const t = try self.nodes.createT(1, .TYPE);
        const node = self.nodes.get(1, t.handle);
        try node.setChild(0, try self.awaitT(.IDENT));
        return t;
    }

    pub fn parseConstDecl(self: *Parser) !NodeId {
        _ = try self.l.next();
        const name = try self.l.next();
        if (name.type != .IDENT)
            return ASTError.BadToken;
        try self.awaitTDiscard(.EQ);
        try self.awaitTDiscard(.PARENL);

        var params = NodeList.init(self.a);
        defer params.deinit();
        while (true) {
            var n = try self.l.peek(0);
            if (n.type == .PARENR) {
                _ = try self.l.next();
            }
            // param consists of a token pointer and a type node pointer
            // param list is a node of size and dynamic memory pointer
            const param_type = try self.parseType();
            const param_name = try self.awaitT(.IDENT);
            const param = try self.nodes.createT(2, .PARAM_DECL);
            var param_cont = self.nodes.get(2, param.handle);
            try param_cont.setChild(0, param_name);
            try param_cont.setChild(1, param_type);
            try params.append(param);
            n = try self.l.next();
            if (n.type == .COMMA)
                continue
            else if (n.type == .PARENR)
                break
            else
                return ASTError.BadToken;
        }
        const params_allocated = try self.nodes.createFromSlice(NodeId, params.items);
        try self.awaitTDiscard(.ARROW);
        const return_type = try self.parseType();
        try self.awaitTDiscard(.CURLYL);
        var stmts = NodeList.init(self.a);
        defer stmts.deinit();
        while (true) {
            const n = try self.l.peek(0);
            if (n.type == .CURLYR) {
                _ = try self.l.next();
                break;
            } else if (n.type == .EOF) return ASTError.BadToken;
            try stmts.append(try self.parseStmt());
        }
        const stmts_allocated = try self.nodes.createFromSlice(NodeId, stmts.items);

        const func = try self.nodes.createT(6, .FUNC_DECL);
        const node = self.nodes.get(6, func.handle);

        try node.setChild(0, name);
        try node.setChild(1, params_allocated);
        try node.setChild(2, params.items.len);
        try node.setChild(3, return_type);
        try node.setChild(4, stmts_allocated);
        try node.setChild(5, stmts.items.len);

        return func;
    }

    pub fn parseTopLevel(self: *Parser) !NodeId {
        const tokid = try self.l.peek(0);
        const node = switch (tokid.type) {
            .CONST => self.parseConstDecl(),
            .EOF => return NodeId{
                .type = .EOF,
                .handle = 0,
                .num_children = 0,
            },
            else => return ASTError.BadToken,
        };
        try self.awaitTDiscard(.SEMICOLON);
        return node;
    }
};

test "ast_working" {
    var parser = Parser.init(
        \\const print = (int a, int c, int b) -> int {
        \\    1+1;
        \\    1+1;
        \\    1+1;
        \\    1+1;
        \\    1+1;
        \\    return 1 + 1;
        \\};
    , testing.allocator);
    defer parser.deinit();
    while (try parser.ok()) {
        const node = try parser.parseTopLevel();
        var printList = UsizeList.init(testing.allocator);
        defer printList.deinit();
        const wr = io.getStdOut().writer();
        try wr.print("\n", .{});
        try printNode(node, &parser.nodes, &parser.l, wr, 0, &printList);
    }
}
