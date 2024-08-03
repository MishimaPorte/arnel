const ascii = @import("std").ascii;
const FIFO = @import("std").fifo.LinearFifo;
const mem = @import("std").mem;
const math = @import("std").math;
const log = @import("std").log;
const Allocator = @import("std").mem.Allocator;
const TokenList = @import("std").ArrayList(Token);

pub const TokenType = enum(u8) {
    // integers
    INT,
    FLOAT,
    STRANGE_INTEGER,

    // keywords
    IMPORT,
    EXPORT,
    RETURN,
    DO,
    CONST,
    TRUE,
    FALSE,
    IDENT,

    // double-tokens
    EQEQ,
    PLUSEQ,
    MINUSEQ,
    PLUSPLUS,
    MINUSMINUS,
    ARROW,

    // single-tokens
    STRING,
    SEMICOLON,
    EQ,
    BRACKETL,
    BRACKETR,
    CURLYL,
    CURLYR,
    PARENL,
    PARENR,
    SLASH,
    STAR,
    PLUS,
    MINUS,
    GT,
    LT,
    GTEQ,
    LTEQ,
    REF,
    HASH,
    DOLLAR,
    DOG,
    BANG,
    BANGEQ,
    PERCENT,
    TILDA,
    BACKTICK,
    COLON,
    QUESTION,
    HAT,
    DOT,
    COMMA,
    BACKSLASH,

    EOF,

    pub fn postfixPrec(comptime self: TokenType) u8 {
        return switch (self) {
            .BANG, .BRACKETL, .PARENL => 6,
            else => unreachable,
        };
    }

    pub fn prefixPrec(comptime self: TokenType) u8 {
        return switch (self) {
            .PLUS, .MINUS => 5,
            else => unreachable,
        };
    }

    pub fn infixPrecs(comptime self: TokenType) struct {
        left: u8,
        right: u8,
    } {
        return switch (self) {
            .PLUS, .MINUS => .{ .left = 1, .right = 2 },
            .SLASH, .STAR => .{ .left = 3, .right = 4 },
            else => unreachable,
        };
    }
};

pub const ErrorLexing = error{
    ParseError,
    BadTokenHandle,
};

pub const Token = struct {
    text: []const u8,
    line: usize,
    col: usize,

    pub fn new(
        text: []const u8,
        line: usize,
        col: usize,
    ) Token {
        return .{
            .text = text,
            .line = line,
            .col = col,
        };
    }
};

pub const TokId = packed struct(usize) {
    type: TokenType,
    handle: u56,
};

pub const Lexer = struct {
    buf: []const u8,
    cur: usize = 0,
    line: usize = 1,
    col: usize = 1,
    fifo: FIFO(TokId, .Dynamic),
    a: Allocator,
    token_arena: TokenList,

    pub fn init(allocator: Allocator, buf: []const u8) Lexer {
        return .{
            .buf = buf,
            .fifo = FIFO(TokId, .Dynamic).init(allocator),
            .a = allocator,
            .token_arena = TokenList.init(allocator),
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.token_arena.deinit();
        self.fifo.deinit();
    }

    inline fn char(self: *Lexer) u8 {
        return self.buf[self.cur];
    }

    inline fn one(self: *Lexer) []const u8 {
        defer self.adv();
        return self.buf[self.cur .. self.cur + 1];
    }

    inline fn adv(self: *Lexer) void {
        self.cur = self.cur + 1;
        if (self.cur == self.buf.len)
            return;
        const c = self.char();
        if (c == '\n') {
            self.col = 0;
            self.line = self.line + 1;
        } else {
            self.col = self.col + 1;
        }
    }

    fn tryParseWord(self: *Lexer, comptime word: []const u8, comptime t: TokenType) !TokenType {
        if (self.buf.len - 1 >= self.cur + word.len and
            mem.eql(u8, self.buf[self.cur .. self.cur + word.len], word) and b: {
            const end = self.buf[self.cur + word.len];
            break :b !ascii.isAlphanumeric(end) and end != '_';
        }) {
            self.cur = self.cur + word.len;
            return t;
        }

        var c = self.char();
        while (ascii.isAlphanumeric(c) or c == '_') {
            self.adv();
            c = self.char();
        }

        return .IDENT;
    }

    inline fn tryParse(self: *Lexer, comptime word: []const u8, comptime tt: TokenType) !TokenType {
        self.adv();
        return try self.tryParseWord(word, tt);
    }

    fn parseIndet(self: *Lexer) !TokenType {
        return try switch (self.char()) {
            'd' => self.tryParse("o", .DO),
            'c' => self.tryParse("onst", .CONST),
            'e' => self.tryParse("xport", .EXPORT),
            'i' => self.tryParse("mport", .IMPORT),
            't' => self.tryParse("rue", .TRUE),
            'f' => self.tryParse("alse", .FALSE),
            'r' => self.tryParse("eturn", .RETURN),
            else => b: {
                var c = self.char();
                while (ascii.isAlphanumeric(c) or c == '_') {
                    self.adv();
                    c = self.char();
                }

                break :b .IDENT;
            },
        };
    }

    fn trimLeft(self: *Lexer) ?TokenType {
        if (self.buf.len <= self.cur) return .EOF;
        while (ascii.isWhitespace(self.char())) {
            if (self.cur == self.buf.len - 1)
                return .EOF;
            self.adv();
        }
        return null;
    }

    fn ifTwoToken(self: *Lexer, comptime c: u8, comptime yes: TokenType, comptime no: TokenType) TokenType {
        defer self.adv();
        if (self.buf.len - 1 >= self.cur + 1 and self.buf[self.cur + 1] == c) {
            self.adv();
            return yes;
        } else return no;
    }

    fn allocToken(self: *Lexer) !u56 {
        const tok_handle = self.token_arena.items.len;
        if (tok_handle > math.maxInt(u56))
            @panic("too many tokens!!!");
        _ = try self.token_arena.addOne();
        return @intCast(tok_handle);
    }

    pub fn getToken(self: *const Lexer, handle: u56) !*Token {
        if (handle >= self.token_arena.items.len)
            return ErrorLexing.BadTokenHandle;
        return &self.token_arena.items[handle];
    }

    fn lexNext(self: *Lexer) !TokId {
        if (self.trimLeft()) |eof| {
            const token_handle = try self.allocToken();
            const token = try self.getToken(token_handle);
            token.* = Token.new(self.buf[self.cur..], self.line, self.col);
            return .{
                .handle = token_handle,
                .type = eof,
            };
        }

        const token_handle = try self.allocToken();
        const token = try self.getToken(token_handle);
        const start = self.cur;
        const start_line = self.line;
        const start_col = self.col;
        const c = self.char();
        switch (c) {
            ';' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .SEMICOLON };
            },
            ':' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .COLON };
            },
            ')' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .PARENR };
            },
            '(' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .PARENL };
            },
            '}' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .CURLYR };
            },
            '{' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .CURLYL };
            },
            ']' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .BRACKETR };
            },
            '[' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .BRACKETL };
            },
            '+' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .PLUS };
            },
            '=' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .EQ };
            },
            '*' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .STAR };
            },
            '`' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .BACKTICK };
            },
            '~' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .TILDA };
            },
            '\'' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .COLON };
            },
            '%' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .PERCENT };
            },
            '&' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .REF };
            },
            '?' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .QUESTION };
            },
            '$' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .QUESTION };
            },
            '#' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .HASH };
            },
            '@' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .DOG };
            },
            '^' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .HAT };
            },
            '/' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .SLASH };
            },
            '\\' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .BACKSLASH };
            },
            '.' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .DOT };
            },
            ',' => {
                token.* = Token.new(self.one(), start_line, start_col);
                return .{ .handle = token_handle, .type = .COMMA };
            },
            '>' => {
                const tt = self.ifTwoToken('=', .GTEQ, .GT);
                token.* = if (tt == .GTEQ) Token{
                    .col = start_col,
                    .line = start_line,
                    .text = self.buf[self.cur - 1 .. self.cur + 1],
                } else Token{
                    .col = start_col,
                    .line = start_line,
                    .text = self.buf[self.cur .. self.cur + 1],
                };
                return .{
                    .handle = token_handle,
                    .type = tt,
                };
            },
            '-' => {
                const tt = self.ifTwoToken('>', .ARROW, .MINUS);
                token.* = if (tt == .LTEQ) Token{
                    .col = start_col,
                    .line = start_line,
                    .text = self.buf[self.cur - 1 .. self.cur + 1],
                } else Token{
                    .col = start_col,
                    .line = start_line,
                    .text = self.buf[self.cur .. self.cur + 1],
                };
                return .{
                    .handle = token_handle,
                    .type = tt,
                };
            },
            '<' => {
                const tt = self.ifTwoToken('=', .LTEQ, .LT);
                token.* = if (tt == .LTEQ) Token{
                    .col = start_col,
                    .line = start_line,
                    .text = self.buf[self.cur - 1 .. self.cur + 1],
                } else Token{
                    .col = start_col,
                    .line = start_line,
                    .text = self.buf[self.cur .. self.cur + 1],
                };
                return .{
                    .handle = token_handle,
                    .type = tt,
                };
            },
            '!' => {
                const tt = self.ifTwoToken('=', .BANGEQ, .BANG);
                token.* = if (tt == .BANGEQ) Token{
                    .col = start_col,
                    .line = start_line,
                    .text = self.buf[self.cur - 1 .. self.cur + 1],
                } else Token{
                    .col = start_col,
                    .line = start_line,
                    .text = self.buf[self.cur .. self.cur + 1],
                };
                return .{
                    .handle = token_handle,
                    .type = tt,
                };
            },
            '"' => {
                self.adv();
                while (self.char() != '"')
                    self.adv();
                self.adv();
                token.* = Token.new(self.buf[start..self.cur], start_line, start_col);
                return .{
                    .handle = token_handle,
                    .type = .STRING,
                };
            },
            else => {
                if (ascii.isDigit(c)) {
                    self.adv();
                    while (ascii.isDigit(self.char()))
                        self.adv();

                    token.* = .{
                        .text = self.buf[start..self.cur],
                        .line = start_line,
                        .col = start_col,
                    };
                    return .{
                        .handle = token_handle,
                        .type = .INT,
                    };
                } else if (c == '_' or ascii.isAlphabetic(c)) {
                    const tt = try self.parseIndet();
                    token.* = .{
                        .text = self.buf[start..self.cur],
                        .line = start_line,
                        .col = start_col,
                    };
                    return .{
                        .handle = token_handle,
                        .type = tt,
                    };
                } else {
                    return error.ParseError;
                }
            },
        }
    }

    pub fn next(self: *Lexer) !TokId {
        if (self.fifo.readableLength() > 0) return self.fifo.readItem() orelse unreachable;

        return self.lexNext();
    }

    pub fn peek(self: *Lexer, n: usize) !TokId {
        const qlen = self.fifo.readableLength();
        try self.fifo.ensureUnusedCapacity(n + 1);
        if (n >= qlen) {
            const needed = n - qlen;
            for (0..needed + 1) |_| {
                self.fifo.writeItemAssumeCapacity(try self.lexNext());
            }
        }

        return self.fifo.peekItem(n);
    }
};

test "lexer operational" {
    const testing = @import("std").testing;
    var lexer = Lexer.init(testing.allocator,
        \\const print = (a: int, b: int) -> {
        \\
        \\};
    );
    defer lexer.deinit();
    const token_stream = [_]TokenType{
        .CONST, .IDENT, .EQ,    .PARENL, .IDENT, .COLON,  .IDENT,  .COMMA,
        .IDENT, .COLON, .IDENT, .PARENR, .ARROW, .CURLYL, .CURLYR, .SEMICOLON,
        .EOF,
    };
    for (token_stream) |expected| {
        const id = try lexer.next();
        testing.expect(id.type == expected) catch |err| {
            log.err("got token: {any}, expected: {any}\n", .{
                id.type, expected,
            });
            return err;
        };
    }
}
