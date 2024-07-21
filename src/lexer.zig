const ascii = @import("std").ascii;
const FIFO = @import("std").fifo.LinearFifo;
const mem = @import("std").mem;
const log = @import("std").log;
const Allocator = @import("std").mem.Allocator;

pub const TokenType = enum {
    // integers
    INT,
    FLOAT,
    STRANGE_INTEGER,

    // keywords
    IMPORT,
    EXPORT,
    RETURN,
    DO,
    TRUE,
    FALSE,
    IDENT,

    // double-tokens
    EQEQ,
    PLUSEQ,
    MINUSEQ,
    PLUSPLUS,
    MINUSMINUS,

    // single-tokens
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
    DCOLON,
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
};

pub const Token = struct {
    type: TokenType,
    text: []u8,
    line: usize,
    col: usize,

    pub fn new(
        typ: TokenType,
        text: []u8,
        line: usize,
        col: usize,
    ) Token {
        return .{
            .type = typ,
            .text = text,
            .line = line,
            .col = col,
        };
    }
};

pub const Lexer = struct {
    buf: []u8,
    cur: usize = 0,
    line: usize = 1,
    col: usize = 1,
    fifo: FIFO(*Token, .Dynamic),
    a: Allocator,

    inline fn char(self: *Lexer) u8 {
        return self.buf[self.cur];
    }

    inline fn one(self: *Lexer) []u8 {
        defer self.adv();
        return self.buf[self.cur .. self.cur + 1];
    }

    inline fn adv(self: *Lexer) void {
        self.cur = self.cur + 1;
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

    fn parseIndet(self: *Lexer) !Token {
        const col = self.col;
        const line = self.line;
        const start = self.cur;
        const tt = try switch (self.char()) {
            'd' => self.tryParse("o", .DO),
            'e' => self.tryParse("xport", .EXPORT),
            'i' => self.tryParse("mport", .IMPORT),
            't' => self.tryParse("rue", .TRUE),
            'f' => self.tryParse("alse", .FALSE),
            'r' => self.tryParse("etrurn", .RETURN),
            else => b: {
                var c = self.char();
                while (ascii.isAlphanumeric(c) or c == '_') {
                    self.adv();
                    c = self.char();
                }

                break :b .IDENT;
            },
        };
        return Token.new(tt, self.buf[start..self.cur], line, col);
    }

    fn trimLeft(self: *Lexer) ?TokenType {
        while (ascii.isWhitespace(self.char())) {
            if (self.cur == self.buf.len - 1)
                return .EOF;
            self.adv();
        }
        return null;
    }

    fn ifTwoToken(self: *Lexer, comptime c: u8, comptime yes: TokenType, comptime no: TokenType, start_col: usize, start_line: usize) Token {
        defer self.adv();
        if (self.buf.len - 1 >= self.cur + 1 and self.buf[self.cur + 1] == c) {
            self.adv();
            return Token{
                .type = yes,
                .col = start_col,
                .line = start_line,
                .text = self.buf[self.cur - 1 .. self.cur + 1],
            };
        } else return Token{
            .type = no,
            .col = start_col,
            .line = start_line,
            .text = self.buf[self.cur .. self.cur + 1],
        };
    }

    fn lexNext(self: *Lexer) !*Token {
        const token = try self.a.create(Token);
        if (self.trimLeft()) |eof| {
            token.* = Token.new(eof, self.buf[self.cur..], self.line, self.col);
            return token;
        }
        const start = self.cur;
        const start_line = self.line;
        const start_col = self.col;

        const c = self.char();
        token.* = switch (c) {
            ';' => Token.new(.SEMICOLON, self.one(), start_line, start_col),
            ':' => Token.new(.COLON, self.one(), start_line, start_col),

            ')' => Token.new(.PARENR, self.one(), start_line, start_col),
            '(' => Token.new(.PARENL, self.one(), start_line, start_col),
            '}' => Token.new(.CURLYR, self.one(), start_line, start_col),
            '{' => Token.new(.CURLYL, self.one(), start_line, start_col),
            ']' => Token.new(.BRACKETR, self.one(), start_line, start_col),
            '[' => Token.new(.BRACKETL, self.one(), start_line, start_col),

            '+' => Token.new(.PLUS, self.one(), start_line, start_col),
            '-' => Token.new(.MINUS, self.one(), start_line, start_col),
            '=' => Token.new(.EQ, self.one(), start_line, start_col),
            '*' => Token.new(.STAR, self.one(), start_line, start_col),
            '`' => Token.new(.BACKTICK, self.one(), start_line, start_col),
            '~' => Token.new(.TILDA, self.one(), start_line, start_col),
            '\'' => Token.new(.COLON, self.one(), start_line, start_col),
            '"' => Token.new(.DCOLON, self.one(), start_line, start_col),
            '%' => Token.new(.PERCENT, self.one(), start_line, start_col),
            '&' => Token.new(.REF, self.one(), start_line, start_col),
            '?' => Token.new(.QUESTION, self.one(), start_line, start_col),
            '$' => Token.new(.QUESTION, self.one(), start_line, start_col),
            '#' => Token.new(.HASH, self.one(), start_line, start_col),
            '@' => Token.new(.DOG, self.one(), start_line, start_col),
            '^' => Token.new(.HAT, self.one(), start_line, start_col),
            '/' => Token.new(.SLASH, self.one(), start_line, start_col),
            '\\' => Token.new(.BACKSLASH, self.one(), start_line, start_col),
            '.' => Token.new(.DOT, self.one(), start_line, start_col),
            ',' => Token.new(.COMMA, self.one(), start_line, start_col),
            '>' => self.ifTwoToken('=', .GTEQ, .GT, start_col, start_line),
            '<' => self.ifTwoToken('=', .LTEQ, .LT, start_col, start_line),
            '!' => self.ifTwoToken('=', .BANGEQ, .BANG, start_col, start_line),
            else => b: {
                if (ascii.isDigit(c)) {
                    self.adv();
                    while (ascii.isDigit(self.char()))
                        self.adv();

                    break :b .{
                        .type = .INT,
                        .text = self.buf[start..self.cur],
                        .line = start_line,
                        .col = start_col,
                    };
                } else if (ascii.isAlphabetic(c)) {
                    break :b try self.parseIndet();
                } else {
                    return error.ParseError;
                }
            },
        };
        return token;
    }

    pub fn next(self: *Lexer) !*Token {
        if (self.fifo.readableLength() > 0) return self.fifo.readItem() orelse unreachable;

        return self.lexNext();
    }

    pub fn peek(self: *Lexer, n: usize) !*Token {
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
