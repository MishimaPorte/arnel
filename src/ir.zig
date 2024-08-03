const Allocator = @import("std").mem.Allocator;
const Node = @import("ast.zig").Node;
const NodeType = @import("ast.zig").NodeType;
const Token = @import("lexer.zig").Token;
const TokId = @import("lexer.zig").TokId;
const NodeId = @import("ast.zig").NodeId;
const UsizeList = @import("std").ArrayList(usize);
const Writer = @import("std").fs.File.Writer;
const ImaginaryRegBitmap = @import("std").bit_set.DynamicBitSet;
const Lexer = @import("lexer.zig").Lexer;
const ASTParser = @import("ast.zig").Parser;
const talloc = @import("std").testing.allocator;
const io = @import("std").io;
const mem = @import("std").mem;

pub const IROp = enum(u8) {
    RET,
    INT_IMM,
    SUM,
    SUB,
    DIV,
    MULT,
    FUNC_ARG_PLACE,
    /// A string literal;
    STR_LITERAL,

    FUNC_DECL,
    FUNC_PARAM_DECL,
    FUNC_RETURN_DECL,

    /// Length of a string literal.
    INTRINSIC_STRLEN,
    /// Temporary instruction before we can properly define a function;
    INTRINSIC_PRINT,
};

// No one ever needs 65536 types or locals, it is ridiculous
pub const ArnelType = packed struct(u16) {
    /// math.maxInt(u16) is invalid id;
    /// This id is a pointer into the types array.
    id: u16,
    pub const invalid = ArnelType{ .id = @import("std").math.maxInt(u16) };
    pub const int = ArnelType{ .id = 0 };
    pub const string_literal = ArnelType{ .id = 1 };
};

// Keeping track of local's type is a compiler, not IR concern;
// IR deals with the most simple 65536 register-based virtual machine,
// that is agnostic of values' types.
pub const Loc = packed struct(u16) {
    id: u16,
};

pub const IRInst = packed struct(u64) {
    // a single byte here
    op: IROp,
    _: u8 = undefined,
    imms: packed struct(u48) {
        zero: u16 = undefined,
        one: u16 = undefined,
        two: u16 = undefined,
    } = undefined,
};

pub const IRError = error{
    NotAFuncInIRCompilation,
    BadASTNode,
};

pub const IRText = struct {
    irs: []IREither,
    type: enum {
        FUNC,
        TYPE,
        CONST,
        VAR,
        EOF,
    },

    /// Reads the rest of the instruction array.
    pub fn writeOp(self: *const IRText, i: usize, writer: Writer, comp: *const IRCompiler) !void {
        var index = i;
        while (true) {
            if (index == self.irs.len) break;
            const expr = self.irs[index].inst;
            switch (expr.op) {
                .RET => try writer.print("    ret x{d}\n", .{expr.imms.zero}),
                .INT_IMM => {
                    const dest = expr.imms.zero;
                    index += 1;
                    const tid = self.irs[index].as(TokId);
                    const tok = try comp.p.l.getToken(tid.handle);
                    try writer.print("    x{d} imm int({s})\n", .{ dest, tok.text });
                },
                .SUM => {
                    const dest = expr.imms.zero;
                    const left = expr.imms.one;
                    const right = expr.imms.two;
                    try writer.print("    x{d} sum x{d} x{d}\n", .{ dest, left, right });
                },
                else => @panic("TODO: implement"),
            }
            index += 1;
        }
    }
    pub fn writeFunc(self: *const IRText, writer: Writer, comp: *const IRCompiler) !void {
        var index: usize = 2;
        if (!(self.irs[0].inst.op == .FUNC_DECL)) return IRError.BadASTNode;
        const name_tid = self.irs[1].as(TokId);
        const name = try comp.p.l.getToken(name_tid.handle);
        try writer.print("sym \"{s}\"\n", .{name.text});
        try writer.print("  prms\n", .{});
        while (true) {
            const inst = self.irs[index].inst;
            if (inst.op == .FUNC_RETURN_DECL) break;

            const t = inst.imms.zero;
            const dest = inst.imms.one;
            index += 1;
            const pnameid = self.irs[index].as(TokId);
            const pname = try comp.p.l.getToken(pnameid.handle);
            try writer.print("    x{d} :{s} t{d}\n", .{ dest, pname.text, t });
            index += 1;
        }
        const ret = self.irs[index].inst;
        index += 1;
        try writer.print("  r t{d}\n", .{ret.imms.zero});
        try writer.print("  stmts\n", .{});

        try self.writeOp(index, writer, comp);
        try writer.print("\"{s}\"\n", .{name.text});
    }

    pub fn writeTo(self: *const IRText, writer: Writer, comp: *const IRCompiler) !void {
        try writer.writeByte('\n');
        switch (self.type) {
            .FUNC => return self.writeFunc(writer, comp),
            else => @panic("TODO: not implemented!"),
        }
        return;
    }
};
const IREither = extern union {
    inst: IRInst,
    any: u64,

    pub fn as(self: IREither, T: type) T {
        return @bitCast(self.any);
    }
};

const IRs = @import("std").ArrayList(IREither);

pub const IRCompiler = struct {
    irs: IRs,

    /// We can have as many of locals as we need.
    /// Set bit indicates a free thing.
    irb: ImaginaryRegBitmap,
    p: *ASTParser,

    pub fn init(parser: *ASTParser) !IRCompiler {
        return .{
            .p = parser,
            .irs = IRs.init(parser.a),
            .irb = b: {
                const irb = try ImaginaryRegBitmap.initFull(parser.a, 40);
                break :b irb;
            },
        };
    }

    pub inline fn allocateLoc(self: *IRCompiler) !Loc {
        const bit = self.irb.toggleFirstSet() orelse b: {
            try self.irb.resize(self.irb.capacity() * 2, true);
            break :b self.irb.toggleFirstSet() orelse unreachable;
        };
        return .{
            .id = @intCast(bit),
        };
    }

    pub inline fn deallocateLoc(self: *IRCompiler, loc: Loc) void {
        return self.irb.toggle(loc.id);
    }

    pub fn compileExpr(self: *IRCompiler, nid: NodeId) !Loc {
        switch (nid.type) {
            .LITERAL_INT => {
                const loc = try self.allocateLoc();
                const node = self.p.nodes.get(1, nid.handle);
                const tid = node.child(TokId, 0);
                try self.appendInst(.{
                    .op = .INT_IMM,
                    .imms = .{
                        .zero = loc.id,
                    },
                });
                try self.appendAny(tid);
                return loc;
            },
            .PLUS => {
                const node = self.p.nodes.get(2, nid.handle);
                const val_left = try self.compileExpr(node.child(NodeId, 0));
                const val_right = try self.compileExpr(node.child(NodeId, 1));
                self.deallocateLoc(val_left);
                self.deallocateLoc(val_right);
                const loc = try self.allocateLoc();
                try self.appendInst(.{ .op = .SUM, .imms = .{
                    .zero = loc.id,
                    .one = val_left.id,
                    .two = val_right.id,
                } });
                return loc;
            },
            .CALL => {
                // how the fuck i process the arguments
            },
            else => return IRError.BadASTNode,
        }
    }

    pub fn compileStmt(self: *IRCompiler, nid: NodeId) !void {
        switch (nid.type) {
            .EXPR_STMT => {
                const node = self.p.nodes.get(0, nid.handle);
                const expr = node.child(NodeId, 0);
                const loc = try self.compileExpr(expr);
                self.deallocateLoc(loc);
            },
            .RETURN => {
                const node = self.p.nodes.get(0, nid.handle);
                const expr = node.child(NodeId, 0);
                const loc = try self.compileExpr(expr);
                try self.appendInst(.{ .op = .RET, .imms = .{
                    .zero = loc.id,
                } });
                self.deallocateLoc(loc);
            },
            else => return IRError.BadASTNode,
        }
    }

    fn appendInst(self: *IRCompiler, inst: IRInst) Allocator.Error!void {
        return self.irs.append(.{
            .inst = inst,
        });
    }

    /// For non-ptr types only, i guess;
    fn appendAny(self: *IRCompiler, any: anytype) Allocator.Error!void {
        return self.irs.append(.{
            .any = @bitCast(any),
        });
    }

    pub fn next(self: *IRCompiler) !IRText {
        const nid = try self.p.parseTopLevel();
        if (nid.type == .EOF) return IRText{
            .type = .EOF,
            .irs = undefined,
        };
        const start = self.irs.items.len;
        switch (nid.type) {
            .FUNC_DECL => {
                const func_node = self.p.nodes.get(6, nid.handle);
                try self.appendInst(.{
                    .op = .FUNC_DECL,
                });
                const name = func_node.child(TokId, 0);
                try self.appendAny(name);

                const args = func_node.child(NodeId, 1);
                const args_len = func_node.child(usize, 2);
                const args_node = self.p.nodes.get(0, args.handle);
                self.irb.setRangeValue(.{
                    .start = 0,
                    .end = args_len,
                }, false);

                for (0..args_len) |i| {
                    const param = args_node.child(NodeId, i);
                    const param_node = self.p.nodes.get(0, param.handle);
                    try self.appendInst(.{
                        .op = .FUNC_PARAM_DECL,
                        .imms = .{
                            .zero = 0, // le type id
                            .one = @intCast(i),
                        },
                    });
                    // name of the parameter
                    try self.appendAny(param_node.child(TokId, 0));
                }
                try self.appendInst(.{
                    .op = .FUNC_RETURN_DECL,
                    .imms = .{
                        .zero = 0, // le type id
                    },
                });

                const stmts_handle = func_node.child(NodeId, 4);
                const stmts_node = self.p.nodes.get(0, stmts_handle.handle);
                const stmts_len = func_node.child(usize, 5);
                for (0..stmts_len) |i| {
                    const stmt = stmts_node.child(NodeId, i);
                    try self.compileStmt(stmt);
                }
            },
            else => @panic("TODO: do"),
        }
        return .{
            .irs = self.irs.items[start..],
            .type = .FUNC,
        };
    }

    pub fn deinit(self: *IRCompiler) void {
        self.irs.deinit();
        self.irb.deinit();
    }
};

test "ir compiler operational" {
    const testing = @import("std").testing;
    var parser = ASTParser.init(
        \\const main = (argv_t argv, argc_t argc) -> int {
        \\    1+1;
        \\    1+1;
        \\    1+1;
        \\    1+1;
        \\    1+1;
        \\    return 1 + 1;
        \\};
    , testing.allocator);
    defer parser.deinit();
    var ir_compiler = try IRCompiler.init(&parser);
    defer ir_compiler.deinit();
    const out = io.getStdOut().writer();
    const irs = try ir_compiler.next();
    try irs.writeTo(out, &ir_compiler);
}
