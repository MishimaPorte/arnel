const posix = @import("std").posix;
const io = @import("std").io;
const fs = @import("std").fs;
const mem = @import("std").mem;
const Strings = @import("std").ArrayList([]u8);
const Allocator = mem.Allocator;
const Bitmap = @import("std").bit_set.IntegerBitSet;

const TokId = @import("../lexer.zig").TokId;
const IRText = @import("../ir.zig").IRText;
const IRCompiler = @import("../ir.zig").IRCompiler;
const ast = @import("../ast.zig");
const NodeId = ast.NodeId;
const Parser = ast.Parser;

pub const CompileError = error{
    BadIR,
    OutOfRegisters,
};

pub const Compiler = struct {
    string_counter: usize = 0,

    ir_regs_map: IRMap,
    string_table: Strings,
    out_writer: fs.File.Writer,
    // bit set indicates that a register is empty
    // r0 through r8
    param_regs: Bitmap(9), // not preserved by a callee
    // r9 through r15
    temporary_regs: Bitmap(7), // not preserved by a callee
    // r19 through r28
    gp_regs: Bitmap(10), // callee-preserved

    const v = union {
        reg: reg,
    };
    const IRMap = @import("std").AutoHashMap(u16, v);

    const plat_reg: u8 = 18;
    const ip1: u8 = 17;
    const ip0: u8 = 16;
    const ind_reg: u8 = 8;
    const lr: []const u8 = "lr"; // r30
    const fp: []const u8 = "fp"; // r29
    const sp: []const u8 = "sp";
    const pc: []const u8 = "pc";
    const xzr: []const u8 = "xzr";

    pub fn init(out: fs.File.Writer, allocator: Allocator) !Compiler {
        return .{
            .ir_regs_map = IRMap.init(allocator),
            .string_table = Strings.init(allocator),
            .out_writer = out,
            .param_regs = Bitmap(9).initFull(),
            .temporary_regs = Bitmap(7).initFull(),
            .gp_regs = Bitmap(10).initFull(),
        };
    }

    const reg = packed struct(u16) {
        val: u8,
        t: enum(u8) {
            gp,
            temp,
            param,
            novalue,
            imm,
        },

        pub inline fn asU8(self: reg) u8 {
            return self.val;
        }
    };

    const value = union {
        reg: reg,
        regs: []reg,
        void: void,

        pub fn unwrapReg(self: value) ?reg {
            return switch (self) {
                .reg => reg,
                else => null,
            };
        }
    };

    pub fn allocateReg(self: *Compiler) CompileError!reg {
        if (self.gp_regs.toggleFirstSet()) |val| {
            return .{
                .val = @intCast(val),
                .t = .gp,
            };
        } else if (self.temporary_regs.toggleFirstSet()) |val| {
            return .{
                .val = @intCast(val),
                .t = .temp,
            };
        } else if (self.param_regs.toggleFirstSet()) |val| {
            return .{
                .val = @intCast(val),
                .t = .param,
            };
        } else return error.OutOfRegisters;
    }

    pub fn deallocateReg(self: *Compiler, r: reg) void {
        switch (r.t) {
            .gp => self.gp_regs.toggle(r.asU8()),
            .temp => self.temporary_regs.toggle(r.asU8()),
            .param => self.param_regs.toggle(r.asU8()),
            .novalue, .imm => return,
        }
    }

    pub fn compInst(self: *Compiler, text: IRText, i: usize, parser: *Parser) !usize {
        var index = i;
        const inst = text.irs[index].inst;
        switch (inst.op) {
            .INT_IMM => {
                const dest = inst.imms.zero;
                const val = try self.allocateReg();
                try self.ir_regs_map.put(dest, .{ .reg = val });
                index += 1;
                const tid = text.irs[index].as(TokId);
                const tok = try parser.l.getToken(tid.handle);
                try self.out_writer.print("  mov x{d}, #{s}\n", .{
                    val.val,
                    tok.text,
                });
                index += 1;
                return index;
            },
            .SUM => {
                const dest = inst.imms.zero;
                const left = inst.imms.one;
                const right = inst.imms.two;
                const leftval = self.ir_regs_map.get(left) orelse return CompileError.BadIR;
                const rightval = self.ir_regs_map.get(right) orelse return CompileError.BadIR;
                self.deallocateReg(leftval.reg);
                self.deallocateReg(rightval.reg);
                const destval = try self.allocateReg();
                try self.ir_regs_map.put(dest, .{ .reg = destval });

                try self.out_writer.print("  add x{d}, x{d}, x{d}\n", .{
                    destval.val,
                    leftval.reg.asU8(),
                    rightval.reg.asU8(),
                });
                index += 1;
                return index;
            },
            .RET => {
                const ret = inst.imms.zero;
                const retval = self.ir_regs_map.get(ret) orelse return CompileError.BadIR;
                try self.out_writer.print("  ret x{d}\n", .{
                    retval.reg.asU8(),
                });
                index += 1;
                return index;
            },
            else => return CompileError.BadIR,
        }
    }

    pub fn saveCallerSaved(self: *Compiler) !void {
        _ = self;
        return;
    }

    pub fn restoreCallerSaved(self: *Compiler) !void {
        _ = self;
        return;
    }

    pub fn startCompilation(self: *Compiler) !void {
        try self.out_writer.writeAll(
            \\.section .text
            \\
            \\
        );
        try self.out_writer.writeAll(
            \\print_debug:
            \\  mov x8, #0x40 
            \\  mov x2, x1
            \\  mov x1, x0
            \\  mov x0, #1
            \\  svc #0
            \\  ret
            \\
        );
        try self.out_writer.writeByte('\n');
    }

    pub fn compile(self: *Compiler, text: IRText, parser: *Parser) !void {
        switch (text.type) {
            .FUNC => {
                var index: usize = 2;
                if (!(text.irs[0].inst.op == .FUNC_DECL)) return CompileError.BadIR;
                const name_tid = text.irs[1].as(TokId);
                const name = try parser.l.getToken(name_tid.handle);
                if (mem.eql(u8, "_start", name.text)) {
                    try self.out_writer.writeAll(
                        \\.global _start
                        \\
                    );
                }
                try self.out_writer.print(
                    \\{s}:
                    \\
                , .{name.text});
                while (true) {
                    const inst = text.irs[index].inst;
                    if (inst.op == .FUNC_RETURN_DECL) break;
                    const t = inst.imms.zero;
                    const dest = inst.imms.one;
                    index += 1;
                    const pnameid = text.irs[index].as(TokId);
                    const pname = try parser.l.getToken(pnameid.handle);
                    try self.out_writer.print("  # parameter x{d} :{s} t{d}\n", .{ dest, pname.text, t });
                    index += 1;
                }
                const rett = text.irs[index].inst;
                index += 1;
                try self.out_writer.print("  # return r t{d}\n", .{rett.imms.zero});
                const is_main = mem.eql(u8, "_start", name.text);
                while (index != text.irs.len) {
                    if (is_main) {
                        const inst = text.irs[index].inst;
                        if (inst.op == .RET) {
                            const ret = inst.imms.zero;
                            const retval = self.ir_regs_map.get(ret) orelse return CompileError.BadIR;
                            try self.out_writer.print(
                                \\  mov x8, #93
                                \\  mov x0, x{d}
                                \\  svc #0
                                \\
                            , .{retval.reg.asU8()});
                            index += 1;
                        } else {
                            index = try self.compInst(text, index, parser);
                        }
                    } else index = try self.compInst(text, index, parser);
                }
                try self.out_writer.writeByte('\n');
            },
            else => @panic("TODO: not implemented!"),
        }
    }
};

test "compiler operational" {
    const testing = @import("std").testing;
    const file = try fs.cwd().openFile("out.s", .{ .mode = .write_only });
    const parser = Parser.init(
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

    var irc = try IRCompiler.init(&parser);
    defer irc.deinit();

    const text = try irc.next();

    const compiler = try Compiler.init(file.writer(), testing.allocator);

    try compiler.compile(text, &parser);
}
