const os = @import("std").os;
const mem = @import("std").mem;
const debug = @import("std").debug;
const log = @import("std").log;
const io = @import("std").io;
const posix = @import("std").posix;
const linux = @import("std").os.linux;
const UsizeList = @import("std").ArrayList(usize);
const Allocator = @import("std").mem.Allocator;
const GPA = @import("std").heap.GeneralPurposeAllocator;
const ASTParser = @import("ast.zig").Parser;
const Node = @import("ast.zig").Node;
const Token = @import("lexer.zig").Token;
const TokenType = @import("lexer.zig").TokenType;

fn displayHelp() !void {
    const help =
        \\USAGE: arnel build [file1] [file2] [file...]
        \\
    ;
    try io.getStdOut().writer().writeAll(help);
}

fn readFileAll(allocator: Allocator, path: [*:0]const u8) ![]u8 {
    const fd = try posix.openZ(path, .{ .ACCMODE = .RDONLY }, 0o777);
    try posix.lseek_END(fd, 0);
    const size = try posix.lseek_CUR_get(fd);
    try posix.lseek_SET(fd, 0);
    var file = try allocator.alloc(u8, size);

    var cur: usize = 0;
    while (cur != size)
        cur = cur + try posix.read(fd, file[cur..]);

    return file;
}

fn build(allocator: Allocator, files_to_compile: [][*:0]const u8) !void {
    var parser: ASTParser = undefined;
    for (files_to_compile) |file_name| {
        const buf = try readFileAll(allocator, file_name);
        ASTParser.init(&parser, buf, allocator);
        while (try parser.ok()) {
            const node = try parser.parseStmt();
            var printList = UsizeList.init(allocator);
            defer printList.deinit();
            try node.print(io.getStdOut().writer(), 0, &printList);
        }
    }
    return;
}

const ErrorMain = error{
    NoArgsProvided,
};

pub fn main() !void {
    var counter: usize = 1;
    if (os.argv.len == 1) {
        try displayHelp();
        return error.NoArgsProvided;
    } else if (mem.eql(u8, mem.span(os.argv[counter]), "build")) {
        var gpa = GPA(.{}){};
        const allocator = gpa.allocator();
        counter = counter + 1;
        return build(allocator, os.argv[counter..]);
    } else {
        try displayHelp();
        return error.NoArgsProvided;
    }
}
