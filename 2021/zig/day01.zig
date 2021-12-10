const std = @import("std");
const stdout = std.io.getStdOut();
const Allocator = std.mem.Allocator;
const parseInt = std.fmt.parseInt;

const data = @embedFile("../data/aoc2021-1-input.txt");

pub fn main() !void {
    // static memory pool for allocations
    var buffer: [16384]u8 = undefined;
    const allocator = std.heap.FixedBufferAllocator.init(&buffer).allocator();

    var tokens = std.mem.tokenize(u8, data, "\n");

    // tokenize the array
    var len: usize = 0;
    while (tokens.next()) |_| len += 1;
    try stdout.writer().print("{d} lines\n", .{len});

    var array: []i32 = try allocator.alloc(i32, len);

    // parse the array
    tokens.reset();

    try parseIntegersInto(array, len, tokens);

    // compute answer 1
    try stdout.writer().print("Answer 1: {d}\n", .{answerOne(array, len)});

    // compute answer 2
    try stdout.writer().print("Answer 2: {d}\n", .{answerTwo(array, len)});
}

fn parseIntegersInto(array: []i32, max_len: usize, tokens: anytype) !void {
    var t = tokens;
    var i: usize = 0;
    while (t.next()) |str| {
        const x = try parseInt(i32, str, 10);
        array[i] = x;
        i += 1;
        if (i >= max_len) break;
    }
}

fn answerOne(array: []i32, len: usize) i32 {
    var i: usize = 1;
    var sum: i32 = 0;
    while (i < len) : (i += 1) {
        if (array[i] > array[i - 1]) sum += 1;
    }
    return sum;
}

fn answerTwo(array: []i32, len: usize) i32 {
    var i: usize = 3;
    var sum: i32 = 0;
    while (i < len) : (i += 1) {
        if (array[i] > array[i - 3]) sum += 1;
    }
    return sum;
}
