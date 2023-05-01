const std = @import("std");

// Assumes list is sorted, remove duplicate items by modifying the argument in
// place. Returns the new range, which is a prefix of the previous one.
fn dedup(comptime list: [][]const u8) [][]const u8 {
    const ret = comptime brk: {
        var idret = 1;
        for (1..list.len) |idlist| {
            if (!std.mem.eql(u8, list[idlist - 1], list[idlist])) {
                list[idret] = list[idlist];
                idret += 1;
            }
        }
        break :brk idret;
    };
    return list[0..ret];
}

test "Uniq strings" {
    const ddup = comptime blk: {
        var strings = [8][]const u8{
            "aaaaa",
            "aaaaa",
            "aaaaa",
            "bcd",
            "efg",
            "efg",
            "eg",
            "fffff",
        };
        break :blk dedup(&strings);
    };
    const expect = std.testing.expect;
    try expect(ddup.len == 5);
    try expect(std.mem.eql(u8, ddup[0], "aaaaa"));
    try expect(std.mem.eql(u8, ddup[1], "bcd"));
    try expect(std.mem.eql(u8, ddup[2], "efg"));
    try expect(std.mem.eql(u8, ddup[3], "eg"));
    try expect(std.mem.eql(u8, ddup[4], "fffff"));
}

fn cmpStr(context: void, str1: []const u8, str2: []const u8) bool {
    _ = context;
    return std.mem.order(u8, str1, str2) == std.math.Order.lt;
}

// Creates a type that stores a tuple of compile-time known strings, to allow
// efficient recognition of them, and efficient equality comparison since two
// strings are equal iff their associated ids are the same. Strings can be
// duplicated in list.
pub fn MkUniqStrings(comptime list: anytype) type {
    const strings = comptime blk: {
        var strings: [list.len][]const u8 = undefined;
        for (list, 0..) |str, id| {
            strings[id] = str;
        }
        std.sort.sort([]const u8, &strings, {}, cmpStr);
        break :blk dedup(&strings);
    };
    const map = comptime blk: {
        const KV = std.meta.Tuple(&.{ []const u8, usize });
        var kvs: [strings.len]KV = undefined;
        var count = 0;
        for (strings, 0..) |str, id| {
            kvs[id] = .{ str, count };
            count += 1;
        }
        const map = std.ComptimeStringMap(usize, kvs);
        break :blk map;
    };
    return struct {
        // Get a specific string
        pub fn get(id: usize) []const u8 {
            return strings[id];
        }

        // Perform lexycographic comparison of stored strings
        pub fn compare(id1: usize, id2: usize) std.math.Order {
            return std.mem.order(
                u8,
                strings[id1],
                strings[id2],
            );
        }

        // Find a string
        pub fn find(str: []const u8) ?usize {
            return map.get(str);
        }

        // Get the number of strings stored
        pub fn len() usize {
            return strings.len;
        }
    };
}

test "String store" {
    const strings = .{ "hello", "world", "mario", "world" };
    const UStrings = MkUniqStrings(strings);
    const expect = std.testing.expect;
    const eq = std.testing.expectEqual;
    const eqstr = std.testing.expectEqualStrings;
    try eq(UStrings.len(), 3);

    try expect(UStrings.find("hello") != null);
    try expect(UStrings.find("world") != null);
    try expect(UStrings.find("mario") != null);
    try expect(UStrings.find("luigi") == null);

    const mario = UStrings.find("mario").?;
    const world = UStrings.find("world").?;
    try eqstr(UStrings.get(mario), "mario");
    try expect(UStrings.compare(mario, world) == std.math.Order.lt);
}
