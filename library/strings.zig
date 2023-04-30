const std = @import("std");

// Creates a type that stores a tuple of compile-time known strings, to allow efficient
// recognition of them, and efficient equality comparison since two strings are equal
// iff their associated ids are the same.
pub fn MkUniqStrings(comptime list: anytype) type {
    // TODO optimise by builtin constant time trie
    const precomputed = comptime blk: {
        const KV = std.meta.Tuple(&.{ []const u8, usize });
        var strings: [list.len][]const u8 = undefined;
        var kvs: [list.len]KV = undefined;
        var count = 0;
        for (list, 0..) |str, id| {
            kvs[id] = .{ str, count };
            strings[id] = str;
            count += 1;
        }
        const map = std.ComptimeStringMap(usize, kvs);
        break :blk .{ .strings = strings, .map = map };
    };
    return struct {
        // Get a specific string
        pub fn get(id: usize) []const u8 {
            return precomputed.strings[id];
        }

        // Perform lexycographic comparison of stored strings
        pub fn compare(id1: usize, id2: usize) std.math.Order {
            return std.mem.order(
                u8,
                precomputed.strings[id1],
                precomputed.strings[id2],
            );
        }

        // Find a string
        pub fn find(str: []const u8) ?usize {
            return precomputed.map.get(str);
        }
    };
}

test "uniq-strings" {
    const strings = .{ "hello", "world", "mario" };
    const UStrings = MkUniqStrings(strings);
    const expect = std.testing.expect;

    try expect(UStrings.find("hello") != null);
    try expect(UStrings.find("world") != null);
    try expect(UStrings.find("mario") != null);
    try expect(UStrings.find("luigi") == null);

    const mario = UStrings.find("mario").?;
    const world = UStrings.find("world").?;
    try expect(std.mem.eql(u8, UStrings.get(mario), "mario"));
    try expect(UStrings.compare(mario, world) == std.math.Order.lt);
}
