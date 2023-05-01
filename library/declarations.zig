const std = @import("std");

// Extractor
// An argument is either the left part of a binary relation that takes the
// entity as first argument, or the path to the entity, or some arbitrary text.
pub const Argument = union(enum) {
    self,
    rel: []const u8,
    text: []const u8,

    fn order(self: Argument, other: Argument) std.math.Order {
        return switch (self) {
            .self => switch (other) {
                .self => std.math.Order.eq,
                .rel => std.math.Order.lt,
                .text => std.math.Order.lt,
            },
            .rel => switch (other) {
                .self => std.math.Order.gt,
                .rel => std.mem.order(u8, self.rel, other.rel),
                .text => std.math.Order.lt,
            },
            .text => switch (other) {
                .self => std.math.Order.gt,
                .rel => std.math.Order.gt,
                .text => std.mem.order(u8, self.text, other.text),
            },
        };
    }

    fn equal(self: Argument, other: Argument) bool {
        return self.order(other) == std.math.Order.eq;
    }
};
pub fn ExtractorT(comptime program: []const u8, comptime args: anytype) type {
    _ = program;
    return struct {
        program: []const u8,
        arguments: [args.len]Argument,
    };
}
pub fn extractor(comptime program: []const u8, comptime args: anytype) ExtractorT(program, args) {
    const parsed_args = comptime blk: {
        var pargs: [args.len]Argument = undefined;
        for (args, 0..) |arg, id| {
            if (arg.len > 1 and arg[0] == '%' and arg[1] == '%') {
                pargs[id] = Argument{ .text = arg[1..] };
            } else if (arg.len > 0 and arg[0] == '%') {
                if (std.mem.eql(u8, arg[1..], "self")) {
                    pargs[id] = Argument{ .self = {} };
                } else {
                    pargs[id] = Argument{ .rel = arg[1..] };
                }
            } else {
                pargs[id] = Argument{ .text = arg };
            }
        }
        break :blk pargs;
    };
    return .{
        .program = program,
        .arguments = parsed_args,
    };
}

test "Declare extractors" {
    const exts = .{
        extractor("identify", .{"%self"}),
        extractor("test", .{ "--arg", "%predicate", "%%width", "--file", "%self" }),
    };
    const expect = std.testing.expect;
    try expect(exts[1].arguments[0].equal(Argument{ .text = "--arg" }));
    try expect(exts[1].arguments[1].equal(Argument{ .rel = "predicate" }));
    try expect(exts[1].arguments[2].equal(Argument{ .text = "%width" }));
    try expect(exts[1].arguments[3].equal(Argument{ .text = "--file" }));
    try expect(exts[1].arguments[4].equal(Argument{ .self = {} }));
}

// Class
pub fn ClassT(
    comptime name: []const u8,
    comptime parent: []const u8,
    comptime ext: anytype,
) type {
    _ = name;
    _ = parent;
    return struct {
        name: []const u8,
        parent: []const u8,
        extractors: @TypeOf(ext),
    };
}

pub fn class(
    comptime name: []const u8,
    comptime parent: []const u8,
    comptime ext: anytype,
) ClassT(name, parent, ext) {
    return .{
        .name = name,
        .parent = parent,
        .extractors = ext,
    };
}

test "Declare classes" {
    const classes = .{
        class("Object", "Entity", null),
        class("Mechanical", "Object", null),
        class("Event", "Entity", null),
        class("Data", "Entity", null),
        class("Class", "Entity", .{extractor("hello", .{ "--test", "%self" })}),
    };
    _ = classes;
}

// Predicate
pub fn PredicateT(comptime name: []const u8, comptime tp: anytype) type {
    _ = name;
    return struct {
        name: []const u8,
        type: [tp.len][]const u8,
    };
}

pub fn predicate(comptime name: []const u8, comptime tp: anytype) PredicateT(name, tp) {
    const ntp = comptime blk: {
        var ntp: [tp.len][]const u8 = undefined;
        for (tp, 0..) |typ, id| {
            ntp[id] = typ;
        }
        break :blk ntp;
    };
    return .{
        .name = name,
        .type = ntp,
    };
}

test "Declare predicates" {
    const preds = .{
        predicate("is_a", .{ "Class", "Class" }),
        predicate("instance_of", .{ "Entity", "Class" }),
        predicate("links_to", .{ "Entity", "Entity" }),
        predicate("spans", .{ "Event", "Time", "Time" }),
    };
    _ = preds;
}

// System
pub fn extract_names(comptime named: anytype) [named.len][]const u8 {
    const strs = comptime blk: {
        var strs: [named.len][]const u8 = undefined;
        for (named, 0..) |nmd, id| {
            strs[id] = nmd.name;
        }
        break :blk strs;
    };
    return strs;
}

pub const Extractor = struct {
    program: []const u8,
    args: []const Argument,
};

pub const Class = struct {
    name: usize,
    parent: usize,
    extractors: []const Extractor,
};

pub const Predicate = struct {
    name: usize,
    type: []const usize,
};

const ustrs = @import("strings.zig");

pub fn System(comptime classes: anytype, comptime preds: anytype) type {
    return struct {
        extractors: []const Extractor,
        classes: []const Class,
        predicates: []const Predicate,

        const CNames = ustrs.MkUniqStrings(extract_names(classes));
        const PNames = ustrs.MkUniqStrings(extract_names(preds));
    };
}

pub fn system(comptime classes: anytype, comptime preds: anytype) System(classes, preds) {
    const helpers = struct {
        fn count_exts(comptime exts: anytype) usize {
            switch (@typeInfo(@TypeOf(exts))) {
                .Null => return 0,
                .Struct => return exts.len,
                else => return 1,
            }
        }
        fn prepare_ext(comptime ext: anytype) Extractor {
            return .{
                .program = ext.program,
                .args = &ext.arguments,
            };
        }
        fn copy_exts_to(comptime exts: anytype, nexts: []Extractor, pos: usize) void {
            switch (@typeInfo(@TypeOf(exts))) {
                .Null => return,
                .Struct => {
                    for (exts, pos..) |ext, id| {
                        nexts[id] = prepare_ext(ext);
                    }
                },
                else => nexts[pos] = prepare_ext(exts),
            }
        }
        fn prep_types(comptime n: usize, comptime cnames: anytype, types: [n][]const u8) []const usize {
            const ret = comptime blk: {
                var mapped: [n]usize = undefined;
                for (types, 0..) |tp, id| {
                    mapped[id] = cnames.find(tp).?;
                }
                break :blk mapped;
            };
            return &ret;
        }
    };
    const tp = System(classes, preds);
    const precomputed = comptime blk: {
        // Count extractors
        var nb_extractors = 0;
        var extractors_starts: [classes.len]usize = undefined;
        for (classes, 0..) |cls, id| {
            extractors_starts[id] = nb_extractors;
            nb_extractors += helpers.count_exts(cls.extractors);
        }

        // Setup extractors and classes
        var exts: [nb_extractors]Extractor = undefined;
        var clss: [classes.len]Class = undefined;
        for (classes, 0..) |cls, id| {
            const pos = tp.CNames.find(cls.name).?;
            const start = extractors_starts[id];
            const end = start + helpers.count_exts(cls.extractors);
            helpers.copy_exts_to(cls.extractors, &exts, start);
            clss[pos] = .{
                .name = pos,
                .parent = tp.CNames.find(cls.parent).?,
                .extractors = exts[start..end],
            };
        }

        // Predicates
        var prds: [preds.len]Predicate = undefined;
        for (preds, 0..) |pred, id| {
            prds[id].name = tp.PNames.find(pred.name).?;
            prds[id].type = helpers.prep_types(pred.type.len, tp.CNames, pred.type);
        }

        break :blk .{
            .extractors = &exts,
            .classes = &clss,
            .predicates = &prds,
        };
    };
    return .{
        .extractors = precomputed.extractors,
        .classes = precomputed.classes,
        .predicates = precomputed.predicates,
    };
}

test "Simple system" {
    const syst = comptime blk: {
        // Root entities are marked with themselves as the parent
        const classes = .{
            class("Class", "Entity", null),
            class("Entity", "Entity", null),
            class("Object", "Entity", null),
            class("Mechanical", "Object", null),
            class("Event", "Entity", null),
            class("Data", "Entity", null),
            class("Class", "Entity", .{extractor("hello", .{ "--test", "%self" })}),
            class("Time", "Time", null),
        };
        const preds = .{
            predicate("is_a", .{ "Class", "Class" }),
            predicate("instance_of", .{ "Entity", "Class" }),
            predicate("links_to", .{ "Entity", "Entity" }),
            predicate("spans", .{ "Event", "Time", "Time" }),
        };
        break :blk system(classes, preds);
    };
    _ = syst;
}
