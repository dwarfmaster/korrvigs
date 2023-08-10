const decls = @import("declarations.zig");

const classes = .{};
const preds = .{};

pub const system: decls.SystemType(classes, preds) = .{
    .classes = classes,
    .predicates = preds,
};
