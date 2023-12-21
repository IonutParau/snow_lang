const std = @import("std");

const values = @import("value.zig");

const SnowValue = values.SnowValue;

const Allocator = std.mem.Allocator;

pub const Action = union(enum) {};

pub const ActionTag = @typeInfo(Action).Union.tag_type.?;

pub const CodeBlockLink = union(enum) {
    jump: *CodeBlock,
    jumpCheck: struct {
        condition: *usize,
        next: *CodeBlock,
        fallback: *CodeBlock,
    },
    none: void,
};

pub const CodeBlock = struct {
    link: CodeBlockLink,
    actions: std.ArrayList(Action),
};

pub const BuildContext = struct {
    upvalues: std.StringHashMap(usize),
    locals: std.StringHashMap(usize),
    constants: std.StringHashMap(SnowValue),
    allocator: Allocator,
};
