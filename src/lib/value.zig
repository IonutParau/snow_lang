const std = @import("std");

const BuiltinType = std.builtin.Type;

const TableHashMapContext = struct {
    const Self = @This();

    pub fn hash(self: Self, key: SnowValue) u64 {
        _ = self;
        return @intCast(key.hash());
    }

    pub fn eql(self: Self, a: SnowValue, b: SnowValue) bool {
        _ = self;
        return a.directly_equals(&b);
    }
};

pub const SnowCell = union(enum) {
    shared: *struct {
        marked: bool,
        value: *SnowValue,
    },
    local: SnowValue,

    const Self = @This();

    pub inline fn isMarked(self: *const Self) bool {
        return switch (self) {
            .shared => self.shared.marked,
            .local => true,
        };
    }

    pub inline fn setMarked(self: *Self, marked: bool) void {
        switch (self) {
            .shared => self.shared.marked = marked,
            else => {},
        }
    }

    pub fn read(self: *const Self) SnowValue {
        return switch (self) {
            .shared => |s| s.value.*,
            .local => |l| l,
        };
    }

    pub fn write(self: *Self, value: SnowValue) void {
        switch (self) {
            .shared => |s| s.value.* = value,
            .local => self.local = value,
        }
    }

    pub fn deinit(self: Self, allocator: Allocator) void {
        switch (self) {
            .shared => |s| {
                // Delete struct
                defer allocator.destroy(s);
                defer allocator.destroy(s.value);
                s.value.deinit(allocator);
            },
            .local => {},
        }
    }
};

const Allocator = std.mem.Allocator;

const errors = @import("errors.zig");

const SnowError = errors.SnowError;
const SnowErrorStore = errors.SnowErrorStore;
const SnowSource = @import("lexer.zig").SnowSource;

pub const SNOW_STACK_SIZE = 16384;

pub const SnowStack = struct {
    frame: []SnowCell,
    frame_idx: usize,
    frame_end: usize,
    allocator: Allocator,
    store: *SnowErrorStore,
    startptr: *SnowCell,

    const Self = @This();

    pub fn new(allocator: Allocator, store: *SnowErrorStore) !Self {
        var frame = try allocator.alloc(SnowCell, SNOW_STACK_SIZE);
        frame.len = 0;

        return .{
            .frame = frame,
            .frame_idx = 0,
            .frame_end = SNOW_STACK_SIZE,
            .allocator = allocator,
            .store = store,
            .startptr = frame.ptr,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.frame);
    }

    pub fn wholeStackFrame(self: *const Self) []SnowCell {
        const frame: []SnowCell = undefined;
        frame.ptr = self.startptr;
        frame.len = self.frame_idx + self.frame_end;

        return frame;
    }

    pub fn shiftFrame(self: *Self, n: usize, source: SnowSource) SnowError!void {
        self.frame_idx += n;
        if (self.frame_idx + self.frame.len >= self.frame_end) {
            self.store.* = try SnowErrorStore.fmt("Stack Overflow", .{}, self.allocator, source);
            return SnowError.RuntimeError;
        }
    }

    pub fn unwindFrame(self: *Self, n: usize, source: SnowSource) SnowError!void {
        if (self.frame_idx < n) {
            self.store.* = try SnowErrorStore.fmt("Stack Overflow", .{}, self.allocator, source);
            return SnowError.RuntimeError;
        }
        self.frame_idx -= n;
    }

    pub fn setFrameSize(self: *Self, size: usize) void {
        self.frame.len = size;
    }

    pub fn pushCell(self: *Self, n: usize, source: SnowSource) SnowError!void {
        self.frame.len += n;
        if (self.frame_idx + self.frame.len >= self.frame_end) {
            self.store.* = try SnowErrorStore.fmt("Stack Overflow", .{}, self.allocator, source);
            return SnowError.RuntimeError;
        }
    }

    pub fn setCell(self: *Self, i: usize, cell: SnowCell) void {
        self.frame[i] = cell;
    }

    pub fn getCell(self: *Self, i: usize) *SnowCell {
        return &self.frame[i];
    }
};

pub const SnowValue = union(enum) {
    number: f64,
    boolean: bool,
    string: *struct {
        marked: bool, // gc stuff
        str: []const u8,
    },
    tuple: *struct {
        marked: bool,
        values: []SnowValue,
    },
    list: *struct {
        marked: bool,
        values: std.ArrayList(SnowValue),
    },
    table: *struct {
        marked: bool,
        map: std.HashMap(SnowValue, SnowValue, TableHashMapContext, std.hash_map.default_max_load_percentage),
    },
    structValue: *struct {
        marked: bool,
        map: std.StringHashMap(SnowValue),
    },

    const Self = @This();

    pub inline fn deinit(self: Self, allocator: Allocator) void {
        switch (self) {
            .string => {
                allocator.free(self.string.str);
                allocator.destroy(self.string);
            },
            .tuple => {
                allocator.free(self.tuple.values);
                allocator.destroy(self.tuple);
            },
            .list => {
                self.list.values.deinit();
                allocator.destroy(self.list);
            },
            .table => {
                self.table.map.deinit();
                allocator.destroy(self.table);
            },
            .structValue => {
                self.structValue.map.deinit();
                allocator.destroy(self.structValue);
            },
            else => {},
        }
    }

    pub inline fn isMarked(self: *const Self) bool {
        return switch (self) {
            .string => self.string.marked,
            .tuple => self.tuple.marked,
            .list => self.list.marked,
            .table => self.table.marked,
            .structValue => self.structValue.marked,
            else => false,
        };
    }

    pub inline fn setMarked(self: *Self, marked: bool) void {
        switch (self) {
            .string => self.string.marked = marked,
            .tuple => self.tuple.marked = marked,
            .list => self.list.marked = marked,
            .table => self.table.marked = marked,
            .structValue => self.structValue.marked = marked,
            else => {},
        }
    }

    pub fn mark(self: *Self) void {
        if (self.isMarked()) return;
        self.setMarked(true);

        switch (self) {
            .tuple => {
                const t = self.tuple.values;
                for (t) |v| {
                    v.mark();
                }
            },
            .list => {
                const l = self.list.values.items;
                for (l) |v| {
                    v.mark();
                }
            },
            .table => {
                const t = self.table.map.valueIterator();
                for (t) |v| {
                    v.mark();
                }
            },
            .structValue => {
                const t = self.structValue.map.valueIterator();
                for (t) |v| {
                    v.mark();
                }
            },
            else => {},
        }
    }

    pub fn hash(self: *const Self) usize {
        return switch (self) {
            .number => |n| {
                const b: u64 = @bitCast(n);
                const h: usize = @intCast(b);
                return h;
            },
            .boolean => |b| if (b) 1 else 0,
            .string => {
                var h: usize = 5381;

                for (self.string.str) |c| {
                    h = ((h << 5) +% h) +% c;
                }

                return hash;
            },
            .list => |l| @intFromPtr(l),
            .table => |t| @intFromPtr(t),
            .structValue => |s| @intFromPtr(s),
            .tuple => {
                const t = self.tuple.values;
                var h: usize = @intFromPtr(t.ptr);
                for (t) |v| {
                    // random bullshit
                    h ^= ((h >> 2) & (v.hash() << 3));
                }

                return h;
            },
        };
    }

    pub fn directly_equals(self: *const Self, other: *const Self) bool {
        const tag: type = @typeInfo(Self).Union.tag_type.?;

        if (@as(tag, self) != @as(tag, other)) {
            return false;
        }

        return switch (self) {
            .number => |n| n == other.number,
            .boolean => |b| b == other.boolean,
            .string => |s| std.mem.eql(u8, s.str, other.string.str),
            .tuple => |t| {
                const other_tuple = other.tuple;
                if (t.values.len != other_tuple.values.len) {
                    return false;
                }

                for (other_tuple.values, 0..) |v, i| {
                    const tv: SnowValue = t.values[i];

                    if (!tv.directly_equals(&v)) {
                        return false;
                    }
                }

                return true;
            },
            .list => |l| l == self.list,
            .table => |t| t == self.table,
            .structValue => |s| s == self.structValue,
        };
    }
};