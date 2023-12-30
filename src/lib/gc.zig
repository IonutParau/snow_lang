const std = @import("std");
const values = @import("value.zig");
const errors = @import("errors.zig");

const Allocator = std.mem.Allocator;

const SnowCell = values.SnowCell;
const SnowValue = values.SnowValue;
const SnowStack = values.SnowStack;

const SnowError = errors.SnowError;
const SnowErrorStore = errors.SnowErrorStore;

const LinkedList = std.SinglyLinkedList;

pub const GarbageCollector = struct {
    allocator: Allocator,
    err: *SnowErrorStore,
    cells: LinkedList(SnowCell),
    values: LinkedList(SnowValue),
    threshold: usize,
    count: usize,

    const Self = @This();

    pub fn init(allocator: Allocator, err: *SnowErrorStore) Self {
        const cells = LinkedList(SnowCell){};
        const vals = LinkedList(SnowValue){};

        return .{
            .allocator = allocator,
            .err = err,
            .cells = cells,
            .values = vals,
            .threshold = 200,
            .count = 0,
        };
    }

    pub fn deinit(self: Self) void {
        var cell = self.cells.first;
        while (cell) |c| {
            cell = c.next;
            c.data.deinit(self.allocator);
            self.allocator.destroy(c);
        }

        var value = self.values.first;
        while (value) |v| {
            value = v.next;
            v.data.deinit(self.allocator);
            self.allocator.destroy(v);
        }
    }

    pub fn addValue(self: *Self, value: SnowValue) !*SnowValue {
        const node = LinkedList(SnowValue).Node{ .data = value };
        const nodep = try self.allocator.create(@TypeOf(node));
        nodep.* = node;
        self.values.prepend(nodep);
        self.count += 1;
        return &nodep.data;
    }

    pub fn addCell(self: *Self, cell: SnowCell) !void {
        const node = LinkedList(SnowCell).Node{ .data = cell };
        const nodep = try self.allocator.create(@TypeOf(node));
        nodep.* = node;
        self.cells.prepend(nodep);
        self.count += 1;
    }

    pub fn makeSharedCell(self: *Self, value: SnowValue) !SnowCell {
        const data = try self.allocator.create(struct { marked: bool, value: *SnowValue });
        errdefer self.allocator.destroy(data);

        const valuep = try self.allocator.create(SnowValue);
        errdefer self.allocator.destroy(valuep);

        valuep.* = value;

        data.marked = false;
        data.value = valuep;

        const cell = SnowCell{
            .shared = data,
        };

        self.addCell(cell);

        return cell;
    }

    pub fn makeTuple(self: *Self, size: usize) !SnowValue {
        const data = try self.allocator.create(struct { marked: bool, values: []SnowValue });
        errdefer self.allocator.destroy(data);

        const buffer = try self.allocator.alloc(SnowValue, size);
        errdefer self.allocator.free(buffer);

        data.marked = false;
        data.values = buffer;

        var tuple = SnowValue{
            .tuple = data,
        };

        try self.addValue(tuple); // The pointer is useful when we need exactly THAT value, but a copy is fine (because its just a pointer)

        return tuple;
    }

    pub fn makeString(self: *Self, data: []const u8) !SnowValue {
        const vdata = try self.allocator.create(struct { marked: bool, str: []const u8 });
        errdefer self.allocator.destroy(data);

        const buffer: []const u8 = try self.allocator.alloc(u8, data.len);
        errdefer self.allocator.free(buffer);
        @memcpy(buffer, data);

        vdata.marked = false;
        vdata.str = buffer;

        var str = SnowValue{
            .string = vdata,
        };

        try self.addValue(str);

        return str;
    }

    pub fn makeList(self: *Self, capacity: usize) !SnowValue {
        const data = try self.allocator.create(struct { marked: bool, values: std.ArrayList(SnowValue) });
        errdefer self.allocator.destroy(data);

        const list = try std.ArrayList(SnowValue).initCapacity(@max(capacity, 5));
        errdefer list.deinit();

        data.marked = false;
        data.values = list;

        var l = SnowValue{
            .list = data,
        };

        try self.addValue(l);

        return l;
    }

    pub fn makeTable(self: *Self) !SnowValue {
        var data = try self.allocator.create(struct { marked: bool, table: values.SnowTable });
        errdefer self.allocator.destroy(data);

        const table = values.SnowTable.init(self.allocator);
        errdefer table.deinit();

        var t = SnowValue{
            .table = data,
        };

        try self.addValue(t);

        return t;
    }

    pub fn mark(self: *const Self, stack: SnowStack) void {
        _ = self;
        var frame = stack.wholeStackFrame();

        // TODO: Multi-threaded marking

        var i: usize = 0;
        while (i < frame.len) : (i += 1) {
            frame[i].setMarked(true);
            frame[i].get().mark();
        }
    }

    pub fn sweep(self: *Self) void {
        // Delete values that are not marked and reset mark
        {
            const it = self.values.first;
            while (it) |node| {
                if (node.data.isMarked()) {
                    node.data.setMarked(false);
                } else {
                    node.data.deinit(self.allocator);
                    self.values.remove(node);
                    self.allocator.destroy(node);
                    self.count -= 1;
                }
            }
        }

        // Delete cells that are not marked and reset mark
        {
            const it = self.cells.first;
            while (it) |node| {
                if (node.data.isMarked()) {
                    node.data.setMarked(false);
                } else {
                    node.data.deinit(self.allocator);
                    self.cells.remove(node);
                    self.allocator.destroy(node);
                    self.count -= 1;
                }
            }
        }
    }

    // literally Mark and Sweep
    pub fn collect(self: *Self, stack: SnowStack) void {
        self.mark(stack);
        self.sweep();
    }

    // Maybe mark and sweep
    pub fn maybeCollect(self: *Self, stack: SnowStack) void {
        if (self.count >= self.threshold) {
            self.mark(stack);
            self.sweep();
            self.threshold = @max(self.count * 2, 200);
        }
    }
};
