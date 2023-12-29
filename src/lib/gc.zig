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
