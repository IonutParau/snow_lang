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

    const Self = @This();

    pub fn init(allocator: Allocator, err: *SnowErrorStore) Self {
        const cells = LinkedList(SnowCell){};
        const vals = LinkedList(SnowValue){};

        return .{
            .allocator = allocator,
            .err = err,
            .cells = cells,
            .values = vals,
        };
    }

    pub fn deinit(self: Self) void {
        self.cells.deinit();
        self.values.deinit();
    }

    pub fn addValue(self: *Self, value: SnowValue) !*SnowValue {
        const node = LinkedList(SnowValue).Node{ .data = value };
        const nodep = try self.allocator.create(@TypeOf(node));
        nodep.* = node;
        self.values.prepend(nodep);
        return &nodep.data;
    }

    pub fn addCell(self: *Self, cell: SnowCell) !void {
        const node = LinkedList(SnowCell).Node{ .data = cell };
        const nodep = try self.allocator.create(@TypeOf(node));
        nodep.* = node;
        self.cells.prepend(nodep);
    }

    pub fn mark(self: *const Self, stack: SnowStack) void {
        _ = self;
        const frame = stack.wholeStackFrame();

        // TODO: Multi-threaded marking

        for (frame) |cell| {
            cell.setMarked(true);
            cell.read().mark();
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
                    self.values.remove(node);
                    self.allocator.destroy(node);
                }
            }
        }
    }

    // Mark and Sweep
    pub fn collect(self: *Self, stack: SnowStack) void {
        self.mark(stack);
        self.sweep();
    }
};
