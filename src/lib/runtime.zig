const std = @import("std");

const values = @import("value.zig");
const errors = @import("errors.zig");
const gc = @import("gc.zig");
const lexing = @import("lexer.zig");
const parsing = @import("parser.zig");

pub const SnowBytecodeReader = struct {
    bytecode: []const u8,

    const Self = @This();

    pub fn init(bytecode: []const u8) Self {
        return Self{
            .bytecode = bytecode,
        };
    }

    pub fn next(self: *Self, comptime T: type) T {
        const LEN = @sizeOf(T);

        const buffer = self.bytecode[0..LEN];
        const stackBuffer: [LEN]u8 = undefined;
        for (buffer, 0..) |byte, i| {
            stackBuffer[i] = byte; // Quick memcopy
        }

        self.bytecode.ptr += LEN;
        self.bytecode.len -= LEN;

        return @bitCast(stackBuffer);
    }

    // This array is towards the memory in the bytecode
    // Mutation to the array is undefined behavior
    pub fn nextArrayOf(self: *Self, comptime T: type) []const T {
        var array: []const T = undefined;
        const SIZE = @sizeOf(T);
        const LEN = self.next(u16); // Up to 65,536 elements. Pretty good.
        array.ptr = self.bytecode.ptr;
        array.len = LEN * SIZE;

        self.bytecode.ptr += LEN * SIZE;
        self.bytecode.len -= LEN * SIZE;

        return array;
    }
};

pub const SnowBytecodeWriter = struct {
    bytecode: std.ArrayList(u8),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .bytecode = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn getBytecode(self: *const Self) []const u8 {
        return self.bytecode.items;
    }

    pub fn write(self: *Self, value: anytype) !void {
        const T = @TypeOf(value);
        const LEN = @sizeOf(T);

        const stackBuffer: [LEN]u8 = @bitCast(value);
        for (stackBuffer) |byte| {
            try self.bytecode.append(byte);
        }
    }

    pub fn writeArray(self: *Self, array: anytype) !void {
        const Type = std.builtin.Type;
        const ArrayType: Type = @typeInfo(@TypeOf(array));

        switch (ArrayType) {
            .Pointer => |p| {
                switch (p.size) {
                    .Slice => {
                        const T = p.child;
                        const arr: []const T = array;
                        const len: u16 = @intCast(arr.len);
                        self.write(len);
                        for (arr) |v| {
                            self.write(v);
                        }
                    },
                    else => {
                        @compileError("writeArray() only supports slices");
                    },
                }
            },
            else => {
                @compileError("writeArray() only supports slices");
            },
        }
    }
};

const Stack = values.SnowStack;
const Value = values.SnowValue;
const Cell = values.SnowCell;

const ErrorStore = errors.SnowErrorStore;
const Error = errors.SnowError;

const Allocator = std.mem.Allocator;

const CallFrame = struct {
    item_count: usize,
    upvalues: []Cell,
    bytecode: ?SnowBytecodeReader,
    returned_value: ?Value,
};

const CallStackSize = 4096;

pub const SnowVM = struct {
    stack: Stack,
    collector: gc.GarbageCollector,
    error_store: *ErrorStore,
    allocator: Allocator,
    call_stack: []CallFrame,

    pub fn init(allocator: Allocator) !SnowVM {
        const store = try allocator.create(ErrorStore);
        const call_stack = try allocator.alloc(CallFrame, CallStackSize);

        return .{
            .stack = Stack.init(allocator, store),
            .collector = gc.GarbageCollector.init(store),
            .error_store = store,
            .allocator = allocator,
            .call_stack = call_stack,
        };
    }

    pub fn deinit(self: SnowVM) void {
        self.stack.deinit();
        self.collector.deinit();
        self.allocator.destroy(self.error_store);
    }
};
