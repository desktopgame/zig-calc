# zig-calc
zig-calc is simple calculator, written in zig.  
only basic arithmetic operations, constant, and function supported.

# example
````
zig build run -- 3 + 4
> 7
zig build run -- 12 + (3 * 4)
> 24
zig build run -- -3 * 2
> -6
zig build run -- 5 + (-3)
> 2
zig build run -- "max(1,2)"
> 2
zig build run -- "min(1,2)"
> 1
````