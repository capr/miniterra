
## Overview

Miniterra is programming language derived from [Terra](https://terralang.org/).

It differs from Terra in the following ways:

### Language

* typechecking is lazy like in previous versions of Terra which solves many
problems accidentally introduced by eager type-checking:
	* automatic function overloading.
	* function overriding that works with overloaded functions.
	* return-type inference.
* variable shadowing is allowed like in Lua.
* operator precedence was changed to be the same as in Lua.
	* operator `^` does `pow()` like in Lua.
	* operator `xor` was added.

### Capabilities

* can't import C headers directly as there's no C parser.
	* but you can use function prototypes declared with `ffi.cdef`.
* can't call Terra code from Lua as there's no JIT compiler.
	* but ffi cdefs and metatypes can be auto-generated for Terra
	functions so you can call into your Terra binaries from Lua via ffi.
	* on the flip side, you don't have to ship a 50 MB binary either,
	just your compiled Terra code (inside or outside your executable).

### Implementation

* unlike Terra which uses the LLVM C++ API to generate LLVM IR, miniterra
generates C code which is then compiled with your favorite C compiler (gcc).
* unlike Terra which is around 4 KLOC of C++ that leverages LLVM and STL,
miniterra is written in Lua, apart from the tokenizer which was taken from
LuaJIT and amounts to 1 KLOC of straightforward C.
* the lexer is a standalone library called `lx` which allows embedding other
languages inside Lua, as described in [the Terra manual](https://terralang.org/api.html#embedding-new-languages-inside-lua).
Miniterra is implemented as an embedded language based on this library.
You can use `lx` to implement your own embedded languages that have nothing
to do with Terra as long as their syntax fits the Lua/Terra tokenizer.

