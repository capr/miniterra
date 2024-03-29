<b>NOTE: Work-in-progress. Do not use yet!</b>

# Miniterra

Miniterra is programming language derived from [Terra](https://terralang.org/)
that compiles to C. It works on Windows, Linux and Mac with LuaJIT 2.1 and GCC.

It differs from Terra in the following ways:

### Language differences

* reintroducing lazy typechecking, which unlocks some meta-programming use cases.
* using Lua's scoping rules, which allows variable shadowing.
* reintroducing automatic function overloading.
* operator precedence is the same as in Lua.
* operator `^` does `pow()` like in Lua.
* operator `xor` was added.
* different semantics for `__for`.
* struct, field and function annotations.
* nested functions with lexical scoping.
* ++, +=, -=, *= /=, ^=, >>=, <<= assignment operators.

### Runtime differences

* can't import unsanitized C headers directly as there's no full-spec C parser.
  * but you can use `ffi.cdef` headers and get a 10x speed-up over clang as a bonus.
* can't call Terra code from Lua as there's no JIT compiler.
  * but LuaJIT ffi bindings (cdefs and metatypes) can be auto-generated for
    Terra functions so you can call into your Terra binaries from Lua directly.
  * on the flip side, you don't have to ship the clang+LLVM 50 MB binary,
    in fact you don't even have to ship lx and miniterra, just your compiled
    Terra code, either as a shared library or as a static library to be
    linked with your executable.

# Building

Binaries are included in the repo for Windows and Linux so you can skip this
step unless you're on a Mac, an old Linux or you don't trust the binaries.

The build script is for GCC. Use MSYS2 on Windows.
For OSX you can build on OSX or cross-compile from Linux.

The build command is:

```
sh c/build.sh
```

The result is static and dynamic libraries in `bin/OS`.

# Installation

Put the Lua code from the `lua` dir in your Lua path.

Put the dynamic libs from `bin/OS` where your LuaJIT exe is.

# Using

`foo_mt.mt` contains Terra code in Miniterra dialect:

```Lua
local foo = {}

struct Foo {
   ...
}

terra new()
   return malloc(sizeof(Foo))
end

terra Foo:free()
   free(self)
end

terra Foo:bar()
   ...
end

function foo:build()
   local lib = miniterra.lib'foo'
   lib:add(new)
   lib:add(Foo)
   lib:build()
end

return foo
```

`foo_build.lua` loads Miniterra and builds `foo_mt`:

```Lua
require'miniterra'
local foo = require'foo_mt' --load `foo_mt.mt`
foo:build()
```

This creates either `bin/windows/foo.dll`, `bin/linux/libfoo.so` or `bin/osx/libfoo.dylib`
depending on your OS and generates Lua bindings for it in `foo_h.lua`.
From there you can use `foo_h` directly or create `foo.lua` manually
and further Luaize the auto-generated binding, and use that:

```Lua
local foo = require'foo'

local foo1 = foo.new()
foo1:bar()
foo1:free()
```

# Standard library

Miniterra comes with a set of basic libraries to get you started:

| Module | Description |
|--------|-------------|
| [miniterra.low           ](lua/miniterra/low.mt           ) | Lua+Terra standard library and low level tools |
| [miniterra.arrayview     ](lua/miniterra/arrayview.mt     ) | Array view type |
| [miniterra.dynarray      ](lua/miniterra/dynarray.mt      ) | Dynamic array type |
| [miniterra.hashmap       ](lua/miniterra/hashmap.mt       ) | Hashmap type |
| [miniterra.rawstringview ](lua/miniterra/rawstringview.mt ) | Raw string arrayview and dynarray type |
| [miniterra.phf           ](lua/miniterra/phf.mt           ) | Perfect hash function generator |
| [miniterra.linkedlist    ](lua/miniterra/linkedlist.mt    ) | self-allocated doubly-linked list |
| [miniterra.fixedfreelist ](lua/miniterra/fixedfreelist.mt ) | Fixed-capacity freelist |
| [miniterra.arrayfreelist ](lua/miniterra/arrayfreelist.mt ) | Freelist based on a dynamic array |
| [miniterra.lrucache      ](lua/miniterra/lrucache.mt      ) | LRU cache, size-limited and count-limited |
| [miniterra.bitarray      ](lua/miniterra/bitarray.mt      ) | 1-D and 2-D bit array type |
| [miniterra.utf8          ](lua/miniterra/utf8.mt          ) | UTF-8 encoding and decoding |
| [miniterra.random        ](lua/miniterra/random.mt        ) | Tausworthe PRNG |
| [miniterra.memcheck      ](lua/miniterra/memcheck.mt      ) | Leak-checking allocator |

# Implementation

Unlike Terra which uses the LLVM C++ API to generate LLVM IR, miniterra
generates C code which is then compiled with your favorite C compiler (gcc).
While Terra is around 4 KLOC of C++ that leverages LLVM and STL, miniterra
is written in Lua, based on a Lua lexer and parser library called `lx`.

## Miniterra as an embedded language

`lx` is a standalone library which allows embedding other languages inside
Lua, another [idea from Terra](https://terralang.org/api.html#embedding-new-languages-inside-lua).
Miniterra is implemented as an embedded language based on this library.
You can use `lx` to implement your own embedded language that has nothing
to do with Terra as long as its syntax is made only of Lua/Terra lexical
units and there's a clear set of keywords or tokens that mark the beginning
of a statement or expression in the language.

`lx` is made of two parts: a tokenizer which was taken from LuaJIT and
amounts to 1 KLOC of straightforward C, and a Lua parser written in Lua
that is more/less a translation of Lua's own parser which is a very hackable
recursive-descent parser with a simple way to handle operator precedence.

`lx` adds a `require` loader that loads `.luax` files. Those are like normal
Lua modules except for the syntax `import'foo'` which you can use anywhere
in the Lua code, in any scope, to load the embedded language `foo` in that
scope. You can import multiple languages in the same Lua scope as long as
each one uses different starting tokens.

A language must be implemented in a Lua module with the same name. A language
module must contain a function called `lang` that returns the keywords that
the language defines, the entrypoints for statements and for expressions,
and a parser constructor that must return a parse function that will be called
whenever a statement or expression of the language is encountered.

Take miniterra as an example:

```Lua
local mt = {} --miniterra module

local function expression_function(lx) --parser constructor
   local function bind()
      -- call the bind functions returned by lx.luaexpr() here.
      return val_of_name1, ...
   end
   return function(self, token, is_statement) --parse function
      --in here, use lx to:
      -- * consume the tokens of this statement or expression:
      --    * use lx.next(), lx.lookahead(), etc.
      -- * create lexical scopes and declare local symbols inside them:
      --    * use lx.begin_scope(), lx.symbol(), lx.end_scope()
      -- * parse embedded Lua expressions to be eval'ed later at bind time:
      --    * use lx.luaexpr() which returns a bind function.
      --    * Lua expressions will see your language's local symbols.
      -- finally, return a bind function and the names of the locals that
      -- this expression or statement defines. the bind function will be
      -- called later and must return the values to assign to those names.
      return bind, {name1, ...}
   end
end

function mt.lang(lx)
   return {
      keywords = {'terra', 'quote', 'struct', 'var'},
      entrypoints = {
         statement = {'terra', 'struct'},
         expression = {'`'},
      },
      expression = expression_function(lx),
   }
end

return mt
```

The parser constructor receives an `lx` object to be used by the parse
function to consume the tokens of a particular statement or expression.
The parse function receives the starting token and a bool that tells whether
this is a statement or an expression. The parse function must consume all the
tokens of a particular statement/expression using the `lx` object which
contains all the necessary functions for that.

`lx` can also be used to parse and eval embedded Lua expressions that can
appear inside your embedded language, if it makes sense for your language to
support such a feature. In addition, you can tell `lx` to create nested
lexical scopes and declare symbols within those scopes. Embedded Lua
expressions will then be able to reference those symbols as well as any
locals from the outer Lua scope. Miniterra uses this feature of `lx` to
implement escapes that can see both Terra vars from the surrounding Terra
scope as well as Lua vars from the outer Lua scope. You can use this feature
in your own embedded language if your language is lexically scoped and you
want to implement escaping to Lua in it with the ability to access symbols
from the lexical scope of the escape.

`lx` also contains an infix expression parser generator on which you can
specify operators (binary and prefix-unary), precedence levels and
associativity, so you don't have to roll your own.

## Miniterra compilation stages

1. **Lexing** - breaks text into a stream of tokens. Literals are parsed on demand.
   Keywords are separated from other names. Comments are skipped.
2. **Parsing** - consumes tokens and generates AST with binding slots in it.
   Checks syntax. Parses expressions using operator precedence rules.
   The input Lua code is patched such that embedded-language code is removed
   and replaced with Lua code meant to resolve names and embedded Lua expressions.
3. **Binding** - fills the binding slots in the AST by running the patched Lua
   code which resolves names and embedded Lua expressions to either Terra
   symbols or Lua values.
4. **Typechecking** - typechecks assignments, operations and call args. Infers
   types, selects overload variants.
5. **Code gen** - generates C code, headers, and ffi bindings.
6. **Build** - builds the generated C code.

