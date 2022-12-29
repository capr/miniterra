
# Overview

Miniterra is programming language derived from [Terra](https://terralang.org/).

It differs from Terra in the following ways:

## Language

* reintroducing lazy typechecking.
* reintroducing automatic function overloading.
* function overriding works with overloaded functions.
* return-type inference works with all of the above.
* variable shadowing is allowed like in Lua.
* operator precedence was changed to be the same as in Lua.
   * operator `^` does `pow()` like in Lua.
   * operator `xor` was added.

## Runtime

* can't import C headers directly as there's no C parser.
   * but you can use function prototypes declared with `ffi.cdef`.
* can't call Terra code from Lua as there's no JIT compiler.
   * but ffi cdefs and metatypes can be auto-generated for Terra
     functions so you can call into your Terra binaries from Lua via ffi.
   * on the flip side, you don't have to ship a 50 MB binary either,
     just your compiled Terra code (inside or outside your executable).

## Implementation

Unlike Terra which uses the LLVM C++ API to generate LLVM IR, miniterra
generates C code which is then compiled with your favorite C compiler (gcc).
While Terra is around 4 KLOC of C++ that leverages LLVM and STL, miniterra
is written in Lua, based on a Lua lexer and parser library called `lx`.

`lx` is a standalone library which allows embedding other languages inside
Lua, another [idea from Terra](https://terralang.org/api.html#embedding-new-languages-inside-lua).
Miniterra is implemented as an embedded language based on this library.
You can use `lx` to implement your own embedded language that has nothing
to do with Terra as long as its syntax is made only of Lua/Terra lexical
units and there's a clear set of keywords that mark the beginning of
a statement or expression in the language.

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

Example:

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
      -- * create local scopes and declare local symbols inside them:
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
specify operators (binary and prefix-unary), precedence levels and associativity.

# Installing

Binaries are included in the repo for Windows and Linux.
Build your own for OSX.

Put the Lua files in your Lua path.

Put the dynamic libraries where your LuaJIT exe is.

# Building

The build script is for GCC. Use MSYS2 on Windows.
For OSX you can build on OSX or cross-compile from Linux.

The build command is:

```
sh build.sh
```

The result is static and dynamic libraries in `bin/OS`.
