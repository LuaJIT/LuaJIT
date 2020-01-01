# Moonjit `jit.*` Library

The functions in this built-in module control the behavior of the JIT compiler
engine. Note that JIT-compilation is fully automatic — you probably won't need
to use any of the following functions unless you have special needs.

## jit.on()
## jit.off()

Turns the whole JIT compiler on (default) or off.

These functions are typically used with the command line options `-j on` or `-j
off`.

## jit.flush()

Flushes the whole cache of compiled code.

## jit.on(func|true [,true|false])
## jit.off(func|true [,true|false])
## jit.flush(func|true [,true|false])

`jit.on` enables JIT compilation for a Lua function (this is the default).

`jit.off` disables JIT compilation for a Lua function and flushes any already
compiled code from the code cache.

`jit.flush` flushes the code, but doesn't affect the enable/disable status.

The current function, i.e. the Lua function calling this library function, can
also be specified by passing true as the first argument.

If the second argument is true, JIT compilation is also enabled, disabled or
flushed recursively for all sub-functions of a function. With false only the
sub-functions are affected.

The `jit.on` and `jit.off` functions only set a flag which is checked when the
function is about to be compiled. They do not trigger immediate compilation.

Typical usage is `jit.off(true, true)` in the main chunk of a module to turn
off JIT compilation for the whole module for debugging purposes.

## jit.flush(tr)

Flushes the root trace, specified by its number, and all of its side traces
from the cache. The code for the trace will be retained as long as there are
any other traces which link to it.

## status, ... = jit.status()

Returns the current status of the JIT compiler. The first result is either true
or false if the JIT compiler is turned on or off. The remaining results are
strings for CPU-specific features and enabled optimizations.

## jit.version

Contains the moonjit version string.  Note that moonjit 2.1.x releases
masquerade as LuaJIT to support existing users of LuaJIT.  moonjit 2.2.0 and
later no longer do this and identify as `moonjit`.

## jit.version\_num

Contains the version number of the moonjit core. Version xx.yy.zz is
represented by the decimal number xxyyzz.

<a name="jit_os"></a>
## jit.os

Contains the target OS name: "Windows", "Linux", "OSX", "BSD", "POSIX" or
"Other".

<a name="jit_arch"></a>
## jit.arch

Contains the target architecture name: "x86", "x64", "arm", "ppc", "ppcspe", or
"mips".

## `jit.opt.*` — JIT compiler optimization control

This sub-module provides the backend for the -O command line option.

You can also use it programmatically, e.g.:

```
jit.opt.start(2) -- same as -O2
jit.opt.start("-dce")
jit.opt.start("hotloop=10", "hotexit=2")
```

This module is built-in and optimization is turned on by default. It's no
longer necessary to run require("jit.opt").start(), which was one of the ways
to enable optimization.

## `jit.util.*` — JIT compiler introspection

This sub-module holds functions to introspect the bytecode, generated traces,
the IR and the generated machine code. The functionality provided by this
module is still in flux and therefore undocumented.

The debug modules `-jbc`, `-jv` and `-jdump` make extensive use of these
functions. Please check out their source code, if you want to know more. 
