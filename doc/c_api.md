# Moonjit C API Extensions

Moonjit adds some extensions to the standard Lua/C API. The moonjit include
directory must be in the compiler search path (`-Ipath`) to be able to include
the required header for C code:

```
#include "luajit.h"
```

Or for C++ code:

```
#include "lua.hpp"
```

## `luaJIT_setmode(L, idx, mode)` — Control VM

This is a C API extension to allow control of the VM from C code. The full
prototype of `luaJIT_setmode` is:

```
LUA_API int luaJIT_setmode(lua_State *L, int idx, int mode);
```

The returned status is either success (1) or failure (0). The second argument
is either 0 or a stack index (similar to the other Lua/C API functions).

The third argument specifies the mode, which is 'or'ed with a flag. The flag
can be `LUAJIT_MODE_OFF` to turn a feature off, `LUAJIT_MODE_ON` to turn a
feature on, or `LUAJIT_MODE_FLUSH` to flush cached code.

The following modes are defined:

### `luaJIT_setmode(L, 0, LUAJIT_MODE_ENGINE|flag)`

Turn the whole JIT compiler on or off or flush the whole cache of compiled code.

### `luaJIT_setmode(L, idx, LUAJIT_MODE_FUNC|flag)`
### `luaJIT_setmode(L, idx, LUAJIT_MODE_ALLFUNC|flag)`
### `luaJIT_setmode(L, idx, LUAJIT_MODE_ALLSUBFUNC|flag)`

This sets the mode for the function at the stack index idx or the parent of the
calling function (idx = 0). It either enables JIT compilation for a function,
disables it and flushes any already compiled code or only flushes already
compiled code. This applies recursively to all sub-functions of the function
with `LUAJIT_MODE_ALLFUNC` or only to the sub-functions with
`LUAJIT_MODE_ALLSUBFUNC`.

### `luaJIT_setmode(L, trace, LUAJIT_MODE_TRACE|LUAJIT_MODE_FLUSH)`

Flushes the specified root trace and all of its side traces from the cache. The
code for the trace will be retained as long as there are any other traces which
link to it.

<a name="mode_wrapcfunc"></a>
### `luaJIT_setmode(L, idx, LUAJIT_MODE_WRAPCFUNC|flag)`

This mode defines a wrapper function for calls to C functions. If called with
`LUAJIT_MODE_ON`, the stack index at idx must be a lightuserdata object holding
a pointer to the wrapper function. From now on all C functions are called
through the wrapper function. If called with `LUAJIT_MODE_OFF` this mode is
turned off and all C functions are directly called.

The wrapper function can be used for debugging purposes or to catch and convert
foreign exceptions. But please read the section on [C++ exception
interoperability](extensions.md#exceptions) first. Recommended usage can be
seen in this C++ code excerpt:

```
#include <exception>
#include "lua.hpp"

// Catch C++ exceptions and convert them to Lua error messages.
// Customize as needed for your own exception classes.
static int wrap_exceptions(lua_State *L, lua_CFunction f)
{
  try {
    return f(L);  // Call wrapped function and return result.
  } catch (const char *s) {  // Catch and convert exceptions.
    lua_pushstring(L, s);
  } catch (std::exception& e) {
    lua_pushstring(L, e.what());
  } catch (...) {
    lua_pushliteral(L, "caught (...)");
  }
  return lua_error(L);  // Rethrow as a Lua error.
}

static int myinit(lua_State *L)
{
  ...
  // Define wrapper function and enable it.
  lua_pushlightuserdata(L, (void *)wrap_exceptions);
  luaJIT_setmode(L, -1, LUAJIT_MODE_WRAPCFUNC|LUAJIT_MODE_ON);
  lua_pop(L, 1);
  ...
}
```

Note that you can only define a single global wrapper function, so be careful
when using this mechanism from multiple C++ modules. Also note that this
mechanism is not without overhead.

## int luaJIT_compat52

This symbol allows to know how luajit was built, ie. with or without
`-DLUAJIT_ENABLE_LUA52COMPAT`.
