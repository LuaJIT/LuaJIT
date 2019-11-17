# `ffi.*` API Functions

This page describes the API functions provided by the FFI library in detail.
It's recommended to read through the [introduction](ffi.md) and the [FFI
tutorial](ffi-tutorial.md) first.

## Glossary

* `cdecl` — An abstract C type declaration (a Lua string).
* `ctype` — A C type object. This is a special kind of `cdata` returned by
  `ffi.typeof()`. It serves as a `cdata` [constructor](#ffi_new) when called.
* `cdata` — A C data object. It holds a value of the corresponding `ctype`.
* `ct` — A C type specification which can be used for most of the API
  functions. Either a `cdecl`, a `ctype` or a `cdata` serving as a template
  type.
* `cb` — A callback object. This is a C data object holding a special function
  pointer. Calling this function from C code runs an associated Lua function.
* `VLA` — A variable-length array is declared with a ? instead of the number of
  elements, e.g. "int[?]". The number of elements (nelem) must be given when
  it's [created](#ffi_new).
* `VLS` — A variable-length struct is a struct C type where the last element is
  a VLA. The same rules for declaration and creation apply.

## Declaring and Accessing External Symbols

External symbols must be declared first and can then be accessed by indexing a
[C library namespace](ffi-semantics.md#clib), which automatically binds the
symbol to a specific library.

### ffi.cdef(def)

Adds multiple C declarations for types or external symbols (named variables or
functions). def must be a Lua string. It's recommended to use the syntactic
sugar for string arguments as follows:

```
ffi.cdef[[
typedef struct foo { int a, b; } foo_t;  // Declare a struct and typedef.
int dofoo(foo_t *f, int n);  /* Declare an external C function. */
]]
```

The contents of the string must be a sequence of [C
declarations](ffi-semantics.md#clang), separated by semicolons. The trailing
semicolon for a single declaration may be omitted.

Please note that external symbols are only declared, but they are not bound to
any specific address, yet. Binding is achieved with C library namespaces (see
below).

***C declarations are not passed through a C pre-processor, yet. No pre-processor
tokens are allowed, except for #pragma pack. Replace #define in existing C
header files with enum, static const or typedef and/or pass the files through
an external C pre-processor (once). Be careful not to include unneeded or
redundant declarations from unrelated header files.***

### ffi.C

This is the default C library namespace — note the uppercase 'C'. It binds to
the default set of symbols or libraries on the target system. These are more or
less the same as a C compiler would offer by default, without specifying extra
link libraries.

On POSIX systems, this binds to symbols in the default or global namespace.
This includes all exported symbols from the executable and any libraries loaded
into the global namespace. This includes at least `libc`, `libm`, `libdl` (on
Linux), `libgcc` (if compiled with GCC), as well as any exported symbols from
the Lua/C API provided by moonjit itself.

On Windows systems, this binds to symbols exported from the `*.exe`, the
`lua51.dll` (i.e. the Lua/C API provided by moonjit itself), the C runtime
library moonjit was linked with (`msvcrt*.dll`), `kernel32.dll`, `user32.dll`
and `gdi32.dll`.

### clib = ffi.load(name [,global])

This loads the dynamic library given by name and returns a new C library
namespace which binds to its symbols. On POSIX systems, if global is true, the
library symbols are loaded into the global namespace, too.

If name is a path, the library is loaded from this path. Otherwise name is
canonicalized in a system-dependent way and searched in the default search path
for dynamic libraries:

On POSIX systems, if the name contains no dot, the extension .so is appended.
Also, the lib prefix is prepended if necessary. So `ffi.load("z")` looks for
`"libz.so"` in the default shared library search path.

On Windows systems, if the name contains no dot, the extension `.dll` is
appended. So `ffi.load("ws2_32")` looks for `"ws2_32.dll"` in the default DLL
search path.

## Creating cdata Objects

The following API functions create cdata objects (type() returns "cdata"). All
created cdata objects are [garbage collected](ffi-semantics.md#gc).

<a name="ffi_new"></a>
### cdata = ffi.new(ct [,nelem] [,init...])
### cdata = *ctype*([nelem,] [init...])

Creates a cdata object for the given ct. VLA/VLS types require the nelem
argument. The second syntax uses a ctype as a constructor and is otherwise
fully equivalent.

The cdata object is initialized according to the [rules for
initializers](ffi-semantics.md#init), using the optional init arguments. Excess
initializers cause an error.

**Performance notice**: if you want to create many objects of one kind, parse
the cdecl only once and get its ctype with ffi.typeof(). Then use the ctype as
a constructor repeatedly.

*Please note that an anonymous struct declaration implicitly creates a new and
distinguished ctype every time you use it for `ffi.new()`. This is probably not
what you want, especially if you create more than one cdata object. Different
anonymous structs are not considered assignment-compatible by the C standard,
even though they may have the same fields! Also, they are considered different
types by the JIT-compiler, which may cause an excessive number of traces. It's
strongly suggested to either declare a named struct or typedef with
`ffi.cdef()` or to create a single ctype object for an anonymous struct with
`ffi.typeof()`.*

### ctype = ffi.typeof(ct)

Creates a ctype object for the given ct.

This function is especially useful to parse a cdecl only once and then use the
resulting ctype object as a [constructor](#ffi_new).

### cdata = ffi.cast(ct, init)

Creates a scalar cdata object for the given ct. The cdata object is initialized
with init using the "cast" variant of the [C type conversion
rules](ffi-semantics.md#convert).

This functions is mainly useful to override the pointer compatibility checks or
to convert pointers to addresses or vice versa.

<a name="ffi_metatype"></a>
### ctype = ffi.metatype(ct, metatable)

Creates a ctype object for the given ct and associates it with a metatable.
Only struct/union types, complex numbers and vectors are allowed. Other types
may be wrapped in a struct, if needed.

The association with a metatable is permanent and cannot be changed afterwards.
Neither the contents of the metatable nor the contents of an `__index` table
(if any) may be modified afterwards. The associated metatable automatically
applies to all uses of this type, no matter how the objects are created or
where they originate from. Note that pre-defined operations on types have
precedence (e.g.  declared field names cannot be overriden).

All standard Lua metamethods are implemented. These are called directly,
without shortcuts and on any mix of types. For binary operations, the left
operand is checked first for a valid ctype metamethod. The `__gc` metamethod
only applies to struct/union types and performs an implicit
[`ffi.gc()`](#ffi_gc) call during creation of an instance.

<a name="ffi_gc"></a>
### cdata = ffi.gc(cdata, finalizer)

Associates a finalizer with a pointer or aggregate cdata object. The cdata
object is returned unchanged.

This function allows safe integration of unmanaged resources into the automatic
memory management of the moonjit garbage collector. Typical usage:

```
local p = ffi.gc(ffi.C.malloc(n), ffi.C.free)
...
p = nil -- Last reference to p is gone.
-- GC will eventually run finalizer: ffi.C.free(p)
```

A cdata finalizer works like the `__gc` metamethod for userdata objects: when
the last reference to a cdata object is gone, the associated finalizer is
called with the cdata object as an argument. The finalizer can be a Lua
function or a cdata function or cdata function pointer. An existing finalizer
can be removed by setting a nil finalizer, e.g. right before explicitly
deleting a resource:

```
ffi.C.free(ffi.gc(p, nil)) -- Manually free the memory.
```

## C Type Information

The following API functions return information about C types. They are most
useful for inspecting cdata objects.

### size = ffi.sizeof(ct [,nelem])

Returns the size of `ct` in bytes. Returns `nil` if the size is not known (e.g.
for "void" or function types). Requires nelem for VLA/VLS types, except for
cdata objects.

### align = ffi.alignof(ct)

Returns the minimum required alignment for `ct` in bytes.

### ofs [,bpos,bsize] = ffi.offsetof(ct, field)

Returns the offset (in bytes) of `field` relative to the start of `ct`, which
must be a struct. Additionally returns the position and the field size (in
bits) for bit fields.

### status = ffi.istype(ct, obj)

Returns true if `obj` has the C type given by `ct`. Returns false otherwise.

C type qualifiers (`const` etc.) are ignored. Pointers are checked with the
standard pointer compatibility rules, but without any special treatment for
`void *`. If ct specifies a struct/union, then a pointer to this type is
accepted, too. Otherwise the types must match exactly.

Note: this function accepts all kinds of Lua objects for the obj argument, but
always returns false for non-cdata objects.

## Utility Functions

### err = ffi.errno([newerr])

Returns the error number set by the last C function call which indicated an
error condition. If the optional newerr argument is present, the error number
is set to the new value and the previous value is returned.

This function offers a portable and OS-independent way to get and set the error
number. Note that only some C functions set the error number. And it's only
significant if the function actually indicated an error condition (e.g. with a
return value of `-1` or `NULL`). Otherwise, it may or may not contain any
previously set value.

You're advised to call this function only when needed and as close as possible
after the return of the related C function. The errno value is preserved across
hooks, memory allocations, invocations of the JIT compiler and other internal
VM activity. The same applies to the value returned by `GetLastError()` on
Windows, but you need to declare and call it yourself.

### str = ffi.string(ptr [,len])

Creates an interned Lua string from the data pointed to by ptr.

If the optional argument len is missing, `ptr` is converted to a `char *` and
the data is assumed to be zero-terminated. The length of the string is computed
with `strlen()`.

Otherwise `ptr` is converted to a `void *` and `len` gives the length of the
data. The data may contain embedded zeros and need not be byte-oriented (though
this may cause endianess issues).

This function is mainly useful to convert (temporary) `const char *` pointers
returned by C functions to Lua strings and store them or pass them to other
functions expecting a Lua string. The Lua string is an (interned) copy of the
data and bears no relation to the original data area anymore. Lua strings are 8
bit clean and may be used to hold arbitrary, non-character data.

**Performance notice**: it's faster to pass the length of the string, if it's
known. E.g. when the length is returned by a C call like `sprintf()`.

### ffi.copy(dst, src, len)
### ffi.copy(dst, str)

Copies the data pointed to by `src` to `dst`. `dst` is converted to a `void *`
and `src` is converted to a `const void *`.

In the first syntax, `len` gives the number of bytes to copy. *Caveat*: if
`src` is a Lua string, then `len` must not exceed `#src+1`.

In the second syntax, the `src` must be a Lua string. All bytes of the string
plus a zero-terminator are copied to `dst` (i.e. `#src+1` bytes).

**Performance notice**: `ffi.copy()` may be used as a faster (inlinable)
replacement for the C library functions `memcpy()`, `strcpy()` and `strncpy()`.

### ffi.fill(dst, len [,c])

Fills the data pointed to by `dst` with `len` constant bytes, given by `c`. If
`c` is omitted, the data is zero-filled.

**Performance notice**: `ffi.fill()` may be used as a faster (inlinable)
replacement for the C library function `memset(dst, c, len)`. Please note the
different order of arguments!

## Target-specific Information

### status = ffi.abi(param)

Returns `true` if param (a Lua string) applies for the target ABI (Application
Binary Interface). Returns `false` otherwise. The following parameters are
currently defined:

Parameter	| Description
----------------|------------
32bit		| 32 bit architecture
64bit		| 64 bit architecture
le		| Little-endian architecture
be		| Big-endian architecture
fpu		| Target has a hardware FPU
softfp		| softfp calling conventions
hardfp		| hardfp calling conventions
eabi		| EABI variant of the standard ABI
win		| Windows variant of the standard ABI

### ffi.os

Contains the target OS name. Same contents as [`jit.os`](../jit.md#jit_os).

### ffi.arch

Contains the target architecture name. Same contents as
[`jit.arch`](../jit.md#jit_arch).

## Methods for Callbacks

The C types for [callbacks](ffi-semantics.md#callback) have some extra methods:

### cb:free()

Free the resources associated with a callback. The associated Lua function is
unanchored and may be garbage collected. The callback function pointer is no
longer valid and must not be called anymore (it may be reused by a subsequently
created callback).

### cb:set(func)

Associate a new Lua function with a callback. The C type of the callback and
the callback function pointer are unchanged.

This method is useful to dynamically switch the receiver of callbacks without
creating a new callback each time and registering it again (e.g. with a GUI
library).

## Extended Standard Library Functions

The following standard library functions have been extended to work with cdata
objects:

### n = tonumber(cdata)

Converts a number cdata object to a `double` and returns it as a Lua number.
This is particularly useful for boxed 64 bit integer values. **Caveat**: this
conversion may incur a precision loss.

### s = tostring(cdata)

Returns a string representation of the value of 64 bit integers (`nnnLL` or
`nnnULL`) or complex numbers (`re±imi`). Otherwise returns a string
representation of the C type of a ctype object (`ctype<type>`) or a cdata
object (`cdata<type>: address`), unless you override it with a `__tostring`
metamethod (see [ffi.metatype()](#ffi_metatype)).

### iter, obj, start = pairs(cdata)
### iter, obj, start = ipairs(cdata)

Calls the `__pairs` or `__ipairs` metamethod of the corresponding ctype.

## Extensions to the Lua Parser

The parser for Lua source code treats numeric literals with the suffixes `LL`
or `ULL` as signed or unsigned 64 bit integers. Case doesn't matter, but
uppercase is recommended for readability. It handles both decimal (`42LL`) and
hexadecimal (`0x2aLL`) literals.

The imaginary part of complex numbers can be specified by suffixing number
literals with `i` or `I`, e.g. `12.5i`. Caveat: you'll need to use `1i` to get
an imaginary part with the value one, since `i` itself still refers to a
variable named `i`.
