# FFI Tutorial

This page is intended to give you an overview of the features of the FFI
library by presenting a few use cases and guidelines.

This page makes no attempt to explain all of the FFI library, though. You'll
want to have a look at the `ffi.*` API function reference and the FFI semantics
to learn more.

## Loading the FFI Library

The FFI library is built into moonjit by default, but it's not loaded and
initialized by default. The suggested way to use the FFI library is to add the
following to the start of every Lua file that needs one of its functions:

```
local ffi = require("ffi")
```

Please note this doesn't define an `ffi` variable in the table of globals — you
really need to use the local variable. The require function ensures the library
is only loaded once.

**Note**: If you want to experiment with the FFI from the interactive prompt of the
command line executable, omit the local, as it doesn't preserve local variables
across lines.

## Accessing Standard System Functions

The following code explains how to access standard system functions. We slowly
print two lines of dots by sleeping for 10 milliseconds after each dot:

```
local ffi = require("ffi")
① ffi.cdef[[
void Sleep(int ms);
int poll(struct pollfd *fds, unsigned long nfds, int timeout);
]]

local sleep
② if ffi.os == "Windows" then
③  function sleep(s)
④    ffi.C.Sleep(s*1000)
  end
else
  function sleep(s)
⑤    ffi.C.poll(nil, 0, s*1000)
  end
end

for i=1,160 do
  io.write("."); io.flush()
⑥  sleep(0.01)
end
io.write("\n")
```

Here's the step-by-step explanation:

① This defines the C library functions we're going to use. The part inside the
double-brackets (in green) is just standard C syntax. You can usually get this
info from the C header files or the documentation provided by each C library or
C compiler.

② The difficulty we're facing here, is that there are different standards to
choose from. Windows has a simple Sleep() function. On other systems there are
a variety of functions available to achieve sub-second sleeps, but with no
clear consensus. Thankfully poll() can be used for this task, too, and it's
present on most non-Windows systems. The check for ffi.os makes sure we use the
Windows-specific function only on Windows systems.

③ Here we're wrapping the call to the C function in a Lua function. This isn't
strictly necessary, but it's helpful to deal with system-specific issues only
in one part of the code. The way we're wrapping it ensures the check for the OS
is only done during initialization and not for every call.

④ A more subtle point is that we defined our sleep() function (for the sake of
this example) as taking the number of seconds, but accepting fractional
seconds. Multiplying this by 1000 gets us milliseconds, but that still leaves
it a Lua number, which is a floating-point value. Alas, the Sleep() function
only accepts an integer value. Luckily for us, the FFI library automatically
performs the conversion when calling the function (truncating the FP value
towards zero, like in C).

Some readers will notice that Sleep() is part of KERNEL32.DLL and is also a
stdcall function. So how can this possibly work? The FFI library provides the
ffi.C default C library namespace, which allows calling functions from the
default set of libraries, like a C compiler would. Also, the FFI library
automatically detects stdcall functions, so you don't need to declare them as
such.

⑤ The poll() function takes a couple more arguments we're not going to use. You
can simply use nil to pass a NULL pointer and 0 for the nfds parameter. Please
note that the number 0 does not convert to a pointer value, unlike in C++. You
really have to pass pointers to pointer arguments and numbers to number
arguments.

The page on FFI semantics has all of the gory details about conversions between
Lua objects and C types. For the most part you don't have to deal with this, as
it's performed automatically and it's carefully designed to bridge the semantic
differences between Lua and C.

⑥ Now that we have defined our own sleep() function, we can just call it from
plain Lua code. That wasn't so bad, huh? Turning these boring animated dots
into a fascinating best-selling game is left as an exercise for the reader. :-)

## Accessing the zlib Compression Library

The following code shows how to access the zlib compression library from Lua
code. We'll define two convenience wrapper functions that take a string and
compress or uncompress it to another string:

```
local ffi = require("ffi")
①ffi.cdef[[
unsigned long compressBound(unsigned long sourceLen);
int compress2(uint8_t *dest, unsigned long *destLen,
	      const uint8_t *source, unsigned long sourceLen, int level);
int uncompress(uint8_t *dest, unsigned long *destLen,
	       const uint8_t *source, unsigned long sourceLen);
]]
②local zlib = ffi.load(ffi.os == "Windows" and "zlib1" or "z")

local function compress(txt)
③  local n = zlib.compressBound(#txt)
  local buf = ffi.new("uint8_t[?]", n)
④  local buflen = ffi.new("unsigned long[1]", n)
  local res = zlib.compress2(buf, buflen, txt, #txt, 9)
  assert(res == 0)
⑤  return ffi.string(buf, buflen[0])
end

⑥local function uncompress(comp, n)
  local buf = ffi.new("uint8_t[?]", n)
  local buflen = ffi.new("unsigned long[1]", n)
  local res = zlib.uncompress(buf, buflen, comp, #comp)
  assert(res == 0)
  return ffi.string(buf, buflen[0])
end

⑦-- Simple test code.
local txt = string.rep("abcd", 1000)
print("Uncompressed size: ", #txt)
local c = compress(txt)
print("Compressed size: ", #c)
local txt2 = uncompress(c, #txt)
assert(txt2 == txt)
```

Here's the step-by-step explanation:

① This defines some of the C functions provided by zlib. For the sake of this
example, some type indirections have been reduced and it uses the pre-defined
fixed-size integer types, while still adhering to the zlib API/ABI.

② This loads the zlib shared library. On POSIX systems it's named libz.so and
usually comes pre-installed. Since ffi.load() automatically adds any missing
standard prefixes/suffixes, we can simply load the "z" library. On Windows it's
named zlib1.dll and you'll have to download it first from the zlib site. The
check for ffi.os makes sure we pass the right name to ffi.load().

③ First, the maximum size of the compression buffer is obtained by calling the
zlib.compressBound function with the length of the uncompressed string. The
next line allocates a byte buffer of this size. The [?] in the type
specification indicates a variable-length array (VLA). The actual number of
elements of this array is given as the 2nd argument to ffi.new().

④ This may look strange at first, but have a look at the declaration of the
compress2 function from zlib: the destination length is defined as a pointer!
This is because you pass in the maximum buffer size and get back the actual
length that was used.

In C you'd pass in the address of a local variable (&buflen). But since there's
no address-of operator in Lua, we'll just pass in a one-element array.
Conveniently it can be initialized with the maximum buffer size in one step.
Calling the actual zlib.compress2 function is then straightforward.

⑤ We want to return the compressed data as a Lua string, so we'll use
ffi.string(). It needs a pointer to the start of the data and the actual
length. The length has been returned in the buflen array, so we'll just get it
from there.

Note that since the function returns now, the buf and buflen variables will
eventually be garbage collected. This is fine, because ffi.string() has copied
the contents to a newly created (interned) Lua string. If you plan to call this
function lots of times, consider reusing the buffers and/or handing back the
results in buffers instead of strings. This will reduce the overhead for
garbage collection and string interning.

⑥ The uncompress functions does the exact opposite of the compress function.
The compressed data doesn't include the size of the original string, so this
needs to be passed in. Otherwise no surprises here.

⑦ The code, that makes use of the functions we just defined, is just plain Lua
code. It doesn't need to know anything about the moonjit FFI — the convenience
wrapper functions completely hide it.

One major advantage of the moonjit FFI is that you are now able to write those
wrappers in Lua. And at a fraction of the time it would cost you to create an
extra C module using the Lua/C API. Many of the simpler C functions can
probably be used directly from your Lua code, without any wrappers.

Side note: the zlib API uses the long type for passing lengths and sizes
around. But all those zlib functions actually only deal with 32 bit values.
This is an unfortunate choice for a public API, but may be explained by zlib's
history — we'll just have to deal with it.

First, you should know that a long is a 64 bit type e.g. on POSIX/x64 systems,
but a 32 bit type on Windows/x64 and on 32 bit systems. Thus a long result can
be either a plain Lua number or a boxed 64 bit integer cdata object, depending
on the target system.

Ok, so the `ffi.*` functions generally accept cdata objects wherever you'd want
to use a number. That's why we get a away with passing n to ffi.string() above.
But other Lua library functions or modules don't know how to deal with this. So
for maximum portability one needs to use tonumber() on returned long results
before passing them on. Otherwise the application might work on some systems,
but would fail in a POSIX/x64 environment.

## Defining Metamethods for a C Type

The following code explains how to define metamethods for a C type. We define a
simple point type and add some operations to it:

```
local ffi = require("ffi")
①ffi.cdef[[
typedef struct { double x, y; } point_t;
]]

②local point
local mt = {
③  __add = function(a, b) return point(a.x+b.x, a.y+b.y) end,
  __len = function(a) return math.sqrt(a.x*a.x + a.y*a.y) end,
④  __index = {
    area = function(a) return a.x*a.x + a.y*a.y end,
  },
}
⑤point = ffi.metatype("point_t", mt)

⑥local a = point(3, 4)
print(a.x, a.y)  --> 3  4
print(#a)        --> 5
print(a:area())  --> 25
local b = a + point(0.5, 8)
print(#b)        --> 12.5
```

Here's the step-by-step explanation:

① This defines the C type for a two-dimensional point object.

② We have to declare the variable holding the point constructor first, because
it's used inside of a metamethod.

③ Let's define an `__add` metamethod which adds the coordinates of two points and
creates a new point object. For simplicity, this function assumes that both
arguments are points. But it could be any mix of objects, if at least one
operand is of the required type (e.g. adding a point plus a number or vice
versa). Our `__len` metamethod returns the distance of a point to the origin.

④ If we run out of operators, we can define named methods, too. Here the
`__index` table defines an area function. For custom indexing needs, one might
want to define `__index` and `__newindex` functions instead.

⑤ This associates the metamethods with our C type. This only needs to be done
once. For convenience, a constructor is returned by ffi.metatype(). We're not
required to use it, though. The original C type can still be used e.g. to
create an array of points. The metamethods automatically apply to any and all
uses of this type.

Please note that the association with a metatable is permanent and the
metatable must not be modified afterwards! Ditto for the `__index` table.

⑥ Here are some simple usage examples for the point type and their expected
results. The pre-defined operations (such as a.x) can be freely mixed with the
newly defined metamethods. Note that area is a method and must be called with
the Lua syntax for methods: a:area(), not a.area().

The C type metamethod mechanism is most useful when used in conjunction with C
libraries that are written in an object-oriented style. Creators return a
pointer to a new instance and methods take an instance pointer as the first
argument. Sometimes you can just point __index to the library namespace and
__gc to the destructor and you're done. But often enough you'll want to add
convenience wrappers, e.g. to return actual Lua strings or when returning
multiple values.

Some C libraries only declare instance pointers as an opaque void * type. In
this case you can use a fake type for all declarations, e.g. a pointer to a
named (incomplete) struct will do: `typedef struct foo_type *foo_handle`. The C
side doesn't know what you declare with the moonjit FFI, but as long as the
underlying types are compatible, everything still works.

## Translating C Idioms

Here's a list of common C idioms and their translation to the moonjit FFI:

Idiom 	| C code 	| Lua code
--------|---------------|---------
Pointer dereference<br/>`int *p;` | `x = *p;`<br/>`*p = y;` | `x = p[0]`<br/>`p[0] = y`
Pointer indexing<br/>`int i, *p;` | `x = p[i];`<br/>`p[i+1] = y;` | `x = p[i]`<br/>`p[i+1] = y`
Array indexing<br/>`int i, a[];` | `x = a[i];`<br/>`a[i+1] = y;` | `x = a[i]`<br/>`a[i+1] = y`
struct/union dereference<br/>`struct foo s;` | `x = s.field;`<br/>`s.field = y;` | `x = s.field`<br/>`s.field = y`
struct/union pointer deref.<br/>`struct foo *sp;` | `x = sp->field;`<br/>`sp->field = y;` | `x = s.field`<br/>`s.field = y`
Pointer arithmetic<br/>`int i, *p;` | `x = p + i;`<br/>`y = p - i;` | `x = p + i`<br/>`y = p - i`
Pointer difference<br/>`int *p1, *p2;` | `x = p1 - p2;` | `x = p1 - p2`
Array element pointer<br/>`int i, a[];` | `x = &a[i];` | `x = a+i`
Cast pointer to address<br/>`int *p;` | `x = (intptr_t)p;` | `x = tonumber(ffi.cast("intptr_t", p))`
Functions with outargs<br/>`void foo(int *inoutlen);` | `int len = x;`<br/>`foo(&len);`<br/>`y = len;` | `local len = ffi.new("int[1]", x)`<br/>`foo(len)`<br/>`y = len[0]`
Vararg conversions<br/>`int printf(char *fmt, ...);` | `printf("%g", 1.0);`<br/>`printf("%d", 1);` | `printf("%g", 1);`<br/>`printf("%d", ffi.new("int", 1))`

## To Cache or Not to Cache

It's a common Lua idiom to cache library functions in local variables or
upvalues, e.g.:

```
local byte, char = string.byte, string.char
local function foo(x)
  return char(byte(x)+1)
end
```

This replaces several hash-table lookups with a (faster) direct use of a local
or an upvalue. This is less important with moonjit, since the JIT compiler
optimizes hash-table lookups a lot and is even able to hoist most of them out
of the inner loops. It can't eliminate all of them, though, and it saves some
typing for often-used functions. So there's still a place for this, even with
moonjit.

The situation is a bit different with C function calls via the FFI library. The
JIT compiler has special logic to eliminate all of the lookup overhead for
functions resolved from a C library namespace! Thus it's not helpful and
actually counter-productive to cache individual C functions like this:

```
local funca, funcb = ffi.C.funca, ffi.C.funcb -- Not helpful!
local function foo(x, n)
  for i=1,n do funcb(funca(x, i), 1) end
end
```

This turns them into indirect calls and generates bigger and slower machine
code. Instead you'll want to cache the namespace itself and rely on the JIT
compiler to eliminate the lookups:

```
local C = ffi.C          -- Instead use this!
local function foo(x, n)
  for i=1,n do C.funcb(C.funca(x, i), 1) end
end
```

This generates both shorter and faster code. So don't cache C functions, but do
cache namespaces! Most often the namespace is already in a local variable at an
outer scope, e.g. from `local lib = ffi.load(...)`. Note that copying it to a
local variable in the function scope is unnecessary.
