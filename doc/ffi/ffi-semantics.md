# FFI Semantics

This page describes the detailed semantics underlying the FFI library and its
interaction with both Lua and C code.

Given that the FFI library is designed to interface with C code and that
declarations can be written in plain C syntax, it closely follows the C
language semantics, wherever possible. Some minor concessions are needed for
smoother interoperation with Lua language semantics.

Please don't be overwhelmed by the contents of this page — this is a reference
and you may need to consult it, if in doubt. It doesn't hurt to skim this page,
but most of the semantics "just work" as you'd expect them to work. It should
be straightforward to write applications using the moonjit FFI for developers
with a C or C++ background.

## C Language Support

The FFI library has a built-in C parser with a minimal memory footprint. It's
used by the [ffi.\* library functions](ffi-api.md) to declare C types or
external symbols.

It's only purpose is to parse C declarations, as found e.g. in C header files.
Although it does evaluate constant expressions, it's not a C compiler. The body
of inline C function definitions is simply ignored.

Also, this is not a validating C parser. It expects and accepts correctly
formed C declarations, but it may choose to ignore bad declarations or show
rather generic error messages. If in doubt, please check the input against your
favorite C compiler.

The C parser complies to the C99 language standard plus the following
extensions:

* The `'\e'` escape in character and string literals.
* The C99/C++ boolean type, declared with the keywords `bool` or `_Bool`.
* Complex numbers, declared with the keywords complex or `_Complex`.
* Two complex number types: `complex` (aka `complex double`) and `complex
  float`.
* Vector types, declared with the GCC `mode` or `vector_size` attribute.
* Unnamed ('transparent') struct/union fields inside a struct/union.
* Incomplete enum declarations, handled like incomplete struct declarations.
* Unnamed enum fields inside a struct/union. This is similar to a scoped C++
  enum, except that declared constants are visible in the global namespace,
  too.
* Scoped `static const` declarations inside a struct/union (from C++).
* Zero-length arrays ([0]), empty struct/union, variable-length arrays (VLA,
  [?]) and variable-length structs (VLS, with a trailing VLA).
* C++ reference types (`int &x`).
* Alternate GCC keywords with `'__'`, e.g. `__const__`.
* GCC `__attribute__` with the following attributes: `aligned`, `packed`,
  `mode`, `vector_size`, `cdecl`, `fastcall`, `stdcall`, `thiscall`.
* The GCC `__extension__` keyword and the GCC `__alignof__` operator.
* GCC `__asm__("symname")` symbol name redirection for function declarations.
* MSVC keywords for fixed-length types: `__int8`, `__int16`, `__int32` and
  `__int64`.
* MSVC `__cdecl`, `__fastcall`, `__stdcall`, `__thiscall`, `__ptr32`,
  `__ptr64`, `__declspec(align(n))` and `#pragma pack`.
* All other GCC/MSVC-specific attributes are ignored.

The following C types are pre-defined by the C parser (like a typedef, except
re-declarations will be ignored):

* Vararg handling: `va_list`, `__builtin_va_list`, `__gnuc_va_list`.
* From `<stddef.h>`: `ptrdiff_t`, `size_t`, `wchar_t`.
* From `<stdint.h>`: `int8_t`, `int16_t`, `int32_t`, `int64_t`, `uint8_t`,
  `uint16_t`, `uint32_t`, `uint64_t`, `intptr_t`, `uintptr_t`.

You're encouraged to use these types in preference to compiler-specific
extensions or target-dependent standard types. E.g. `char` differs in
signedness and `long` differs in size, depending on the target architecture and
platform ABI.

The following C features are not supported:

* A declaration must always have a type specifier; it doesn't default to an
  `int` type.
* Old-style empty function declarations (K&R) are not allowed. All C functions
  must have a proper prototype declaration. A function declared without
  parameters (`int foo();`) is treated as a function taking zero arguments,
  like in C++.
* The `long double` C type is parsed correctly, but there's no support for the
  related conversions, accesses or arithmetic operations.
* Wide character strings and character literals are not supported.
* See below for features that are currently not implemented.

## C Type Conversion Rules

### Conversions from C types to Lua objects

These conversion rules apply for read accesses to C types: indexing pointers,
arrays or struct/union types; reading external variables or constant values;
retrieving return values from C calls:

Input 			| Conversion 				| Output
------------------------|---------------------------------------|-------
int8_t, int16_t		| →sign-ext int32_t → double		| number
uint8_t, uint16_t	| →zero-ext int32_t → double		| number
int32_t, uint32_t	| → double				| number
int64_t, uint64_t	| boxed value				| 64 bit int cdata
double, float		| → double				| number
bool			| 0 → false, otherwise true		| boolean
enum			| boxed value				| enum cdata
Complex number		| boxed value				| complex cdata
Vector			| boxed value				| vector cdata
Pointer			| boxed value				| pointer cdata
Array			| boxed reference			| reference cdata
struct/union		| boxed reference			| reference cdata

Bitfields are treated like their underlying type.

Reference types are dereferenced before a conversion can take place — the
conversion is applied to the C type pointed to by the reference.

### Conversions from Lua objects to C types

These conversion rules apply for write accesses to C types: indexing pointers,
arrays or struct/union types; initializing cdata objects; casts to C types;
writing to external variables; passing arguments to C calls:

Input 		| Conversion 				| Output
----------------|---------------------------------------|-------
number		| →					| double
boolean		| false → 0, true → 1			| bool
nil		| NULL →				| (void *)
lightuserdata	| lightuserdata address →		| (void *)
userdata	| userdata payload →			| (void *)
io.* file	| get FILE * handle →			| (void *)
string		| match against enum constant		| enum
string		| copy string data + zero-byte		| int8_t[], uint8_t[]
string		| string data →				| const char[]
function	| [create callback](#callback) →	| C function type
table		| [table initializer](#init_table)	| Array
table		| [table initializer](#init_table)	| struct/union
cdata		| cdata payload →			| C type

If the result type of this conversion doesn't match the C type of the
destination, the [conversion rules between C types](#convert_between) are
applied.

Reference types are immutable after initialization ("no re-seating of
references"). For initialization purposes or when passing values to reference
parameters, they are treated like pointers. Note that unlike in C++, there's no
way to implement automatic reference generation of variables under the Lua
language semantics. If you want to call a function with a reference parameter,
you need to explicitly pass a one-element array.

<a name="convert_between"></a>
### Conversions between C types

These conversion rules are more or less the same as the standard C conversion
rules. Some rules only apply to casts, or require pointer or type
compatibility:

Input 			| Conversion 			| Output
------------------------|-------------------------------|-------
Signed integer		| →narrow or sign-extend	| Integer
Unsigned integer	| →narrow or zero-extend	| Integer
Integer			| →round			| double, float
double, float		| →trunc int32_t →narrow	| (u)int8_t, (u)int16_t
double, float		| →trunc			| (u)int32_t, (u)int64_t
double, float		| →round			| float, double
Number			| n == 0 → 0, otherwise 1	| bool
bool			| false → 0, true → 1		| Number
Complex number		| convert real part		| Number
Number			| convert real part, imag = 0	| Complex number
Complex number		| convert real and imag part	| Complex number
Number			| convert scalar and replicate	| Vector
Vector			| copy (same size)		| Vector
struct/union		| take base address (compat)	| Pointer
Array			| take base address (compat)	| Pointer
Function		| take function address		| Function pointer
Number			| convert via uintptr_t (cast)	| Pointer
Pointer			| convert address (compat/cast)	| Pointer
Pointer			| convert address (cast)	| Integer
Array			| convert base address (cast)	| Integer
Array			| copy (compat)			| Array
struct/union		| copy (identical type)		| struct/union

Bitfields or enum types are treated like their underlying type.

Conversions not listed above will raise an error. E.g. it's not possible to
convert a pointer to a complex number or vice versa.

### Conversions for vararg C function arguments

The following default conversion rules apply when passing Lua objects to the
variable argument part of vararg C functions:

Input 			| Conversion 			| Output
------------------------|-------------------------------|-------
number			| →				| double
boolean			| false → 0, true → 1		| bool
nil			| NULL →			| (void *)
userdata		| userdata payload →		| (void *)
lightuserdata		| lightuserdata address →	| (void *)
string			| string data →			| const char *
float cdata		| →				| double
Array cdata		| take base address		| Element pointer
struct/union cdata	| take base address		| struct/union pointer
Function cdata		| take function address		| Function pointer
Any other cdata		| no conversion			| C type

To pass a Lua object, other than a cdata object, as a specific type, you need
to override the conversion rules: create a temporary cdata object with a
constructor or a cast and initialize it with the value to pass:

Assuming x is a Lua number, here's how to pass it as an integer to a vararg
function:

```
ffi.cdef[[
int printf(const char *fmt, ...);
]]
ffi.C.printf("integer value: %d\n", ffi.new("int", x))
```

If you don't do this, the default Lua number → double conversion rule applies.
A vararg C function expecting an integer will see a garbled or uninitialized
value.

## Initializers

Creating a cdata object with [`ffi.new()`](ffi-api.md#ffi_new) or the
equivalent constructor syntax always initializes its contents, too. Different
rules apply, depending on the number of optional initializers and the C types
involved:

* If no initializers are given, the object is filled with zero bytes.
* Scalar types (numbers and pointers) accept a single initializer. The Lua
  object is converted to the scalar C type.
* Valarrays (complex numbers and vectors) are treated like scalars when a
  single initializer is given. Otherwise they are treated like regular arrays.
* Aggregate types (arrays and structs) accept either a single cdata initializer
  of the same type (copy constructor), a single table initializer, or a flat
  list of initializers.
* The elements of an array are initialized, starting at index zero. If a single
  initializer is given for an array, it's repeated for all remaining elements.
  This doesn't happen if two or more initializers are given: all remaining
  uninitialized elements are filled with zero bytes.
* Byte arrays may also be initialized with a Lua string. This copies the whole
  string plus a terminating zero-byte. The copy stops early only if the array
  has a known, fixed size.
* The fields of a struct are initialized in the order of their declaration.
  Uninitialized fields are filled with zero bytes.
* Only the first field of a union can be initialized with a flat initializer.
* Elements or fields which are aggregates themselves are initialized with a
  single initializer, but this may be a table initializer or a compatible
  aggregate.
* Excess initializers cause an error.

## Table Initializers

The following rules apply if a Lua table is used to initialize an Array or a
struct/union:

* If the table index [0] is non-nil, then the table is assumed to be
  zero-based. Otherwise it's assumed to be one-based.
* Array elements, starting at index zero, are initialized one-by-one with the
  consecutive table elements, starting at either index [0] or [1]. This process
  stops at the first nil table element.
* If exactly one array element was initialized, it's repeated for all the
  remaining elements. Otherwise all remaining uninitialized elements are filled
  with zero bytes.
* The above logic only applies to arrays with a known fixed size. A VLA is only
  initialized with the element(s) given in the table. Depending on the use
  case, you may need to explicitly add a NULL or 0 terminator to a VLA.
* A struct/union can be initialized in the order of the declaration of its
  fields. Each field is initialized with consecutive table elements, starting
  at either index [0] or [1]. This process stops at the first nil table element.
* Otherwise, if neither index [0] nor [1] is present, a struct/union is
  initialized by looking up each field name (as a string key) in the table.
  Each non-nil value is used to initialize the corresponding field.
* Uninitialized fields of a struct are filled with zero bytes, except for the
  trailing VLA of a VLS.
* Initialization of a union stops after one field has been initialized. If no
  field has been initialized, the union is filled with zero bytes.
* Elements or fields which are aggregates themselves are initialized with a
  single initializer, but this may be a nested table initializer (or a
  compatible aggregate).
* Excess initializers for an array cause an error. Excess initializers for a
  struct/union are ignored. Unrelated table entries are ignored, too.

Example:

```
local ffi = require("ffi")

ffi.cdef[[
struct foo { int a, b; };
union bar { int i; double d; };
struct nested { int x; struct foo y; };
]]

ffi.new("int[3]", {})            --> 0, 0, 0
ffi.new("int[3]", {1})           --> 1, 1, 1
ffi.new("int[3]", {1,2})         --> 1, 2, 0
ffi.new("int[3]", {1,2,3})       --> 1, 2, 3
ffi.new("int[3]", {[0]=1})       --> 1, 1, 1
ffi.new("int[3]", {[0]=1,2})     --> 1, 2, 0
ffi.new("int[3]", {[0]=1,2,3})   --> 1, 2, 3
ffi.new("int[3]", {[0]=1,2,3,4}) --> error: too many initializers

ffi.new("struct foo", {})            --> a = 0, b = 0
ffi.new("struct foo", {1})           --> a = 1, b = 0
ffi.new("struct foo", {1,2})         --> a = 1, b = 2
ffi.new("struct foo", {[0]=1,2})     --> a = 1, b = 2
ffi.new("struct foo", {b=2})         --> a = 0, b = 2
ffi.new("struct foo", {a=1,b=2,c=3}) --> a = 1, b = 2  'c' is ignored

ffi.new("union bar", {})        --> i = 0, d = 0.0
ffi.new("union bar", {1})       --> i = 1, d = ?
ffi.new("union bar", {[0]=1,2}) --> i = 1, d = ?    '2' is ignored
ffi.new("union bar", {d=2})     --> i = ?, d = 2.0

ffi.new("struct nested", {1,{2,3}})     --> x = 1, y.a = 2, y.b = 3
ffi.new("struct nested", {x=1,y={2,3}}) --> x = 1, y.a = 2, y.b = 3
```

## Operations on cdata Objects

All of the standard Lua operators can be applied to cdata objects or a mix of a
cdata object and another Lua object. The following list shows the pre-defined
operations.

Reference types are dereferenced before performing each of the operations below
— the operation is applied to the C type pointed to by the reference.

The pre-defined operations are always tried first before deferring to a
metamethod or index table (if any) for the corresponding ctype (except for
`__new`). An error is raised if the metamethod lookup or index table lookup
fails.

### Indexing a cdata object

* **Indexing a pointer/array**: a cdata pointer/array can be indexed by a cdata
  number or a Lua number. The element address is computed as the base address
  plus the number value multiplied by the element size in bytes. A read access
  loads the element value and converts it to a Lua object. A write access
  converts a Lua object to the element type and stores the converted value to the
  element. An error is raised if the element size is undefined or a write access
  to a constant element is attempted.
* **Dereferencing a struct/union field**: a cdata struct/union or a pointer to a
  struct/union can be dereferenced by a string key, giving the field name. The
  field address is computed as the base address plus the relative offset of the
  field. A read access loads the field value and converts it to a Lua object. A
  write access converts a Lua object to the field type and stores the converted
  value to the field. An error is raised if a write access to a constant
  struct/union or a constant field is attempted. Scoped enum constants or static
  constants are treated like a constant field.
* **Indexing a complex number**: a complex number can be indexed either by a
  cdata number or a Lua number with the values 0 or 1, or by the strings "re"
  or "im". A read access loads the real part ([0], .re) or the imaginary part
  ([1], .im) part of a complex number and converts it to a Lua number. The
  sub-parts of a complex number are immutable — assigning to an index of a
  complex number raises an error. Accessing out-of-bound indexes returns
  unspecified results, but is guaranteed not to trigger memory access violations.
* **Indexing a vector**: a vector is treated like an array for indexing purposes,
  except the vector elements are immutable — assigning to an index of a vector
  raises an error.

A ctype object can be indexed with a string key, too. The only pre-defined
operation is reading scoped constants of struct/union types. All other accesses
defer to the corresponding metamethods or index tables (if any).

Note: since there's (deliberately) no address-of operator, a cdata object
holding a value type is effectively immutable after initialization. The JIT
compiler benefits from this fact when applying certain optimizations.

As a consequence, the elements of complex numbers and vectors are immutable.
But the elements of an aggregate holding these types may be modified of course.
I.e. you cannot assign to foo.c.im, but you can assign a (newly created)
complex number to foo.c.

The JIT compiler implements strict aliasing rules: accesses to different types
do not alias, except for differences in signedness (this applies even to char
pointers, unlike C99). Type punning through unions is explicitly detected and
allowed.

### Calling a cdata object

* **Constructor**: a ctype object can be called and used as a constructor. This
  is equivalent to ffi.new(ct, ...), unless a `__new` metamethod is defined.
  The `__new` metamethod is called with the ctype object plus any other arguments
  passed to the contructor. Note that you have to use `ffi.new` inside of it,
  since calling `ct(...)` would cause infinite recursion.
* **C function call**: a cdata function or cdata function pointer can be
  called. The passed arguments are converted to the C types of the parameters
  given by the function declaration. Arguments passed to the variable argument
  part of vararg C function use special conversion rules. This C function is
  called and the return value (if any) is converted to a Lua object.  On
  Windows/x86 systems, `__stdcall` functions are automatically detected and a
  function declared as `__cdecl` (the default) is silently fixed up after the
  first call.

### Arithmetic on cdata objects

* **Pointer arithmetic**: a cdata pointer/array and a cdata number or a Lua
  number can be added or subtracted. The number must be on the right hand side
  for a subtraction. The result is a pointer of the same type with an address
  plus or minus the number value multiplied by the element size in bytes. An
  error is raised if the element size is undefined.
* **Pointer difference**: two compatible cdata pointers/arrays can be
  subtracted. The result is the difference between their addresses, divided by
  the element size in bytes. An error is raised if the element size is
  undefined or zero.
* **64 bit integer arithmetic**: the standard arithmetic operators (+ - * / % ^
  and unary minus) can be applied to two cdata numbers, or a cdata number and a
  Lua number. If one of them is an `uint64_t`, the other side is converted to
  an `uint64_t` and an unsigned arithmetic operation is performed. Otherwise
  both sides are converted to an `int64_t` and a signed arithmetic operation is
  performed. The result is a boxed 64 bit cdata object.
  If one of the operands is an enum and the other operand is a string, the
  string is converted to the value of a matching enum constant before the above
  conversion.
  These rules ensure that 64 bit integers are "sticky". Any expression
  involving at least one 64 bit integer operand results in another one. The
  undefined cases for the division, modulo and power operators return `2LL ^ 63` or
  `2ULL ^ 63`.
  You'll have to explicitly convert a 64 bit integer to a Lua number (e.g. for
  regular floating-point calculations) with tonumber(). But note this may incur a
  precision loss.

### Comparisons of cdata objects

* **Pointer comparison**: two compatible cdata pointers/arrays can be compared.
  The result is the same as an unsigned comparison of their addresses. nil is
  treated like a NULL pointer, which is compatible with any other pointer type.
* **64 bit integer comparison**: two cdata numbers, or a cdata number and a Lua
  number can be compared with each other. If one of them is an `uint64_t`, the
  other side is converted to an `uint64_t` and an unsigned comparison is
  performed. Otherwise both sides are converted to an `int64_t` and a signed
  comparison is performed.
  If one of the operands is an enum and the other operand is a string, the
  string is converted to the value of a matching enum constant before the above
  conversion.
* **Comparisons for equality/inequality** never raise an error. Even
  incompatible pointers can be compared for equality by address. Any other
  incompatible comparison (also with non-cdata objects) treats the two sides as
  unequal.

### cdata objects as table keys

Lua tables may be indexed by cdata objects, but this doesn't provide any useful
semantics — cdata objects are unsuitable as table keys!

A cdata object is treated like any other garbage-collected object and is hashed
and compared by its address for table indexing. Since there's no interning for
cdata value types, the same value may be boxed in different cdata objects with
different addresses. Thus t[1LL+1LL] and t[2LL] usually do not point to the
same hash slot and they certainly do not point to the same hash slot as t[2].

It would seriously drive up implementation complexity and slow down the common
case, if one were to add extra handling for by-value hashing and comparisons to
Lua tables. Given the ubiquity of their use inside the VM, this is not
acceptable.

There are three viable alternatives, if you really need to use cdata objects as
keys:

* If you can get by with the precision of Lua numbers (52 bits), then use
  tonumber() on a cdata number or combine multiple fields of a cdata aggregate
  to a Lua number. Then use the resulting Lua number as a key when indexing
  tables.
  One obvious benefit: `t[tonumber(2LL)]` does point to the same slot as t[2].
* Otherwise use either `tostring()` on 64 bit integers or complex numbers or
  combine multiple fields of a cdata aggregate to a Lua string (e.g. with
  `ffi.string()`). Then use the resulting Lua string as a key when indexing
  tables.
* Create your own specialized hash table implementation using the C types
  provided by the FFI library, just like you would in C code. Ultimately this
  may give much better performance than the other alternatives or what a
  generic by-value hash table could possibly provide.

## Parameterized Types

To facilitate some abstractions, the two functions ffi.typeof and ffi.cdef
support parameterized types in C declarations. Note: none of the other API
functions taking a cdecl allow this.

Any place you can write a typedef name, an identifier or a number in a
declaration, you can write $ (the dollar sign) instead. These placeholders are
replaced in order of appearance with the arguments following the cdecl string:

```
-- Declare a struct with a parameterized field type and name:
ffi.cdef([[
typedef struct { $ $; } foo_t;
]], type1, name1)

-- Anonymous struct with dynamic names:
local bar_t = ffi.typeof("struct { int $, $; }", name1, name2)
-- Derived pointer type:
local bar_ptr_t = ffi.typeof("$ *", bar_t)

-- Parameterized dimensions work even where a VLA won't work:
local matrix_t = ffi.typeof("uint8_t[$][$]", width, height)
```

*Caveat*: this is not simple text substitution! A passed ctype or cdata object
is treated like the underlying type, a passed string is considered an
identifier and a number is considered a number. You must not mix this up: e.g.
passing "int" as a string doesn't work in place of a type, you'd need to use
`ffi.typeof("int")` instead.

The main use for parameterized types are libraries implementing abstract data
types (example), similar to what can be achieved with C++ template
metaprogramming. Another use case are derived types of anonymous structs, which
avoids pollution of the global struct namespace.

Please note that parameterized types are a nice tool and indispensable for
certain use cases. But you'll want to use them sparingly in regular code, e.g.
when all types are actually fixed.

## Garbage Collection of cdata Objects

All explicitly (`ffi.new()`, `ffi.cast()` etc.) or implicitly (accessors)
created cdata objects are garbage collected. You need to ensure to retain valid
references to cdata objects somewhere on a Lua stack, an upvalue or in a Lua
table while they are still in use. Once the last reference to a cdata object is
gone, the garbage collector will automatically free the memory used by it (at
the end of the next GC cycle).

Please note that pointers themselves are cdata objects, however they are not
followed by the garbage collector. So e.g. if you assign a cdata array to a
pointer, you must keep the cdata object holding the array alive as long as the
pointer is still in use:

```
ffi.cdef[[
typedef struct { int *a; } foo_t;
]]

local s = ffi.new("foo_t", ffi.new("int[10]")) -- WRONG!

local a = ffi.new("int[10]") -- OK
local s = ffi.new("foo_t", a)
-- Now do something with 's', but keep 'a' alive until you're done.
```

Similar rules apply for Lua strings which are implicitly converted to `const
char *`: the string object itself must be referenced somewhere or it'll be
garbage collected eventually. The pointer will then point to stale data, which
may have already been overwritten. Note that string literals are automatically
kept alive as long as the function containing it (actually its prototype) is
not garbage collected.

Objects which are passed as an argument to an external C function are kept
alive until the call returns. So it's generally safe to create temporary cdata
objects in argument lists. This is a common idiom for passing specific C types
to vararg functions.

Memory areas returned by C functions (e.g. from malloc()) must be manually
managed, of course (or use ffi.gc()). Pointers to cdata objects are
indistinguishable from pointers returned by C functions (which is one of the
reasons why the GC cannot follow them).

## Callbacks

The moonjit FFI automatically generates special callback functions whenever a
Lua function is converted to a C function pointer. This associates the
generated callback function pointer with the C type of the function pointer and
the Lua function object (closure).

This can happen implicitly due to the usual conversions, e.g. when passing a
Lua function to a function pointer argument. Or you can use ffi.cast() to
explicitly cast a Lua function to a C function pointer.

Currently only certain C function types can be used as callback functions.
Neither C vararg functions nor functions with pass-by-value aggregate argument
or result types are supported. There are no restrictions for the kind of Lua
functions that can be called from the callback — no checks for the proper
number of arguments are made. The return value of the Lua function will be
converted to the result type and an error will be thrown for invalid
conversions.

It's allowed to throw errors across a callback invocation, but it's not
advisable in general. Do this only if you know the C function, that called the
callback, copes with the forced stack unwinding and doesn't leak resources.

One thing that's not allowed, is to let an FFI call into a C function get
JIT-compiled, which in turn calls a callback, calling into Lua again. Usually
this attempt is caught by the interpreter first and the C function is
blacklisted for compilation.

However, this heuristic may fail under specific circumstances: e.g. a message
polling function might not run Lua callbacks right away and the call gets
JIT-compiled. If it later happens to call back into Lua (e.g. a rarely invoked
error callback), you'll get a VM PANIC with the message `bad callback`. Then
you'll need to manually turn off JIT-compilation with jit.off() for the
surrounding Lua function that invokes such a message polling function (or
similar).

### Callback resource handling

Callbacks take up resources — you can only have a limited number of them at the
same time (500 - 1000, depending on the architecture). The associated Lua
functions are anchored to prevent garbage collection, too.

**Callbacks due to implicit conversions are permanent!** There is no way to
guess their lifetime, since the C side might store the function pointer for
later use (typical for GUI toolkits). The associated resources cannot be
reclaimed until termination:

```
ffi.cdef[[
typedef int (__stdcall *WNDENUMPROC)(void *hwnd, intptr_t l);
int EnumWindows(WNDENUMPROC func, intptr_t l);
]]

-- Implicit conversion to a callback via function pointer argument.
local count = 0
ffi.C.EnumWindows(function(hwnd, l)
  count = count + 1
  return true
end, 0)
-- The callback is permanent and its resources cannot be reclaimed!
-- Ok, so this may not be a problem, if you do this only once.
```

Note: this example shows that you must properly declare `__stdcall` callbacks
on Windows/x86 systems. The calling convention cannot be automatically
detected, unlike for `__stdcall` calls to Windows functions.

For some use cases it's necessary to free up the resources or to dynamically
redirect callbacks. Use an explicit cast to a C function pointer and keep the
resulting cdata object. Then use the `cb:free()` or `cb:set()` methods on the
cdata object:

```
-- Explicitly convert to a callback via cast.
local count = 0
local cb = ffi.cast("WNDENUMPROC", function(hwnd, l)
  count = count + 1
  return true
end)

-- Pass it to a C function.
ffi.C.EnumWindows(cb, 0)
-- EnumWindows doesn't need the callback after it returns, so free it.

cb:free()
-- The callback function pointer is no longer valid and its resources
-- will be reclaimed. The created Lua closure will be garbage collected.
```

### Callback performance

**Callbacks are slow!** First, the C to Lua transition itself has an
unavoidable cost, similar to a `lua_call()` or `lua_pcall()`. Argument and
result marshalling add to that cost. And finally, neither the C compiler nor
moonjit can inline or optimize across the language barrier and hoist repeated
computations out of a callback function.

Do not use callbacks for performance-sensitive work: e.g. consider a numerical
integration routine which takes a user-defined function to integrate over. It's
a bad idea to call a user-defined Lua function from C code millions of times.
The callback overhead will be absolutely detrimental for performance.

It's considerably faster to write the numerical integration routine itself in
Lua — the JIT compiler will be able to inline the user-defined function and
optimize it together with its calling context, with very competitive
performance.

As a general guideline: use callbacks only when you must, because of existing C
APIs. E.g. callback performance is irrelevant for a GUI application, which
waits for user input most of the time, anyway.

For new designs avoid push-style APIs: a C function repeatedly calling a
callback for each result. Instead use pull-style APIs: call a C function
repeatedly to get a new result. Calls from Lua to C via the FFI are much faster
than the other way round. Most well-designed libraries already use pull-style
APIs (read/write, get/put).

## C Library Namespaces

A C library namespace is a special kind of object which allows access to the
symbols contained in shared libraries or the default symbol namespace. The
default `ffi.C` namespace is automatically created when the FFI library is
loaded. C library namespaces for specific shared libraries may be created with
the `ffi.load()` API function.

Indexing a C library namespace object with a symbol name (a Lua string)
automatically binds it to the library. First the symbol type is resolved — it
must have been declared with ffi.cdef. Then the symbol address is resolved by
searching for the symbol name in the associated shared libraries or the default
symbol namespace. Finally, the resulting binding between the symbol name, the
symbol type and its address is cached. Missing symbol declarations or
nonexistent symbol names cause an error.

This is what happens on a read access for the different kinds of symbols:

* External functions: a cdata object with the type of the function and its
  address is returned.
* External variables: the symbol address is dereferenced and the loaded value
  is converted to a Lua object and returned.
* Constant values (static const or enum constants): the constant is converted
  to a Lua object and returned.

This is what happens on a write access:

* External variables: the value to be written is converted to the C type of the
  variable and then stored at the symbol address.
* Writing to constant variables or to any other symbol type causes an error,
  like any other attempted write to a constant location.

C library namespaces themselves are garbage collected objects. If the last
reference to the namespace object is gone, the garbage collector will
eventually release the shared library reference and remove all memory
associated with the namespace. Since this may trigger the removal of the shared
library from the memory of the running process, it's generally not safe to use
function cdata objects obtained from a library if the namespace object may be
unreferenced.

**Performance notice:** the JIT compiler specializes to the identity of namespace
objects and to the strings used to index it. This effectively turns function
cdata objects into constants. It's not useful and actually counter-productive
to explicitly cache these function objects, e.g. `local strlen = ffi.C.strlen`.
OTOH it is useful to cache the namespace itself, e.g. `local C = ffi.C`.

## No Hand-holding!

The FFI library has been designed as a low-level library. The goal is to
interface with C code and C data types with a minimum of overhead. This means
you can do anything you can do from C: access all memory, overwrite anything in
memory, call machine code at any memory address and so on.

The FFI library provides no memory safety, unlike regular Lua code. It will
happily allow you to dereference a NULL pointer, to access arrays out of bounds
or to misdeclare C functions. If you make a mistake, your application might
crash, just like equivalent C code would.

This behavior is inevitable, since the goal is to provide full interoperability
with C code. Adding extra safety measures, like bounds checks, would be futile.
There's no way to detect misdeclarations of C functions, since shared libraries
only provide symbol names, but no type information. Likewise there's no way to
infer the valid range of indexes for a returned pointer.

Again: the FFI library is a low-level library. This implies it needs to be used
with care, but it's flexibility and performance often outweigh this concern. If
you're a C or C++ developer, it'll be easy to apply your existing knowledge.
OTOH writing code for the FFI library is not for the faint of heart and
probably shouldn't be the first exercise for someone with little experience in
Lua, C or C++.

As a corollary of the above, the FFI library is not safe for use by untrusted
Lua code. If you're sandboxing untrusted Lua code, you definitely don't want to
give this code access to the FFI library or to any cdata object (except 64 bit
integers or complex numbers). Any properly engineered Lua sandbox needs to
provide safety wrappers for many of the standard Lua library functions —
similar wrappers need to be written for high-level operations on FFI data
types, too.

## Current Status

The initial release of the FFI library has some limitations and is missing some
features. Most of these will be fixed in future releases.

C language support is currently incomplete:

* C declarations are not passed through a C pre-processor, yet.
* The C parser is able to evaluate most constant expressions commonly found in
  C header files. However it doesn't handle the full range of C expression
  semantics and may fail for some obscure constructs.
* static const declarations only work for integer types up to 32 bits. Neither
  declaring string constants nor floating-point constants is supported.
* Packed struct bitfields that cross container boundaries are not implemented.
* Native vector types may be defined with the GCC mode or `vector_size`
  attribute. But no operations other than loading, storing and initializing
  them are supported, yet.
* The volatile type qualifier is currently ignored by compiled code.
* `ffi.cdef` silently ignores most re-declarations. Note: avoid re-declarations
  which do not conform to C99. The implementation will eventually be changed to
  perform strict checks.

The JIT compiler already handles a large subset of all FFI operations. It
automatically falls back to the interpreter for unimplemented operations (you
can check for this with the -jv command line option). The following operations
are currently not compiled and may exhibit suboptimal performance, especially
when used in inner loops:

* Bitfield accesses and initializations.
* Vector operations.
* Table initializers.
* Initialization of nested struct/union types.
* Allocations of variable-length arrays or structs.
* Allocations of C types with a size > 128 bytes or an alignment > 8 bytes.
* Conversions from lightuserdata to `void *`.
* Pointer differences for element sizes that are not a power of two.
* Calls to C functions with aggregates passed or returned by value.
* Calls to ctype metamethods which are not plain functions.
* ctype `__newindex` tables and non-string lookups in ctype `__index` tables.
* `tostring()` for cdata types.
* Calls to `ffi.cdef()`, `ffi.load()` and `ffi.metatype()`.

Other missing features:

* Bit operations for 64 bit types.
* Arithmetic for complex numbers.
* Passing structs by value to vararg C functions.
* [C++ exception interoperability](exceptions.md#exceptions) does not extend to
  C functions called via the FFI, if the call is compiled.
