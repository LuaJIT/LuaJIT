# Running Moonjit

Moonjit has only a single stand-alone executable, called luajit on POSIX
systems or luajit.exe on Windows. It can be used to run simple Lua statements
or whole Lua applications from the command line. It has an interactive mode,
too.

## Command Line Options

The luajit stand-alone executable is just a slightly modified version of the
regular lua stand-alone executable. It supports the same basic options, too.
`luajit -h` prints a short list of the available options. Please have a look at
the [Lua manual](http://www.lua.org/manual/5.1/manual.html#6) for details.

Moonjit has some additional options:

<a name="b_opt"></a>
### `-b[options] input output`

This option saves or lists bytecode. The following additional options are
accepted:

* `-l`: Only list bytecode.
* `-s`: Strip debug info (this is the default).
* `-g`: Keep debug info.
* `-n name`: Set module name (default: auto-detect from input name)
* `-t` type: Set output file type (default: auto-detect from output name).
* `-a` arch: Override architecture for object files (default: native).
* `-o` os: Override OS for object files (default: native).
* `-e` chunk: Use chunk string as input.
* `- ` (a single minus sign): Use stdin as input and/or stdout as output.

The output file type is auto-detected from the extension of the output file name:

* `c` — C source file, exported bytecode data.
* `h` — C header file, static bytecode data.
* `obj or o` — Object file, exported bytecode data (OS- and architecture-specific).
* `raw or any other extension` — Raw bytecode file (portable).

#### Notes:

* See also [string.dump()](extensions.md#string_dump) for information on
  bytecode portability and compatibility.
* A file in raw bytecode format is auto-detected and can be loaded like any Lua
  source file. E.g. directly from the command line or with `loadfile()`, `dofile()`
  etc.
* To statically embed the bytecode of a module in your application, generate an
  object file and just link it with your application.
* On most ELF-based systems (e.g. Linux) you need to explicitly export the
  global symbols when linking your application, e.g. with: `-Wl,-E require()`
  tries to load embedded bytecode data from exported symbols (in \*.exe or
  lua51.dll on Windows) and from shared libraries in package.cpath.

Typical usage examples:

```
luajit -b test.lua test.out                 # Save bytecode to test.out
luajit -bg test.lua test.out                # Keep debug info
luajit -be "print('hello world')" test.out  # Save cmdline script

luajit -bl test.lua                         # List to stdout
luajit -bl test.lua test.txt                # List to test.txt
luajit -ble "print('hello world')"          # List cmdline script

luajit -b test.lua test.obj                 # Generate object file
# Link test.obj with your application and load it with require("test")
```

### `-j cmd[=arg[,arg...]]`

This option performs a moonjit control command or activates one of the loadable
extension modules. The command is first looked up in the `jit.*` library. If no
matching function is found, a module named `jit.<cmd>` is loaded and the
`start()` function of the module is called with the specified arguments (if
any). The space between `-j` and `cmd` is optional.

Here are the available moonjit control commands:

* `-jon` — Turns the JIT compiler on (default).
* `-joff` — Turns the JIT compiler off (only use the interpreter).
* `-jflush` — Flushes the whole cache of compiled code.
* `-jv` — Shows verbose information about the progress of the JIT compiler.
* `-jdump` — Dumps the code and structures used in various compiler stages.
* `-jp` — Start the integrated profiler.

The `-jv` and `-jdump` commands are extension modules written in Lua. They are mainly used for debugging the JIT compiler itself. For a description of their options and output format, please read the comment block at the start of their source. They can be found in the `lib` directory of the source distribution or installed under the `jit` directory. By default this is `/usr/local/share/moonjit-<version>/jit` on POSIX systems.

### `-O[level]`
### `-O[+]flag   -O-flag`
### `-Oparam=value`

This options allows fine-tuned control of the optimizations used by the JIT compiler. This is mainly intended for debugging moonjit itself. Please note that the JIT compiler is extremely fast (we are talking about the microsecond to millisecond range). Disabling optimizations doesn't have any visible impact on its overhead, but usually generates code that runs slower.

The first form sets an optimization level — this enables a specific mix of optimization flags. `-O0` turns off all optimizations and higher numbers enable more optimizations. Omitting the level (i.e. just `-O`) sets the default optimization level, which is `-O3` in the current version.

The second form adds or removes individual optimization flags. The third form sets a parameter for the VM or the JIT compiler to a specific value.

You can either use this option multiple times (like `-Ocse -O-dce -Ohotloop=10`) or separate several settings with a comma (like `-O+cse,-dce,hotloop=10`). The settings are applied from left to right and later settings override earlier ones. You can freely mix the three forms, but note that setting an optimization level overrides all earlier flags.

Here are the available flags and at what optimization levels they are enabled:

Flag	| -O1			| -O2			| -O3			| Description
--------|-----------------------|-----------------------|-----------------------|------------
fold	| :white_check_mark:	| :white_check_mark:	| :white_check_mark:	| Constant Folding, Simplifications and Reassociation
cse	| :white_check_mark:	| :white_check_mark:	| :white_check_mark:	| Common-Subexpression Elimination
dce	| :white_check_mark:	| :white_check_mark:	| :white_check_mark:	| Dead-Code Elimination
narrow	|			| :white_check_mark:	| :white_check_mark:	| Narrowing of numbers to integers
loop	|			| :white_check_mark:	| :white_check_mark:	| Loop Optimizations (code hoisting)
fwd	|			|			| :white_check_mark:	| Load Forwarding (L2L) and Store Forwarding (S2L)
dse	|			|			| :white_check_mark:	| Dead-Store Elimination
abc	|			|			| :white_check_mark:	| Array Bounds Check Elimination
sink	|			|			| :white_check_mark:	| Allocation/Store Sinking
fuse	|			|			| :white_check_mark:	| Fusion of operands into instructions

Here are the parameters and their default settings:

Parameter 	| Default 	| Description
----------------|---------------|------------
maxtrace	| 1000		| Max. number of traces in the cache
maxrecord	| 4000		| Max. number of recorded IR instructions
maxirconst	| 500		| Max. number of IR constants of a trace
maxside		| 100		| Max. number of side traces of a root trace
maxsnap		| 500		| Max. number of snapshots for a trace
hotloop		| 56		| Number of iterations to detect a hot loop or hot call
hotexit		| 10		| Number of taken exits to start a side trace
tryside		| 4		| Number of attempts to compile a side trace
instunroll	| 4		| Max. unroll factor for instable loops
loopunroll	| 15		| Max. unroll factor for loop ops in side traces
callunroll	| 3		| Max. unroll factor for pseudo-recursive calls
recunroll	| 2		| Min. unroll factor for true recursion
sizemcode	| 32		| Size of each machine code area in KBytes (Windows: 64K)
maxmcode	| 512		| Max. total size of all machine code areas in KBytes
