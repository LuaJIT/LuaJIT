This directory contains the LuaJIT test suite, or at least something which
will evolve into the LuaJIT test suite. Large chunks of the suite can also
be run with any other Lua 5.1 or 5.2 interpreter.

## Running the test suite ##

To run the default test suite, run `test.lua` using the Lua interpreter you
wish to test, for example:

    $ ~/luajit-2.0/src/luajit test.lua

If the test suite passes, the final line printed to stdout will be
`NNN passed`, and the exit code of the process will be zero. If any tests
fail, the exit code will be non-zero. If the failures caused catastrophic
termination of the entire process (such as a segmentation fault or assertion
failure), the last line of output will be number and name of the test which
caused the catastrophe. If the failures weren't catastrophic, the penultimate
line of output will be `NNN passed, MMM failed`, and the last line will say
how to re-run just the failing tests.

Various flags and options can be passed to `test.lua` to control which tests
are run, and in which order. Run `lua test.lua --help` for details.

## Structure of the test suite ##

The test suite consists of a directory tree. Within said tree there are various
`.lua` files, and within every `.lua` file there are one or more tests. Every
directory in the tree contains a file called `index`, which enumerates the
members of the directory which contribute to the test suite (this is done to
avoid an external dependency for directory iteration, and to allow metadata to
be specified at the file/directory level). Every `.lua` file is structured as:

    << local definitions >>
    << test 1 >>
    ...
    << test N >>

Where `<< local definitions >>` consists of Lua code to act as a common prefix
for every test in the file, and each `<< test >>` looks like:

    do --- <<name>> <<metadata>>
      << code >>
    end

Where `<<name>>` is (almost) free-form, and `<< code >>` is Lua code which
performs some actions and probably calls `assert` alot. The `<<metadata>>`
fragment can be used to specify the conditions under which the test should
or should not run, to adjust the environment in which the test is run, and to
allow key/value pairs to be specified in a standard place/format.

Some common pieces of metadata are:
  * `+luajit>=2.1` - The test requires LuaJIT 2.1 or later to run.
  * `+lua<5.2` - The test requires Lua 5.1 or earlier to run (all versions of
                 LuaJIT report themselves as 5.1).
  * `+ffi` - The test requires the `ffi` library to be present.
  * `+bit` - The test requires the `bit` library to be present.
  * `+jit` - The test requires JIT compilation be available and turned on.
  * `+slow` - The test is too slow to run as part of the default suite, and
              thus requires `+slow` to be specified on the command line.
  * `!private_G` - The test modifies globals, and thus needs to be run with a
                   private (shallow) copy of `_G`.

Lua code which is common across all (or some) tests in a single file can be
written at the top of the file as part of `<< local definitions >>`. Code
which is common across multiple files lives in the `common` directory, and
is pulled into applicable tests by means of `local x = require"common.x"`.

It is intended that most `.lua` files in the test suite can be exercised
without the test runner by just passing them to a Lua interpreter. In such
cases, metadata is ignored, the tests are executed from top to bottom, and
any failure results in later tests not running. Also note that the test
runner converts every test into a separate function, which causes references
to local definitions to become upvalue accesses rather than local variable
accesses - in some cases this can cause differences in behaviour.

## Extending the test suite ##

First of all, decide where your new test(s) should live. This might be within
an existing `.lua` file, or might involve creating new files and/or directories.
If new files are created, remember to add them to the `index` file of the
enclosing directory. If new directories are created, remember to create an
`index` file in said directory, and add the new directory to the `index` file
in the parent directory.

Once you've decided in which file the test(s) should live, you're ready to add
your test(s) to said file. Each test should be wrapped in a `do`/`end` block,
and given some kind of name (after the `do` keyword, as in `do --- <<name>>`).
The test should call `assert` to confirm whether the thing under test is
behaving, or otherwise raise an error if the thing under test is misbehaving.
Your test(s) should not write to stdout or stderr, nor should they mutate
global state. After your test(s) are written, you should be able to determine
which features they require, and put on metadata appropriately.

## Completing the tidy-up of the test suite ##

Some files/directories in this directory need some thought:

  * `common/ffi_util.inc` - Needs renaming and being made `require`-able.
  * `lib/ffi` - Tests need converting to structure described in this document.
  * `lib/table/misc.lua` - Tests need organising and converting to structure
                           described in this document.
  * `misc` - Tests need organising and converting to structure described in
             this document.
  * `src` - C/C++ source which needs to be compiled into a dynamic library and
            loaded for certain tests. Need to figure out a good way of handling
            C/C++ source.
  * `sysdep` - Need to figure out a good way of handling these.
  * `unportable` - Need to figure out a good way of handling these.

After that, consult the README file by Mike in the directory above this one.
