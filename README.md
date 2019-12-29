# moonjit

[Moonjit](https://github.com/moonjit/moonjit) is a Just-In-Time Compiler (JIT)
for the [Lua](http://www.lua.org/) programming language.  Lua is a powerful,
dynamic and light-weight programming language. It may be embedded or used as a
general-purpose, stand-alone language.

Moonjit is a fork of the inactive [LuaJIT](https://luajit.org) project and aims
to provide a way forward for existing users of LuaJIT looking for continuity in
development and maintenance.

## Documentation

Here is an index of the documentation for moonjit.  Old documentation for
LuaJIT is unmaintained in [doc.luajit](doc.luajit) and is only there as a
historical record.

* [Installing moonjit](INSTALL.md)
* [Running moonjit](doc/moonjit.md)
* [Moonjit/LuaJIT API and Extensions Reference](doc/extensions.md)
* [Moonjit FFI](doc/ffi/ffi.md)
  * [FFI Tutorial](doc/ffi/ffi-tutorial)
  * [FFI API](doc/ffi/ffi-api.md)
  * [FFI Semantics](doc/ffi/ffi-semantics.md)

## Current Status

Moonjit ought to run all Lua 5.1-compatible source code just fine. It is
considered a serious bug if the VM crashes or produces unexpected results —
please report this.  Lua 5.2 support is in two parts; the backward compatible
elements are built in by default whereas features that break compatibility have
to be enabled using the `-DLUAJIT_ENABLE_LUA52COMPAT` flag.  The [installation
instructions](INSTALL.md) have more details.

## Contributing

Please note the branches

* Fork the moonjit repo on github
* Clone your fork and make changes in a separate topic branch
  ``` shell
  git clone git@github.com:<your-username-here>/moonjit.git
  git checkout my-changes
  ```
* Make your changes and test them:
  ```
  make
  make check
  ```
* Commit your changes using the `-s` flag to sign off your commit and then push
  it to your branch.
  ```
  git commit -a -s
  git push origin my-changes
  ```
* Write a detailed commit log message describing the issue you're fixing so that the information is retained for future reference. If you're fixing an issue, please mention the issue number as fixed using the annothation `Fixes: #NUM` in the body of the commit log.
* Create a Pull Request on GitHub.

### Testing

moonjit has a built-in testsuite that can be exercised by running the following command:

```
make check
```

There is also a benchmark suite that can be run as follows:

```
make bench
```

If you're fixing a bug in moonjit, make sure you add a test case to exercise the bug so that it does not regress.  [test/README.md](test/README) has more details on how to execute and update the testsuite.

### Branches

The project repository on github has the following main branches:

* **master**: This is the active development branch for moonjit and it is where
  all new features and targets will be incorporated.
* **v2.1**: This is a bug fix branch for v2.1.x releases of moonjit.  This
  branch aims to remain compatible with the LuaJIT v2.1 branch.
* **v2.0**: This remains a bug fix branch that tracks the v2.0 branch of
  upstream LuaJIT.  This branch currently does not have a maintainer.

### Release Process

The following steps need to be performed before tagging a moonjit release:

* Update `RELVER` to the new version number and set `PREREL` to an empty string
  in [Makefile](Makefile)
* Update [etc/luajit.pc](etc/luajit.pc) with the new version
* Update `LUA_LJDIR` in [src/luaconf.h](src/luaconf.h)
* Update `LUAJIT_VERSION`, `LUAJIT_VERSION_NUM` and `LUAJIT_VERSION_SYM` in
  [src/luajit.h](src/luajit.h)
* Update version check assertions in scripts in [src/jit](src/jit). They are of
  the form:
  `assert(jit.version_num == xxxxxx, "moonjit core/library version mismatch")`
  where `xxxxxx` is the value of `LUAJIT_VERSION_NUM`.
* Update the source package name in [INSTALL.md](INSTALL.md).
* File a PR with these changes.
* Draft release notes based on changes between `HEAD` and the last tag in a PR
  comment and update it based on feedback.
* On PR approval, Tag release and put in release notes on GitHub.
* Update `PREREL` in [Makefile(Makefile) to `-dev` to indicate that development
  is now open.
* Announce.
* Hack on next release.
