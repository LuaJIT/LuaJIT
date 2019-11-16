# moonjit

[Moonjit](https://github.com/moonjit/moonjit) is a Just-In-Time Compiler (JIT)
for the [Lua](http://www.lua.org/) programming language.  Lua is a powerful,
dynamic and light-weight programming language. It may be embedded or used as a
general-purpose, stand-alone language.

Moonjit is a fork of the inactive [LuaJIT](https://luajit.org) project and aims
to provide a way forward for existing users of LuaJIT looking for continuity in
development and maintenance.

## Documentation

Documentation for moonjit is in [doc](doc), the index is below.  However, a lot
of documentation is still unported and is in [doc.luajit](doc.luajit).  The
technical content of this old documentation is still accurate and has been
updated to reflect changes made in moonjit.

* [Installing moonjit](INSTALL.md)
* [Running moonjit](doc/moonjit.md)
* [Moonjit/LuaJIT API and Extensions Reference](doc/extensions.md)

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
* Create a Pull Request on GitHub.

### Branches

The project repository on github has the following main branches:

* **master**: This is the active development branch for moonjit and it is where
  all new features and targets will be incorporated.
* **v2.1**: This is a bug fix branch for v2.1.x releases of moonjit.  This
  branch aims to remain compatible with the LuaJIT v2.1 branch.
* **v2.0**: This remains a bug fix branch that tracks the v2.0 branch of
  upstream LuaJIT.  This branch currently does not have a maintainer.
