LuaJIT 2.1 - exploitation challenge
-----------------------------------

LuaJIT is a Just-In-Time (JIT) compiler for the Lua programming language.

- [LuaJIT Project Homepage](https://luajit.org/)

## A few resources / hints for the challenge:

- See our commits to understand what has been modified
- [Programming in Lua](https://www.lua.org/pil/contents.html)
    > You need no more than basic language constructs. No fancy language feature. (The authors of this challenge had previously never written a single line of Lua)
- [Just-in-time compilation (Wikipedia)](https://en.wikipedia.org/wiki/Just-in-time_compilation)
    > Worth reading if you have never heard of Just-in-time compilation before.
- [Introducing LuaJIT ](https://staff.fnwi.uva.nl/h.vandermeer/docs/lua/luajit/luajit_intro.html)
    > Explains concepts but it is slightly outdated (eg. `jit.compile()` does not exist anymore)

- [LuaJIT web inspector](https://luajit.me/)
    > Compile and inspect LuaJIT bytecode / generated Assembly
- [Online Assembler and Disassembler](https://disasm.pro/)
    > x86_64 assembler/disassembler
- You are in a sandbox
    - flag is in memory
    - seccomp syscall filter prevents you from just running `os.execute("/bin/get_flag")` 
    - anyway, we deleted almost every global variable

- Heavy workload is required to trigger JIT compilation.

This code will not be JIT-compiled:
```lua
f = function()
  for i=0, 10, 1 do
    end
end
f()
```
But this code will (notice loop boundaries):
```lua
f = function()
  for i=0, 100, 1 do
    end
end
f()
```