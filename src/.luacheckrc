codes = true
std = 'luajit'

files['jit/*.lua'].ignore = { '542' }  -- empty if branch
files['jit/bcsave.lua'].ignore = { '231/is64' }  -- variable is64 is never accessed
files['jit/dis_x86.lua'].ignore = { '212/name', '212/pat' }  -- unused argument
files['jit/dump.lua'].ignore = { '211', '232/callee' }
