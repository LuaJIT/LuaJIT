codes = true
std = 'luajit'

files['host/genlibbc.lua'].ignore = { '213/i' } -- unused loop variable i
files['jit/*.lua'].ignore = { '542' }  -- empty if branch
files['jit/bcsave.lua'].ignore = { '231/is64' }  -- variable is64 is never accessed
files['jit/dis_ppc.lua'].ignore = { '212/t' }  -- unused argument t
files['jit/dis_x86.lua'].ignore = { '212/name', '212/pat' }  -- unused argument
files['jit/dump.lua'].ignore = { '211', '212/tr', '232/callee' }

