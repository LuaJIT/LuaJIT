------------------------------------------------------------------------------
-- DynASM RISC-V 64 module.
--
-- Copyright (C) 2022-2026 ISRC, ISCAS. All rights reserved.
-- See dynasm.lua for full copyright notice.
------------------------------------------------------------------------------
-- This module just sets 64 bit mode for the combined RISC-V module.
-- All the interesting stuff is there.
------------------------------------------------------------------------------

riscv64 = true -- Using a global is an ugly, but effective solution.
return require("dasm_riscv")
