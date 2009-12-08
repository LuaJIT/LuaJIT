##############################################################################
# LuaJIT top level Makefile for installation. Requires GNU Make.
#
# Suitable for POSIX platforms (Linux, *BSD, OSX etc.).
# Note: src/Makefile has many more configurable options.
#
# ##### This Makefile is NOT useful for installation on Windows! #####
# For MSVC, please follow the instructions given in src/msvcbuild.bat.
# For MinGW and Cygwin, cd to src and run make with the Makefile there.
# NYI: add wininstall.bat
#
# Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
##############################################################################

BASEVER= 2.0.0
VERSION= 2.0.0-beta1

##############################################################################
#
# Change the installation path as needed and modify src/luaconf.h accordingly.
# Note: PREFIX must be an absolute path!
#
PREFIX= /usr/local
##############################################################################

INSTALL_BIN= $(PREFIX)/bin
INSTALL_NAME= luajit-$(VERSION)
INSTALL_T= $(INSTALL_BIN)/$(INSTALL_NAME)
INSTALL_TSYM= $(INSTALL_BIN)/luajit
INSTALL_INC= $(PREFIX)/include/luajit-$(BASEVER)
INSTALL_JITLIB= $(PREFIX)/share/luajit-$(VERSION)/jit

MKDIR= mkdir -p
SYMLINK= ln -f -s
INSTALL_X= install -m 0755
INSTALL_F= install -m 0644

FILES_T= luajit
FILES_INC= lua.h lualib.h lauxlib.h luaconf.h lua.hpp luajit.h
FILES_JITLIB= bc.lua v.lua dump.lua dis_x86.lua dis_x64.lua vmdef.lua

##############################################################################

INSTALL_DEP= src/luajit

all $(INSTALL_DEP):
	@echo "==== Building LuaJIT $(VERSION) ===="
	$(MAKE) -C src
	@echo "==== Successfully built LuaJIT $(VERSION) ===="

install: $(INSTALL_DEP)
	@echo "==== Installing LuaJIT $(VERSION) to $(PREFIX) ===="
	$(MKDIR) $(INSTALL_BIN) $(INSTALL_INC) $(INSTALL_JITLIB)
	cd src && $(INSTALL_X) $(FILES_T) $(INSTALL_T)
	cd src && $(INSTALL_F) $(FILES_INC) $(INSTALL_INC)
	cd lib && $(INSTALL_F) $(FILES_JITLIB) $(INSTALL_JITLIB)
	@echo "==== Successfully installed LuaJIT $(VERSION) to $(PREFIX) ===="
	@echo ""
	@echo "Note: the beta releases deliberately do NOT install a symlink for luajit"
	@echo "You can do this now by running this command (with sudo):"
	@echo ""
	@echo "  $(SYMLINK) $(INSTALL_NAME) $(INSTALL_TSYM)"
	@echo ""

##############################################################################

amalg:
	@echo "Building LuaJIT $(VERSION)"
	$(MAKE) -C src amalg

clean:
	$(MAKE) -C src clean

cleaner:
	$(MAKE) -C src cleaner

distclean:
	$(MAKE) -C src distclean

SUB_TARGETS= amalg clean cleaner distclean

.PHONY: all install $(SUB_TARGETS)

##############################################################################
