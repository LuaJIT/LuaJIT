##############################################################################
# LuaJIT top level Makefile for installation. Requires GNU Make.
#
# Please read doc/install.html before changing any variables!
#
# Suitable for POSIX platforms (Linux, *BSD, OSX etc.).
# Note: src/Makefile has many more configurable options.
#
# ##### This Makefile is NOT useful for Windows! #####
# For MSVC, please follow the instructions given in src/msvcbuild.bat.
# For MinGW and Cygwin, cd to src and run make with the Makefile there.
#
# Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
##############################################################################

MAJVER=  2
MINVER=  0
RELVER=  0
PREREL=  -beta9
VERSION= $(MAJVER).$(MINVER).$(RELVER)$(PREREL)
ABIVER=  5.1
NODOTABIVER=  51

##############################################################################
#
# Change the installation path as needed. This automatically adjusts
# the paths in src/luaconf.h, too. Note: PREFIX must be an absolute path!
#
export PREFIX= /usr/local
##############################################################################

DPREFIX= $(DESTDIR)$(PREFIX)
INSTALL_BIN=   $(DPREFIX)/bin
INSTALL_LIB=   $(DPREFIX)/lib
INSTALL_SHARE= $(DPREFIX)/share
INSTALL_INC=   $(DPREFIX)/include/luajit-$(MAJVER).$(MINVER)

INSTALL_JITLIB= $(INSTALL_SHARE)/luajit-$(VERSION)/jit
INSTALL_LMOD= $(INSTALL_SHARE)/lua/$(ABIVER)
INSTALL_CMOD= $(INSTALL_LIB)/lua/$(ABIVER)
INSTALL_MAN= $(INSTALL_SHARE)/man/man1
INSTALL_PKGCONFIG= $(INSTALL_LIB)/pkgconfig

INSTALL_TNAME= luajit-$(VERSION)
INSTALL_TSYMNAME= luajit
INSTALL_ANAME= libluajit-$(ABIVER).a
INSTALL_SONAME= libluajit-$(ABIVER).so.$(MAJVER).$(MINVER).$(RELVER)
INSTALL_SOSHORT= libluajit-$(ABIVER).so
INSTALL_DYLIBNAME= libluajit-$(NODOTABIVER).$(MAJVER).$(MINVER).$(RELVER).dylib
INSTALL_DYLIBSHORT1= libluajit-$(NODOTABIVER).dylib
INSTALL_DYLIBSHORT2= libluajit-$(NODOTABIVER).$(MAJVER).dylib
INSTALL_PCNAME= luajit.pc

INSTALL_STATIC= $(INSTALL_LIB)/$(INSTALL_ANAME)
INSTALL_DYN= $(INSTALL_LIB)/$(INSTALL_SONAME)
INSTALL_SHORT1= $(INSTALL_LIB)/$(INSTALL_SOSHORT)
INSTALL_SHORT2= $(INSTALL_LIB)/$(INSTALL_SOSHORT)
INSTALL_T= $(INSTALL_BIN)/$(INSTALL_TNAME)
INSTALL_TSYM= $(INSTALL_BIN)/$(INSTALL_TSYMNAME)
INSTALL_PC= $(INSTALL_PKGCONFIG)/$(INSTALL_PCNAME)

INSTALL_DIRS= $(INSTALL_BIN) $(INSTALL_LIB) $(INSTALL_INC) $(INSTALL_MAN) \
  $(INSTALL_PKGCONFIG) $(INSTALL_JITLIB) $(INSTALL_LMOD) $(INSTALL_CMOD)

RM= rm -f
MKDIR= mkdir -p
SYMLINK= ln -sf
INSTALL_X= install -m 0755
INSTALL_F= install -m 0644
LDCONFIG= ldconfig -n
SED_PC= sed -e "s|^prefix=.*|prefix=$(PREFIX)|"

FILE_T= luajit
FILE_A= libluajit.a
FILE_SO= libluajit.so
FILE_MAN= luajit.1
FILE_PC= luajit.pc
FILES_INC= lua.h lualib.h lauxlib.h luaconf.h lua.hpp luajit.h
FILES_JITLIB= bc.lua v.lua dump.lua dis_x86.lua dis_x64.lua dis_arm.lua \
	      dis_ppc.lua bcsave.lua vmdef.lua

ifeq (,$(findstring Windows,$(OS)))
  ifeq (Darwin,$(shell uname -s))
    INSTALL_SONAME= $(INSTALL_DYLIBNAME)
    INSTALL_SHORT1= $(INSTALL_LIB)/$(INSTALL_DYLIBSHORT1)
    INSTALL_SHORT2= $(INSTALL_LIB)/$(INSTALL_DYLIBSHORT2)
    LDCONFIG= :
  endif
endif

##############################################################################

INSTALL_DEP= src/luajit

default all $(INSTALL_DEP):
	@echo "==== Building LuaJIT $(VERSION) ===="
	$(MAKE) -C src
	@echo "==== Successfully built LuaJIT $(VERSION) ===="

install: $(INSTALL_DEP)
	@echo "==== Installing LuaJIT $(VERSION) to $(PREFIX) ===="
	$(MKDIR) $(INSTALL_DIRS)
	cd src && $(INSTALL_X) $(FILE_T) $(INSTALL_T)
	cd src && test -f $(FILE_A) && $(INSTALL_F) $(FILE_A) $(INSTALL_STATIC) || :
	$(RM) $(INSTALL_DYN) $(INSTALL_SHORT1) $(INSTALL_SHORT2)
	cd src && test -f $(FILE_SO) && \
	  $(INSTALL_X) $(FILE_SO) $(INSTALL_DYN) && \
	  $(LDCONFIG) $(INSTALL_LIB) && \
	  $(SYMLINK) $(INSTALL_SONAME) $(INSTALL_SHORT1) && \
	  $(SYMLINK) $(INSTALL_SONAME) $(INSTALL_SHORT2) || :
	cd etc && $(INSTALL_F) $(FILE_MAN) $(INSTALL_MAN)
	cd etc && $(SED_PC) $(FILE_PC) > $(FILE_PC).tmp && \
	  $(INSTALL_F) $(FILE_PC).tmp $(INSTALL_PC) && \
	  $(RM) $(FILE_PC).tmp
	cd src && $(INSTALL_F) $(FILES_INC) $(INSTALL_INC)
	cd lib && $(INSTALL_F) $(FILES_JITLIB) $(INSTALL_JITLIB)
	@echo "==== Successfully installed LuaJIT $(VERSION) to $(PREFIX) ===="
	@echo ""
	@echo "Note: the beta releases deliberately do NOT install a symlink for luajit"
	@echo "You can do this now by running this command (with sudo):"
	@echo ""
	@echo "  $(SYMLINK) $(INSTALL_TNAME) $(INSTALL_TSYM)"
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

.PHONY: all install amalg clean cleaner distclean

##############################################################################
