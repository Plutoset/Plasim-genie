#
# $Id: makefile 3707 2008-01-14 14:28:27Z gethin-williams $
#

export

LIBNAME = libigcm
LIBDIR  = lib
SRCDIR  = src/fortran

.PHONY: all $(LIBNAME).$(LIB_EXT) clean tags

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT):
	\cd $(SRCDIR) ; $(MAKE) all
	\cd $(LIBDIR) ; $(MAKE) all

clean:
	\cd $(SRCDIR) ; $(MAKE) clean
	\cd $(LIBDIR) ; $(MAKE) clean

tags:
	\cd $(SRCDIR); $(MAKE) tags
