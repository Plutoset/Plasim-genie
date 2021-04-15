#
# $Id: Makefile 3274 2007-04-30 14:51:29Z cvs-gw $
#

SRCDIR = ../src/fortran

.PHONY : all clean

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT): $(SRCDIR)/*.$(OBJ_EXT)
	$(LIB_CMD)$@ $^

clean:
	rm -f $(LIBNAME).$(LIB_EXT)
