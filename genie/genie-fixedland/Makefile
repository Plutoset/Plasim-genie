#
# $Id: Makefile 3320 2007-06-14 14:22:56Z andrew-price $
#

export

LIBNAME = libfixedland
LIB_DIR = lib
MOD_DIR = mod
FOR_DIR = src/fortran

.PHONY : all $(LIBNAME).$(LIB_EXT) clean tags

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT) :
	\cd $(FOR_DIR) ; $(MAKE) all
	\cd $(LIB_DIR) ; $(MAKE) all
	if [ ! -d $(MOD_DIR) ] ; then mkdir $(MOD_DIR) ; fi
	\cp -f $(FOR_DIR)/*.$(MODEXT) $(MOD_DIR)

clean :
	\cd $(FOR_DIR) ; $(MAKE) clean
	\cd $(LIB_DIR) ; $(MAKE) clean
	\rm -f $(MOD_DIR)/*.$(MODEXT)

tags:
	\cd $(FOR_DIR) ; $(MAKE) tags
