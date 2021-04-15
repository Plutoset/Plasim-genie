#
# $Id: makefile 3320 2007-06-14 14:22:56Z andrew-price $
#

export

LIBNAME = libichem
LIB_DIR  = lib
MOD_DIR = mod
SRC_DIR  = src/fortran

.PHONY: all $(LIBNAME).$(LIB_EXT) clean tags

all: $(LIBNAME).$(LIB_EXT)

$(LIBNAME).$(LIB_EXT):
	\cd $(SRC_DIR) ; $(MAKE) all
	\cd $(LIB_DIR) ; $(MAKE) all
	if [ ! -d $(MOD_DIR) ] ; then mkdir $(MOD_DIR) ; fi
	\cp -f $(SRC_DIR)/*.$(MODEXT) $(MOD_DIR)

clean:
	\cd $(SRC_DIR) ; $(MAKE) clean
	\cd $(LIB_DIR) ; $(MAKE) clean
	\rm -f $(MOD_DIR)/*.$(MODEXT)

tags:
	\cd $(SRC_DIR); $(MAKE) tags
