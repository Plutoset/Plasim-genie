#
# $Id$
#

export

LIBNAME = libwind
LIB_DIR  = lib
MOD_DIR = mod
SRC_DIR  = src/fortran
DATA_DIR = data

.PHONY: all $(LIBNAME).$(LIB_EXT) clean tags data

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

data:
	\cd $(DATA_DIR)/download/; $(MAKE)
	\cd $(DATA_DIR)/input/; $(MAKE)
