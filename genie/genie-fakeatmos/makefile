#
# $Id: makefile 3320 2007-06-14 14:22:56Z andrew-price $
#

export

LIBNAME = libfakeatmos
LIBDIR  = lib
SRCDIR  = src/fortran
MODDIR  = mod

.PHONY: all $(LIBNAME).$(LIB_EXT) clean tags

$(LIBNAME).$(LIB_EXT):
	\cd $(SRCDIR) ; $(MAKE) all
	\cd $(LIBDIR) ; $(MAKE) all
	if [ ! -d $(MODDIR) ] ; then mkdir $(MODDIR) ; fi
	\cp -f $(SRCDIR)/*.$(MODEXT) $(MODDIR)/

clean:
	\cd $(SRCDIR) ; $(MAKE) clean
	\cd $(LIBDIR) ; $(MAKE) clean
	\rm -f $(MODDIR)/*.$(MODEXT)

tags:
	\cd $(SRCDIR) ; $(MAKE) tags
