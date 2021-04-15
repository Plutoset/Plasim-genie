#
# $Id: makefile 6303 2011-03-30 14:01:52Z gethin-williams $
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(LIBAUX1PREC)

LIBNAME = libaux1
OBJECTS = ismax.$(OBJ_EXT) ismin.$(OBJ_EXT) lensig.$(OBJ_EXT) \
		second.$(OBJ_EXT) ssum.$(OBJ_EXT)

OTHER_FILES=makefile ../../genie-main/makefile.arc

$(LIBNAME).$(LIB_EXT): $(OBJECTS) $(OTHER_FILES)
	$(LIB_CMD)$(LIBNAME).$(LIB_EXT) $(OBJECTS)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(LIB_EXT) *__genmod.f90 *__genmod.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f'

ismax.$(OBJ_EXT)		: ismax.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) ismax.f

ismin.$(OBJ_EXT)		: ismin.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) ismin.f

lensig.$(OBJ_EXT)		: lensig.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) lensig.f

second.$(OBJ_EXT)		: second.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) second.f

ssum.$(OBJ_EXT)		: ssum.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) ssum.f

