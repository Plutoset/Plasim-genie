#
# $Id: makefile 3274 2007-04-30 14:51:29Z cvs-gw $
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(FIXEDOCEANPREC) $(BOUNDS_FLAGS)

LIBNAME = libfixedocean
OBJECTS = fixedocean.$(OBJ_EXT) initialise_fixedocean.$(OBJ_EXT)

INCLUDE_FILES=netcdf.inc fixedocean_com.cmn  precision.inc

OTHER_FILES=../../../genie-main/makefile.arc makefile

all : $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F'

fixedocean.$(OBJ_EXT) : fixedocean.F  netcdf.inc fixedocean_com.cmn \
precision.inc $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(GENIEPRECISIONOPTS) \
	$(LIBUTIL1PRECISIONOPTS) fixedocean.F

initialise_fixedocean.$(OBJ_EXT) : initialise_fixedocean.F precision.inc \
$(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(GENIEPRECISIONOPTS) \
	initialise_fixedocean.F
