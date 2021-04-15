#
# $Id: makefile 3274 2007-04-30 14:51:29Z cvs-gw $
#
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(FIXEDSEAICEPREC) $(BOUNDS_FLAGS)

OBJECTS = fixedseaice.$(OBJ_EXT) initialise_fixedseaice.$(OBJ_EXT)

INCLUDE_FILES=netcdf.inc fixedseaice_com.cmn precision.inc

OTHER_FILES=../../../genie-main/makefile.arc makefile

all : $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F'

fixedseaice.$(OBJ_EXT) : fixedseaice.F netcdf.inc fixedseaice_com.cmn \
precision.inc $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(GENIEPRECISIONOPTS) \
	$(LIBUTIL1PRECISIONOPTS) fixedseaice.F

initialise_fixedseaice.$(OBJ_EXT) : initialise_fixedseaice.F precision.inc \
$(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(GENIEPRECISIONOPTS) \
	initialise_fixedseaice.F
