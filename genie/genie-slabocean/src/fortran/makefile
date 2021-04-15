#
#
#

# Note these vars are exported from genie-main/makefile.arc 
ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(SLABOCEANPREC) $(BOUNDS_FLAGS) $(LOC_INC_FLAGS)

OBJECTS = slabocean.$(OBJ_EXT) initialise_slabocean.$(OBJ_EXT)
INCLUDE_FILES=netcdf.inc slabocean_com.cmn precision.inc
OTHER_FILES=../../../genie-main/makefile.arc makefile

all : $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F'

slabocean.$(OBJ_EXT) : slabocean.F netcdf.inc slabocean_com.cmn \
precision.inc $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(GENIEPRECISIONOPTS) \
	$(LIBUTIL1PRECISIONOPTS) slabocean.F

initialise_slabocean.$(OBJ_EXT) : initialise_slabocean.F precision.inc $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(GENIEPRECISIONOPTS) \
	initialise_slabocean.F
