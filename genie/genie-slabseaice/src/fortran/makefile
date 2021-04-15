#
# NB makefile VARS exported from genie-main/makefile.arc
#

ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(SLABSEAICEPREC) $(BOUNDS_FLAGS) $(LOC_INC_FLAGS)
OBJECTS = slabseaice.$(OBJ_EXT) initialise_slabseaice.$(OBJ_EXT)
INCLUDE_FILES=netcdf.inc slabseaice_com.cmn precision.inc
OTHER_FILES=../../../genie-main/makefile.arc makefile

all : $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F'

slabseaice.$(OBJ_EXT) : slabseaice.F netcdf.inc slabseaice_com.cmn resolution_slabseaice.inc \
precision.inc $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) \
	$(GENIEPRECISIONOPTS) \
	$(LIBUTIL1PRECISIONOPTS) slabseaice.F

initialise_slabseaice.$(OBJ_EXT) : initialise_slabseaice.F resolution_slabseaice.inc \
precision.inc $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) \
	$(GENIEPRECISIONOPTS) \
	initialise_slabseaice.F
