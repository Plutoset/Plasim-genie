#
# $Id: makefile 5556 2010-05-25 23:51:27Z simon-mueller $
#
LIBNAME = 	libfixedatmos
OBJECTS = 	fixed_atmosphere.$(OBJ_EXT) fixedatmos_common.$(OBJ_EXT) \
		fixedatmos_nc.$(OBJ_EXT)

INCLUDE_FILES = netcdf.inc precision.inc
OTHER_FILES   = ../../../genie-main/makefile.arc makefile 

ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FFLAGS) $(F90FLAGS) $(LOC_INC_FLAGS) $(FIXEDATMOSOPTS)

all : $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean :
	\rm -f *.$(OBJ_EXT) *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

fixed_atmosphere.$(OBJ_EXT) : fixed_atmosphere.F90 fixedatmos_common.$(OBJ_EXT) \
fixedatmos_nc.$(OBJ_EXT) netcdf.inc precision.inc $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) \
	$(LIBUTIL1PRECISIONOPTS) fixed_atmosphere.F90

fixedatmos_common.$(OBJ_EXT) : fixedatmos_common.f90 $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) \
                $(LIBNC1PRECISIONOPTS) fixedatmos_common.f90

fixedatmos_nc.$(OBJ_EXT) : fixedatmos_nc.f90  netcdf.inc $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) fixedatmos_nc.f90
