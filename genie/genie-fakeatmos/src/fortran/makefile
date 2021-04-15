#
# $Id: makefile 3274 2007-04-30 14:51:29Z cvs-gw $
#
LIBNAME = 	libfakeatmos
OBJECTS = 	fake_atmosphere.$(OBJ_EXT) fakeatmos_common.$(OBJ_EXT) \
		fakeatmos_nc.$(OBJ_EXT)

INCLUDE_FILES = netcdf.inc precision.inc
OTHER_FILES   = ../../../genie-main/makefile.arc makefile 

all : $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean :
	\rm -f *.$(OBJ_EXT) *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

fake_atmosphere.$(OBJ_EXT) : fake_atmosphere.f90 fakeatmos_common.$(OBJ_EXT) \
fakeatmos_nc.$(OBJ_EXT) netcdf.inc precision.inc $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(FFLAGS) $(F90FLAGS) $(FAKEATMOSOPTS) \
	$(LIBUTIL1PRECISIONOPTS) fake_atmosphere.f90

fakeatmos_common.$(OBJ_EXT) : fakeatmos_common.f90 $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(FFLAGS) $(F90FLAGS) $(FAKEATMOSOPTS) \
                $(LIBNC1PRECISIONOPTS) fakeatmos_common.f90

fakeatmos_nc.$(OBJ_EXT) : fakeatmos_nc.f90  netcdf.inc $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(FFLAGS) $(F90FLAGS) $(FAKEATMOSOPTS) fakeatmos_nc.f90
