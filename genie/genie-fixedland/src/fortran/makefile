#
# $Id: makefile 3274 2007-04-30 14:51:29Z cvs-gw $
#

LIBNAME=libfixedland
THISFL=./makefile

OBJS=fixed_land.$(OBJ_EXT)

all : $(OBJS) $(THISFL)

.PHONY : clean tags
clean:
	rm -f *.$(OBJ_EXT) *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

fixed_land.$(OBJ_EXT) : fixed_land.f90 $(THISFL)
	$(F77) $(COMPILEONLY) $(FFLAGS) $(F90FLAGS) $(GENIEPREC) fixed_land.f90
