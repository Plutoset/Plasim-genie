################################
# USE GENIE VARIABLES
################################
ARCFL=../../../../genie-main/makefile.arc
include $(ARCFL)

################################
# SETUP OTHER VARIABLES
################################
EXENOM=test_tethys.exe
THISFILE=./Makefile

LIBDIR=$(GENIE_ROOT)/genie-lib
LIBNAMES = nc1 util1
LINK_LIBRARIES = $(LIBNAMES:%=-L$(LIBDIR)/lib%)
LINK_LIBRARIES += $(LIBNAMES:%=-l%)
LINK_LIBRARIES += -L$(GENIE_ROOT)/genie-land/lib -lland 
LINK_LIBRARIES += -L$(NETCDF_LIB) -lnetcdf

GGL=$(GENIE_ROOT)/genie-land/src/fortran
LINK_MODS=-M$(GGL) -M$(NETCDF_LIB)/src/f90 -M$(GENIE_ROOT)/genie-land/data/input/make_runoff

## Alternative values ATMEND=igcm, igcm-2
ATMEND=embm

MAIN=make_runoff_$(ATMEND)
GEOFIX=geofix_$(ATMEND)

OBJECTS=make_runoff_map.o $(MAIN).o $(GEOFIX).o

################################
# RULES
################################
.PHONY : clean

$(EXENOM) : $(OBJECTS) $(THISFILE) $(ARCFL)
	$(F77) $(F77FLAGS) $(OBJECTS) $(LINK_LIBRARIES) $(LINK_MODS) -o $(EXENOM)

make_runoff_map.o : make_runoff_map.f90 $(THISFILE) $(ARCFL)
	cd $(GENIE_ROOT)/genie-land ; $(MAKE)
	$(F77) $(LANDPREC) $(F77FLAGS) $(LINK_MODS) -c make_runoff_map.f90

$(MAIN).o : $(MAIN).f90 $(THISFILE) $(ARCFL)
	$(F77) $(LANDPREC) $(F77FLAGS) $(LINK_MODS) -c  $(MAIN).f90

$(GEOFIX).o : $(GEOFIX).f90 $(THISFILE) $(ARCFL)
	$(F77) $(LANDPREC) $(F77FLAGS) $(LINK_MODS) -c $(GEOFIX).f90
clean : 
	rm -f *.o make_runoff_map.$(MODEXT) $(EXENOM) fort.7*

##
## End of makefile
##
