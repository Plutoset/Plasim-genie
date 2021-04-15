################################
# USE GENIE VARIABLES
################################
ARCFL=../../../../genie-main/makefile.arc
include $(ARCFL)

################################
# SETUP OTHER VARIABLES
################################
EXENOM=make_restart.exe
STDOFILE=land_rs_00000000.nc
THISFILE=./Makefile

LIBDIR=$(GENIE_ROOT)/genie-lib
LIBNAMES = nc1 util1
LINK_LIBRARIES = $(LIBNAMES:%=-L$(LIBDIR)/lib%)
LINK_LIBRARIES += $(LIBNAMES:%=-l%)
LINK_LIBRARIES += -L$(GENIE_ROOT)/genie-land/lib -lland 
LINK_LIBRARIES += -L$(NETCDF_LIB) -l$(NETCDF_NAME)

GGL=$(GENIE_ROOT)/genie-land/src/fortran
LINK_MODS = -M$(GGL) -M$(NETCDF_LIB)/src/f90 -M$(GENIE_ROOT)/genie-land/data/input/make_vegetation

F77FLAGS += -C -u $(PREPROCFLAG)

USEICE=FALSE
ifeq ($(USEICE),TRUE)
  F77FLAGS += -Duseice
endif

################################
# Alternatively invoke make with
# ATMEND=embm, igcm-2
# DATAEND=baresoil, potential
################################
ATMEND=igcm
DATAEND=igbp

OBJECTS=make_restart_$(DATAEND).o get_atm_grid_$(ATMEND).o


################################
# RULES
################################
.PHONY : clean

$(EXENOM) : $(OBJECTS) $(ARCFL) $(THISFILE)
	$(F77) $(F77FLAGS) $(OBJECTS) $(GGL)/land_restart.o -o $(EXENOM) $(LINK_LIBRARIES) $(LINK_MODS)

make_restart_$(DATAEND).o : make_restart_$(DATAEND).f90 $(ARCFL) $(THISFILE)
	$(MAKE) -C $(GENIE_ROOT)/genie-land
	$(F77) $(LANDPREC) $(F77FLAGS) $(LINK_MODS) -c make_restart_$(DATAEND).f90

get_atm_grid_$(ATMEND).o : get_atm_grid_$(ATMEND).f90 $(ARCFL) $(THISFILE)
	$(F77) $(LANDPREC) $(F77FLAGS) $(LINK_MODS) -c get_atm_grid_$(ATMEND).f90 


all : 
	$(MAKE) clean
	$(MAKE) ATMEND=igcm DATAEND=igbp
	$(EXENOM)
	mv $(STDOFILE) land_rs_igcm_t21.nc

	$(MAKE) clean
	$(MAKE) ATMEND=igcm DATAEND=potential
	$(EXENOM)
	mv $(STDOFILE) land_rs_igcm_t21_potential.nc

	$(MAKE) clean
	$(MAKE) ATMEND=embm DATAEND=igbp
	$(EXENOM)
	mv $(STDOFILE) land_rs_embm_36x36.nc

	$(MAKE) clean
	$(MAKE) ATMEND=embm DATAEND=potential
	$(EXENOM)
	mv $(STDOFILE) land_rs_embm_36x36_potential.nc

clean : 
	rm -f *.o *.$(MODEXT) *.exe pft_*.out
##
## End of makefile
##
