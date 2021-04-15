OBJECTS = land_const.o phys_const.o calibrate_stoma.o canopy.o leaf_c3.o leaf_c4.o
EXENOM  = CALIBRATE_STOMA.EXE
OTHERS  = ./Makefile


BUILD   = 
F90     = f95
LIBS   += -L$(NAGDIR) -lnag -lF77
MODS    = -M$(HOME)/genie_cvs/genie/genie-land/src/fortran
FLAGS   = -xtypemap=real:64,double:64,integer:32 -Dreal8 -fpp
FLAGS  += -u -C
MODEXT  = .mod

ifeq ($(BUILD),DEBUG)
  FLAGS += -g -ftrap=common
endif

.PHONY : clean

$(EXENOM) : $(OBJECTS) $(OTHERS)
	$(F90) $(FLAGS) $(OBJECTS) -o $(EXENOM) $(LIBS) $(MODS)
land_const.o : land_const.f90 $(OTHERS)
	$(F90) $(FLAGS) -c land_const.f90 $(LIBS) $(MODS)
phys_const.o : phys_const.f90 $(OTHERS)
	$(F90) $(FLAGS) -c phys_const.f90 $(LIBS) $(MODS)
calibrate_stoma.o : calibrate_stoma.f90 $(OTHERS)
	$(F90) $(FLAGS) -c calibrate_stoma.f90 $(LIBS) $(MODS)
canopy.o : canopy.f  $(OTHERS)
	$(F90) $(FLAGS) -c canopy.f $(LIBS) $(MODS)
leaf_c3.o : leaf_c3.f  $(OTHERS)
	$(F90) $(FLAGS) -c leaf_c3.f $(LIBS) $(MODS)
leaf_c4.o : leaf_c4.f  $(OTHERS)
	$(F90) $(FLAGS) -c leaf_c4.f $(LIBS) $(MODS)

clean :
	rm -f *.o *.$(MODEXT) $(EXENOM)



