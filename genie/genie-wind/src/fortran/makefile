
LOCALFLAGS=$(FFLAGS) $(WINDPREC) $(BOUNDS_FLAGS)
ifeq ($(MACHINE),WIN32)
	MAIN_DIR = ..\\..\\..\\genie-main\\
else
        MAIN_DIR = ../../../genie-main/
endif
LOCINCPATH = $(MOD_INC_FLAG)$(MAIN_DIR)
LIBNAME = libwind
INCLUDE_FILES = 
OTHER_FILES=../../../genie-main/makefile.arc makefile

OBJECTS = wind_main.$(OBJ_EXT) initialise_wind.$(OBJ_EXT) end_wind.$(OBJ_EXT)

all: $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.dep *.mod

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

# Pattern rule to make object files
$(OBJECTS): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(LOCINCPATH) $(F90FLAGS) $(GENIEPRECISIONOPTS) \
	$(LIBUTIL1PRECISIONOPTS) $(FPPFLAGS) $< $(OBJ_FLAG)$@

# Rules to create '.dep' files from sources
# We want make to halt if $(PYTHON) is not found
# A failing 'which' command coupled with a -e shell invocation
# provides the trigger to halt the compilation. 
MAKE_DEPS = $(SHELL) -ec 'which $(PYTHON); $(PYTHON) $(MAIN_DIR)/finc.py $< | sed '\''s/$*\.$(OBJ_EXT)/& $@/g'\'' > $@'

%.dep: %.F
	$(MAKE_DEPS)

%.dep: %.f90
	$(MAKE_DEPS)

# Include the '.dep' files
ifneq ($(MAKECMDGOALS),clean)
-include $(OBJECTS:.$(OBJ_EXT)=.dep)
endif
