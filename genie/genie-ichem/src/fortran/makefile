#
# $Id: makefile 5556 2010-05-25 23:51:27Z simon-mueller $
#

ROOT_DIR = ../../../genie-main
ifeq ($(MACHINE),WIN32)
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOC_INC_FLAGS=$(MOD_INC_FLAG)$(MAIN_DIR)
endif
LOCALFLAGS=$(FFLAGS) $(F90FLAGS) $(BOUNDS_FLAGS) \
            $(FPPFLAGS) $(DOPTS) $(LOC_INC_FLAGS)

INCLUDE_FILES = precision.inc netcdf.inc netdata.cmn

OTHER_FILES=../../../genie-main/makefile.arc makefile

OBJS1 = ichem_var.$(OBJ_EXT) ichem_util.$(OBJ_EXT) \
        ichem_main.$(OBJ_EXT) write_nc.$(OBJ_EXT)

OBJECTS = $(OBJS1)

MODMODS=ichem_var.$(MODEXT) ichem_util.$(MODEXT) ichem_main.$(MODEXT) 

all: $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.dep *.$(MODEXT)

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

# Pattern rules to make object files
# One rule for each compilation permutation
$(OBJS1): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) \
        $(F90FLAGS) $(DOPTS) $(GENIEPRECISIONOPTS) \
	$(LIBUTIL1PRECISIONOPTS) $< $(OBJ_FLAG)$@

# Rules to create '.dep' files from sources
# We want make to halt if $(PYTHON) is not found
# A failing 'which' command coupled with a -e shell invocation
# provides the trigger to halt the compilation. 
MAKE_DEPS = $(SHELL) -ec 'which $(PYTHON); $(PYTHON) $(ROOT_DIR)/finc.py $< | sed '\''s/$*\.$(OBJ_EXT)/& $@/g'\'' > $@'

%.dep: %.f90
	$(MAKE_DEPS)

# Include the '.dep' files
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),tags)
-include $(OBJECTS:.$(OBJ_EXT)=.dep)
endif
endif
