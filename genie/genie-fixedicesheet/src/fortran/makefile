#
# $Id: makefile 5556 2010-05-25 23:51:27Z simon-mueller $
#

LOCALFLAGS=$(FFLAGS) $(FIXEDICESHEETPREC) $(BOUNDS_FLAGS) 
# gain access to e.g. genie_util.mod and allow use of
# utility routines such as 'message' and 'die' 
ifeq ($(MACHINE),WIN32)
  LOCALFLAGS+=$(MOD_INC_FLAG)$(GENIE_ROOT_WIN)\\genie-main
else
  LOCALFLAGS+=$(MOD_INC_FLAG)$(MAIN_DIR)
endif

ROOT_DIR = ../../../genie-main
LIBNAME = libfixedicesheet

INCLUDE_FILES = fixice.cmn

OTHER_FILES=../../../genie-main/makefile.arc makefile

OBJS1 = fixedicesheet.$(OBJ_EXT)
OBJS2 = initialise_fixedicesheet.$(OBJ_EXT)

OBJECTS = $(OBJS1) $(OBJS2)

all: $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.$(MODEXT) *.dep

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

# Pattern rules to make object files
# One rule for each compilation permutation
$(OBJS1): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) \
	$(GENIEPRECISIONOPTS) $(LIBUTIL1PRECISIONOPTS) $< $(OBJ_FLAG)$@

$(OBJS2): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(GENIEPRECISIONOPTS) $(LIBUTIL1PRECISIONOPTS) $< $(OBJ_FLAG)$@

# Rules to create '.dep' files from sources
# We want make to halt if $(PYTHON) is not found
# A failing 'which' command coupled with a -e shell invocation
# provides the trigger to halt the compilation. 
MAKE_DEPS = $(SHELL) -ec 'which $(PYTHON); $(PYTHON) $(ROOT_DIR)/finc.py $< | sed '\''s/$*\.$(OBJ_EXT)/& $@/g'\'' > $@'

%.dep: %.F
	$(MAKE_DEPS)

%.dep: %.f90
	$(MAKE_DEPS)

# Include the '.dep' files
ifneq ($(MAKECMDGOALS),clean)
-include $(OBJECTS:.$(OBJ_EXT)=.dep)
endif
