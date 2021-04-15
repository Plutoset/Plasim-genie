#
# $Id: makefile 3481 2007-08-15 08:42:14Z gethin-williams $
#

LOCALFLAGS=$(FFLAGS) $(F77FLAGS) $(FIXEDCHEMPREC) $(BOUNDS_FLAGS)

ROOT_DIR = ../../../genie-main
LIBNAME = libfixedchem

INCLUDE_FILES = 

OTHER_FILES=../../../genie-main/makefile.arc makefile

OBJS1 = fixedchem.$(OBJ_EXT) initialise_fixedchem.$(OBJ_EXT)

OBJECTS = $(OBJS1)

all: $(OBJECTS) $(OTHER_FILES) $(INCLUDE_FILES)

.PHONY: clean tags
clean : 
	\rm -f *.$(OBJ_EXT) *.dep

tags:
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F'

# Pattern rules to make object files
# One rule for each compilation permutation
$(OBJS1): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) \
	$(GENIEPRECISIONOPTS) \
	$(LIBUTIL1PRECISIONOPTS) $< $(OBJ_FLAG)$@

# Rules to create '.dep' files from sources
# We want make to halt if $(PYTHON) is not found
# A failing 'which' command coupled with a -e shell invocation
# provides the trigger to halt the compilation. 
MAKE_DEPS = $(SHELL) -ec 'which $(PYTHON); $(PYTHON) $(ROOT_DIR)/finc.py $< | sed '\''s/$*\.$(OBJ_EXT)/& $@/g'\'' > $@'

%.dep: %.F
	@ $(MAKE_DEPS)

# Include the '.dep' files
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),tags)
-include $(OBJECTS:.$(OBJ_EXT)=.dep)
endif
endif
