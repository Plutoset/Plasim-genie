#
# top-level makefile for the GENIE build
#
# Note the inclusion of several other makefiles
# inluding testing.mak and makefile.arc, which
# in turn includes user.mak
#
export

include makefile.arc

C_DIR             = src/c
CONFIG_DIR        = configs
F_DIR             = src/fortran

LOCALFLAGS = $(FFLAGS) $(GENIEPREC) $(BOUNDS_FLAGS) 

OTHER_FILES = makefile makefile.arc testing.mak user.mak

EXE_MAIN = genie.exe
EXE_COSTFN = genie_costfn_offline.exe
EXE_DIAG = genie_diag_offline.exe
EXECUTABLES = $(EXE_MAIN) nccompare

OBJ_MAIN = genie.$(OBJ_EXT)
OBJ_COSTFN = genie_costfn_offline.$(OBJ_EXT)
OBJ_DIAG = genie_diag_offline.$(OBJ_EXT)
OBJS2 = end_graphics.$(OBJ_EXT)
OBJS3 = ini_graphics.$(OBJ_EXT)
OBJS4 = initialise_genie.$(OBJ_EXT) \
	write_averages.$(OBJ_EXT) \
	ini_averages.$(OBJ_EXT) \
        genie_restarts.$(OBJ_EXT)
OBJS5 = awi_genie.$(OBJ_EXT) \
	end_genie.$(OBJ_EXT) \
	extrap.$(OBJ_EXT)
OBJS6 = precision.$(OBJ_EXT) \
	averages.$(OBJ_EXT) \
	coasts.$(OBJ_EXT) \
	weights.$(OBJ_EXT) \
        genie_readsizes.$(OBJ_EXT) \
	write_netcdf_genie.$(OBJ_EXT)
OBJS7 = check_fluxes.$(OBJ_EXT)	
OBJS8 = genie_ini_wrappers.$(OBJ_EXT) \
	genie_loop_wrappers.$(OBJ_EXT) \
	genie_end_wrappers.$(OBJ_EXT)
CONTROL_OBJS = 	genie_control.$(OBJ_EXT) \
		genie_global.$(OBJ_EXT) \
		local_netcdf.$(OBJ_EXT) \
		genie_util.$(OBJ_EXT) \
		local_output.$(OBJ_EXT) \
		local_input.$(OBJ_EXT)

OBJECTS = $(CONTROL_OBJS) $(OBJS6) $(OBJS7) $(OBJS8) $(OBJS2) $(OBJS3) $(OBJS4) $(OBJS5)
OBJECTS_MAIN = $(OBJECTS) $(OBJ_MAIN)
OBJECTS_COSTFN = $(OBJECTS) $(OBJ_COSTFN)
OBJECTS_DIAG = $(OBJECTS) $(OBJ_DIAG)
OBJECTS_ALL = $(OBJECTS) $(OBJ_COSTFN) $(OBJ_DIAG) $(OBJ_MAIN)

LOCAL_NAMES = gem

MODULE_NAMES = slabocean \
               slabseaice \
               fixedocean \
               fixedseaice \
               goldstein \
               embm \
               plasim \
               goldsteinseaice \
               fixedatmos \
               fakeatmos \
               fixedland \
               fixedicesheet \
               fixedchem \
	        atchem \
               biogem \
               sedgem \
               rokgem \
               ichem \
               ents \
               wind

#ifeq ($(FLAG_MOSESTRIFFID),ON)
#MODULE_NAMES += mosestriffid
#FPPFLAGS += $(DEFINE)USE_mosestriffid
#endif

UTIL_NAMES = aux1 \
             blas1 \
             fft1 \
             nc1
ifeq ($(GRAPHICS),ON)  # see makefile.arc
  UTIL_NAMES += zxp
endif
UTIL_NAMES += util1 


# Construct variables/lists used for linking and dependencies
# construct vars for the 'external' modules
MODULE_LIBS_PROTO=$(MODULE_NAMES:%=$(GENIE_ROOT)/genie-%/lib/$(LIB_PREFIX))
MODULE_LIBS = $(addsuffix .$(LIB_EXT),$(join $(MODULE_LIBS_PROTO),$(MODULE_NAMES)))
MODULE_DATA=$(MODULE_NAMES:%=$(GENIE_ROOT)/genie-%/)
ifeq ($(MACHINE),WIN32)
	LINK_MODULE_LIBRARIES = $(MODULE_NAMES:%=$(LIB_SEARCH_FLAG)$(GENIE_ROOT_WIN)\\genie-%\\lib)
	LINK_MODULE_LIBRARIES += $(MODULE_NAMES:%=$(LIB_FLAG)lib%.$(LIB_EXT))
else
	LINK_MODULE_LIBRARIES = $(MODULE_NAMES:%=$(LIB_SEARCH_FLAG)$(GENIE_ROOT)/genie-%/lib)
	LINK_MODULE_LIBRARIES += $(MODULE_NAMES:%=$(LIB_FLAG)%)
endif
# link in the slap library
# note that icesheet module target builds glimmer and slap
ifeq ($(FLAG_GLIMMER),ON)  # see makefile.arc
  LINK_MODULE_LIBRARIES += -lslap
endif

# the 'local' modules
LOCAL_LIBS_PROTO=$(LOCAL_NAMES:%=$(MAIN_DIR)/src/fortran/cmn%/$(LIB_PREFIX)cmn)
LOCAL_LIBS = $(addsuffix .$(LIB_EXT),$(join $(LOCAL_LIBS_PROTO),$(LOCAL_NAMES)))
ifeq ($(MACHINE),WIN32)
	LINK_MODULE_LIBRARIES += $(LOCAL_NAMES:%=$(LIB_SEARCH_FLAG)$(GENIE_ROOT_WIN)\\genie-main\\src\\fortran\\cmn%)
	LINK_MODULE_LIBRARIES += $(LOCAL_NAMES:%=$(LIB_FLAG)libcmn%.$(LIB_EXT))
else
	LINK_MODULE_LIBRARIES += $(LOCAL_NAMES:%=$(LIB_SEARCH_FLAG)$(MAIN_DIR)/src/fortran//cmn%)
	LINK_MODULE_LIBRARIES += $(LOCAL_NAMES:%=$(LIB_FLAG)cmn%)
endif

# f90 modules included
INCLUDE_MODULE_LIBRARIES = $(MOD_INC_FLAG)$(INC_DIR)

# NB need to fix for WIN32 
ifeq ($(FLAG_GLIMMER),ON)  # see makefile.arc
  INCLUDE_MODULE_LIBRARIES += $(MOD_INC_FLAG)$(ICESHEET_DIR)/include
endif

UTIL_LIBS_PROTO=$(UTIL_NAMES:%=$(LIB_DIR)/lib%/$(LIB_PREFIX))
UTIL_LIBS = $(addsuffix .$(LIB_EXT),$(join $(UTIL_LIBS_PROTO),$(UTIL_NAMES)))
ifeq ($(MACHINE),WIN32)
	LINK_UTIL_LIBRARIES = $(UTIL_NAMES:%=$(LIB_SEARCH_FLAG)$(LIB_DIR_WIN)\\lib%)
	LINK_UTIL_LIBRARIES += $(UTIL_NAMES:%=$(LIB_FLAG)lib%.$(LIB_EXT))
else
	LINK_UTIL_LIBRARIES = $(UTIL_NAMES:%=$(LIB_SEARCH_FLAG)$(LIB_DIR)/lib%)
	LINK_UTIL_LIBRARIES += $(UTIL_NAMES:%=$(LIB_FLAG)%)
endif

# version of the model recorded in the code at build time
REV:=$(shell svn info | grep 'Last Changed Rev:' | cut -d' ' -f 4,4 | sed 's/^/REV=/')
INST:=INST="\"$(INSTITUTION)\""
TIMESTAMP:=TIMESTAMP="\"$(shell date)\""

# this is very odd--I don't understand why this is necessary.
all: $(EXECUTABLES)

costfn: $(EXE_COSTFN)

diag: $(EXE_DIAG)

data: $(MODULE_DATA)

.PHONY : costfn diag cleanall clean cleanutils cleanmodules \
         $(CLEAN_UTIL_TARGETS) $(CLEAN_MODULE_TARGETS) \
	 tags tagutils tagmodules $(CLEAN_LOCAL_TARGETS) \
         $(TAG_UTIL_TARGETS) $(TAG_MODULE_TARGETS) \
	 FORCE

# ================= CLEANING RULES ===================
# construct target names for cleaning rules
CLEAN_LOCAL_TARGETS = $(LOCAL_NAMES:%=clean_%)
CLEAN_MODULE_TARGETS = $(MODULE_NAMES:%=clean_%)
CLEAN_UTIL_TARGETS = $(UTIL_NAMES:%=clean_%)

# cleanall - cleans locally, in modules and util libs
# Note that an individual module or util can be cleaned
# with an appropriate target, e.g. 'make clean_igcm3'
cleanall : clean cleanutils cleanmodules
	\rm -f include/*.mod

# clean  - just local files
clean : $(CLEAN_LOCAL_TARGETS)
	\rm -f *.$(OBJ_EXT) $(EXECUTABLES) *.dep *.mod
	cd $(C_DIR); $(MAKE) clean
	cd $(F_DIR); $(MAKE) -I ../../ clean

$(CLEAN_LOCAL_TARGETS) : clean_% :
	\cd $(F_DIR)/cmn$*; $(MAKE) clean

# cleanutils - just util libs
cleanutils : $(CLEAN_UTIL_TARGETS)
$(CLEAN_UTIL_TARGETS) : clean_% :
	\cd $(LIB_DIR)/$(LIB_PREFIX)$*; $(MAKE) clean

# cleanmodules - just modules
cleanmodules : $(CLEAN_MODULE_TARGETS)
$(CLEAN_MODULE_TARGETS) : clean_% :
	\cd $(GENIE_ROOT)/genie-$*; $(MAKE) clean

# =========== OBJECT FILES AND EXECUTABLE ============

# CONTROL_OBJS is made first, so that components can use
# e.g. the grid size information
$(EXE_MAIN) : $(CONTROL_OBJS) $(UTIL_LIBS) $(LOCAL_LIBS) $(MODULE_LIBS) $(OBJECTS_MAIN) $(OTHER_FILES)
	$(F77_LD) $(LDFLAGS) $(OBJECTS_MAIN) $(OUT_FLAG)$(EXE_MAIN) \
        $(LINK_MODULE_LIBRARIES) $(LINK_UTIL_LIBRARIES) \
	$(NETCDF) $(GLIMMER) $(FORTRANLIBS) $(XLIB) $(BOUNDS_FLAGS)

$(EXE_COSTFN) : $(EXE) $(CONTROL_OBJS) $(UTIL_LIBS) $(LOCAL_LIBS) $(MODULE_LIBS) $(OBJECTS_COSTFN) $(OTHER_FILES)
	$(F77_LD) $(LDFLAGS) $(OBJECTS_COSTFN) $(OUT_FLAG)$(EXE_COSTFN) \
        $(LINK_MODULE_LIBRARIES) $(LINK_UTIL_LIBRARIES) \
	$(NETCDF) $(GLIMMER) $(FORTRANLIBS) $(XLIB) $(BOUNDS_FLAGS)

$(EXE_DIAG) : $(EXE) $(CONTROL_OBJS) $(UTIL_LIBS) $(LOCAL_LIBS) $(MODULE_LIBS) $(OBJECTS_DIAG) $(OTHER_FILES)
	$(F77_LD) $(LDFLAGS) $(OBJECTS_DIAG) $(OUT_FLAG)$(EXE_DIAG) \
        $(LINK_MODULE_LIBRARIES) $(LINK_UTIL_LIBRARIES) \
	$(NETCDF) $(GLIMMER) $(FORTRANLIBS) $(XLIB) $(BOUNDS_FLAGS)

# Rule to make custom netcdf comparison program
nccompare :
	\cd $(C_DIR); $(MAKE)

# Rule to make custom netcdf comparison program
errfn_ea_go_gs :
	\cd $(F_DIR); $(MAKE) -I ../..

# Rules to make custom executables for model-data evaluation and the computation of diagnostics from model output
.PHONY: costfn.exe
costfn.exe :
	\cd $(F_DIR); $(MAKE) -I ../..
	\cp $(F_DIR)/costfn.exe ./

# rule to set up data directories of individual modules
$(MODULE_DATA): FORCE
	\cd $(@D); if [ -d data ]; then $(MAKE) data; fi

# rule to build 'local' libs
$(LOCAL_LIBS) : FORCE
	\cd $(@D); $(MAKE) $(@F)
	\cp $(@D)/*.mod $(INC_DIR)

# rule to build module libraries
$(MODULE_LIBS) : FORCE
	\cd $(@D)/..; $(MAKE) $(@F)
	if [ -d $(@D)/../mod ] ; then \
		\cp $(@D)/../mod/*.mod $(INC_DIR); \
	fi

# rule to build util libraries
$(UTIL_LIBS) : FORCE
	\cd $(@D); $(MAKE) $(@F)

# a forcing rule
FORCE:


# pattern rules to make object files
# one rule for compilation permutation
$(OBJ_MAIN): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) \
	$(INCLUDE_MODULE_LIBRARIES) $(DOPTS) $(NETCDF_INC) $(GLIMMER_INC) $(GENIEGLIMMEROPTS) \
	$< $(OBJ_FLAG)$@

$(OBJ_COSTFN): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) \
	$(INCLUDE_MODULE_LIBRARIES) $(DOPTS) $(NETCDF_INC) $(GLIMMER_INC) $(GENIEGLIMMEROPTS) \
	$< $(OBJ_FLAG)$@

$(OBJ_DIAG): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) \
	$(INCLUDE_MODULE_LIBRARIES) $(DOPTS) $(NETCDF_INC) $(GLIMMER_INC) $(GENIEGLIMMEROPTS) \
	$< $(OBJ_FLAG)$@

$(OBJS2): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) \
	$(DOPTS) $< $(OBJ_FLAG)$@

$(OBJS3): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) $(DOPTS) \
	$(LIBZXPPRECISIONOPTS) $(INCLUDE_MODULE_LIBRARIES) $< $(OBJ_FLAG)$@

$(OBJS4): %.$(OBJ_EXT): %.F $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) \
	$(LIBUTIL1PRECISIONOPTS) $(INCLUDE_MODULE_LIBRARIES) $(GLIMMER_INC) \
	$< $(OBJ_FLAG)$@

$(OBJS5): %.$(OBJ_EXT): %.f $(OTHER_FILES)
	$(F77) $(COMPILEONLY) $(LOCALFLAGS) $(F77FLAGS) $< $(OBJ_FLAG)$@

$(OBJS6): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(LIBUTIL1PRECISIONOPTS) $(INCLUDE_MODULE_LIBRARIES) $(GLIMMER_INC) $(GENIEGLIMMEROPTS) \
	$(IGCMHRLATSOPTS) $< $(OBJ_FLAG)$@

$(OBJS7): %.$(OBJ_EXT): %.F90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(INCLUDE_MODULE_LIBRARIES) $(GLIMMER_INC) $(GENIEGLIMMEROPTS) \
	$< $(OBJ_FLAG)$@

$(OBJS8): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(INCLUDE_MODULE_LIBRARIES) $(GLIMMER_INC) $(GENIEGLIMMEROPTS) \
	$< $(OBJ_FLAG)$@

$(CONTROL_OBJS): %.$(OBJ_EXT): %.f90 $(OTHER_FILES)
	$(F77) $(FPPFLAGS) $(COMPILEONLY) $(LOCALFLAGS) $(F90FLAGS) \
	$(GLIMMER_INC) $(GENIEGLIMMEROPTS) $(DEFINE)$(REV) $(DEFINE)$(TIMESTAMP) $(DEFINE)$(INST) $< $(OBJ_FLAG)$@

# ================== TESTING RULES ===================
include testing.mak
include old_testing.mak

# ==== RULES TO ATUO' CREATE DEPENDENCY FILES ==== 
# Rules to create '.dep' files from sources
# We want make to halt if $(PYTHON) is not found
# A failing 'which' command coupled with a -e shell invocation
# provides the trigger to halt the compilation. 
MAKE_DEPS = @ $(SHELL) -ec 'which $(PYTHON) > /dev/null; \
		$(PYTHON) finc.py $< | sed '\''s/$*\.$(OBJ_EXT)/& $@/g'\'' > $@'

%.dep: %.f
	$(MAKE_DEPS)

%.dep: %.F
	$(MAKE_DEPS)

%.dep: %.f90
	$(MAKE_DEPS)

# Include the '.dep' files
ifneq ($(MAKECMDGOALS),clean)
-include $(OBJECTS_ALL:.$(OBJ_EXT)=.dep)
endif

# ==== RULES FOR AUTOMATIC DOCUMENTATION =====

docs :
	\cd $(DOC_DIR); make all

# ==== RULES FOR MAKING TAGS =====
# construct target names for cleaning rules
TAG_MODULE_TARGETS = $(MODULE_NAMES:%=tag_%)
TAG_UTIL_TARGETS = $(UTIL_NAMES:%=tag_%)

tags: tagmain tagutils tagmodules

tagutils : $(TAG_UTIL_TARGETS)
$(TAG_UTIL_TARGETS) : tag_% :
	\cd $(LIB_DIR)/$(LIB_PREFIX)$*; $(MAKE) tags

tagmodules : $(TAG_MODULE_TARGETS)
$(TAG_MODULE_TARGETS) : tag_% :
	\cd $(GENIE_ROOT)/genie-$*; $(MAKE) tags

tagmain:
	if [ -f $(TAGFILE) ] ; then \rm $(TAGFILE) ; fi
	@ $(SHELL) -ec 'which $(ETAGS) > /dev/null; \
		$(ETAGS) -a -o $(TAGFILE) *.F; \
		$(ETAGS) -a -o $(TAGFILE) *.f; \
		$(ETAGS) -a -o $(TAGFILE) *.f90'

# ==== RULES FOR NETCDF LIBRARIES =====

UNIDATA_HDF5_DOWNLOAD_URL=ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4
UNIDATA_NETCDF_DOWNLOAD_URL=http://www.unidata.ucar.edu/downloads/netcdf/ftp

netcdf: netcdf-4.0

netcdf-4.2: FORCE
	# NB you'll need to change NETCDF_FORT_LIB_NAME in user.mak to 'netcdff'
	# Note the extra 'f'
	# In order to link GENIE against versions of NetCDF >= 4.1
	$(eval HDF5_VERSION = 1.8.9)
	$(eval NETCDF_C_VERSION = 4.2.1.1)
	$(eval NETCDF_FORTRAN_VERSION = 4.2)
	$(eval NETCDF_C++_VERSION = 4.2)
	# First, HDF5
	# CC and CXX are set in user.mak, but we must overwite FC and LD flags
	# so that we don't pick up a GENIE specific build environment  
	\wget $(UNIDATA_HDF5_DOWNLOAD_URL)/hdf5-$(HDF5_VERSION).tar.gz;\
	\tar -xzf hdf5-$(HDF5_VERSION).tar.gz;\
	\cd hdf5-$(HDF5_VERSION);\
	FC=$(F77) FFLAGS='' LDFLAGS='' ./configure --prefix=$(NETCDF_DIR) --enable-fortran --enable-cxx;\
	$(MAKE) install;\
	# Next, NetCDF C distribution
	\wget $(UNIDATA_NETCDF_DOWNLOAD_URL)/netcdf-$(NETCDF_C_VERSION).tar.gz;\
	\tar -xzf netcdf-$(NETCDF_C_VERSION).tar.gz;\
	\cd netcdf-$(NETCDF_C_VERSION);\
	FC=$(F77) CPPFLAGS=-I$(NETCDF_DIR)/include LDFLAGS=-L$(NETCDF_DIR)/lib ./configure --prefix=$(NETCDF_DIR) --enable-netcdf-4;\
	$(MAKE) install;\
	# Lastly, NetCDF Fortran ...
	\wget $(UNIDATA_NETCDF_DOWNLOAD_URL)/netcdf-fortran-$(NETCDF_FORTRAN_VERSION).tar.gz;\
	\tar -xzf netcdf-fortran-$(NETCDF_FORTRAN_VERSION).tar.gz;\
	\cd netcdf-fortran-$(NETCDF_FORTRAN_VERSION);\
	FC=$(F77) CPPFLAGS=-I$(NETCDF_DIR)/include ./configure --prefix=$(NETCDF_DIR);\
	$(MAKE) install;\
	# and C++ distros
	\wget $(UNIDATA_NETCDF_DOWNLOAD_URL)/netcdf-cxx-$(NETCDF_FORTRAN_VERSION).tar.gz;\
	\tar -xzf netcdf-cxx-$(NETCDF_FORTRAN_VERSION).tar.gz;\
	\cd netcdf-cxx-$(NETCDF_FORTRAN_VERSION);\
	CPPFLAGS=-I$(NETCDF_DIR)/include ./configure --prefix=$(NETCDF_DIR);\
	$(MAKE) install

netcdf-4.0: FORCE
	$(eval NETCDF_VERSION = 4.0)
	\wget $(UNIDATA_NETCDF_DOWNLOAD_URL)/netcdf-$(NETCDF_VERSION).tar.gz;\
	\tar -xzf netcdf-$(NETCDF_VERSION).tar.gz;\
	\cd netcdf-$(NETCDF_VERSION);\
	FC=$(F77) ./configure --prefix=$(NETCDF_DIR);\
	$(MAKE) install;
