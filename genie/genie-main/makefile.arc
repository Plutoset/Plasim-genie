#

#

# ====================================================================
#
# This is the master 'settings' file for the GENIE build.
#
# In this file, variables are assigned values.  Defaults, which are
# commonly required to be changed to suit your particular setup are
# found in 'user.mak'.  The actual rules to be carried out in the
# build process are mostly in 'makefile'; the testing specific rules
# are in 'testing.mak'.  
#
# In addition to editing this file, variables can be overridden on
# the command-line.  For example, you may type:
#    make NIGHTLYBUILD=TRUE
# and other variables will be set appropriately.
#
# If you would like any changes to this file, please email your
# requirements to gethin.williams@bristol.ac.uk and I will take
# care of them.
#
# ====================================================================

# === get the setup-specific varaibles === 
include user.mak

# === main path variables ===
MAIN_DIR          = $(GENIE_ROOT)/genie-main
LIB_DIR           = $(GENIE_ROOT)/genie-lib
TOOLS_DIR         = $(GENIE_ROOT)/genie-tools
ICESHEET_DIR      = $(GENIE_ROOT)/genie-icesheet
FIXEDATMOS_DIR    = $(GENIE_ROOT)/genie-fixedatmos
FAKEATMOS_DIR     = $(GENIE_ROOT)/genie-fakeatmos
FIXEDLAND_DIR     = $(GENIE_ROOT)/genie-fixedland
FIXEDICESHEET_DIR = $(GENIE_ROOT)/genie-fixedicesheet
ICHEM_DIR         = $(GENIE_ROOT)/genie-ichem
KNOWNGOOD_DIR     = $(GENIE_ROOT)/genie-knowngood
DOC_DIR           = $(MAIN_DIR)/doc
ifeq ($(MACHINE),WIN32)
  GENIE_ROOT_PROTO = $(shell cygpath -w $(GENIE_ROOT))
  GENIE_ROOT_WIN   = $(subst \,\\,$(GENIE_ROOT_PROTO))
  LIB_DIR_PROTO    = $(shell cygpath -w $(GENIE_ROOT)/genie-lib)
  LIB_DIR_WIN      = $(subst \,\\,$(LIB_DIR_PROTO))
  INC_DIR_PROTO    = $(shell cygpath -w $(MAIN_DIR)/include)
  INC_DIR          = $(subst \,\\,$(INC_DIR_PROTO))
else
  INC_DIR          = $(MAIN_DIR)/include
endif

# === Switch for nightly build settings ===
NIGHTLYBUILD=FALSE
NIGHTLYBUILD2=FALSE
#NIGHTLYBUILD=TRUE

# path settings for the nightly build
ifeq ($(NIGHTLYBUILD),TRUE)
  OUT_DIR=$(HOME)/genie_testoutput
  GENIE_ROOT=$(HOME)/genie-test
  RUNTIME_ROOT=../../genie-test
endif

ifeq ($(NIGHTLYBUILD2),TRUE)
  OUT_DIR=$(HOME)/scratch/genie_testoutput
  GENIE_ROOT=$(HOME)/scratch/genie-test
  RUNTIME_ROOT=../../genie-test
endif

# === Ancillary Tools ===
PYTHON = python
PERL   = perl
ETAGS  = etags
TAGFILE=$(MAIN_DIR)/TAGS

# == seeding ===
FFLAGS=$(GENIE_FFLAGS) $(NETCDF_INC)
F77FLAGS=$(GENIE_F77FLAGS)
F90FLAGS=$(GENIE_F90FLAGS)
CPPFLAGS=$(GENIE_CPPFLAGS)
FPPFLAGS=$(GENIE_FPPFLAGS)
CXXFLAGS=$(GENIE_CXXFLAGS)
CCFLAGS=$(GENIE_CCFLAGS)
LDFLAGS=$(GENIE_LDFLAGS)
BOUNDS_FLAGS=$(GENIE_BOUNDS_FLAGS)

# Library archive command.
# Please ensure that there is a space at the end of the string "ar rcvs ",
# ditto "ranlib -c ".
# Note that 's' adds an index and so ranlib shouldn't be needed.
# However, Mac OSX doesn't use GNU, and we need the extra call to
# ranlib, but only in that circumstance (-c not accepted by GNU ranlib).
LIB_CMD=ar rcvs 
RANLIB_CMD=ranlib -c 

# === OS Specific flags and file extensions ===
# *NB* For _UNIX_ OBJ_FLAG must have a trailing space, e.g. "-o " 
COMPILEONLY =-c
DEFINE      =-D
OUT_FLAG    =-o 
OBJ_FLAG    =-o 
OBJ_EXT     =o
LIB_EXT     =a
LIB_PREFIX  =lib
LIB_SEARCH_FLAG = -L
LIB_FLAG    =-l
INC_FLAG    =-I
PATH_QUOTE  =
MOD_INC_FLAG = -I
# OVERRIDES for WIN32
ifeq ($(MACHINE),WIN32)
  OBJ_FLAG  =
  OBJ_EXT   =obj
  LIB_EXT   =lib
  LIB_SEARCH_FLAG = /LIBPATH:
  LIB_FLAG    =lib
  MOD_INC_FLAG = /INCLUDE:
endif

# === Intel Fortran compiler (v7.X) ===
ifeq ($(F77),ifc)
  F77_LD=ifc
  FPPFLAGS += -fpp
  FLAGR4=
  FLAGR8=-r8
  ifeq ($(BUILD),SHIP)
    FFLAGS += -O2
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -O2 -p
  endif
endif

# === Intel Fortran compiler (v8.0 upwards) ===
# Scalar variables are 'saved' by default
# ig_fi_fi seg faults with -auto
# -noautomatic gives identical exe speeds to default
ifeq ($(F77),ifort)
  F77_LD=ifort
  FPPFLAGS += -fpp
  FLAGR4=
  FLAGR8=-r8
  FFLAGS += -warn all -implicitnone -extend-source
  #FFLAGS += -diag-disable remark
  #FFLAGS +=  -noautomatic
  ifeq ($(BUILD),SHIP)
    FFLAGS += -O3
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -traceback -fpe0 -fpstkchk -CU
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -O3 -p
    LDFLAGS += -p
  endif
  ifeq ($(BUILD),BOUNDS)
    BOUNDS_FLAGS += -CB
  endif
endif

# === f95 compiler (Solaris) ===
# -O4 much slower to compile than -O3 (defualt)
# but gives a x2 speedup
# -fast -native is slow to compile but gives a v. fast exe 
# -stackval (equiv. to -auto) gives a seg fault, as expected
# implies default is to 'save'  
ifeq ($(F77),f95)
  F77_LD=f95
  FPPFLAGS += -fpp
  FLAGR4=-xtypemap=real:32,double:64,integer:32 
  FLAGR8=-xtypemap=real:64,double:64,integer:32 
  MOD_INC_FLAG=-M
  FFLAGS += -e 
  ifeq ($(BUILD),SHIP)
    FFLAGS += -fast -native 
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -xcheck=all
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -pg -ftrap=%none
  endif
  ifeq ($(BUILD),BOUNDS)
    FFLAGS += -C
  endif
endif

# === f90 compiler (SGI or LINUX) ===
ifeq ($(F77),f90)
  F77_LD=f90
  ifeq ($(MACHINE),LINUX)
    FPPFLAGS=-fpp
  endif
  ifeq ($(MACHINE),SGI)
    FPPFLAGS=-ftpp
  endif
  # compiler opt for sgi machines: 'treat locals as static'
  ifeq ($(MACHINE),SGI)
    FFLAGS += -static
  endif
  FLAGR4=-r4
  FLAGR8=-r8
  ifeq ($(BUILD),SHIP)
    FFLAGS += -O3
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -check_bounds
  endif
  # can't find a profiling option for compiler
  ifeq ($(BUILD),PROFILE)
    FFLAGS +=
  endif
endif

# === Portland compiler ===
# -fastsse good deal faster than -fast
# Similar compile time to select better
# -Msave does create slowdown
# able to run without, however, so some must be 'saved'? 
ifeq ($(F77),pgf90)
  F77_LD=pgf90
  FLAGR4=-r4
  FLAGR8=-r8
  FPPFLAGS += -Mpreprocess
  FFLAGS += -fastsse -Mextend
  #FFLAGS += -Msave
  ifeq ($(BUILD),SHIP)
    FFLAGS +=
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -C
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -Mprof=func
    LDFLAGS += -Mprof=func
  endif
  ifeq ($(BUILD),BOUNDS)
    BOUNDS_FLAGS += -Mbounds
  endif
endif

# === Portland compiler ===
ifeq ($(F77),pgf95)
  F77_LD=pgf95
  FLAGR4=-r4
  FLAGR8=-r8
  FPPFLAGS += -Mpreprocess
  FFLAGS += -Mextend
  ifeq ($(BUILD),SHIP)
    FFLAGS += -O4
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -C
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -Mprof=func
    LDFLAGS += -Mprof=func
  endif
  ifeq ($(BUILD),BOUNDS)
    BOUNDS_FLAGS += -Mbounds
  endif
endif

# === Pathscale compiler ===
ifeq ($(F77),pathf90)
  F77_LD=pathf90
  FLAGR4=-r4
  FLAGR8=-r8
  FFLAGS += -Wall -fno-second-underscore
  FPPFLAGS += -ftpp
  ifeq ($(BUILD),SHIP)
    FFLAGS += -Ofast
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -pg
    LDFLAGS += -pg
  endif
  ifeq ($(BUILD),BOUNDS)
    BOUNDS_FLAGS += -ffortran-bounds-check
  endif
endif

# === GNU 'gfortran' compiler ===
# need -fno-automatic for the moment..
ifeq ($(F77),gfortran)
  F77_LD=gfortran
  FLAGR4=
  FLAGR8=-fdefault-real-8 -fdefault-double-8
  F77FLAGS += -x f77-cpp-input -ffixed-line-length-80
  F90FLAGS += -x f95-cpp-input -ffree-line-length-none
  FFLAGS += -Wall -fimplicit-none
  FFLAGS +=  -fno-automatic
  ifeq ($(BUILD),SHIP)
    FFLAGS += -O2
    FFLAGS += -O3 
    FFLAGS += -funroll-loops 
    FFLAGS += -msse
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -ffpe-trap=zero,overflow,invalid -O0 -Wall
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -pg
    LDFLAGS += -pg
  endif
  ifeq ($(BUILD),BOUNDS)
    BOUNDS_FLAGS += -fbounds-check
  endif
endif

# === GNU 'gfortran' compiler for Win32 ===
ifeq ($(F77),gfc.exe)
  F77=gfc
  F77_LD=gfc
  FLAGR4=
  FLAGR8=-fdefault-real-8
  F77FLAGS += -x f77-cpp-input -ffixed-line-length-80
  F90FLAGS += -x f95-cpp-input -ffree-line-length-none
  FFLAGS += -Wall -Wtabs
  FFLAGS +=  -fno-automatic
  ifeq ($(BUILD),SHIP)
    FFLAGS += -O2
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -ffpe-trap=zero,overflow,invalid -O0 -Wuninitialized
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -pg
    LDFLAGS += -pg
  endif
  ifeq ($(BUILD),BOUNDS)
    BOUNDS_FLAGS += -fbounds-check
  endif
endif

# === GNU 'g95' compiler ===
# also appears to need -fstatic to run exe correctly
# (ig_go_sl grinds to a halt without it) 
# -O3 no noticeable improvement over -O2
# ditto -march=i686 or -msse2 (on Xeon)
ifeq ($(F77),g95)
  F77_LD=g95
  FLAGR4=
  FLAGR8=-r8
  FPPFLAGS += -cpp
  FFLAGS += -fno-second-underscore -Wall -O2
  FFLAGS += -fstatic
  F77FLAGS += -ffixed-line-length-80 
  F90FLAGS += -ffree-line-length-huge
  ifneq ($(BUILD),DEBUG)
    FFLAGS += -Werror
  endif
  ifeq ($(BUILD),SHIP)
    FFLAGS += -O3
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -ftrace=full
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -pg
    LDFLAGS += -pg
  endif
  ifeq ($(BUILD),BOUNDS)
    BOUNDS_FLAGS += -fbounds-check
  endif
endif

# === GNU 'g95' compiler for Win32 ===
ifeq ($(F77),g95.exe)
  F77=g95
  F77_LD=g95
  FLAGR4=
  FLAGR8=-r8
  FPPFLAGS += -cpp
  FFLAGS += -Wall
  F77FLAGS += -ffixed-line-length-80 
  F90FLAGS += -ffree-line-length-huge
  ifeq ($(BUILD),SHIP)
    FFLAGS += -O2
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += -g -ftrace=full
  endif
  ifeq ($(BUILD),PROFILE)
    FFLAGS += -pg
    LDFLAGS += -pg
  endif
  ifeq ($(BUILD),BOUNDS)
    BOUNDS_FLAGS += -fbounds-check
  endif
endif

# This is the optimisation level applied to genie-goldstein, genie-embm 
# and genie-seaice.  Note that if BUILD=SHIP then the optimisation level
# in FFLAGS overrides that set by GOLDOPTIM no matter which one is
# greater.
GOLDOPTIM=
ifneq ($(findstring -O,$(FFLAGS)),-O)
  GOLDOPTIM=-O
endif

# === Compaq Visual Fortran v6.6 ===
ifeq ($(F77),f90.exe)
  GOLDOPTIM   =/optimize:4
  COMPILEONLY =/compile_only /nologo
  LIB_CMD     =lib /OUT:
  DEFINE      =/define:
  F77_LD      =link.exe /STACK:4194304
  FPPFLAGS   +=/fpp:" /m"
  FFLAGS     +=/warn:\(all,notruncated_source\)
  F77FLAGS   +=/extend_source:132
  OUT_FLAG    =/OUT:
  OBJ_FLAG    =/object:
  LIB_SEARCH_FLAG =/LIBPATH:
  LIB_FLAG    =
  INC_FLAG    =/INCLUDE:
  PATH_QUOTE  ="
  FLAGR4      =
  FLAGR8      =/real_size:64
  ifeq ($(BUILD),SHIP)
    FFLAGS += 
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += /debug:full
  endif
  ifeq ($(BUILD),PROFILE)
    F77_LD += /profile
  endif
endif

# === Intel Visual Fortran v9.1 ===
# NB. NetCDF F90 library needs to be built with "ifort.exe /iface:cvf ..."
ifeq ($(F77),ifort.exe)
  GOLDOPTIM   =/optimize:4
  COMPILEONLY =/compile-only /nologo
  LIB_CMD     =lib /OUT:
  DEFINE      =/define:
  F77_LD      =link.exe /STACK:4194304
  FPPFLAGS   +=/fpp:" /m"
  FFLAGS     +=/warn:all,notruncated_source /iface:cvf /extend-source:132
  OUT_FLAG    =/OUT:
  OBJ_FLAG    =/object:
  LIB_SEARCH_FLAG =/LIBPATH:
  LIB_FLAG    =
  INC_FLAG    =/INCLUDE:
  PATH_QUOTE  ="
  FLAGR4      =
  FLAGR8      =/real-size:64
  ifneq ($(BUILD),DEBUG)
#    FFLAGS += /warn:errors
  endif
  ifeq ($(BUILD),SHIP)
    FFLAGS += 
  endif
  ifeq ($(BUILD),DEBUG)
    FFLAGS += /debug:full
  endif
  ifeq ($(BUILD),PROFILE)
    F77_LD += /profile
  endif
endif

# ===CC/GCC===
ifeq ($(BUILD),SHIP)
  CCFLAGS += -O2
  CXXFLAGS += -O2
endif

ifeq ($(BUILD),DEBUG)
  CCFLAGS += -g
  CXXFLAGS += -g
endif

ifeq ($(BUILD),PROFILE)
  CCFLAGS += -O2 -pg
  CXXFLAGS += -O2 -pg
endif

ifeq ($(MACHINE),LINUX)
  ifeq ($(F77),pgf95)
    CCFLAGS += -DPGI -DLINUX
  else
    CCFLAGS += -DIFC -DLINUX
  endif
endif

ifeq ($(MACHINE),SGI)
  CCFLAGS += -DSGI
endif

ifeq ($(MACHINE),SOLARIS)
  CCFLAGS += -DSOLARIS
endif

# === NetCDF paths ===
NETCDF=$(LIB_SEARCH_FLAG)$(PATH_QUOTE)$(NETCDF_DIR)/lib$(PATH_QUOTE)
NETCDF+=$(LIB_FLAG)$(NETCDF_CXX_LIB_NAME)
NETCDF+=$(LIB_FLAG)$(NETCDF_FORT_LIB_NAME)
NETCDF+=$(LIB_FLAG)$(NETCDF_C_LIB_NAME)

NETCDF_INC=$(INC_FLAG)$(PATH_QUOTE)$(NETCDF_DIR)/include$(PATH_QUOTE)
ifeq ($(F77),f95)
  NETCDF_INC += $(MOD_INC_FLAG)$(PATH_QUOTE)$(NETCDF_DIR)/include$(PATH_QUOTE)
endif
ifeq ($(MACHINE),WIN32)
  NETCDF_INC=$(INC_FLAG)$(PATH_QUOTE)$(NETCDF_DIR)\\include$(PATH_QUOTE)
  NETCDF += $(LIB_FLAG)netcdf_f90.lib
endif

# === GLIMMER paths ===

ifeq ($(FLAG_GLIMMER),ON)
  GLIMMER_NAMES=$(LIB_FLAG)glint $(LIB_FLAG)glide $(LIB_FLAG)climate $(LIB_FLAG)glimmer $(LIB_FLAG)slap
  GLIMMER=$(LIB_SEARCH_FLAG)$(PATH_QUOTE)$(GLIMMER_DIR)/lib$(PATH_QUOTE) $(GLIMMER_NAMES)
  GLIMMER_INC=$(INC_FLAG)$(PATH_QUOTE)$(GLIMMER_DIR)/include$(PATH_QUOTE)
  ifeq ($(F77),f95)
    GLIMMER_INC += $(MOD_INC_FLAG)$(PATH_QUOTE)$(GLIMMER_DIR)/include$(PATH_QUOTE)
  endif
else
  GLIMMER_NAMES=
  GLIMMER=
  GLIMMER_INC=
endif

# === Graphics libs and compiler options ===
ifeq ($(GRAPHICS),ON)
  XLIB=-L/usr/X11R6/lib -lX11
  DOPTS=-Dlgraph
else
  XLIB=
  DOPTS=
endif

# === Glimmer options for genie-main ===
ifeq ($(FLAG_GLIMMER),ON)
  GENIEGLIMMEROPTS=$(DEFINE)glimmeron
else
  GENIEGLIMMEROPTS=
endif

# === Tcl/Tk (wishx and xqplot.tcl) ===
LOCFLAGS=-DWISHX=\"$(CODE_DIR)/genie-main/inputdata/wishx\" -DXQPLOTTCL=\"$(CODE_DIR)/genie-main/inputdata/xqplot.tcl\" -DXFONT=\"$(CODE_DIR)/genie-main/inputdata/fonts\"

# ----------------------------------------------------------------------
#
# Precision of the genie module.  real8 strongly reccommended
#    (unless using igcm and not goldocean, goldseaice, goldembm.)
# Compilation flags for each module
# -r8 strongly reccommended for genie,
#    (unless using igcm and not goldocean, goldseaice, goldembm.)
# -r8 STRONGLY reccommended for goldocean, goldseaice, goldembm.
# GENIEPREC must be the same as GENIEPRECISIONOPTS - this ifeq statement
# sees to that (gnu-make specific).
#####################################################################
# N.B. IF YOU CHANGE IGCMATMOSPREC, YOU WILL HAVE TO RE-RUN MAKE_LIBS.
#####################################################################
# AUX1,BLAS1 and FFT1 are used exclusively used by the igcm (why don't 
# we just put them into the igcm folder?!).
# NC1,UTIL1,ZXP are set to the same precision as igcm, but are
# in the process of being given their own independent precision.
# It means that for now, any variables being passed to the 
# libnc1,libutil1,libzxp routines (i.e. from goldstein) MUST be in the 
# same precision as these routines.
# (they are hard-wired to real*4 at the moment I think).
#####################################################################
# SO FAR I HAVE ONLY TESTED WITH: 
# EVERYTHING R4:
#   igcm3-fixedocean-fixedseaice
#   igcm3-slabocean-slabseaice
# EVERYTHING R4 except GENIEPREC=R8:
#   igcm3-fixedocean-fixedseaice NOT CLEAN RESTART ANY MORE, 18/10/2004
#   igcm3-slabocean-slabseaice
# EVERYTHING R8:
#   igcm3-fixedocean-fixedseaice
#   igcm3-slabocean-slabseaice
#   igcm3-goldsteinocean-slabseaice
# I will add others I test them.  By 'test' I mean that after 1 day,
#   the weather is the same as in the 'EVERYTHING R4' version, to about 
#   1 part in 10^6 ish, and that the restarts are perfectly clean.
#####################################################################
#
ifeq ($(GENIEDP),TRUE)
  GENIEPREC_temp=$(FLAGR8)
else
  GENIEPREC_temp=$(FLAGR4)
endif

ifeq ($(IGCMATMOSDP),TRUE)
  IGCMATMOSPREC_temp=$(FLAGR8)
else
  IGCMATMOSPREC_temp=$(FLAGR4)
endif

# In time floating point precision for
# these componets could be controlled
# separately.
LIBAUX1PREC=$(IGCMATMOSPREC_temp)
LIBBLAS1PREC=$(IGCMATMOSPREC_temp)
LIBFFT1PREC=$(IGCMATMOSPREC_temp)
LIBNC1PREC_temp=$(IGCMATMOSPREC_temp)
LIBUTIL1PREC=$(IGCMATMOSPREC_temp)
LIBZXPPREC=$(IGCMATMOSPREC_temp)
FIXEDOCEANPREC=$(IGCMATMOSPREC_temp)
SLABOCEANPREC=$(IGCMATMOSPREC_temp)
FIXEDSEAICEPREC=$(IGCMATMOSPREC_temp)
SLABSEAICEPREC=$(IGCMATMOSPREC_temp)
FIXEDICESHEETPREC=$(IGCMATMOSPREC_temp)
FIXEDCHEMPREC=$(IGCMATMOSPREC_temp)
FIXEDATMOSOPTS_temp=$(FLAGR8)
FAKEATMOSOPTS_temp=$(FLAGR8)
ATCHEMPREC=$(FLAGR8)
BIOGEMPREC=$(FLAGR8)
SEDGEMPREC=$(FLAGR8)
ROKGEMPREC=$(FLAGR8)
LANDPRECOPT=$(GENIEPREC_temp)
EMBMATMOSPREC=$(FLAGR8)  #NOT YET IMPLEMENTED
GOLDOCEANPREC=$(FLAGR8)  #NOT YET IMPLEMENTED
GOLDSEAICEPREC=$(FLAGR8) #NOT YET IMPLEMENTED
#FIXEDLANDPREC=$(FLAGR8) #NOT YET IMPLEMENTED
WINDPREC=$(GENIEPREC_temp)
#
# The statements below ensure that the precisionopts correspond
# correctly to the precs above.  Note that the ifeq...else...endif
# syntax below is gnu-make (gmake) specific.  If you have trouble
# getting it to work, comment out the ifeq, etc. statements that you
# don't need.
# 
ifeq ($(LIBUTIL1PREC),$(FLAGR4))
LIBUTIL1PRECISIONOPTS=$(DEFINE)utreal4
else
LIBUTIL1PRECISIONOPTS=$(DEFINE)utreal8
endif
ifeq ($(LIBNC1PREC_temp),$(FLAGR4))
LIBNC1PRECISIONOPTS=$(DEFINE)ncreal4
else
LIBNC1PRECISIONOPTS=$(DEFINE)ncreal8
endif
ifeq ($(LIBZXPPREC),$(FLAGR4))
LIBZXPPRECISIONOPTS=$(DEFINE)zxreal4
else
LIBZXPPRECISIONOPTS=$(DEFINE)zxreal8
endif
ifeq ($(GENIEPREC_temp),$(FLAGR4))
GENIEPRECISIONOPTS=$(DEFINE)real4
else
GENIEPRECISIONOPTS=$(DEFINE)real8
endif
#
# These then add on the pre-processor options to those modules which
# need them.  
#
# ****This should really be in each individual makefile****
# 
FIXEDATMOSOPTS=$(FIXEDATMOSOPTS_temp) $(GENIEPRECISIONOPTS) $(FPPFLAGS)
FAKEATMOSOPTS=$(FAKEATMOSOPTS_temp) $(GENIEPRECISIONOPTS) $(FPPFLAGS)
LIBNC1PREC=$(LIBNC1PREC_temp) $(FPPFLAGS)
IGCMATMOSPREC=$(IGCMATMOSPREC_temp) $(LIBUTIL1PRECISIONOPTS) $(FPPFLAGS)
GENIEPREC=$(GENIEPREC_temp) $(LIBZXPPRECISIONOPTS) $(LIBNC1PRECISIONOPTS)
LANDPRECOPT += $(GENIEPRECISIONOPTS) $(LIBNC1PRECISIONOPTS) $(FPPFLAGS)
EMBMATMOSPREC += $(FPPFLAGS)
GOLDOCEANPREC += $(FPPFLAGS)
GOLDSEAICEPREC += $(FPPFLAGS)
#
# ----------------------------------------------------------------------
#
# How to get ordered list of objects to put in a library
#
ORDER_OBJECTS=*.$(OBJ_EXT)
#
# ----------------------------------------------------------------------
#
# Libraries needed for final fortran linkage
#
FORTRANLIBS=
#
# ----------------------------------------------------------------------
#

.c.$(OBJ_EXT):
	$(CC) -c $(CCFLAGS) $*.c

.f.$(OBJ_EXT):
	$(F77) $(COMPILEONLY) $(FFLAGS) $(F77FLAGS) $*.f
