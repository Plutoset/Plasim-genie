#

#

# ====================================================================
#
#          === PLEASE MAKE YOUR EDITS IN THIS FILE ===
#
# These edits may include the name of your Fortran or C compiler, 
# the location of your version of the NetCDF libraries, or GLIMMER 
# icesheet component.
#
# Please note that in addition to editing this file, variables can be
# overridden on the command line, e.g.:
#       make BUILD=SHIP F77=pgf90 MACHINE=SGI
#
# Note also, however that to _ADD_ to compiler flags, such as
# LDFLAGS, one should set GENIE_LDFLAGS to your desired
# additions, e.g.:
#       make GENIE_LDFLAGS=-L/path/to/mylibs
# LDFLAGS is then seeded with GENIE_LDFLAGS--this is done
# because command line overrides are immutable. 
#
# (genie_example.job uses the command-line override machanism to
# pass compile-time configuration options through to makefile.arc
# from a .config file)
#
# ====================================================================

# === Compre the values below to those in user.sh ===
GENIE_ROOT        =  /genie
OUT_DIR           =  /genie_output
RUNTIME_ROOT      =  ../../genie
#GENIE_ROOT        = $(HOME)/genie/
#OUT_DIR           = $(GENIE_ROOT)/genie-experiments
#RUNTIME_ROOT      = ../..

# === Fortran compiler (ifc/ifort/f90/pgf90) ===
F77=ifort
#F77=ifc
#F77=f90
#F77=f95
#F77=pgf90
#F77=pathf90
#F77=gfortran
#F77=g95
#F77=f90.exe
#F77=ifort.exe
#F77=gfc.exe
#F77=g95.exe

# === C Compiler (gcc/cc) ===
CC=icc
CXX=icpc
#CXX=cl.exe
#CC=cc

# === Build type (NORMAL/SHIP/DEBUG/PROFILE/BOUNDS) ===
BUILD=NORMAL
#BUILD=SHIP
#BUILD=DEBUG
#BUILD=PROFILE
#BUILD=BOUNDS

# === Machine type (LINUX/SLOARIS/SGI) ===
MACHINE=LINUX
#MACHINE=SGI
#MACHINE=SOLARIS
#MACHINE=WIN32
#MACHINE=OSX

# === Graphics (ON/OFF) ===
#GRAPHICS=ON
GRAPHICS=OFF

# === Glimmer integration ===
FLAG_GLIMMER=OFF
#FLAG_GLIMMER=ON
GLIMMER_DIR=$(HOME)/glimmer-genie-version

# === MosesTriffid integration ===
FLAG_MOSESTRIFFID=OFF
#FLAG_MOSESTRIFFID=ON

# === Extension for module files ===
MODEXT=mod
#MODEXT=d

# === NetCDF library ===
# Please edit 'NETCDF_DIR' to point to the top-level location
# of the NetCDF library. 'NETCDF_NAME' is the name of the library file
# (minus lib prefix and .a suffix for Unix/Linux)
# The locations of the library file and the .mod file for the
# f90 interface are then determined by appending the lower-level lib
# and include directories onto this stem.
# (http://www.unidata.ucar.edu/packages/netcdf/index.html)

#NETCDF_DIR=/usr/local/netcdf
#NETCDF_DIR=/opt/local/CentOS-64/netcdf/3.6.2/sun_fc_12
#NETCDF_DIR=/opt/local/CentOS-64/netcdf/4.0/gnu_fc_gfortran
#NETCDF_DIR=/usr/local/netcdf
#NETCDF_DIR=/usr/local/netcdf-3.6.1
#NETCDF_DIR=/nerc/packages/netcdf/3.4
#NETCDF_DIR=/usr/local/netCDF
NETCDF_DIR=/usr/local
#NETCDF_DIR=D:\\NetCDF\\netcdf-3.6.1
#NETCDF_DIR=/cygdrive/d/NetCDF/netcdf-3.6.2
#NETCDF_DIR=/esdata/env/pvp06gzu/netcdf-3.6.1
#NETCDF_DIR=/esdata/env/pvp06gzu/netcdf
#NETCDF_DIR=/cvos/apps/netcdf-4.0

NETCDF_C_LIB_NAME=netcdf
NETCDF_FORT_LIB_NAME=netcdff
NETCDF_CXX_LIB_NAME=netcdf_c++
#NETCDF_NAME=netcdf.lib
NETCDF_NAME=netcdf

# === Floating point precision ===
GENIEDP=FALSE
#GENIEDP=TRUE
IGCMATMOSDP=FALSE
#IGCMATMOSDP=TRUE
