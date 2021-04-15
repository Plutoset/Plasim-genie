#
# $Id: makefile.arc 3053 2006-11-27 19:28:55Z sp1003 $
#
##################################################################
#                       Please edit below                        #
##################################################################

# ============= Settings for callee side glue build ==============

F90LINK                    = ${F77_LD} -shared


# ============= Settings for caller side glue build ==============

LINKER                    = gcc -shared


##################################################################
#                   Please do not edit below                     #
##################################################################

# ============== Settings for callee side glue build =============

GENIE_LIB_INCLUDE          = -L${GENIE_ROOT}/genie-lib/libnc1/ -lnc1 \
                             -L${GENIE_ROOT}/genie-lib/libutil1 -lutil1 \
                             -L${GENIE_ROOT}/genie-lib/libaux1 -laux1 \
                             -L${GENIE_ROOT}/genie-lib/libblas1 -lblas1 \
                             -L${GENIE_ROOT}/genie-lib/libfft1 -lfft1 

NETCDF_INCLUDE             = -L${NETCDF_DIR}/lib -lnetcdf

LINK_LIBS_IMPL             = `${BABEL_HOME}/bin/babel-config --libs-c-client`

# =================== Settings for caller side glue build =======

TOP_CLASS                  = Client.java

OUTPUT                     = ${OUT_DIR}/${EXPID}

JNI_INCLUDES               = `${BABEL_HOME}/bin/babel-config --includes-jni`

# =================== Settings for the experiment run ============

SIDL_DLL_PATH              = "${BABEL_HOME}/lib;${GENIE_ROOT}/genie-goldstein/babel/wrapper;${GENIE_ROOT}/genie-embm/babel/wrapper;${GENIE_ROOT}/genie-seaice/babel/wrapper;${GENIE_ROOT}/genie-land/babel/wrapper;"

WRAPPERS_PATHS             = ${GENIE_ROOT}/genie-goldstein/babel/wrapper:${GENIE_ROOT}/genie-embm/babel/wrapper:${GENIE_ROOT}/genie-seaice/babel/wrapper:${GENIE_ROOT}/genie-land/babel/wrapper
