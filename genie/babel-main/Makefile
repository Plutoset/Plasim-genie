##################################################################
#                      WRAP ALL COMPONENTS                       #
##################################################################

include makefile.arc
include ../genie-main/makefile.arc

EMBM_PATH = ${GENIE_ROOT}/genie-embm
GOLDSTEIN_PATH = ${GENIE_ROOT}/genie-goldstein
SEAICE_PATH = ${GENIE_ROOT}/genie-seaice
LAND_PATH = ${GENIE_ROOT}/genie-land


##################################################################
#                       MORE SETTINGS                            #
##################################################################

all:
	cd ${EMBM_PATH}/babel ; ${MAKE}
	cd ${GOLDSTEIN_PATH}/babel ; ${MAKE}
	cd ${SEAICE_PATH}/babel ; ${MAKE}
	cd ${LAND_PATH}/babel ; ${MAKE}


cleanall:
	cd ${EMBM_PATH}/babel ; ${MAKE} clean
	cd ${GOLDSTEIN_PATH}/babel ; ${MAKE} clean
	cd ${SEAICE_PATH}/babel ; ${MAKE} clean
	cd ${LAND_PATH}/babel ; ${MAKE} clean
