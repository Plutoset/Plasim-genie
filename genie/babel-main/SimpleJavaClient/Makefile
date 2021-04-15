#
# $Id: Makefile 3056 2006-11-27 19:34:16Z sp1003 $
#
#
##################################################################
#                    MAIN CLIENT eb_go_gs                        #
##################################################################

include ../makefile.arc
include ../../genie-main/makefile.arc
include ../../genie-main/configs/genie_eb_go_gs_mine.config

##################################################################
#                      Please do not edit                        #
##################################################################

OBJS = ${TOP_CLASS:.java=.class}
INCLUDE_PATHS=${BABEL_HOME}/lib/sidl-0.11.0.jar

#========================== Targets =============================#

.SUFFIXES: .java .class

.java.class:
	javac -classpath ${CLASSPATH}:${INCLUDE_PATHS}:${WRAPPERS_PATHS} $< 

top: ${OBJS}

clean:
	rm -f *.class
copy:
	rm -f goin* ; echo ${OUT_DIR} ; echo ${EXPID}; cp ${OUTPUT}/goin_EMBM ${OUTPUT}/goin_GOLD ${OUTPUT}/goin_SIC .

run: top copy
	SIDL_DLL_PATH=${SIDL_DLL_PATH} && \
	export SIDL_DLL_PATH && \
	time java -classpath ${CLASSPATH}:${INCLUDE_PATHS}:${WRAPPERS_PATHS} \
	-Djava.library.path=${LD_LIBRARY_PATH}:${BABEL_HOME}/lib Client