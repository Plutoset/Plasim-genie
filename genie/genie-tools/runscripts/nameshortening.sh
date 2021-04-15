#!/bin/bash
#$ -q short.q
#$ -cwd
#$ -j y
#$ -o ../../../genie_runlog/
#
#for purposes of keeping output pathnames short for Mathematica
if [ -e shortname ]
   then
   rm -f shortname
fi	
NEWID=`echo "$1" | sed 's/genie_eb_go_gs_ac_bg_sg_//'`
NEWID=`echo "$NEWID" | sed 's/ensemble_09//'`
NEWID=`echo "$NEWID" | sed 's/preindustrial_fulCC/preindust/'`
echo "$NEWID" > shortname
#
