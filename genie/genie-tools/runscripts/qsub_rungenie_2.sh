#!/bin/bash
#$ -q short.q
#$ -cwd
#$ -j y
#$ -o ../../../genie_runlog/
#
# Script to submit very long genie runs
# by Greg Colbourn (g.colbourn@uea.ac.uk)
#
# see runcupcke.sh for explanation of arguments
#
. /etc/profile.d/modules.sh
module add shared sge
#
date
#
# (1) GET PASSED PARAMETERS
# -------------------------
# [1] base configuration ID
if [ -z "$1" ]; then
    echo "Usage: '$1' 1st parameter must be the config ID e.g. genie_eb_go_gs_ac_bg"
    exit 65
  else
    MODELID="$1"
fi
# [2] set run ID (config patch file name and forcings sub-directory name)
if [ -z "$2" ]; then
    echo "Usage: '$2' 3rd parameter must be the run ID e.g. worbe2_preindustrial_1"
    exit 65
  else
    RUNID="$2"
fi
# [3] set run duration
if
 [ -z "$3" ]; then
    echo "Usage: '$3' 4th parameter must be the run length (years)"
    exit 65
  else
    RUNLENGTH="$3"
fi
# [4] set number of years in each chunk
if
 [ -z "$4" ]; then
    echo "Usage: '$4' 4th parameter must be the number of years in each chunk of model run"
    exit 65
  else
    MAXYEARS="$4"
fi
# [5] set year counter
if [ -z "$5" ]
  then
    echo "5th argument is year index to icrement should be set to 0"
    exit 60
  else
    J="$5"
fi
# [6] restart path (optional)
if [ -n "$6" ]; then
  RESTARTPATH="$6"
fi
#
echo "Arguments for genie_myr.sh: $MODELID $RUNID $RUNLENGTH $MAXYEARS $J $6"
#
# define root paths
SCRIPTSDIR=$PWD
#ROOT=$PWD/../../..
#for purposes of keeping output pathnames short for netCDF:
ROOT=`echo "$PWD/../../.." | sed 's\/genie/genie-tools/runscripts/../../..\/genie/..\'`
PATCHES=$ROOT/genie_configpatches
RUNLOG=$ROOT/genie_runlog
#
# submit genie_myr.sh script as qsub job
#
cp -f qsub_rungenie_temp.sh $RUNLOG/$MODELID.$RUNID.sh
echo "cd $SCRIPTSDIR" >> $RUNLOG/$MODELID.$RUNID.sh
echo "pwd" >> $RUNLOG/$MODELID.$RUNID.sh
echo "/bin/bash genie_myr.sh $MODELID $RUNID $RUNLENGTH $MAXYEARS $J $6" >> $RUNLOG/$MODELID.$RUNID.sh
echo " " >> $RUNLOG/$MODELID.$RUNID.sh
cat $RUNLOG/$MODELID.$RUNID.sh
qsub $RUNLOG/$MODELID.$RUNID.sh
#
echo "submitted $RUNLOG/$MODELID.$RUNID.sh"
#

