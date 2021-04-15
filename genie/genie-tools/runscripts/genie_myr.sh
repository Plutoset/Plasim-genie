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
module add pgi/7.0.7
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
#ROOT=$PWD/../../..
#for purposes of keeping output pathnames short for netCDF:
ROOT=`echo "$PWD/../../.." | sed 's\/genie/genie-tools/runscripts/../../..\/genie/..\'`
PATCHES=$ROOT/genie_configpatches
SCRIPTSDIR=$PWD
#OUTDIR=$ROOT/genie_output/$MODELID.$RUNID
#for purposes of keeping output pathnames short for Mathematica:
NEWOUTDIR=`echo "$MODELID.$RUNID" | sed 's/genie_eb_go_gs_ac_bg_sg_//'`
NEWOUTDIR=`echo "$NEWOUTDIR" | sed 's/ensemble_09//'`
NEWOUTDIR=`echo "$NEWOUTDIR" | sed 's/preindustrial_fullCC_//'`
OUTDIR=$ROOT/genie_output/$NEWOUTDIR
ARCDIR=$ROOT/genie_archive
RUNLOG=$ROOT/genie_runlog
#
# make if loop for repeat running until runlength is reached
#
if [ $J -lt $RUNLENGTH ]
  then
	# time the gap between script submissions - if too short then model has crashed - see 'after' above
          before="$(date +%s)"
          # write in start year
	echo ">> Replacing simulation start year in goin with $J"
  	if [ -e $PATCHES/$RUNID ]
          then
              echo "" >> $PATCHES/$RUNID
	     echo "bg_par_misc_t_start=$J.0" >> $PATCHES/$RUNID
              echo "rg_start_year=$J.0" >> $PATCHES/$RUNID
              echo "sg_start_year=$J.0" >> $PATCHES/$RUNID
          else
              touch $PATCHES/$RUNID
              echo "" >> $PATCHES/$RUNID
              echo "sg_start_year=$J.0" >> $PATCHES/$RUNID
              echo "rg_start_year=$J.0" >> $PATCHES/$RUNID
              echo "bg_par_misc_t_start=$J.0" >> $PATCHES/$RUNID   
          fi
	#
	# run the model
	# (run from restart unless first run)
	if [ $J -eq 0 ]
	then
		cd $SCRIPTSDIR
  		echo "running rungenie script..."
  		if [ -n "$6" ]
  		# if continuing select restart id:
  		then
                    echo "./rungenie.sh $MODELID $RUNID $MAXYEARS $RESTARTPATH"
  			/bin/bash rungenie.sh $MODELID $RUNID $MAXYEARS $RESTARTPATH
  		# otherwise run from scratch:
  		else
                    echo "./rungenie.sh $MODELID $RUNID $MAXYEARS"
  			/bin/bash rungenie.sh $MODELID $RUNID $MAXYEARS
  		fi
                    # create archive direcory
                    mkdir $ARCDIR/$MODELID.$RUNID
	else	
		cd $SCRIPTSDIR
  		echo "running rungenie script..."
                    echo "./rungenie.sh $MODELID $RUNID $MAXYEARS $OUTDIR"
  			/bin/bash rungenie.sh $MODELID $RUNID $MAXYEARS $OUTDIR
	fi
	#
	# increment J by $MAXYEARS
	J=`expr $J + $MAXYEARS`
	# adjust newid for submission script name (make condensed so can see in job list)
	NEWRUNID=`echo "$MODELID.$RUNID" | sed 's/genie_eb_go_gs_ac_bg_sg_rg.//'`
	NEWRUNID=`echo "$NEWRUNID" | sed 's/ensemble_09/e/'`
          NEWRUNID=`echo "$NEWRUNID" | sed 's/preindustrial_fullCC_//'`
          #NEWID=`echo "$MODELID.$RUNID"`
          #./nameshortening.sh $RUNID
          #NEWRUNID=`cat $SCRIPTSDIR/shortname`
	NEWJ=`echo "$J" | sed s/000//`
	NEWID=$NEWRUNID"_"$NEWJ
	#
	# move output to save directory	
	cd $ARCDIR/fresh
	pwd
	mv -v $NEWOUTDIR.tar.gz $ARCDIR/$MODELID.$RUNID/$NEWOUTDIR.$J.tar.gz
	cd $SCRIPTSDIR
	pwd
	#
	# change J in $RUNLOG/$NEWID.sh script and resubmit job
	cp -f qsub_rungenie_temp.sh $RUNLOG/$NEWID.sh
	echo "cd $SCRIPTSDIR" >> $RUNLOG/$NEWID.sh
	echo "pwd" >> $RUNLOG/$NEWID.sh
  	echo "/bin/bash genie_myr.sh $MODELID $RUNID $RUNLENGTH $MAXYEARS $J" >> $RUNLOG/$NEWID.sh
	cat $RUNLOG/$NEWID.sh
          after="$(date +%s)"
          # if elapsed seconds between script submissions is too short then model has crashed so exit
          elapsed_seconds="$(expr $after - $before)"
          if [ $elapsed_seconds -lt 10 ]
          then 
             echo "model has most likely crashed - only $elapsed_seconds seconds between successive script submits!"
             exit
          fi
	qsub $RUNLOG/$NEWID.sh
  else
	echo "Yo! it's finished - Did it work?.."
	echo "Cleaning up run logs..."
	cp -f $SCRIPTSDIR/qsub_rungenie_temp.sh $SCRIPTSDIR/qsub_sort_runlog.sh
	echo "pwd" >> $SCRIPTSDIR/qsub_sort_runlog.sh
	echo "/bin/bash sort_runlog_date.sh $RUNLOG" >> $SCRIPTSDIR/qsub_sort_runlog.sh
	cat $SCRIPTSDIR/qsub_sort_runlog.sh
	qsub $SCRIPTSDIR/qsub_sort_runlog.sh
    	exit	
fi
