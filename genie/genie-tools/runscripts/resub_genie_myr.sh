#!/bin/bash
#
# Script to restart ensemble run that is half-way through
# by Greg Colbourn (g.colbourn@uea.ac.uk)
#
date
#
#ROOT=$PWD/../../..
#for purposes of keeping output pathnames short for netCDF:
ROOT=`echo "$PWD/../../.." | sed 's\/genie/genie-tools/runscripts/../../..\/genie/..\'`
SCRIPTSDIR=$PWD
EXECUTEROOT=$ROOT/genie_archive/to_resub
ARCHIVE=$ROOT/genie_archive
OUTPUTDIR=genie_output
MODELID=genie_eb_go_gs_ac_bg_sg_rg
PREFIX=genie
RUNLENGTH=18
MAXYEARS=2
#
if [ -e resub_genie_myr_exe.sh ]
   then
   rm -f resub_genie_myr_exe.sh
fi
cp -f $SCRIPTSDIR/qsub_rungenie_temp.sh $SCRIPTSDIR/resub_genie_myr_exe.sh
echo "pwd" >> resub_genie_myr_exe.sh
echo ". /etc/profile.d/modules.sh" >> resub_genie_myr_exe.sh
echo "module add shared sge" >> resub_genie_myr_exe.sh
#
if [ -e runids ]	
	then   		
	rm -f runids
fi
if [ -e configs ]	
	then   		
	rm -f configs
fi	
cd $EXECUTEROOT
ls -l | grep $PREFIX |
awk '	{print ""$9""}' > $SCRIPTSDIR/runids
sed 's\[.]\ \1' $SCRIPTSDIR/runids > $SCRIPTSDIR/configs
J=0
NRUNS=`cat -n $SCRIPTSDIR/runids | tail -n1 | gawk '{print $1}'`
#NRUNS=`expr $NRUNS + 1`
while [ $J -lt $NRUNS ]
do
	J=`expr $J + 1`
	RUNID=`head -$J $SCRIPTSDIR/runids | tail -1`
          echo "$EXECUTEROOT/$RUNID"
	cd $EXECUTEROOT/$RUNID
	# clear pre-existing files	
	if [ -e tarballs ]
		then
   		rm -f tarballs
	fi
	if [ -e years ]
		then
   		rm -f years
	fi
	# Only tarballs bigger than a set size [1000000] are used - smaller ones signify a model crash
	ls -l |
	awk '	{if (NR > 1)
		{if ($5 > 1000000)
		{print ""$9""}}}' > $SCRIPTSDIR/tarballs
	cd $SCRIPTSDIR
          #follow shortening done in rungenie.sh
          ./nameshortening.sh $RUNID
          NEWID=`cat $SCRIPTSDIR/shortname`
	sed "s/$NEWID.//g 
	     s/.tar.gz//g " tarballs > years
	STARTYEAR=`sort years -gr | head -n 1`
          #STARTYEAR=80000
	echo "start year = $STARTYEAR"
	#
	CONFIG=`head -$J $SCRIPTSDIR/configs | tail -1`
	echo "config = $CONFIG"
	#
	cd $EXECUTEROOT/$RUNID
	chmod 755 *
	FILENAME=$NEWID.$STARTYEAR.tar.gz
	if [ ! -e "$FILENAME" ]    # Check if file exists.
	then
		continue           # On to next.
	fi
	#unzip tarball
	gunzip -c $FILENAME | tar xvf - 
	#move restart files to genie_output
	echo "results folder = $NEWID"
          cd $ROOT/$OUTPUTDIR
          chmod 755 *
	rm -rf $NEWID
	cd $EXECUTEROOT/$RUNID
	mv -f $NEWID $ROOT/$OUTPUTDIR/
	# write to the generated script
	cd $SCRIPTSDIR
	echo "/bin/bash qsub_rungenie_2.sh $CONFIG $RUNLENGTH $MAXYEARS $STARTYEAR $ROOT/$OUTPUTDIR/$NEWID" >> resub_genie_myr_exe.sh
          echo "sleep 3" >> resub_genie_myr_exe.sh
done	
# run the generated script
cd $SCRIPTSDIR
chmod 755 resub_genie_myr_exe.sh
qsub resub_genie_myr_exe.sh
echo "all `expr $NRUNS` resubmitted"
# move archive folders back to right place
mv $EXECUTEROOT/* $EXECUTEROOT/../
