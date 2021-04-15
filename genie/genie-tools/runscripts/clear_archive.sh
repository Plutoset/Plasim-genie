#!/bin/bash
#
# Script to clear out archive folders of tarballs, keeping only the last one (for use when data appending)
# by Greg Colbourn (g.colbourn@uea.ac.uk)
#
date
#
#ROOT=$PWD/../../..
#for purposes of keeping output pathnames short for netCDF:
ROOT=`echo "$PWD/../../.." | sed 's\/genie/genie-tools/runscripts/../../..\/genie/..\'`
SCRIPTSDIR=$PWD
EXECUTEROOT=$ROOT/genie_archive/to_clearout
ARCHIVE=$ROOT/genie_archive
PREFIX=genie
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
	ENDYEAR=`sort years -gr | head -n 1`
          #ENDYEAR=80000
	echo "end year = $ENDYEAR"
	#
	cd $EXECUTEROOT/$RUNID
	chmod 755 *
	FILENAME=$NEWID.$ENDYEAR.tar.gz
	if [ ! -e "$FILENAME" ]    # Check if file exists.
	then
		continue           # On to next.
	fi
	#move tarball to archive folder
          mv -f $FILENAME $ARCHIVE
          #clear out archive of the rest of the tarballs (nothing should be lost if append data is used, apart from option to restart at earlier time)
          rm $EXECUTEROOT/$RUNID/*
          #move kept tarball back
          mv -f $ARCHIVE/$FILENAME $EXECUTEROOT/$RUNID
done	
# move archive folders back to right place
mv $EXECUTEROOT/* $EXECUTEROOT/../
