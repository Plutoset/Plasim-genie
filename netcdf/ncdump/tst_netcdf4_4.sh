#!/bin/sh
# This shell script runs extra tests ncdump for netcdf-4
# Dennis Heimbigner, Ward Fisher

if test "x$srcdir" = x ; then srcdir=`pwd`; fi 
. ../test_common.sh

set -e

echo ""
echo "*** Running extra netcdf-4 tests."

#
# In windows, these tests fail because srcdir is prepended.
# e.g., Instead of 'ncdump ref_tst_compounds2' the file would
# contain:
#        'ncdump ./ref_tst_compounds2'. This causes the test to fail.
# But, 'srcdir' is necessary for make distcheck.
#
# Short term solution, use sed when on windows/MSYS to 
# remove the './','../../ncdump'.
#

echo "*** running tst_string_data to create test files..."
${execdir}/tst_string_data

if [ `uname | cut -d "_" -f 1` = "MINGW32" ]; then # MINGW Platforms

    echo "*** dumping tst_string_data.nc to tst_string_data.cdl..."
    ${NCDUMP} tst_string_data.nc > tst_string_data.cdl
    TMPNAME=`head -n 1 tst_string_data.cdl | cut -d" " -f 2`
    NEWNAME=`basename $TMPNAME`
    sed "s,$TMPNAME,$NEWNAME,g" tst_string_data.cdl > tmp.cdl
    mv tmp.cdl tst_string_data.cdl
    echo "*** comparing tst_string_data.cdl with ref_tst_string_data.cdl..."
    diff -b tst_string_data.cdl $srcdir/ref_tst_string_data.cdl

    echo '*** testing reference file ref_tst_compounds2.nc...'
    ${NCDUMP} $srcdir/ref_tst_compounds2.nc > tst_compounds2.cdl
    TMPNAME=`head -n 1 tst_compounds2.cdl | cut -d" " -f 2`
    NEWNAME=`basename $TMPNAME`
    sed "s,$TMPNAME,$NEWNAME,g" tst_compounds2.cdl > tmp.cdl
    mv tmp.cdl tst_compounds2.cdl
    diff -b tst_compounds2.cdl $srcdir/ref_tst_compounds2.cdl

    echo '*** testing reference file ref_tst_compounds3.nc...'
    ${NCDUMP} $srcdir/ref_tst_compounds3.nc > tst_compounds3.cdl
    TMPNAME=`head -n 1 tst_compounds3.cdl | cut -d" " -f 2`
    NEWNAME=`basename $TMPNAME`
    sed "s,$TMPNAME,$NEWNAME,g" tst_compounds3.cdl > tmp.cdl
    mv tmp.cdl tst_compounds3.cdl
    diff -b tst_compounds3.cdl $srcdir/ref_tst_compounds3.cdl

    echo '*** testing reference file ref_tst_compounds4.nc...'
    ${NCDUMP} $srcdir/ref_tst_compounds4.nc > tst_compounds4.cdl
    TMPNAME=`head -n 1 tst_compounds4.cdl | cut -d" " -f 2`
    NEWNAME=`basename $TMPNAME`
    sed "s,$TMPNAME,$NEWNAME,g" tst_compounds4.cdl > tmp.cdl
    mv tmp.cdl tst_compounds4.cdl
    diff -b tst_compounds4.cdl $srcdir/ref_tst_compounds4.cdl

else # Non-MINGW Platforms

    echo "*** dumping tst_string_data.nc to tst_string_data.cdl..."
    ${NCDUMP} tst_string_data.nc > tst_string_data.cdl
    echo "*** comparing tst_string_data.cdl with ref_tst_string_data.cdl..."
    diff -b tst_string_data.cdl $srcdir/ref_tst_string_data.cdl

#echo '*** testing non-coordinate variable of same name as dimension...'
#${NCGEN} -v4 -b -o tst_noncoord.nc $srcdir/ref_tst_noncoord.cdl

    echo '*** testing reference file ref_tst_compounds2.nc...'
    ${NCDUMP} $srcdir/ref_tst_compounds2.nc > tst_compounds2.cdl
    diff -b tst_compounds2.cdl $srcdir/ref_tst_compounds2.cdl

    echo '*** testing reference file ref_tst_compounds3.nc...'
    ${NCDUMP} $srcdir/ref_tst_compounds3.nc > tst_compounds3.cdl
    diff -b tst_compounds3.cdl $srcdir/ref_tst_compounds3.cdl

    echo '*** testing reference file ref_tst_compounds4.nc...'
    ${NCDUMP} $srcdir/ref_tst_compounds4.nc > tst_compounds4.cdl
    diff -b tst_compounds4.cdl $srcdir/ref_tst_compounds4.cdl

    # Exercise Jira NCF-213 bug fix
#    rm -f tst_ncf213.cdl tst_ncf213.nc
    ${NCGEN} -b -o tst_ncf213.nc $srcdir/ref_tst_ncf213.cdl
    ${NCDUMP} -s -h tst_ncf213.nc \
    | sed -e 's/netcdflibversion=.*[|]/netcdflibversion=0.0.0|/' \
    | sed -e 's/hdf5libversion=.*"/hdf5libversion=0.0.0"/' \
    | sed -e 's|_SuperblockVersion = [0-9]|_SuperblockVersion = 0|' \
    | cat  >tst_ncf213.cdl
    # Now compare
    ok=1;
    if diff -b $srcdir/ref_tst_ncf213.cdl tst_ncf213.cdl ; then ok=1; else ok=0; fi
    # cleanup
#    rm -f tst_ncf213.cdl tst_ncf213.nc
    if test $ok = 0 ; then
      echo "*** FAIL: NCF-213 Bug Fix test"
      exit 1
    fi
fi

echo "*** All ncgen and ncdump extra test output for netCDF-4 format passed!"
exit 0

