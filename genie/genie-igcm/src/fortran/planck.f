      REAL FUNCTION PLANCK (IBND,TEMP)
C     
C     This function returns the Planck function, integrated over a wide
C     band at specific temperature
C     
C     INPUT:  Band index (IBND)
C     Temperature (TEMP)
C     OUTPUT: Planck Function, integrated over band IBND at
C     temperature TEMP (PLANCK)
C     The integrated Planck function is tabulated for each band in the
C     array B in plfunc, over a range of temps. from 151 to 350 K,
C     with a step of 1 K. A simple linear interpolation in temperature
C     is employed.
C--------------------------------------------------
C     WARNING: The function works only for temperatures
C     between 151 and 350 K
C--------------------------------------------------

      IMPLICIT NONE
#include "parray.cmn"
      include 'files.cmn'
C
C     The SAVE below was assumed.  Need to specify for modern (F90) compilers.
C     The var 'B', for example is loaded only and but expected to be there
C     for subsequent calls to the function!
C 
      SAVE
C     
C     Input Variables
C     
      INTEGER IBND,IFAIL
      REAL TEMP,TEMP1,TEMP2
C     
C     Internal Variables
C     
      INTEGER IPF               ! Table element index
      REAL SLP,INTCPT           ! Variables used for interpolation
      REAL B(0:MXBAND,200)      ! Planck function pre-computed values
      integer ncid

      INTEGER IFIRST
      DATA IFIRST/1/

C     READ in planck function
      IF (IFIRST.EQ.1) THEN
         IFIRST=0
!-------------------------------------------
!     open the planck function file
!-------------------------------------------
         print*,' Opening ',fname_plfunc(1:ifname_plfunc)
         call open_file_nc(fname_plfunc(1:ifname_plfunc), ncid)
         call get2d_data_nc(ncid,'b',mxband+1,200,b,ifail)
         call close_file_nc(fname_plfunc(1:ifname_plfunc), ncid)
      ENDIF

C-------------------------
C     Band 0:    0-3000 cm-1
C     Band 1:   540-820 cm-1
C     Band 2:  980-1100 cm-1
C     Band 3: 1200-1400 cm-1
C     Band 4: 2100-2300 cm-1
C     Band 5:   900-980 cm-1
C     Band 6:   650-770 cm-1
C     Band 7:   550-630 cm-1
C     Band 8: 1100-1200 cm-1
C     Band 9:   820-900 cm-1
C-------------------------

C     Catch temperatures that are out of bounds
      IF(TEMP.LT.151.0.OR.TEMP.GT.350.0)THEN
         print *,'WARNING: TEMP out of bounds for Planck function'
         print *,'model will crash. TEMP= ',TEMP
      ENDIF

      IPF=INT(TEMP-150.)
      TEMP1=AINT(TEMP)
      TEMP2=TEMP+1.

      IF ((TEMP-TEMP1).LE.0.01) THEN ! No need to interpolate
         PLANCK=B(IBND,IPF)
      ELSE                      ! Linear interpolation
         SLP=(B(IBND,IPF)-B(IBND,IPF+1))/(TEMP1-TEMP2)
         INTCPT=TEMP1*B(IBND,IPF+1)-TEMP2*B(IBND,IPF)
         INTCPT=INTCPT/(TEMP1-TEMP2)
         PLANCK=SLP*TEMP+INTCPT
      END IF

      END
