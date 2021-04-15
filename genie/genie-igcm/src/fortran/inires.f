*DECK INIRES
C************************************************************
C                  SUBROUTINE INIRES
C************************************************************
      SUBROUTINE INIRES

      IMPLICIT NONE

C
C     Sets up restoration variables and arrays. Sets NAMELIST
C     variables to their default settings, then reads NAMELIST
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'restor.cmn'

      REAL RESTIM
      integer ios
C
      NAMELIST/INPRS/ RESTIM
C
      RESTIM=0.0
C
      read(UNIT=7,NML=INPRS,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INPRS namelist'
         stop
      end if
      WRITE(6,INPRS)
C
C     Dimensionless coefficient for Newtonian cooling friction
C     and timestep. A day is 2*pi in non dimensional
C     units using omega as the unit of frequency.
C
      IF (RESTIM.GT.0.0) THEN
         DAMP=1.0/(PI2*RESTIM)
      ELSE
         DAMP=0.0
      ENDIF
C
      END
