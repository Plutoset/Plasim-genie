
*DECK NOISE
C**********************************************************
C             SUBROUTINE NOISE
C**********************************************************
      SUBROUTINE NOISE

      IMPLICIT NONE

C
C     Adds white noise perturbation to ln(surface pressure)
C     balanced initial state at T=0.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'spectr.cmn'
      include 'outcon.cmn'

      INTEGER IBAS,IEND,IDUM,IHEM,I
      REAL    EPS,SCALE,ZR1,RANF,ZR2

       EXTERNAL RANF
  200 FORMAT(' WHITE NOISE SURFACE PRESSURE PERTURBATION AT T=0'/)
C
C     Eps sets magnitude of the noise
C
      EPS=1.E-4
      WRITE (6,200)
      SCALE=EPS/SQRT(2.0)
      IBAS=IDM+1
      IEND=NWJ2
      IDUM=-1
      DO 800 IHEM=1,NHEM
         DO 10 I=IBAS,IEND
            ZR1=RANF(IDUM)-0.5
            ZR2=RANF(IDUM)-0.5
            SP(I)=SP(I)+SCALE*CMPLX(ZR1,ZR2)
            SPMI(I)=SP(I)
10       CONTINUE
         IBAS=IBAS+NWJ2
         IEND=IEND+NWJ2
800   CONTINUE
C
      RETURN
      END
