*DECK LTIDT
C**********************************************************
C             SUBROUTINE LTIDT
C**********************************************************
      SUBROUTINE LTIDT

      IMPLICIT NONE

C
C     Inverse Legendre transform, from spectral to Fourier space
C     for the derivatives of temperature required to calculate PV.
C     HEXP transforms fields having the same symmetry and type of
C     Legendre function.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'gridp3.cmn'
      include 'legau.cmn'
      include 'polyno.cmn'
      include 'spectr.cmn'

      INTEGER L,I
      REAL    GRPAD

C
C     Calls to HEXPFL give following Fourier fields :
C        CHIG :   x-derivative of temperature field
C        SFG  :   y-derivative of temperature field
C     These are only the temporary contents for PV calculation.
C
      DO 10 L=1,NL
         DO 20 I=1,IGL
            CHIG(I,L)=0.
            SFG(I,L)=0.
 20      CONTINUE
 10   CONTINUE
      CALL HEXP(T,CHIG,NL,2)
      CALL HEXP(T,SFG,NL,4)
      DO 30 L=1,NL
         DO 40 I=1,IGL
            CHIG(I,L)=CMPA(I)*CHIG(I,L)
 40      CONTINUE
 30   CONTINUE
C
      RETURN
      END
