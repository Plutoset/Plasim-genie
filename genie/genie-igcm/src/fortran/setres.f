*DECK SETRES
C**********************************************************
C             SUBROUTINE SETRES
C**********************************************************
      SUBROUTINE SETRES

      IMPLICIT NONE

C
#include "param1.cmn"
      include 'param2.cmn'
      include 'spectr.cmn'
      include 'restor.cmn'

      INTEGER IHEM,I,IR,J,L

C
C     Set up restoration state from the KOUNT=0 zonally averaged
C     state and write this to FT13 for future use.
C     This is only done when KOUNT=0 and DAMP.GT.0.0.
C
 2200 FORMAT(/' RESTORATION RECORD WRITTEN TO CHANNEL ',I3)
C
      IF (DAMP.LE.0) RETURN
C
      DO 840 IHEM=1,NHEM
         I=NWJ2*(IHEM-1)
         IR=IDM*(IHEM-1)
         DO 100 J=1,IDM
            I=I+1
            IR=IR+1
            SPRES(IR)=real(SP(I))
  100    CONTINUE
         DO 850 L=1,NL
            I=NWJ2*(IHEM-1)+(L-1)*IGA
            IR=IDM*(IHEM-1)+(L-1)*IGM
            DO 860 J=1,IDM
               I=I+1
               IR=IR+1
               ZRES(IR)=real(Z(I))
               DRES(IR)=real(D(I))
               TRES(IR)=real(T(I))
  860       CONTINUE
  850    CONTINUE
  840 CONTINUE
C
      REWIND 13
      WRITE(13)ZRES,DRES,TRES,SPRES
      WRITE(6,2200)13
C
      END
