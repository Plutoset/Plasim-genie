*DECK SETTEE
C**********************************************************
C             SUBROUTINE SETTEE
C**********************************************************
      SUBROUTINE SETTEE

      IMPLICIT NONE

C
C     Subroutine to give annual cycle of TRES if wanted
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'outcon.cmn'
      include 'restij.cmn'

      INTEGER IADB,L,IAD
      REAL    YPHS

C
C     If YRLEN is zero then no seasonal cycle.
C
      IF (NINT(YRLEN) .EQ. 0) THEN
         YPHS = (1./SQRT(6.))
      ELSE
         YPHS=(1./SQRT(6.))*SIN(PI2*DAY/YRLEN)
      ENDIF
      IADB=NWJ2+1
      DO 10 L=1,NL
        IAD=IADB+(L-1)*IGA
        TTRES(IAD)=FAC(L)*DTNS*YPHS
10    CONTINUE
      RETURN
      END
