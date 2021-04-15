*DECK PVCR
C**********************************************************
C             SUBROUTINE PVCR
C**********************************************************
      SUBROUTINE PVCR(TFIELD,TTYPE)

      IMPLICIT NONE

C
C     Calculate potential temperature and Ertel potential vorticity
C     on model levels.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'comgrm.cmn'
      include 'gridp2.cmn'
      include 'legau.cmn'

      INTEGER L,II,IHEM,IOF,I
      REAL    PMKG,PM1KG,AKT,TXP,TYP

      REAL PSG(IGC,NL),TFIELD(IGC,NL)
      INTEGER TTYPE
C
C     Calculate pressure on model levels.
C
      DO 10 L=1,NL
         DO 11 II=1,IGC
            PSG(II,L)=SIGMA(L)*PLG(II)
 11      CONTINUE
 10   CONTINUE
C
      DO 100 IHEM=1,NHEM
         IOF=(IHEM-1)*MGPP
C
C     Calculate potential temperature and PV on model levels.
C     Generalised formula using array of model level pressures is used
C     to calculate potential temperature.  But note that the reference
C     pressure for adiabatic processes is assumed to be P0, the non-
C     dimensionalising pressure.
C
C     TFIELD is the tracer field to be initialised.
C     TTYPE=1   : Initialise tracer as potential temperature.
C     TTYPE=2   : Initialise tracer as Ertel PV.
C
C     Remember that at this point:-
C     CHIG contains the x-derivative of T.
C     SFG  contains the y-derivative of T.
C
         DO 20 L=1,NL
            DO 30 I=1,MG
               II=I+IOF
               PMKG=PSG(II,L)**(-AKAP)
               PM1KG=PMKG/PSG(II,L)
               AKT=AKAP*TG(II,L)
               TXP=CHIG(II,L)-AKT*PMG(II)
               TYP=SFG(II,L)-AKT*PJG(II)
               IF(TTYPE.EQ.1) THEN
                  TFIELD(II,L)=TG(II,L)*PMKG
               ELSE
                  TFIELD(II,L)=PM1KG*(ZG(II,L)*(-DTDLSG(II,L)+AKT)
     :                 +SECSQ(JH)*(-DUDLSG(II,L)*TYP+DVDLSG(II,L)*TXP))
               ENDIF
 30         CONTINUE
 20      CONTINUE
 100  CONTINUE
C
      RETURN
      END
