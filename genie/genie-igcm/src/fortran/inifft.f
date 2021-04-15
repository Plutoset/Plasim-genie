      SUBROUTINE INIFFT(TRIG,IFAX,CMPA,MM,MOCT,NHEM,MG)
      IMPLICIT NONE
      REAL TRIG(*)
      INTEGER IFAX(*),MM,MOCT,NHEM,MG
      COMPLEX CMPA(*)
C
C     SETS UP ARRAYS FOR USE IN FFT ROUTINES ETC
C
      INTEGER I,NROW,MP,IDL,IGL
C
      IDL=(MG+2)/2
      IGL=IDL*NHEM
C
      DO 10 I=1,IGL
10    CMPA(I)=0.
      NROW=0
      DO 20 MP=1,MM,MOCT
         NROW=NROW+1
20       CMPA(NROW)=CMPLX(0.,FLOAT(MP-1))
      IF(NHEM.EQ.2)THEN
         NROW=0
         DO 30 MP=1,MM,MOCT
            NROW=NROW+1
30          CMPA(NROW+IDL)=CMPA(NROW)
      ENDIF
c      CALL SET99(TRIG,IFAX,MG)
      CALL RFFTI(MG,TRIG,IFAX)
      RETURN
      END
C
      SUBROUTINE INIFFT8(TRIG,IFAX,CMPA,MM,MOCT,NHEM,MG)
      IMPLICIT NONE
      REAL*8 TRIG(*)
      INTEGER IFAX(*),MM,MOCT,NHEM,MG
      COMPLEX*16 CMPA(*)
C
C     SETS UP ARRAYS FOR USE IN FFT ROUTINES ETC
C
      INTEGER I,NROW,MP,IDL,IGL
C
      IDL=(MG+2)/2
      IGL=IDL*NHEM
C
      DO 10 I=1,IGL
10    CMPA(I)=0.
      NROW=0
      DO 20 MP=1,MM,MOCT
         NROW=NROW+1
20       CMPA(NROW)=CMPLX(0.,FLOAT(MP-1))
      IF(NHEM.EQ.2)THEN
         NROW=0
         DO 30 MP=1,MM,MOCT
            NROW=NROW+1
30          CMPA(NROW+IDL)=CMPA(NROW)
      ENDIF
c      CALL SET99(TRIG,IFAX,MG)
      CALL RFFTI8(MG,TRIG,IFAX)
      RETURN
      END