*DECK INIGAU
C**************************************************************
C                    SUBROUTINE INIGAU
C**************************************************************
      SUBROUTINE INIGAU

      IMPLICIT NONE

C
C     This subroutine calculates gaussian weights and latitudes
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'bats.cmn'
      include 'legau.cmn'

      INTEGER J,K,I,II,M,N
      REAL    WEIGHT

C
  230 FORMAT(/' *** ABORT *** PARAMETER JGL IS DIFFERENT FROM 1 OR JG')
  202 FORMAT(' GAUSSIAN LATITUDES')
  201 FORMAT(10F7.2)
C
      IF(JGL.NE.1.AND.JGL.NE.JG) THEN
         WRITE(6,230)
         CALL ABORT
      ENDIF
      IF(JGL.EQ.1) JINC=0
      IF(JGL.EQ.JG)JINC=1
      JL=1
      DO 8 J=1,JG
         JH=J
         CALL GWTLT(SI(J),WEIGHT,J,JG)
         SISQ(J)=SI(J)*SI(J)
         CSSQ(J)=1.-SISQ(J)
         SECSQ(J)=1./CSSQ(J)
         CS(J)=SQRT(CSSQ(J))
         ALAT(J)=ATAN(SI(J)/CS(J))*180.0/PI
         GWT(J)=WEIGHT/REAL(NHEM)
         AW(J)=WEIGHT*2.0*SECSQ(J)
C
C        Compute Legendre functions at the current latitude.
C
         CALL LGNDRE(NN,MM,MOCT,ALPJ,DALPJ,MJP,1,SI(JH),CS(JH))
C
C        Reorder Legendre functions, separating even/odd functions.
C
         DO 58 K=1,2
            I=0
            II=K-2
            DO 57 M=0,MM-1,MOCT
               DO 56 N=M,NN-1,2
                  I=I+1
                  II=II+2
                  ALP(I,K,JL)=ALPJ(II)
                  DALP(I,K,JL)=DALPJ(II)
                  RLP(I,K,JL)=-RSQ(N+K)*ALP(I,K,JL)
                  RDLP(I,K,JL)=-RSQ(N+K)*DALP(I,K,JL)
   56          CONTINUE
   57       CONTINUE
   58    CONTINUE
C
         IF(JGL.EQ.1) WRITE(65) ALP,DALP,RLP,RDLP
C
         JL=JL+JINC
    8 CONTINUE
C
      IF (NHEM.EQ.2) THEN
CDIR$    IVDEP
         DO 59 J=1,JG
            SI(JGGP-J)=-SI(J)
            CS(JGGP-J)=CS(J)
            SISQ(JGGP-J)=SISQ(J)
            CSSQ(JGGP-J)=CSSQ(J)
            SECSQ(JGGP-J)=SECSQ(J)
            ALAT(JGGP-J)=-ALAT(J)
            GWT(JGGP-J)=GWT(J)
            AW(JGGP-J)=AW(J)
   59    CONTINUE
      ENDIF
C
C     Output the Gaussian latitudes
C
      WRITE(6,202)
      WRITE(6,201)(ALAT(J),J=1,JGG)
C
      END
