*DECK BALANC
C**********************************************************
C             SUBROUTINE BALANC
C**********************************************************
      SUBROUTINE BALANC

      IMPLICIT NONE
      
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'bats.cmn'
      include 'balan.cmn'
      DIMENSION GP(NL)
      COMPLEX TA,GP,GSI1,VPS,TK,SRGT

      INTEGER I1,IHEM,MP,IN,INR,K,IL,L,KK,M,I,J

C
C**********************************************************************
C     2-grid vertical temperature wave is not removed if orography
C     is included. Program aborts in INITAL if attempted.
C**********************************************************************
C
      I1=0
      DO 800 IHEM=1,NHEM
         DO 3 MP=1,MFP,MOCT
            DO 4 IN=MP,NFP,MH
               I1=I1+1
               INR=IN+IHEM-1
               IF (INR.GT.1) THEN
                  VPS=VP(I1)
                  K=I1
                  GSI1=GS(I1)
                  IL=0
                  DO 10 L=1,NL
                     TA=(0.,0.)
                     KK=I1
                     DO 9 M=1,NL
                        IL=IL+1
                        TA=TA+G(IL)*TT(KK)
                        KK=KK+IGA
    9                CONTINUE
                     TA=(T0(L)*VPS-TA)*DELT - RSQ(INR)*DT(K)
                     GP(L)=TA-GSI1
                     K=K+IGA
   10             CONTINUE
                  IL=0
                  K=I1
                  SRGT=(0.,0.)
                  DO 12 L=1,NL
                     TK=(0.,0.)
                     DO 11 M=1,NL
                        IL=IL+1
                        TK=TK+RG(IL)*GP(M)
   11                CONTINUE
                     T(K)=TK
                     SRGT=SRGT+BFILT(L)*TK
                     K=K+IGA
   12             CONTINUE
                  SRGT=SRGT/SRGT0
                  SP(I1)=SRGT
                  K=I1
                  DO 13 L=1,NL
                     T(K)=T(K)-RGT0(L)*SRGT
                     K=K+IGA
   13             CONTINUE
               ENDIF
    4       CONTINUE
    3    CONTINUE
         I1=NWJ2
  800 CONTINUE
C
      IF (KOUNT.EQ.0) THEN
         DO 2 I=1,IGA
            SPMI(I)=SP(I)
    2    CONTINUE
         DO 5 J=1,IGB
            ZMI(J)=Z(J)
            DMI(J)=D(J)
            TMI(J)=T(J)
    5    CONTINUE
      ENDIF
C
      RETURN
      END
