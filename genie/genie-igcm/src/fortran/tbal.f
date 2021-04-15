*DECK TBAL
C**********************************************************
C             SUBROUTINE TBAL
C**********************************************************
      SUBROUTINE TBAL

      IMPLICIT NONE

C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'bats.cmn'
      include 'balan.cmn'

      INTEGER I1,MP,J2,J22,IJ,IN,K,IL,L,KK,M,I,J,IZJ
      INTEGER IK,IJS,IE,J2L,J22L,IZ
      REAL    PMNRE

      DIMENSION PMNRE(IDJ)
      COMPLEX ERR(IDK),VPS,GSI1,TA,SRGT
C
C***********************************************************************
C     Note that this scheme does not converge if wavenumbers M are
C     present for which there are only a small number of modes in the
C     truncation. For a small number of iterations the problem is not
C     serious but it may be removed altogether by limiting the range of
C     the 20 and 80 loops. For a given M, 7 modes are sufficient and 3
C     are insufficient. The loop terminator MFTBAL is set in INITAL.
C***********************************************************************
C
      REWIND 99
      I1=0
      DO 20 MP=1,MFTBAL,MOCT
         J2=(NFP-MP)/2+1
         J22=J2*J2
         IJ=0
         DO 3 IN=MP,NFP,MH
            I1=I1+1
            IF (IN.GT.1) THEN
               VPS=VP(I1)
               K=I1
               GSI1=GS(I1)
               IL=0
               DO 10 L=1,NL
                  TA=(0.,0.)
                  KK=I1
                  SRGT=0.
                  DO 9 M=1,NL
                     IL=IL+1
                     TA=TA+G(IL)*TT(KK)
                     SRGT=SRGT+G(IL)*T(KK)
                     KK=KK+IGA
    9             CONTINUE
                  IJ=IJ+1
                  ERR(IJ)=-DT(K)-SQ(IN)*
     +                    (SRGT+GSI1+T0(L)*SP(I1)+DELT*(TA-T0(L)*VPS))
                  K=K+IGA
   10          CONTINUE
            ENDIF
    3    CONTINUE
         IF (MP.GT.1) THEN
            READ(99) (PMNRE(I),I=1,J22)
            IJ=0
            DO 24 J=1,J2
               IZJ=IZJ+1
               IZ=IZJ
               DO 23 L=1,NL
                  IK=L
                  DO 22 K=1,J2
                     IJ=IJ+1
                     Z(IZ)=Z(IZ)+PMNRE(IJ)*ERR(IK)
                     IK=IK+NL
   22             CONTINUE
                  IZ=IZ+IGA
                  IJ=IJ-J2
   23          CONTINUE
               IJ=IJ+J2
   24       CONTINUE
         ELSE
            IJS=(J2-2)*NL
            IL=J2
            DO 28 L=1,NL
               IE=J2
               IZ=IL
               SRGT=0.
               IJ=IJS+L
               DO 26 J=2,J2
                  SRGT=(ERR(IJ)-EP2(IE)*SRGT)/EP1(IE-1)
                  IJ=IJ-NL
                  IZ=IZ-1
                  Z(IZ)=Z(IZ)+SRGT
                  IE=IE-1
   26          CONTINUE
               IL=IL+IGA
   28       CONTINUE
            IZJ=J2
         ENDIF
   20 CONTINUE
C
      IF (NHEM.EQ.2) THEN
         I1=NWJ2
         DO 80 MP=1,MFTBAL,MOCT
            J2=(NFP-MP)/2+1
            J2L=J2-1
            J22L=J2*J2L
            IJ=0
            DO 60 IN=MP,NFP,MH
               I1=I1+1
               VPS=VP(I1)
               GSI1=GS(I1)
               K=I1
               IL=0
               DO 58 L=1,NL
                  TA=(0.0,0.0)
                  SRGT=(0.0,0.0)
                  KK=I1
                  DO 56 M=1,NL
                     IL=IL+1
                     TA=TA + G(IL)*TT(KK)
                     SRGT=SRGT + G(IL)*T(KK)
                     KK=KK+IGA
   56             CONTINUE
                  IJ=IJ+1
                  ERR(IJ)=-DT(K)-SQ(IN+1)*(SRGT+GSI1+T0(L)*SP(I1)+
     +                    DELT*(TA-T0(L)*VPS))
                  K=K+IGA
   58          CONTINUE
   60       CONTINUE
            IF (MP.EQ.1) THEN
               READ(99) (PMNRE(I),I=1,J22L)
               IZJ=1+NWJ2
               IJ=0
               DO 64 J=1,J2L
                  IZJ=IZJ+1
                  IZ=IZJ
                  DO 63 L=1,NL
                     IK=L
                     DO 62 K=1,J2
                        IJ=IJ+1
                        Z(IZ)=Z(IZ) + PMNRE(IJ)*ERR(IK)
                        IK=IK+NL
   62                CONTINUE
                     IZ=IZ+IGA
                     IJ=IJ-J2
   63             CONTINUE
                  IJ=IJ+J2
   64          CONTINUE
            ELSE
               IJS=(J2-1)*NL
               IL=I1
               DO 78 L=1,NL
                  IE=I1
                  IZ=IL
                  SRGT=(0.0,0.0)
                  IJ=IJS+L
                  DO 76 J=1,J2
                     SRGT=(ERR(IJ)-EP2(IE)*SRGT)/EP1(IE)
                     Z(IZ)=Z(IZ)+SRGT
                     IJ=IJ-NL
                     IZ=IZ-1
                     IE=IE-1
   76             CONTINUE
                  IL=IL+IGA
   78          CONTINUE
            ENDIF
   80    CONTINUE
      ENDIF
C
      IF(KOUNT.EQ.0)THEN
         DO 4 I=1,IGA
            SPMI(I)=SP(I)
    4    CONTINUE
         DO 8 J=1,IGB
            ZMI(J)=Z(J)
            DMI(J)=D(J)
            TMI(J)=T(J)
    8    CONTINUE
      ENDIF
C
      RETURN
      END
