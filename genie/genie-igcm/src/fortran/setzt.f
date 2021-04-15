*DECK SETZT
C**********************************************************
C             SUBROUTINE SETZT
C**********************************************************
      SUBROUTINE SETZT

      IMPLICIT NONE

C
C     This subroutine sets up restoration temperature field.
C     The temperature at SIGMA = 1 is TGR, entered in Kelvin.
C     a lapse rate of ALR k/m is assumed under the tropopause and
C     zero above. The actual profile tends to this away from the
C     tropopause, with smooth interpolation depending on DTTRP
C     at the model tropopause. The height of
C     the tropopause is given as ZTROP m.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'restij.cmn'
      include 'bats.cmn'

      INTEGER I,L
      REAL    SIGPREV,TPREV,ZPREV,ZP,TP,TPM,ZPP,TPP


      DO 100 I=1,IGB
         TTRES(I)=(0.0,0.0)
100   CONTINUE
      DTTRP=DTTRP*CT
      SIGPREV=1.
      TPREV=TGR
      ZPREV=0.
      DO 150 L=NL,1,-1
         ZP=ZPREV+(GASCON*TPREV/GA)*LOG(SIGPREV/SIGMA(L))
         TP=TGR-ZTROP*ALR
         TP=TP+SQRT((.5*ALR*(ZP-ZTROP))**2+DTTRP**2)
         TP=TP-.5*ALR*(ZP-ZTROP)
         TPM=.5*(TPREV+TP)
         ZPP=ZPREV+(GASCON*TPM/GA)*LOG(SIGPREV/SIGMA(L))
         TPP=TGR-ZTROP*ALR
         TPP=TPP+SQRT((.5*ALR*(ZPP-ZTROP))**2+DTTRP**2)
         TPP=TPP-.5*ALR*(ZPP-ZTROP)
         TRS(L)=TPP
         ZPREV=ZPREV+(.5*(TPP+TPREV)*GASCON/GA)*LOG(SIGPREV/SIGMA(L))
         TPREV=TPP
         SIGPREV=SIGMA(L)
150   CONTINUE
C
      WRITE(6,2000)
      WRITE(6,2010) TRS
 2000 FORMAT(/' RESTORATION TEMPERATURE STRATIFICATION IN K ')
 2010 FORMAT(10F7.2)
C
      DO 170 L=1,NL
         TRS(L)=TRS(L)/CT
 170  CONTINUE
C
C     Now the latitudinal variation in TTRES is set up
C     (this being in terms of a deviation from T0 which
C     is usually constant with height)
C
      DO 200 L=1,NL
         I=(L-1)*IGA
         TTRES(I+1)=SQRT(2.)*(TRS(L)-T0(L))
         TTRES(I+2)=-2./3.*SQRT(0.4)*DTEP*FAC(L)
         TTRES(I+NWJ2+1)=(1./SQRT(6.))*DTNS*FAC(L)
200   CONTINUE
      DTTRP=DTTRP/CT
C
      END
