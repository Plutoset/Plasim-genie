*DECK MGRMLT
C**********************************************************
C             SUBROUTINE MGRMLT
C**********************************************************
      SUBROUTINE MGRMLT

      IMPLICIT NONE

C
C     Computes nonlinear tendencies in grid point space
C     for the present latitude
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridp2.cmn'

      INTEGER IOFM,IHEM,I,J,L,LL,KK,K
      REAL    CC,SUMD,SDOTP,TPTA,TPTB,VGPG,TSUM,FAC,FAC2

      DIMENSION SDOTP(MG,NLM),SUMD(MG),TPTA(MG),TPTB(MG)
      DIMENSION CC(NL,NL)
      EQUIVALENCE (CC(1,1),C(1))
C
      IOFM=0
C
C     Loop over hemispheres
C
      DO 800 IHEM=1,NHEM
         DO 100 I=1,MG
            J=I+IOFM
            SUMD(I)=0.0
            VPG(J)=0.0
C
C           Change from Ln(PSTAR) to PSTAR
C
            SPG(J)=EXP(PLG(J))-1.0
C
 100     CONTINUE
         DO 110 L=1,NLM
            DO 120 I=1,MG
              J=I+IOFM
              SUMD(I)=SUMD(I)+DSIGMA(L)*DG(J,L)
              VPG(J)=VPG(J)+DSIGMA(L)*SECSQ(JH)*(UG(J,L)*PMG(J)+
     1               VG(J,L)*PJG(J))
              SDOTP(I,L)=SUMD(I)+VPG(J)
 120        CONTINUE
 110     CONTINUE
         DO 121 I=1,MG
            J=I+IOFM
            SUMD(I)=SUMD(I)+DSIGMA(NL)*DG(J,NL)
            VPG(J)=VPG(J)+DSIGMA(NL)*SECSQ(JH)*(UG(J,NL)*PMG(J)+
     1             VG(J,NL)*PJG(J))
 121     CONTINUE
         DO 130 L=1,NLM
            DO 140 I=1,MG
               J=I+IOFM
               SDOTP(I,L)=SIGMAH(L)*(SUMD(I)+VPG(J))-SDOTP(I,L)
 140        CONTINUE
 130     CONTINUE
         DO 150 I=1,MG
            SUMD(I)=0.0
 150     CONTINUE
         DO 160 L=1,NL
            DO 170 I=1,MG
               TPTA(I)=0.0
               TPTB(I)=0.0
 170        CONTINUE
            DO 180 LL=1,L
               DO 190 I=1,MG
                  J=I+IOFM
                  VGPG=SECSQ(JH)*(UG(J,LL)*PMG(J)+VG(J,LL)*PJG(J))
                  TPTA(I)=TPTA(I)+CC(LL,L)*VGPG
                  TPTB(I)=TPTB(I)+CC(LL,L)*(VGPG+DG(J,LL))
 190           CONTINUE
 180        CONTINUE
            DO 200 I=1,MG
               J=I+IOFM
               UTG(J,L)=UG(J,L)*TG(J,L)
               VTG(J,L)=VG(J,L)*TG(J,L)
               EG(J,L)=UG(J,L)*UG(J,L)+VG(J,L)*VG(J,L)
 200        CONTINUE
            DO 202 KK=1,NTRAC
               DO 201 I=1,MG
                  J=I+IOFM
                  UTRAG(J,L,KK)=UG(J,L)*TRAG(J,L,KK)
                  VTRAG(J,L,KK)=VG(J,L)*TRAG(J,L,KK)
  201          CONTINUE
  202       CONTINUE
            IF (L.GT.1.AND.L.LT.NL) THEN
               DO 210 I=1,MG
                  J=I+IOFM
                  TSUM=SECSQ(JH)*(UG(J,L)*PMG(J)+VG(J,L)*PJG(J))
                  SUMD(I)=SUMD(I)+TSUM*DSIGMA(L)
                  TNLG(J,L)=TG(J,L)*DG(J,L)+AKAP*TG(J,L)*(TSUM-TPTB(I))+
     1                      TKP(L)*(TSUM-TPTA(I))-
     2                      RDSIG(L)*(SDOTP(I,L)*(TG(J,L+1)-TG(J,L))+
     3                      SDOTP(I,L-1)*(TG(J,L)-
     4                      TG(J,L-1))+VPG(J)*(T01S2(L)*SIGMAH(L)+
     5                      T01S2(L-1)*SIGMAH(L-1))-
     6                      SUMD(I)*(T01S2(L-1)+T01S2(L))+
     7                      TSUM*DSIGMA(L)*T01S2(L-1))
                  FVG(J,L)=-UG(J,L)*ZG(J,L)-PJG(J)*TG(J,L)-
     1                     RDSIG(L)*(SDOTP(I,L)*(VG(J,L+1)-VG(J,L))+
     2                     SDOTP(I,L-1)*(VG(J,L)-VG(J,L-1)))
                  FUG(J,L)=VG(J,L)*ZG(J,L)-PMG(J)*TG(J,L)-
     1                     RDSIG(L)*(SDOTP(I,L)*(UG(J,L+1)-UG(J,L))+
     2                     SDOTP(I,L-1)*(UG(J,L)-UG(J,L-1)))
 210           CONTINUE
               DO 213 KK=1,NTRAC
                  IF (.NOT.LFLUX) THEN
                     DO 211 I=1,MG
                        J=I+IOFM
                        TRANLG(J,L,KK)=TRAG(J,L,KK)*DG(J,L)-RDSIG(L)*
     1                    (SDOTP(I,L  )*(TRAG(J,L+1,KK)-TRAG(J,L  ,KK))
     2                    +SDOTP(I,L-1)*(TRAG(J,L  ,KK)-TRAG(J,L-1,KK)))
  211                CONTINUE
                  ELSE
                     DO 212 I=1,MG
                        J=I+IOFM
                        TRANLG(J,L,KK)=RDSIG(L)*
     1                    (SDOTP(I,L  )*(TRAG(J,L+1,KK)+TRAG(J,L  ,KK))
     2                    -SDOTP(I,L-1)*(TRAG(J,L  ,KK)+TRAG(J,L-1,KK)))
  212                CONTINUE
                  ENDIF
  213          CONTINUE
            ELSE
               FAC=1.0
               FAC2=1.
               K=L
               IF (L.EQ.NL) THEN
                  K=L-1
                  FAC=0.0
                  FAC2=-1.
               ENDIF
               DO 220 I=1,MG
                  J=I+IOFM
                  TSUM=SECSQ(JH)*(UG(J,L)*PMG(J)+VG(J,L)*PJG(J))
                  SUMD(I)=SUMD(I)+TSUM*DSIGMA(L)*FAC
                  TNLG(J,L)=TG(J,L)*DG(J,L)+AKAP*TG(J,L)*(TSUM-TPTB(I))+
     1                      TKP(L)*(TSUM-TPTA(I))-
     2                      RDSIG(L)*(SDOTP(I,K)*(TG(J,K+1)-TG(J,K))+
     3                      T01S2(K)*(SIGMAH(K)*VPG(J)-SUMD(I)))
                  FVG(J,L)=-UG(J,L)*ZG(J,L)-PJG(J)*TG(J,L)-
     1                     RDSIG(L)*(SDOTP(I,K)*(VG(J,K+1)-VG(J,K)))
                  FUG(J,L)=VG(J,L)*ZG(J,L)-PMG(J)*TG(J,L)-
     1                     RDSIG(L)*(SDOTP(I,K)*(UG(J,K+1)-UG(J,K)))
 220           CONTINUE
               DO 223 KK=1,NTRAC
                  IF (.NOT.LFLUX) THEN
                     DO 221 I=1,MG
                        J=I+IOFM
                        TRANLG(J,L,KK)=TRAG(J,L,KK)*DG(J,L)-RDSIG(L)*
     1                         SDOTP(I,K)*(TRAG(J,K+1,KK)-TRAG(J,K,KK))
  221                CONTINUE
                  ELSE
                     DO 222 I=1,MG
                        J=I+IOFM
                        TRANLG(J,L,KK)=RDSIG(L)*FAC2*
     1                         SDOTP(I,K)*(TRAG(J,K+1,KK)+TRAG(J,K,KK))
  222                CONTINUE
                  ENDIF
  223          CONTINUE
            ENDIF
 160     CONTINUE
         IOFM=MGPP
 800  CONTINUE
      RETURN
      END
