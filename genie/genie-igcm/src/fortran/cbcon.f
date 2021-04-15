      SUBROUTINE CBCON(NCRB,NCRT,J,IHEM)

      IMPLICIT NONE
C     
C     DEEP CONVECTION SCHEME. NON-ENTRAINING CLOUD MODEL
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
C     
      INTEGER NCRBM,NCRB,NCRT,NCRTP,J,L,LM,IHEM
      REAL    DSELP,QELP,DSEL,QEL,DSELM,TTEND,QTEND,QELM

      REAL QG(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))

      NCRBM=NCRB-1
      NCRTP=NCRT+1
      RRCR(J)=RRCR(J)+PLG(J)*CCR*(QG(J,NCRB)-QC(NCRT))
      DSELP=-CTQ*QG(J,NCRB)
      QELP=QG(J,NCRB)
      DSEL=DSELP
      QEL=QELP
      DO L=NCRB,NCRTP,-1
         LM=L-1
         DSELM=TG(J,LM)-TC(LM)-CTQ*QC(LM)
         TTEND=FWS(L)*(DSELP-DSELM)
         QTEND=FWS(L)*(QELP-QG(J,LM))
         TTMC(L)=TTMC(L)+TTEND
         QTMC(L)=QTMC(L)+QTEND
         TG(J,L)=TG(J,L)+DELT2C*TTEND
         QG(J,L)=QG(J,L)+DELT2C*QTEND
         DSELP=DSEL
         DSEL=DSELM
         QELP=QEL
         QEL=QG(J,LM)
      END DO
C     
      DSELM=TC(NCRT)-TG(J,NCRT)-CTQ*QC(NCRT)
      QELM=QC(NCRT)+QC(NCRT)-QG(J,NCRT)
      TTEND=FWS(NCRT)*(DSELP-DSELM)
      QTEND=FWS(NCRT)*(QELP-QELM)
      TTMC(NCRT)=TTMC(NCRT)+TTEND
      QTMC(NCRT)=QTMC(NCRT)+QTEND
      TG(J,NCRT)=TG(J,NCRT)+DELT2C*TTEND
      QG(J,NCRT)=QG(J,NCRT)+DELT2C*QTEND
C     
      DO L=NCRT,NCRBM
         CTCR(L,IHEM)=CTCR(L,IHEM)+1.0
      END DO
C     
      RETURN
      END
