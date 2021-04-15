      SUBROUTINE CUDIF(NCRB,NCT,J,IHEM)

      IMPLICIT NONE
C     
C     NON-PRECIPITATING CONVECTION. DIFFUSION TOWARDS MIXING-LINE THETA
C     AND CONSTANT Q. CLOUD-TOP FLUXES AND VARIABLE K'S WHEN SHALLOW.
C     OPTIONAL RAINOUT OVER A SPECIFIED REL-HUM.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
      include 'bats.cmn'
      REAL QG(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))

      REAL PL(NL),PFK(NL),TR(NL)

      REAL,EXTERNAL :: FTSL,SSUM,SDOT,PQSAT

      INTEGER J,NCT,NCRT,NCRB,NLEV,NCRTP,NCRBM,L,LP,IHEM
      REAL    QGMIN,TLIM,ESCON,AKTCU,AKQCU,RKAP,THB,QQG,TSB
      REAL    PSLB,THM,QM,TMT,TSLM,PSLM,TLAPSM,THRL,TLPH
      REAL    FPH,DQPH,DTPH,FMH,DQMH,DTMH,SINT,TTCOR,TGP
      REAL    QGP,QS,QEXS,QEX,QTCOR

C     
 2001 FORMAT(' CUDIF QMIN APPL. AT BAS: I,J,NCRB,NCRT,QG =',5I5,F10.4)
 2002 FORMAT(' CUDIF QMIN APPL. FOR QM: I,J,NCRB,NCRT,QM =',4I5,F10.4)
 2103 FORMAT(' CUBM LAPSE RATE LIMIT IMPOSED: I,J,NCRB,NCRT = ',4I5)
 2104 FORMAT(' CUBM LAPSE RATE LIMIT IMPOSED: I,J,NCRB,NCRT,TLIM
     +     = ',4I5,F7.4)
C     
C     Minimum moisture for mixing line calculations.
      QGMIN=1.E-6
C     Stable limit to mixing line slope d(theta)/dp=-10^-3 K/Pa
      TLIM=-1.0E-3*P0/CT
C     
      ESCON=1./PLG(J)
      IF(NCT.GE.NCUTOP) THEN
         IF(BLVB(J).EQ.0.0)RETURN
         NCRT=NCT-1
         AKTCU=BLVB(J)*BLCD(J)*CUT2/CUT1
      ELSE IF (NCRB.GE.NCUTOP) THEN
C     Limit the convective top for swap points to the maximum
C     height of shallow convection.
C     Note that NCRT includes the level above cloud top.
         NCRT=NCUTOP-1
         AKTCU=AKTC
      ENDIF
      AKQCU=AKTCU*AKQC/AKTC
      NLEV=1+NCRB-NCRT
      NCRTP=NCRT+1
      NCRBM=NCRB-1
C     
      RKAP=1.0/AKAP
      DO L=NCRT,NCRB
         PL(L)=SIGMA(L)*PLG(J)
         PFK(L)=PL(L)**AKAP
      END DO
      THB=TG(J,NCRB)/PFK(NCRB)
      IF (QG(J,NCRB).LT.QGMIN) THEN
         WRITE(6,2001) J,JH,L,NCRB,NCRT,CQ*QG(J,NCRB)
      ENDIF
C     Apply a consistent minimum moisture criterion in the mixing line
C     calculations.
      QQG=MAX(QG(J,NCRB),QGMIN)
      TSB=FTSL(TG(J,NCRB),QQG,PL(NCRB))
      PSLB=PL(NCRB)*(TSB/TG(J,NCRB))**RKAP
      THM=0.5*(THB+TG(J,NCRT)/PFK(NCRT))
      QM =0.5*(QG(J,NCRB)+QG(J,NCRT))
      IF (QM.LT.QGMIN) THEN
         WRITE(6,2002) J,JH,NCRB,NCRT,CQ*QM
      ENDIF
      QM=MAX(QM,QGMIN)
      TMT=THM*PFK(NCRT)
      TSLM=FTSL(TMT,QM,PL(NCRT))
      PSLM=PL(NCRT)*(TSLM/TMT)**RKAP
      TLAPSM=(THM-THB)/(PSLM-PSLB)
      TLAPSM=MAX(TLAPSM,TLIM)   ! limit stability
      TLAPSM=MIN(TLAPSM,0.0)    ! prevents instability
      IF (TLAPSM.GT.0.0) THEN
         WRITE(6,2103) J,JH,NCRB,NCRT
      ELSEIF (TLAPSM.LT.TLIM)THEN
         WRITE(6,2104) J,JH,NCRB,NCRT,TLIM
      ENDIF

      DO L=NCRT,NCRB
         THRL=THB+(PL(L)-PL(NCRB))*TLAPSM
         TR(L)=THRL*PFK(L)
      END DO
C     
      TLPH=TG(J,NCRT)+TG(J,NCRTP)
      FPH=FB(NCRT)/(TLPH*TLPH)
      DQPH=QG(J,NCRTP)-QG(J,NCRT)
      DTPH=SK(NCRT)*(TG(J,NCRTP)-TR(NCRTP))-(TG(J,NCRT)-TR(NCRT))
      TTMC(NCRT)=AKTCU*FPH*DTPH/DSIGMA(NCRT)
      QTMC(NCRT)=AKQCU*FPH*DQPH/DSIGMA(NCRT)
C     
      IF(NLEV.GT.2) THEN
         DO L=NCRTP,NCRBM
            LP=L+1
            FMH=FPH
            DQMH=DQPH
            DTMH=DTPH/SK(L-1)
            TLPH=TG(J,LP)+TG(J,L)
            FPH=FB(L)/(TLPH*TLPH)
            DQPH=QG(J,LP)-QG(J,L)
            DTPH=SK(L)*(TG(J,LP)-TR(LP))-(TG(J,L)-TR(L))
            TTMC(L)=AKTCU*(FPH*DTPH-FMH*DTMH)/DSIGMA(L)
            QTMC(L)=AKQCU*(FPH*DQPH-FMH*DQMH)/DSIGMA(L)
            CTCR(L,IHEM)=CTCR(L,IHEM)+1.0
         END DO
      ENDIF
C     
      TTMC(NCRB)=-AKTCU*FPH*DTPH/(SK(NCRBM)*DSIGMA(NCRB))
      QTMC(NCRB)=-AKQCU*FPH*DQPH/DSIGMA(NCRB)
C     
      SINT=SSUM(NLEV,DSIGMA(NCRT),1)
      TTCOR=SDOT(NLEV,TTMC(NCRT),1,DSIGMA(NCRT),1)/SINT
      DO L=NCRT,NCRB
         TTMC(L)=TTMC(L)-TTCOR
      END DO
C     
      IF(CURHM.LT.1.00001) THEN
         DO L=NCRT,NCRB
            TGP=TG(J,L)+DELT2C*TTMC(L)
            QGP=QG(J,L)+DELT2C*QTMC(L)
            QS=CURHM*ESCON*PQSAT(TGP)/SIGMA(L)
            QEXS=QGP-QS
            QEX=QEXS/(1.0+CTQ*QS*ESCONB/(TGP*TGP))
            IF (QEXS.LT.0.0) THEN
               QEX=0.0
            END IF
            QTCOR=-QEX/DELT2C
            TTMC(L)=TTMC(L)-CTQ*QTCOR
            QTMC(L)=QTMC(L)+QTCOR
            RRCR(J)=RRCR(J)-QTCOR*CLR(L)*PLG(J)
         END DO
      ENDIF
C     
      DO L=NCRT,NCRB
         TG(J,L)=TG(J,L)+DELT2C*TTMC(L)
         QG(J,L)=QG(J,L)+DELT2C*QTMC(L)
      END DO
C     
      RETURN
      END
C
      REAL FUNCTION FTSL(T,Q,P)
      IMPLICIT NONE
      REAL T,Q,P
#include "param1.cmn"
      include 'param2.cmn'
      include 'physca.cmn'
C
      FTSL=TSLA+TSLB/(TSLC*LOG(T)-LOG(Q*P)+TSLD)
C
      RETURN
      END

