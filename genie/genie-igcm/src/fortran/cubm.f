      SUBROUTINE CUBM(NCRB,NCT,J,IHEM)

      IMPLICIT NONE

C     
C     BETTS-MILLER NON-PRECIPITATING CONVECTION. FOLLOWS ECMWF TECH REP
C     NO 43. VARIABLE TIMESCALE AND CLOUD-TOP FLUXES WHEN SHALLOW.
C     OPTIONAL RAINOUT OVER A SPECIFIED REL-HUM.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
      include 'bats.cmn'
      include 'cpiers.cmn'
      include 'igcm_cloud.cmn'
      include 'rough.cmn'
      REAL QG(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))
C     

      REAL PL(NL),TH(NL),PSL(NL),TSL(NL),SCRIP(NL),PFK(NL)
     :     ,TR(NL),QR(NL),THR(NL)
C
c      NOW IN NAMELIST     
c      real cloudshallow
c      parameter(cloudshallow=0.35)
      real qgmin
      parameter(qgmin=1e-6)

      REAL,EXTERNAL :: SSUM
      REAL,EXTERNAL :: SDOT

      INTEGER J,NCT,NCRT,NCRB,NLEV,L,IHEM
      REAL    TSCUR,RGZ0,RSIGF,THNL,DTH,THBAR,RZZ0,RLZZ02,RCSJ,VM
      REAL    CUTAU,TLIM,RKAP,QQG,FTSL,THM,QM,TMT,TSLM,PSLM,TLAPSM
      REAL    SINT,SCRINT,TGINT,TRINT,DTINT,PQSAT,QGINT,QRINT
      REAL    DQINT,TGP,QGP,QS,QEXS,CTQUSE,QEX,QTCOR

 2103 FORMAT(' CUBM LAPSE RATE LIMIT IMPOSED: I,J,NCRB,NCRT = ',4I5)
 2104 FORMAT(' CUBM LAPSE RATE LIMIT IMPOSED: I,J,NCRB,NCRT,TLIM
     +     = ',4I5,F7.4)
C     
c     *************************************************************
c     This is the same code as was in blsurf and now igcm_surflux.f
c     I should look at this in more detail....
               tscur=tstar(j,jh)
               rgz0=rough(j,jh)
               RSIGF=2.*(1-SIGMA(NL))/(1+SIGMA(NL))   
               THNL=TG(J,NL)/SKAP(NL)
               DTH=TSCUR-THNL
               THBAR=0.5*(THNL+TSCUR)
               RZZ0=RSIGF*THBAR/RGZ0
               RLZZ02=(LOG(RZZ0))**2
               blcd(j)=0.1681/RLZZ02
               rcsj=SECSQ(jh)
               VM=SQRT(RCSJ*(Ug(j,nl)*Ug(j,nl)+Vg(j,nl)*Vg(j,nl)))
               IF (DTH.GT.0.) THEN
                  BLVB(j)=5.95*RLZZ02*SQRT(RGZ0*DTH/THBAR)
                  IF (BLVB(j)/VM.GT.4.) BLVB(j)=VM*4.
               else
                  blvb(j)=0.0
               endif
c     *************************************************************
C     Minimum moisture for mixing line calculations.
c      QGMIN=1.E-6
      IF(NCT.GE.NCUTOP) THEN
         IF(BLVB(J).EQ.0.0)RETURN
         NCRT=NCT-1
         CUTAU=CUT1/(BLVB(J)*BLCD(J))
      ELSE IF (NCRB.GE.NCUTOP) THEN
C     Limit the convective top for swap points to the maximum
C     height of shallow convection.
C     Note that NCRT includes the level above cloud top.
         NCRT=NCUTOP-1
         CUTAU=CUBMT
      ELSE
C     
C     No adjustment for mid-level swap points.
C     
         RETURN
      ENDIF
C     Stable limit to mixing line slope d(theta)/dp=-10^-3 K/Pa
      TLIM=-1.0E-3*P0/CT
C     shallow convection
      ICFLAG(J,4,1)=NCRB
      ICFLAG(J,4,2)=NCRT
C     cloud fraction for all shallow convection
      CFRAC(J,4)=cloudshallow       
      NLEV=1+NCRB-NCRT
      RKAP=1.0/AKAP
      DO L=NCRT,NCRB
         PL(L)=SIGMA(L)*PLG(J)
         PFK(L)=PL(L)**AKAP
         TH(L)=TG(J,L)/PFK(L)
C     
C     Apply a consistent minimum moisture criterion in the mixing line
C     calculations.
C     
         QQG=MAX(QG(J,L),QGMIN)
         TSL(L)=FTSL(TG(J,L),QQG,PL(L))
         PSL(L)=PL(L)*(TSL(L)/TG(J,L))**RKAP
         SCRIP(L)=PSL(L)-PL(L)
      END DO
C     
      THM=0.5*(TH(NCRB)+TH(NCRT))
      QM=0.5*(QG(J,NCRB)+QG(J,NCRT))
      QM=MAX(QM,QGMIN)
      TMT=THM*PFK(NCRT)
      TSLM=FTSL(TMT,QM,PL(NCRT))
      PSLM=PL(NCRT)*(TSLM/TMT)**RKAP
      TLAPSM=(THM-TH(NCRB))/(PSLM-PSL(NCRB))
      TLAPSM=MAX(TLAPSM,TLIM)   ! limit stability
      TLAPSM=MIN(TLAPSM,0.0)    ! prevents instability
      IF (TLAPSM.GT.0.0) THEN
         WRITE(6,2103) J,JH,NCRB,NCRT
      ELSE IF (TLAPSM.LT.TLIM)THEN
         WRITE(6,2104) J,JH,NCRB,NCRT,TLIM
      ENDIF
      SINT=SSUM(NLEV,DSIGMA(NCRT),1)
      SCRINT=SDOT(NLEV,SCRIP(NCRT),1,DSIGMA(NCRT),1)/SINT
      SCRINT=MIN(0.0,SCRINT)
C     
      DO L=NCRT,NCRB
         THR(L)=TH(NCRB)+TLAPSM*(PL(L)-PL(NCRB))
         TR(L) =THR(L)*PFK(L)
      END DO
      TGINT=SDOT(NLEV,TG(J,NCRT),IGC,DSIGMA(NCRT),1)/SINT
      TRINT=SDOT(NLEV,TR(NCRT),1,DSIGMA(NCRT),1)/SINT
      DTINT=TGINT-TRINT
      DO L=NCRT,NCRB
         TR(L)=TR(L)+DTINT
         PSL(L)=SCRINT+PL(L)
         TSL(L)=TR(L)*(PSL(L)/PL(L))**AKAP
         QR(L)=PQSAT(TSL(L))/PSL(L)
      END DO
      QGINT=SDOT(NLEV,QG(J,NCRT),IGC,DSIGMA(NCRT),1)/SINT
      QRINT=SDOT(NLEV,QR(NCRT),1,DSIGMA(NCRT),1)/SINT
      DQINT=QGINT-QRINT
      DO L=NCRT,NCRB
         QR(L)=QR(L)+DQINT
         TTMC(L)=(TR(L)-TG(J,L))/CUTAU
         QTMC(L)=(QR(L)-QG(J,L))/CUTAU
      END DO
C     
      IF (CURHM.LT.1.00001) THEN
         DO L=NCRT,NCRB
            TGP=TG(J,L)+DELT2C*TTMC(L)
            QGP=QG(J,L)+DELT2C*QTMC(L)
            QS=CURHM*PQSAT(TGP)/PL(L)
            QEXS=QGP-QS
C     Choose appropriate value of latent heat based on ground T
c     ie, we assume it snows if the ground is colder than 273.15K
            IF (tstar(j,jh).gt.0.363224029) THEN
               ctquse=ctq
            ELSE
               ctquse=ctqi
            ENDIF
            QEX=QEXS/(1.0+CTQ*QS*ESCONB/(TGP*TGP))
            IF (QEXS.LT.0.0) THEN
               QEX=0.0
            END IF
            QTCOR=-QEX/DELT2C
            TTMC(L)=TTMC(L)-CTQUSE*QTCOR
            QTMC(L)=QTMC(L)+QTCOR
            RRCR(J)=RRCR(J)-QTCOR*CLR(L)*PLG(J)
         END DO
      ENDIF
C     
      DO L=NCRT,NCRB
         TG(J,L)=TG(J,L)+DELT2C*TTMC(L)
         QG(J,L)=QG(J,L)+DELT2C*QTMC(L)
         CTCR(L,IHEM)=CTCR(L,IHEM)+1.0
      END DO
      CTCR(NCRB,IHEM)=CTCR(NCRB,IHEM)-1.0
      CTCR(NCRT,IHEM)=CTCR(NCRT,IHEM)-1.0
C     
      RETURN
      END
