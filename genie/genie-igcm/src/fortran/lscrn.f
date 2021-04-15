*DECK LSCRN
C**********************************************************
C             SUBROUTINE LSCRN
C**********************************************************
      SUBROUTINE LSCRN

      IMPLICIT NONE

C
C  GRID SCALE CONDENSATION TO SATURATION FOR PRESENT LATITUDE.
C  The cloud diagnostic scheme is based on Slingo (1987).
C  Cloud fractions computed for Low (sigma>0.7),
C  mid(0.7>sigma>0.35) and  high (0.35>sigma>0.12) cloud
C  types. Shallow convective and deep convective cloud fractions
C  computed in CUBM and LBADJ respectively.

#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
      include 'cpiers.cmn'
      include 'igcm_cloud.cmn'
      REAL QG(IGC,NL),QTLR(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))
     *            ,(QTLR(1,1),UTRAG(1,1,1))
C
      INTEGER IHEM,IOFM,I,J,L
      REAL    ESCON,QS,PQSAT,RH,CFR,QEXS,CTQUSE,QEX
      REAL,EXTERNAL :: SDOT

      REAL DQDT(NL)
C
      DO 800 IHEM=1,NHEM
        IOFM=(IHEM-1)*MGPP
        DO 100 I=1,MG
          J=I+IOFM
          ESCON=1./PLG(J)
          DO 20 L=1,NL
            QS=ESCON*PQSAT(TG(J,L))/SIGMA(L)
C LAYER CLOUD , High, middle, low.
C Picks level (within low, mid, high) with largest RH
C and puts cloud there
C
            RH=QG(J,L)/QS  ! relative humidity
            IF (RH.GT.humcloudmin) THEN
              CFR=((RH-humcloudmin)/(humcloudmax-humcloudmin))**
     :             powercloud   ! cloud fract mid/low/high cloud

c             I think I shoul dbe able to get rid of this next line, but 
c               if I do then restarts no longer work......very odd.
c             It should work because the limit is still imposed below.
              IF (CFR.GT.1.0) CFR=1.0
C high cloud
              IF (SIGMA(L).GT.0.12.AND.SIGMA(L).LE.0.35) THEN
                IF (CFR*cloudhfact.GT.CFRAC(J,3)) THEN
                  ICFLAG(J,3,1)=L
                  ICFLAG(J,3,2)=L
                  CFRAC(J,3)=CFR*cloudhfact
                  IF (CFRAC(J,3).GT.1.0) CFRAC(J,3)=1.0
                ENDIF
              ENDIF
c mid cloud
              IF (SIGMA(L).GT.0.35.AND.SIGMA(L).LE.0.70) THEN
                IF (CFR*cloudmfact.GT.CFRAC(J,2)) THEN
                  ICFLAG(J,2,1)=L
                  ICFLAG(J,2,2)=L
                  CFRAC(J,2)=CFR*cloudmfact
                  IF (CFRAC(J,2).GT.1.0) CFRAC(J,2)=1.0
                ENDIF
              ENDIF
c low cloud not in level nl
              IF (SIGMA(L).GT.0.70.AND.L.LT.NL) THEN
                IF (CFR*cloudlfact.GT.CFRAC(J,1)) THEN
                  ICFLAG(J,1,1)=L
                  ICFLAG(J,1,2)=L
                  CFRAC(J,1)=CFR*cloudlfact
                  IF (CFRAC(J,1).GT.1.0) CFRAC(J,1)=1.0
                ENDIF
              ENDIF
            ENDIF
            QEXS=QG(J,L)-QS
C     Choose appropriate value of latent heat based on ground T
c     ie, we assume it snows if the ground is colder than 273.15K
            if (tstar(j,jh).gt.0.363224029) then
              ctquse=ctq
            else
              ctquse=ctqi
            endif
            QEX=QEXS/(1.0+CTQUSE*QS*ESCONB/(TG(J,L)*TG(J,L)))
            IF (QEXS.LT.0.0) THEN
              QEX=0.0
            END IF
            IF (QEXS.GE.0.0) THEN
              CTLR(L,IHEM)=CTLR(L,IHEM)+1.0
            END IF
            DQDT(L)=-QEX/DELT2C
            QTLR(J,L)=QTLR(J,L)+DQDT(L)
            TTLR(J,L)=-CTQUSE*DQDT(L)
            QG(J,L)=QG(J,L)-QEX
            TG(J,L)=TG(J,L)+CTQUSE*QEX
   20     CONTINUE
          RRLR(J)=-SDOT(NL,DQDT,1,CLR,1)*PLG(J)
  100   CONTINUE
  800 CONTINUE
      RETURN
      END
