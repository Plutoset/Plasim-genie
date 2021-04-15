*DECK DRYADJ
C**********************************************************
C             SUBROUTINE DRYADJ
C**********************************************************
      SUBROUTINE DRYADJ(NCRB,NCRT,J,IHEM)

      IMPLICIT NONE

C
C     DRY ADIABATIC ADJUSTMENT TO NEUTRALITY IN DELT2 STEP
C     Always mix moisture, irrespective of the parcel origin level.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
      REAL QG(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))
C
      REAL,EXTERNAL :: SSUM,SDOT

      INTEGER NLEV,NCRB,NCRT,J,L,IHEM
      REAL    SINT,SKINT,TINT,QINT,TL,QL

      NLEV=1+NCRB-NCRT

      SINT=SSUM(NLEV,DSIGMA(NCRT),1)
c     ssum is in genie/genie-lib/libaux1
c     sint is the vertical sum of dsigma from index ncrt to ncrb
c     for 7 levels, dsigma(*)=0.1428571
c      print*,ncrb,ncrt
c      print*,dsigma
c      print*,sint

      SKINT=SDOT(NLEV,SKAP(NCRT),1,DSIGMA(NCRT),1)
c     sdot is in genie/genie-lib/libblas1
c     skap is defined in iniphys.f
c     skap=
c     akap (kappa) is defined in iniset.f = 0.286
c     skint is the vertical sum of dsigma*(sigma^kappa)

      TINT=SDOT(NLEV,TG(J,NCRT),IGC,DSIGMA(NCRT),1)/SKINT
c     The igc takes care of the odd ordering of TG.
c     tint is the vertical sum of T*dsigma, divided by skint
c     in other words, TINT=MEAN POTENTIAL TEMPERATURE.
c     TINT IS THE NEW POTENTIAL TEMPERATURE OF EVERY LAYER.

      QINT=SDOT(NLEV,QG(J,NCRT),IGC,DSIGMA(NCRT),1)/SINT
c     qint is the vertical sum of Q*dsigma, divided by sint
c     in other words, QINT=MEAN MOISTURE.


      DO 10 L=NCRT,NCRB
        TL=SKAP(L)*TINT
c       tl is the new temperature of the layer.
c       tint is the new potential temperature.

        QL=QINT
c       ql is the new humidity of the layer.

        TTDC(L)=(TL-TG(J,L))/DELT2C
c       ttdc is the rate of change of temperature

        QTDC(L)=(QL-QG(J,L))/DELT2C
c       qtdc is the rate of change of humidity

        TG(J,L)=TL
c       tg given its new value

        QG(J,L)=QL
c       qg given its new value


        CTCR(L,IHEM)=CTCR(L,IHEM)+2.0
   10 CONTINUE
      CTCR(NCRB,IHEM)=CTCR(NCRB,IHEM)-2.0
c     ctcr appears to be a flag.
c     set to +2 if this dry adjustment has been carried out.
c     apart from at the lowest adjusted level, where is set to +0.


      RETURN
      END
