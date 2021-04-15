      SUBROUTINE CBADJ(NCRB,NCRT,J,IHEM)

      IMPLICIT NONE

C     
C     PRECIPITATING DEEP CONVECTION SCHEME.
C     ADJUSTMENT TOWARDS A SUBSATURATED MOIST ADIABAT.
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
C     
      INTEGER NCRB,NCRT,J,IHEM

      REAL PL(NL),PSL(NL),PSK(NL),TSL(NL),TR(NL),QR(NL),CBC(NL),GH1(NL)

      INTEGER NLEV,L,NIT
      REAL    PQSAT,SINT,TGINT,QGINT,TRINT,QRINT,QN,TTEND
      REAL    QTEND,P,CTQUSE,EPSIT,DHL,DTMAX,DTSL,HGINT

      REAL,EXTERNAL :: SSUM,SDOT

      REAL QG(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))

c      NOW IN NAMELIST
c      real pcloudmin
c      parameter(pcloudmin=0.15)
c      real pcloudfact
c      parameter(pcloudfact=0.125)
C     
C     Choose appropriate value of latent heat based on ground T
C     ie, we assume it snows if the ground is colder than 273.15K
C     
      if (tstar(j,jh).gt.0.363224029) then
         ctquse=ctq
      else
         ctquse=ctqi
      endif
      NLEV=1+NCRB-NCRT
      EPSIT=CTQUSE*EPSIQ
C     
      DO L=NCRT,NCRB
         PL(L)=SIGMA(L)*PLG(J)
         PSL(L)=PL(L)+CBADJP
         PSK(L)=(PSL(L)/PL(L))**AKAP
         CBC(L)=CTQUSE*PSK(L)/PSL(L)
         TSL(L)=TC(L)*PSK(L)
         QR(L)=PQSAT(TSL(L))/PSL(L)
      END DO
C     
      SINT=SSUM(NLEV,DSIGMA(NCRT),1)
      TGINT=SDOT(NLEV,TG(J,NCRT),IGC,DSIGMA(NCRT),1)
      QGINT=SDOT(NLEV,QG(J,NCRT),IGC,DSIGMA(NCRT),1)
      TRINT=SDOT(NLEV,TC(NCRT),1,DSIGMA(NCRT),1)
      QRINT=SDOT(NLEV,QR(NCRT),1,DSIGMA(NCRT),1)
      HGINT=TGINT+CTQUSE*QGINT
      DHL=(HGINT-TRINT-CTQUSE*QRINT)/SINT
      DO L=NCRT,NCRB
         GH1(L)=(TC(L)+CTQUSE*QR(L)+DHL)*PSK(L)
      END DO
C     
      NIT=0
 20   NIT=NIT+1
      DTMAX=0.0
      DO L=NCRT,NCRB
         QN=CBC(L)*PQSAT(TSL(L))
         DTSL=(TSL(L)+QN-GH1(L))/(1.0+ESCONB*QN/(TSL(L)*TSL(L)))
         DTMAX=MAX(ABS(DTSL),DTMAX)
         TSL(L)=TSL(L)-DTSL
      END DO
      IF (DTMAX.LT.EPSIT) GOTO 40
      IF (NIT.GT.10) GOTO 40
      GOTO 20
 40   CONTINUE
C     
      DO L=NCRT,NCRB
         TR(L)=TSL(L)/PSK(L)
         QR(L)=PQSAT(TSL(L))/PSL(L)
      END DO
C     
      QRINT=SDOT(NLEV,QR(NCRT),1,DSIGMA(NCRT),1)
      IF(QRINT.GT.QGINT) THEN
C     *      WRITE(6,2001) J,JH,NCRB,NCRT,CQ*QRINT/SINT,CQ*QGINT/SINT
         IF(LCUBM)      CALL CUBM (NCRB,NCRT,J,IHEM)
         IF(.NOT.LCUBM) CALL CUDIF(NCRB,NCRT,J,IHEM)
      ELSE
         TRINT=SDOT(NLEV,TR(NCRT),1,DSIGMA(NCRT),1)
         DHL=(HGINT-TRINT-CTQUSE*QRINT)/SINT
         DO L=NCRT,NCRB
            TR(L)=TR(L)+DHL
            TTEND=(TR(L)-TG(J,L))/CBADJT
            QTEND=(QR(L)-QG(J,L))/CBADJT
            TTMC(L)=TTMC(L)+TTEND
            QTMC(L)=QTMC(L)+QTEND
            TG(J,L)=TG(J,L)+DELT2C*TTEND
            QG(J,L)=QG(J,L)+DELT2C*QTEND
            CTCR(L,IHEM)=CTCR(L,IHEM)+1.0
         END DO
         CTCR(NCRB,IHEM)=CTCR(NCRB,IHEM)-1.0
         RRCR(J)=RRCR(J)-RCON*PLG(J)*(QRINT-QGINT)/CBADJT
C
C     deep convection parameterised by precip.:  P is mm/day
C
         P=-REAL(ITSPD)*RCON*PLG(J)*(QRINT-QGINT)/CBADJT
         IF (P.GT.pcloudmin) THEN
            CFRAC(J,5)=-pcloudfact*log(pcloudmin)+pcloudfact*LOG(P)
            CFRAC(J,5)=MAX(0.0,MIN(1.0,CFRAC(J,5)))
            ICFLAG(J,5,1)=NCRB
            ICFLAG(J,5,2)=NCRT
         ENDIF
      ENDIF
C     
      RETURN
      END
