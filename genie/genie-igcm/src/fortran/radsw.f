      SUBROUTINE RADSW (ZTAVE,ZWV,PQOF,PCLFR,PQLWP,AMU0,
     :                  ZCARDI,ZSCT,PDP,PR,SWALB,FLS,GA)

      IMPLICIT NONE

C
C inputs
C ZTAVE - misd slab temps(K) all vertical -level 1 ground
C ZWV - water MMR
C PQOF -ozone mmr
C PCLFR - cloud fractions
C PQLWP - cloud Liquid water path  g/m^2
C PR - flux pressures (mb)
C -sWALB  surface albedo
C -AMU0    cosine
C
C ZCARDI Co2 mass mixing ratio
C ZSCT - solar constant * day length * earth-sun factor(Wm2)
C
C Outputs
C FLS - NET SW fluxes level 1 ground
C PDP - shortwave DPs
C
C        ORIGINAL : 23-2-98
C        Calls SW scheme and returns fluxes
#include "param1.cmn"
      include 'igcm_radn.cmn'
      INTEGER NLON,NLEV,NLM,KDLON,JLON,JK,IAER,KFLEV
     : ,JAE,NLP
      PARAMETER(NLON=1,NLEV=NL+1)
      PARAMETER(NLM=NLEV-1,KDLON=NLON
     :          ,KFLEV=NLEV,NLP=KFLEV+1)
      REAL PRMU0(NLON),SWALB,FLS(NLP)
      REAL PQLWP(NLON,NLEV),AMU0
      REAL ZEPSCO,ZEPSC,ZEPSCQ,ZEPSCT,ZEPSCW,ZEELOG
c      REAL CDAY
c      real DAYL
      REAL ZALBSU(NLON,2),ZFSUP(NLON,NLP),ZFSDWN(NLON,NLP)
     :,     ZPMB(NLON,NLP)
      REAL ZTAVE(NLON,NLEV),ZOZ(NLON,NLEV),ZCLDSW(NLON,NLEV)
     :,    ZTAU(NLON,2,NLEV),ZCG(NLON,2,NLEV)
     :,    ZOMEGA(NLON,2,NLEV),PAER(NLON,NLEV,5)
c      real ZHEATR(NLON,NLEV)
c      REAL ZLWGKG,ZFCC
      REAL ZFLWP,PIERSLWP
     :,    ZTAUEQ
C      REAL ZFCCA,ZFCCB
      REAL PPSOL(NLON),PR(NLON,NLEV),
     :      PQOF(NLON,NLEV),PCLFR(NLON,NLEV),ZWV(NLON,NLEV)
      REAL PDP(NLON,NLEV)
      REAL ZZRMUZ
      REAL ZSCT,ZCARDI
      REAL GA
c      real zradef
C------------------------------------------------------------
C
C      ZCARDI=546.0E-6  !360 ppmv C02

      IAER=0
      PRMU0(1)=AMU0
C    CONTROL VARIABLES FOR RADIATION: INTERNAL SWITCH AND INDICES
C
C ZEELOG:  SECURITY THRESHOLD FOR ABSORBER AMOUNT IN LAPLACE TRANSFORM
C ZEPSC :  SECURITY THRESHOLD FOR CLOUD COVER
C ZEPSCO:  SECURITY THRESHOLD FOR OZONE AMOUNT
C ZEPSCQ:  SECURITY THRESHOLD FOR WATER VAPOR
C ZEPSCT:  SECURITY THRESHOLD FOR SHORTWAVE OPTICAL THICKNESS
C ZEPSCW:  SECURITY THRESHOLD FOR CLOUD LIQUID WATER PATH
C
      ZEPSC  = 1.E-04
      ZEPSCO = 1.E-10
      ZEPSCQ = 1.E-07
      ZEPSCT = 1.E-04
      ZEPSCW = 1.E-12
      ZEELOG = 1.E-10
C
c      DAYL=86400.0  !Question
C
C*         1.1    INITIALIZE VARIOUS FIELDS
C                 -------------------------

      DO 113 JLON = 1 , KDLON
         ZALBSU(JLON,1)=SWALB
         ZALBSU(JLON,2)=SWALB
         ZFSUP(JLON,KFLEV+1) = 0.
         ZFSDWN(JLON,KFLEV+1) = ZEPSCO
         PPSOL(JLON)=100.*PR(JLON,1)   ! surface Pa
         ZPMB(JLON,1)=PPSOL(JLON)/100.0  ! mb surface p
 113  CONTINUE
        ZPMB(1,NLP)=0.0
        ZPMB(1,2)=ZPMB(1,1)  ! bottom level has no thickness
        DO JK=KFLEV,1,-1
         ZPMB(1,JK+1) = PR(1,JK)
        ENDDO
        DO JK=2,KFLEV
         PDP(1,JK)=100.0*(ZPMB(1,JK)-ZPMB(1,JK+1))
        ENDDO
         PDP(1,1)=0.0
      DO 115 JK = 1 , KFLEV
         DO 114 JLON = 1 , KDLON
         ZOZ(JLON,JK)   = PDP(JLON,JK)*PQOF(JLON,JK) * 46.6968 / GA
         ZTAU(JLON,1,JK) = ZEPSCW
         ZTAU(JLON,2,JK) = ZEPSCW
         ZOMEGA(JLON,1,JK) = 0.9994
         ZOMEGA(JLON,2,JK) = 0.9963
         ZCG(JLON,1,JK) = 0.865
         ZCG(JLON,2,JK) = 0.910
c         ZHEATR(JLON,JK) = 0.
         ZFSUP(JLON,JK) = 0.
         ZFSDWN(JLON,JK) = 0.
 114     CONTINUE
 115  CONTINUE
C
C
C*         2.     CLOUD AND AEROSOL PARAMETERS
C                 ----------------------------
      PIERSLWP=0.0
      DO 202 JK = 2 , KFLEV-1
       DO 201 JLON = 1 , KDLON
         IF (PCLFR(JLON,JK).GT.ZEPSC) THEN
c             ZLWGKG =PQLWP(JLON,JK)*1000.
c             ZFLWP= ZLWGKG*PDP(JLON,JK)/(GA*PCLFR(JLON,JK))
              ZFLWP=PQLWP(JLON,JK)
         ELSE
             ZFLWP=ZEPSC
         ENDIF
         PIERSLWP=PIERSLWP+ZFLWP
         IF (PCLFR(JLON,JK).GT.ZEPSC) THEN
c          NOW IN NAMELIST
c          ZRADEF = 5.0
          ZTAUEQ = 1.5 * ZFLWP* PCLFR(JLON,JK)/ ZRADEF
          ZTAU(JLON,1,JK) = ZTAUEQ
          ZTAU(JLON,2,JK) = ZTAUEQ
          ZOMEGA(JLON,1,JK) = 0.9999 - 5.0E-04*EXP(-0.5 * ZTAUEQ)
          ZOMEGA(JLON,2,JK) = 0.9988 - 2.5E-03*EXP(-0.05 * ZTAUEQ)
          ZCG(JLON,1,JK)=0.865
          ZCG(JLON,2,JK)=0.910
         ENDIF
         ZCLDSW(JLON,JK) = MAX(PCLFR(JLON,JK),ZEPSCW)
 201   CONTINUE
 202  CONTINUE
C
      DO 203 JLON = 1 , KDLON
        ZPMB(JLON,KFLEV+1)=0.0
 203  CONTINUE
C
      IF (IAER.EQ.0) THEN
       DO 206 JK = 1 , KFLEV
        DO 205 JAE = 1 , 5
         DO 204 JLON = 1 , KDLON
         PAER(JLON,JK,JAE)=1.E-15
 204     CONTINUE
 205    CONTINUE
 206   CONTINUE
      END IF
C
      ZZRMUZ=0.
      DO 401 JLON = 1 , KDLON
         ZZRMUZ = MAX(ZZRMUZ, AMU0)
 401  CONTINUE
C
      IF (ZZRMUZ.GT.0.) THEN
C
         CALL SW
     :  (IAER,PAER,ZEPSCQ,ZEPSCT,ZEELOG,
     :   ZSCT,PRMU0,ZCARDI,PPSOL,ZWV,
     :   ZCLDSW,ZOZ,ZPMB,ZTAVE,ZFSDWN,ZFSUP,
     :   ZCG,ZOMEGA,ZTAU,ZALBSU)
C

      END IF
C
C
C     ---------------------------------------------------------------
C
C*         5.     FILL UP THE MODEL NET LW AND SW RADIATIVE FLUXES
C                 ------------------------------------------------
C
      DO 503 JK = NLEV+1,1,-1
         DO 502 JLON = 1 , KDLON
            FLS(JK) = ZFSDWN(JLON,JK) - ZFSUP(JLON,JK)
 502     CONTINUE
 503  CONTINUE

C
C
C*         5.1     HEATING/COOLING RATES (K/DAY)

CCC  Combine the lowest two layer as one layer to correspond
CCC  the radiative convective model. (leave the lowest level
CCC  heating rate undefined---set to zero)   D.LI 8/8 1991
c      DO 518 JLON = 1, KDLON
c       ZDFNET = ZFSUP(JLON,1) - ZFSDWN(JLON,1)
c     S        -ZFSUP(JLON,3) + ZFSDWN(JLON,3)
c       ZHEATR(JLON,2) = CDAY*ZDFNET/(PDP1(JLON,2)+PDP1(JLON,1))
c       ZHEATR(JLON,1)=0.0D0
c 518  CONTINUE
CCC
C
      RETURN
      END
