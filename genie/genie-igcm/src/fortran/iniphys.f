      SUBROUTINE INIPHYS

      IMPLICIT NONE
      
C     
C     Sets up PHYSICS variables and arrays. Sets NAMELIST variables
C     to their default settings, then reads NAMELIST
C     (Piers Forster, 23/10/96)
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'bats.cmn'
      include 'legau.cmn'
      include 'physca.cmn'
      include 'igcm_vdiff.cmn'
      include 'igcm_cloud.cmn'
      include 'igcm_radn.cmn'
      include 'igcm_nlevs.cmn'
      include 'igcm_orbit.cmn'      
      include 'butterfly.cmn'
      include 'igcm_prec.cmn'
      include 'convtie.cmn'
      include 'varalb.cmn'
      include 'flux_adjust.cmn'
      include 't42.cmn'

      INTEGER NCNT,L,IHEM,J
      REAL    FW,FRADC,CLATNTI,SFAC,CDIF,FC

      integer ios

C     
C     physics namelist
C     
      NAMELIST/INPHYS/LVD,LCR,LCR_TIEDTKE,LLR,LRD,LCUBM,LCBADJ
     :     ,CD,AKVV,AKTV,AKQV,AKTC,AKQC,NLCR,NCUTOP,FW,FRADC
     :     ,CURHM,CUBMT,CBADJT,CBADJP
     :     ,LNOICE,LOLDBL,LCOND,LNNSK,ITSLL,ITSLO
     :     ,PSMIN, TRCNV, RHBL, RHIL, ENTMAX, SMF
     :     ,TRSHC, TRVDI, TRVDS, REDSHC, RHGRAD, SEGRAD     
     :     ,lownprecip
C     
c     vdiff namelist
      NAMELIST/INIVDIFF/increaseu,increaseq,increaset

c     cloud namelist
      NAMELIST/INICLOUD/pcloudfact,pcloudmin,cloudshallow,
     :    humcloudmin,humcloudmax,powercloud,
     :    cloudhfact,cloudmfact,cloudlfact

c     radiation namelist
      NAMELIST/INIRADN/zradef,
     :      supersatlarg,supersatconv,
     :      cloudconvnorm,cloudconvbase,
     :      emissconsta,emissconstb

c     orbit nameilst
      NAMELIST/INIORBIT/p_zecc,
     :      p_zobl,p_zw,p_zproff,
     :      exact_orbit,vary_exact_orbit,
     :      dt_vals,iniday,
     :      fname_orbit_igcm

c     nlevs namelist
      NAMELIST/ININLEVS/difusestrength,coldstart,lcalc_blayer_vals

c     butterfly namelist
      NAMELIST/INIBUTTERFLY/lbutterfly,butterfly_u

c     varying albedo namelist
      NAMELIST/INIVARALB/lvar_albedo

c     flux correction namelist
      NAMELIST/INIFLUXADJUST/water_transport,
     :  lfluxadjust_water,
     :  fname_fluxadjust

c     t42 namelist
      NAMELIST/INIT42/tempzero,coldstart_grid,coldstart_gmsp

      LNOICE=.FALSE.
      LOLDBL=.FALSE.
      LCOND=.TRUE.
      LNNSK=.TRUE.
      ITSLL=6.
      ITSLO=6.
C     
 240  FORMAT(' PHYSICAL PROCESSES INCLUDED ARE AS FOLLOWS:')
c 241  FORMAT(' SURFACE FLUXES : CD, RH-FACTOR =',F10.4,F10.1,'%')
 242  FORMAT(' CONVECTION THROUGH',I4,' LAYERS FROM LOWEST LEVEL'/
     :     ' DRY CONVEC: ADJUSTMENT TO NEUTRALITY IN SINGLE TIMESTEP')
 246  FORMAT(' PRECIPITATING CONVECTION : NON-ENTRAINING CLOUD MODEL'
     :     ,' WITH ENVIRONMENTAL SDOT =',E7.1)
 247  FORMAT(' PRECIPITATING CONVECTION : ADJUSTMENT TO SUBSATURATED'
     :     ,' MOIST ADIABAT WITH SCRIPT-P(MB) TAU(HOURS) =',2F6.1)
 248  FORMAT(' NON-PRECIPITATING DIFFUSIVE MOIST CONVECTION WITH'
     :     ,' BACKGROUND KT KQ =',2F6.1,' M2/S')
 249  FORMAT(' NON-PRECIPITATING BETTS-MILLER MOIST CONVECTION WITH'
     :     ,' BACKGROUND TIMESCALE ',F6.1,' HOURS')
 250  FORMAT(' FORMALLY NON-PRECIPITATING CONVECTION INCLUDES RAINOUT'
     :     ,' EXCESS OVER',F6.1,'% RH')
 243  FORMAT(' LARGE-SCALE CONDENSATION TO SATURATION IN TIMESTEP')
 244  FORMAT(' RADIATION: MORCRETTE CODE(SMR+PMF), DIAGNOSTIC CLOUD
     :     VERSION 2.0')
 245  FORMAT(' VERTICAL DIFFUSION OF MOMENTUM HEAT AND MOISTURE WITH',
     :     ' CONSTANT COEFFS AKVV AKTV AKQV =',3F8.2)
C     
C     Preset namelist values.
C     
      LVD=.FALSE.
      LCR=.FALSE.
      LLR=.FALSE.
      LRD=.FALSE.
      LCUBM=.FALSE.
      LCBADJ=.FALSE.
      CD=0.001
      AKVV=1.0
      AKTV=1.0
      AKQV=1.0
      AKTC=10.0
      AKQC=10.0
      FW=1.0E-6
      FRADC=1.25
      CURHM=200.0
      CUBMT=3.0
      CBADJT=3.0
      CBADJP=-30.0      
      PSMIN=0.8 
      TRCNV=6.0 
      RHBL=0.9 
      RHIL=0.7 
      ENTMAX=0.5 
      SMF=0.8
      TRSHC=6.0 
      TRVDI=24. 
      TRVDS=6.0 
      REDSHC=0.5 
      RHGRAD=0.5 
      SEGRAD=0.1 
C     
C     INITIALISE PARAMETERS FOR PHYSICAL PROCESSES
C     
      lownprecip=.false.
      NLCR=NLM
      NCNT=0
      DO L=1,NL
         IF(SIGMA(L).LT.0.75) GOTO 188
         NCNT=NCNT+1
 188     CONTINUE
      END DO
      NCUTOP=NLP-NCNT
      read(UNIT=7,NML=INPHYS,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INPHYS namelist'
         stop
      end if
      WRITE(6,INPHYS)
      WRITE(6,240)
c      WRITE(6,241)CD
      IF(LCR) WRITE(6,242)NLCR
      IF(LCR.AND..NOT.LCBADJ)WRITE(6,246)FW
      IF(LCR.AND.LCBADJ)     WRITE(6,247)CBADJP,CBADJT
      IF(LCR.AND..NOT.LCUBM) WRITE(6,248)AKTC,AKQC
      IF(LCR.AND.LCUBM)      WRITE(6,249)CUBMT
      IF(LCR.AND.(CURHM.LT.100.01))WRITE(6,250)CURHM
      IF(LLR) WRITE(6,243)
      IF(LRD) WRITE(6,244)
      IF(LVD) WRITE(6,245)AKVV,AKTV,AKQV
      CLATNTI=2.834E6
      SFAC=0.5**KITS
      IF(LRSTRT.AND..NOT.LSHORT) SFAC=1.0
      CTQ=CLATNT/(CPD*CT)
      CTQI=CLATNTI/(CPD*CT)
      if (LNOICE) CTQI=CTQ
      CCC=CLATNT*CLATNT/(RV*CPD*CT*CT)
      ESCONB=CLATNT/RV
      ESCONA=RD*EXP(ESCONB/273.15)*610.7/(RV*P0)
      ESCONB=ESCONB/CT
      EPSIQ=0.01/CQ
C     Used to be multiplied by old (constant) CD. Now done in BLAYER
      DRAG=GA*RADEA/(RD*CT*DSIGMA(NL))

      BLA=500.0/CV              ! A=500m/s
      FW=FW/WW
      DTBUOY=1.0E-5/CT
      TSLA=55.0/CT
      TSLB=2840.0/CT
      TSLC=3.5
      TSLD=3.5*LOG(CT)-LOG(P0)-0.67485
      CBADJT=CBADJT*3600.0*WW
      CBADJP=CBADJP*100.0/P0
      CURHM=CURHM/100.0
      CUBMT=CUBMT*3600.0*WW
C     
C     Used to be divided by old CD, now done in CUDIF and CUBM
C     
      CUT1=1.0E4*DSIGMA(NL)/(RADEA*0.2)
      CUT2=1.0E4*DSIGMA(NLM)/RADEA
      CUT2=CUT2*CUT2*0.5
      AKTC=AKTC/(RADEA*CV)
      AKQC=AKQC/(RADEA*CV)
      CDIF=GA/(CV*WW)
      CDIF=CDIF*CDIF
      RCON=P0*DELT/(SFAC*GA)
      CCR=RCON*FW
C     
C     Initialise values for the soil model
C     
      sdsnd=300.                ! density of snow (kg/m3) dimensional
      sdsn=sdsnd*(RD*CT)/P0     ! density of snow dedimensionalised
      sdw=1000.*(RD*CT)/P0      ! density of water (kg/m3)
      shcs=2085.0E3*CT/P0       ! rho*c for 50% saturated soil
      shcsp=15.312E6*CT/P0      ! equ. heat cap. freezing of soil water
      shcsn=627.0E3*CT/P0       ! rho*c for snow
      skse=1.072818*CT/(RADEA*CV*P0) ! effective soil conductivity
      sksn=0.24*CT/(RADEA*CV*P0) ! thermal conductivity for snow
      slhf=3.5E5/(RD*CT)        ! latent heat of fusion of ice
      sd1=0.06/RADEA            ! depth of upper soil level
      sd2=2.2999/RADEA          ! depth of lower soil level
      ssmc=0.5/RADEA            ! depth equivalent to capacity of model
C     
C     Albedos for snow and sea ice, depth for snow albedo function
C     
      sasnow=0.8                ! deep snow albedo
      saice=0.6                 ! ice albedo
      shsstar=0.30/RADEA        ! depth snow for albedo transfer funct.
C     
C     max depth of snow for heat capacity of top soil layer
C     Note - equal to e-folding depth/sqrt(2)
C     
      shsmax=1.4/RADEA
      shco=25.*1000.*4190.*CT/P0/RADEA ! heat capacity of ocean mixed
                                ! layer. Mixed layer depth 25m.
      write(6,*)'Mixed Layer Depth is 25m'
      shci=2.*900.*2100.*CT/P0/RADEA
      DO  L=1,NLM
         CLR(L)=RCON*DSIGMA(L)
         FWS(L) =-FW*RDSIG(L)
         FB(L)=CDIF*4.0*SIGMAH(L)*SIGMAH(L)/(SIGMA(L+1)-SIGMA(L))
         SKAP(L)=SIGMA(L)**AKAP
         SK(L)  =(SIGMA(L)/SIGMA(L+1))**AKAP
         WVI(L,1)=1./(LOG(SIGMA(L+1))-LOG(SIGMA(L)))
         WVI(L,2)=(LOG(SIGMAH(L))-LOG(SIGMA(L)))*WVI(L,1)
         GRDSIG(L) = GA/(DSIGMA(L)*P0)
         GRDSCP(L) = GRDSIG(L)/CPD 
         XGEOP1(L)=RD*LOG(SIGMAH(L)/SIGMA(L))
         XGEOP2(L+1)=RD*LOG(SIGMA(L+1)/SIGMAH(L))
      END DO
      CLR(NL)=RCON*DSIGMA(NL)
      FWS(NL)=-FW*RDSIG(NL)
      SKAP(NL)=SIGMA(NL)**AKAP
      FC=FRADC/(PI2*CT)
      XGEOP1(NL)=RD*LOG((SIGMAH(NLM)+DSIGMA(NL))/SIGMA(NL))
      WVI(NL,1)=0.
      WVI(NL,2)=(LOG(SIGMAH(NLM)+DSIGMA(NL))-LOG(SIGMA(NL)))
     :*WVI(NL-1,1)
      GRDSIG(NL) = GA/(DSIGMA(NL)*P0)
      GRDSCP(NL) = GRDSIG(NL)/CPD     
!     
!     Cooling rate now identical at all latitudes.
!     
      DO IHEM=1,NHEM
         DO J=1,JG  
            FRAD(J,IHEM)=-FC
         END DO
      END DO
C     
      AKVV=AKVV/(RADEA*CV)
      AKTV=AKTV/(RADEA*CV)
      AKQV=AKQV/(RADEA*CV)
      NAVRD=20
      NAVWT=21
C     
      increaseu=5.0
      increaseq=1.0
      increaset=1.0
      read(UNIT=7,NML=INIVDIFF,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIVDIFF namelist'
         stop
      end if
      WRITE(6,INIVDIFF)


      pcloudfact=0.125
      pcloudmin=0.15
      cloudshallow=0.35
      humcloudmin=0.8
      humcloudmax=1.0
      powercloud=2.0
      cloudhfact=1.0
      cloudmfact=1.0
      cloudlfact=1.0
      read(UNIT=7,NML=INICLOUD,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INICLOUD namelist'
         stop
      end if
      WRITE(6,INICLOUD)

      zradef=5.0
      supersatlarg=0.01
      supersatconv=0.01
      cloudconvnorm=0.25
      cloudconvbase=1.0
      emissconsta=1.08942549
      emissconstb=100.0
      read(UNIT=7,NML=INIRADN,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIRADN namelist'
         stop
      end if
      WRITE(6,INIRADN)

c/climap
c/values of:        ZECC      ZOBL    ZW      ZPROFF
c/for
c/ present day:     0.016724  23.440  282.04  -2.35
c/          1k:     0.017116  23.570  264.96  +14.88
c/          6k:     0.018682  24.113  180.86  +102.10
c/          9k:     0.019280  24.238  131.26  +154.12
c/         18k:     0.019454  23.445  343.76  -63.66
c/         21k:     0.018994  22.944  294.49  -15.08
c/        115k:     0.041421  22.411  291.02  -14.00    

c     More values can be obtained from DJL if required.
c     I'll put the code on CVS asap!!
 
      p_zecc=0.016724
      p_zobl=23.440
      p_zw=282.04
      p_zproff=-2.35
      exact_orbit=.false.
      vary_exact_orbit=.false.
      fname_orbit_igcm='xxx'
      iniday=0
      dt_vals=1
      read(UNIT=7,NML=INIORBIT,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIORBIT namelist'
         stop
      end if
      WRITE(6,INIORBIT)
c     ------------------------
c     These lines also in solang.f:
      p_zobl=p_zobl*pi/180.
      p_zw=p_zw*pi/180.        
c     Should change this to 360......?
      p_zproff=p_zproff*2.*pi/365.
c     ------------------------

      difusestrength=0.0625
      coldstart=.false.
      lcalc_blayer_vals=.false.
      read(UNIT=7,NML=ININLEVS,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM ININLEVS namelist'
         stop
      end if
      WRITE(6,ININLEVS)

      lbutterfly=.false.
      butterfly_u=0.0
      read(UNIT=7,NML=INIBUTTERFLY,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIBUTTERFLY namelist'
         stop
      end if
      WRITE(6,INIBUTTERFLY)
      
      lvar_albedo=.false.
      read(UNIT=7,NML=INIVARALB,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIVARALB namelist'
         stop
      end if
      WRITE(6,INIVARALB)

      water_transport(1:3)=0.0
      fname_fluxadjust='xxx'
      lfluxadjust_water=.false.
      read(UNIT=7,NML=INIFLUXADJUST,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIFLUXADJUST namelist'
         stop
      end if
      WRITE(6,INIFLUXADJUST)

      tempzero=0.0
      coldstart_grid=.false.
      coldstart_gmsp=.false.
      read(UNIT=7,NML=INIT42,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIT42 namelist'
         stop
      end if
      WRITE(6,INIT42)

      RETURN
      END
