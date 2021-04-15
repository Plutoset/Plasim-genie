      SUBROUTINE INISET
      implicit none
C     
C     Sets up various variables and arrays. Sets NAMELIST variables
C     to their default settings, then reads NAMELIST
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'bats.cmn'
      include 'outcon.cmn'
      include 'comfft.cmn'
      include 'polyno.cmn'
      include 'restor.cmn'
      include 'stats.cmn'
      include 'files.cmn'
      include 'climatologies.cmn'
C     
      INTEGER IFAIL,ibegyear,ndel,l,i,nfpp,nw,
     :     np,ndelh,nwjch,mp,jp,kk,ntrwg,ntrgw,ntrgd,nrow,lnsig1
     :     ,ncid
      LOGICAL LEXIST
      real tspd,tdiss,akk
c     hours per day
      real HPD
c     seconds per hour
      real SPH

      integer ios
C     
      REAL*8 AKK1
      NAMELIST/INPPL/ GA,GASCON,RADEA,AKAP,P0,RV,CLATNT,HPD
C     
      NAMELIST/INPRN/ KRUN,BEGDAY,TSPD,KITS,PNU,TDISS
     +     ,LFLUX
     +     ,IBEGYEAR
     +     ,NDEL,T0,LRSTRT,LSTRETCH,LSHORT,LTVEC,LBALAN,LRESTIJ
     +     ,LCLIM, LPERPET, L22L,LOROG,LCSFCT
     +     ,KOLOUR,LNOISE,LMASCOR,LMASPRT,lcheckwater
C     
      NAMELIST/INPOP/RNTAPE,KOUNTH,KOUNTR,KOUNTE
     +     ,LGPO,LSPO,lnetcdf,ldaily,lmonthly,lannual,ldecadal
     +     ,RNTAPO,NTRACO
     $     ,LSHIST,LMINIH
C     
      namelist/files/
     :     fname_gastabl,fname_gastabn,
     :     fname_plfunc,fname_spectral,fname_gridpt,
     :     fname_vegetation,fname_runoff,
     :     fname_waterlines,fname_orog_spec,
     :     fname_ozone,fname_watervapour,fname_sstetc,
     :     outputdir_name
C     
 205  FORMAT(/' *****RNTAPE*****',F12.3)
 207  FORMAT(' PRINTED OUTPUT EVERY ',I3,' TIMESTEPS'/
     +     ' RMS QUANTITIES OUTPUT EVERY ',I3,' TIMESTEPS'/
     +     ' HISTORY RECORD WRITTEN EVERY ',I3,' TIMESTEPS'/
     +     ' RESTART RECORD WRITTEN EVERY ',I3,' TIMESTEPS')
 209  FORMAT(' INTEGRATION WITH',I3,
     +     ' LEVELS IN THE VERTICAL (NL=',I3,')'/
     +     ' JAGGED TRIANGULAR/TRAPEZOIDAL TRUNCATION AT TOTAL WAVENO. '
     +     ,I3/' AND ZONAL WAVENO. ',I3,' (NN=',I3,' MM=',I3,')')
 221  FORMAT(' NO LATERAL DISSIPATION')
 222  FORMAT(' DEL',I2,' LATERAL DISSIPATION ON VORTICITY, DIVERGENCE'/
     +     ' AND TEMPERATURE WITH DIFFUSION COEFFICIENT  ',E10.4,
     +     ' m**',I1,'/s'/' THE E-FOLDING TIME FOR SMALLEST RESOLVED',
     +     ' SCALE IS ',F5.3,' DAYS')
 223  FORMAT(' NO TIME FILTER')
 224  FORMAT(' ROBERT TIME FILTER WITH PARAMETER PNU ',F5.2)
 280  FORMAT(' GLOBAL DOMAIN: BOTH EVEN AND ODD COEFFICIENTS INCLUDED',
     +     ' (NHEM=',I1,')')
 281  FORMAT(' HEMISPHERIC DOMAIN: ONLY EVEN DIVERGENCE TEMPERATURE'/
     +     ' SURFACE PRESSURE AND ODD VORTICITY COEFFICIENTS INCLUDED',
     +     ' (NHEM=',I1,')')
 210  FORMAT(' ',I2,'-FOLD SYMMETRY IN LONGITUDE IMPOSED AND ONLY'/
     +     ' 1 /',I2,' OF THE DOMAIN USED (MOCT=',I2,')')
 211  FORMAT(' NON LINEAR TERMS EVALUATED ON GRID OF ',I3,
     +     ' GAUSSIAN LATITUDES '/' AND ',I3,
     +     ' EVENLY SPACED LONGITUDES (JG=',I3,' MG=',I3,')')
 212  FORMAT(' ECMWF ANGULAR MOMENTUM CONSERVING VERTICAL SCHEME')
 213  FORMAT(' GLOBAL MASS CORRECTION SWITCHED ON')
 214  FORMAT(' GLOBAL MASS CORRECTION SWITCHED OFF')
 225  FORMAT(/' ***ABORT*** CORRECT VALUE OF NWJ2',I5,' VALUE GIVEN',I5)
 240  FORMAT(/' ***ABORT*** NTRACO IS GREATER THAN NTRAC')
C
      PARAMETER (SPH=3600.0)
C     Set default values and override as desired through NAMELIST input
C     Mass correction namelist defaults
C     
C     mass correction switched on
      LMASCOR=.TRUE. 
C     mass correcion printed every KOUNTD steps
      LMASPRT=.FALSE.           
C     
      GA=9.80665
      GASCON=287.0
      RADEA=6371000.0
      AKAP=0.286
      P0=100000.0
      RV=461.51
      CLATNT=2.5E6
      HPD=24.0
c
c     removed from namelist
c     WW=7.292E-5
      WW=2.0*pi/(HPD*SPH)
      CPD=GASCON/AKAP
C     
      KRUN=0
      BEGDAY=0.0
      IBEGYEAR=2000
      INETCOUNT=0
      KFLAG=0
      IMONTH_CH=0
      TSPD=24.0
      KITS=3
      PNU=0.02
      TDISS=0.25
      NDEL=6
      DO 17 L=1,NL
         T0(L)=250.0
 17   CONTINUE
      LRSTRT=.FALSE.
      LSTRETCH =.FALSE.
      LPERPET=.TRUE.
      LCLIM=.FALSE.
      L22L=.TRUE.
      LOROG=.FALSE.
      LCSFCT=.TRUE.
      LSHORT=.FALSE.
      LTVEC =.TRUE.
      LFLUX=.TRUE.
      LBALAN=.FALSE.
      LRESTIJ=.FALSE.
      LNOISE=.FALSE.
      lcheckwater=.false.
C     
      RNTAPE=0.0
      RNTAPO=-999.0
      NTRACO=0
      KOUNTH=0
      KOUNTR=0
      KOUNTD=0
      KOUNTE=0
      LSHIST=.TRUE.
      LMINIH=.TRUE.
      DO 18 I=1,NL
         LSPO(I)=.FALSE.
         LGPO(I)=.FALSE.
 18   CONTINUE
      lnetcdf=.true.
      ldaily=.true.
      lmonthly=.true.
      lannual=.true.
      ldecadal=.true.
      lannual_restart=lannual
      ldecadal_restart=ldecadal
C     
      fname_gastabl='xxx'
      fname_gastabn='xxx'
      fname_plfunc='xxx'
      fname_spectral='xxx'
      fname_gridpt='xxx'
      fname_vegetation='xxx'
      fname_runoff='xxx'
      fname_waterlines='xxx'
      fname_orog_spec='xxx'
      fname_ozone='xxx'
      fname_watervapour='xxx'
      fname_sstetc='xxx'
C     
C     Read NAMELISTs, overwrite defaults and write them out
C     

      read(UNIT=7,NML=INPPL,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INPPL namelist'
         stop
      end if

      WRITE(6,INPPL)

      read(UNIT=7,NML=INPRN,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INPRN namelist'
         stop
      end if

      WRITE(6,INPRN)

      read(UNIT=7,NML=INPOP,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INPOP namelist'
         stop
      end if

      IF (RNTAPO.EQ.-999.0) RNTAPO=RNTAPE
      WRITE(6,INPOP)

      read(UNIT=7,NML=FILES,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM FILES namelist'
         stop
      end if
C     
      lannual_restart=lannual
      ldecadal_restart=ldecadal
c
      if (fname_gastabl.eq.'xxx') then
         print*,' Missing filename for gastab lblm file '
         ifail=1
      else
         ifname_gastabl=lnsig1(fname_gastabl)
         inquire(file=fname_gastabl(1:ifname_gastabl),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',fname_gastabl(1:ifname_gastabl)
            ifail=1
         end if
      end if
c
      if (fname_gastabn.eq.'xxx') then
         print*,' Missing filename for gastab nbm file'
         ifail=1
      else
         ifname_gastabn=lnsig1(fname_gastabn)
         inquire(file=fname_gastabn(1:ifname_gastabn),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',fname_gastabn(1:ifname_gastabn)
            ifail=1
         end if
      end if
c
      if (fname_plfunc.eq.'xxx') then
         print*,' Missing filename for plfunc file'
         ifail=1
      else
         ifname_plfunc=lnsig1(fname_plfunc)
         inquire(file=fname_plfunc(1:ifname_plfunc),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',fname_plfunc(1:ifname_plfunc)
            ifail=1
         end if
      end if
c
      if (fname_spectral.eq.'xxx') then
         print*,' Missing filename for spectral restart '
         ifail=1
      else
         ifname_spectral=lnsig1(fname_spectral)
         inquire(file=fname_spectral(1:ifname_spectral),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',fname_spectral(1:ifname_spectral)
            ifail=1
         end if
      end if
c
      if (fname_gridpt.eq.'xxx') then
         print*,' Missing filename for grid point restart '
         ifail=1
      else
         ifname_gridpt=lnsig1(fname_gridpt)
         inquire(file=fname_gridpt(1:ifname_gridpt),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',fname_gridpt(1:ifname_gridpt)
            ifail=1
         end if
      end if
c
      if (fname_vegetation.eq.'xxx') then
         print*,' Missing filename for vegetation '
         ifail=1
      else
         ifname_vegetation=lnsig1(fname_vegetation)
         inquire(file=fname_vegetation(1:ifname_vegetation),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file '
     :           ,fname_vegetation(1:ifname_vegetation)
            ifail=1
         end if
      end if
c
      if (fname_runoff.eq.'xxx') then
         print*,' Missing filename for runoff '
         ifail=1
      else
         ifname_runoff=lnsig1(fname_runoff)
         inquire(file=fname_runoff(1:ifname_runoff),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file '
     :           ,fname_runoff(1:ifname_runoff)
            ifail=1
         end if
      end if
c
      if (fname_waterlines.eq.'xxx') then
         print*,' Missing filename for waterlines file '
         ifail=1
      else
         ifname_waterlines=lnsig1(fname_waterlines)
         inquire(file=fname_waterlines(1:ifname_waterlines),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',
     :           fname_waterlines(1:ifname_waterlines)
            ifail=1
         end if
      end if
c
      if (fname_orog_spec.eq.'xxx') then
         print*,' Missing filename for orog spectral file '
         ifail=1
      else
         ifname_orog_spec=lnsig1(fname_orog_spec)
         inquire(file=fname_orog_spec(1:ifname_orog_spec),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',fname_orog_spec(1:ifname_orog_spec)
            ifail=1
         end if
      end if
c
      if (fname_ozone.eq.'xxx') then
         print*,' Missing filename for ozone '
         ifail=1
      else
         ifname_ozone=lnsig1(fname_ozone)
         inquire(file=fname_ozone(1:ifname_ozone),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',fname_ozone(1:ifname_ozone)
            ifail=1
         else 
            print*,' Opening ',fname_ozone(1:ifname_ozone)
            call open_file_nc(fname_ozone(1:ifname_ozone),ncid)
            call get4d_data_nc(ncid,'ozone',mg,jgg,15,12,
     :                         ozone_clim,ifail)
            if (ifail.ne.0) then
               print*,' Error in reading ozone file ',ifail
            end if
            call close_file_nc(fname_ozone(1:ifname_ozone),ncid)
         end if
      end if
c
      if (fname_watervapour.eq.'xxx') then
         print*,' Missing filename for watervapour '
         ifail=1
      else
         ifname_watervapour=lnsig1(fname_watervapour)
         inquire(file=fname_watervapour(1:ifname_watervapour),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',
     :             fname_watervapour(1:ifname_watervapour)
            ifail=1
         else
            print*,' Opening ',fname_watervapour(1:ifname_watervapour)
            call open_file_nc(fname_watervapour(1:ifname_watervapour),
     :                        ncid)
            call get4d_data_nc(ncid,'water',mg,jgg,15,12,
     :                         water_clim,ifail)
            if (ifail.ne.0) then
               print*,' Error in reading watervapour file ',ifail
            end if
            call close_file_nc(fname_watervapour(1:ifname_watervapour),
     :                         ncid)
         end if
      end if
c
      if (fname_sstetc.eq.'xxx') then
         print*,' Missing filename for sst etc '
         ifail=1
      else
         ifname_sstetc=lnsig1(fname_sstetc)
         inquire(file=fname_sstetc(1:ifname_sstetc),
     :        exist=lexist)
         if (.not.lexist) then
            print*,' Missing file ',fname_sstetc(1:ifname_sstetc)
            ifail=1
         else
            print*,' Opening ',fname_sstetc(1:ifname_sstetc)
            call open_file_nc(fname_sstetc(1:ifname_sstetc),ncid)
            call get3d_data_nc(ncid,'tropht',mg,jgg,12,
     :                                          tropht_clim,ifail)
            if (ifail.ne.0) then
               print*,' Error in reading sstetc file :tropht ',ifail
            end if
            call close_file_nc(fname_sstetc(1:ifname_sstetc),ncid)
         end if
      end if
c
      if (ifail.eq.1) then
         print*,' Sort out files and resubmit '
         stop 1
      end if
      idirname_output=lnsig1(outputdir_name)
C     
C     Set remaining physical constants.
C     
      RD=GASCON
      IYEAR=IBEGYEAR
C     
C     Write out details of model run
C     
      WRITE(6,205)RNTAPE
      WRITE(6,209)NL,NL,NN,MM,NN,MM
      IF(NHEM.EQ.2) WRITE(6,280) NHEM
      IF(NHEM.EQ.1) WRITE(6,281) NHEM
      WRITE(6,210) MOCT,MOCT,MOCT
      WRITE(6,211)JG,MG,JG,MG
      WRITE(6,212)
      IF (LMASCOR) THEN
         WRITE(6,213)
      ELSE
         WRITE(6,214)
      ENDIF
C     
C     Set resolution dependent quantities
C     
      MF=MM-1
      MFP=MF+1
      NF=NN-1
      NFP=NF+1
      NFPP=NFP+1
      AIOCT=(0.,1.)*MOCT
C     
C     Set various spectral limits and coefficients which
C     depend on wavenumber
C     
      NW=1+MF/MOCT
      RMG=1./REAL(MG)
      JZF=MGPP-NW-NW
      DO 4 NP=1,NFPP
         SQ(NP)=NP*(NP-1)
         SQH(NP)=0.5*SQ(NP)
         IF (NP.GT.1) THEN
            RSQ(NP)=1./SQ(NP)
         ELSE
            RSQ(1)=0.
         ENDIF
    4 CONTINUE
C     
C     Compute internal diffusion parameter
C     
      IF(TDISS.EQ.0.0) THEN
         AKK=0.0
      ELSE
c     
c     Slightly more complicated version to cope with r4 precision:
c     AKK=WW*(RADEA**NDEL)/(2.0*PI*TDISS*((NN*(NN+1))**REAL(NDEL/2)))
c     AKK=AKK/(WW*(RADEA**NDEL))
c     
         AKK=NN*(NN+1)
         AKK=AKK**REAL(NDEL/2)
         AKK=1.0/(2.0*PI*TDISS*AKK)
         AKK1=RADEA
         AKK1=WW*AKK1**NDEL
         AKK1=AKK*AKK1
      END IF
      IF(AKK.EQ.0.0) WRITE(6,221)
      IF(AKK.NE.0.0) WRITE(6,222) NDEL,AKK,NDEL,TDISS
      NDELH=NDEL/2
      DO 3 NP=1,NNP
         AK(NP)=AKK*(SQ(NP)**NDELH)
    3 CONTINUE
C     
C     Set time variables and counters
C     
      IF(PNU.EQ.0.0)WRITE(6,223)
      IF(PNU.NE.0.0)WRITE(6,224)PNU
      WRITE(6,207)KOUNTE,KOUNTH,KOUNTR
C     
      IF(KOUNTE.EQ.0) KOUNTE=-999
      IF(KOUNTH.EQ.0) KOUNTH=-999
      IF(KOUNTR.EQ.0) KOUNTR=-999
      DELT=PI2/TSPD
      PNU2=PNU+PNU
      PNU21=1.0-PNU2
      ITSPD=NINT(TSPD)
      KOUNTD=ITSPD
C     
C     Check variables make sense
C     
      NWJCH=0
      DO 310 MP=1,MFP,MOCT
         DO 320 JP=MP,NFP,MH
            NWJCH=NWJCH+1
 320     CONTINUE
 310  CONTINUE
      IF (NWJ2.NE.NWJCH) THEN
         WRITE(6,225) NWJCH,NWJ2
         CALL ABORT
      ENDIF
      IF (NTRACO.GT.NTRAC) THEN
         WRITE(6,240)
         CALL ABORT
      ENDIF
C     
C     Set dimensionalising factors
C     
      EZ=1.0/SQRT(.375)
      CV=RADEA*WW
      CG=CV*CV
      CT=CG/GASCON
      CQ=1000.0
      PFAC=0.5*CV*CV*1.0E5/GA
      SQR2=SQRT(2.0)
      RSQR2=1.0/SQR2
      EAM1=SQR2/3.
      EAM2=SQRT(2./45.)
C     
C     Make T0 dimensionless
C     
      DO 61 L=1,NL
         T0(L)=T0(L)/CT
 61   CONTINUE
      DO 30 KK=1,NTRAC
c     Hard wires water to Tracer Number 1
C     water has "colour" of 3.
         IF (KK.EQ.1) THEN
            KOLOUR(KK)=3
c     convert from g/Kg
            CTRA(KK)=CQ
C     ELSE
            IF(KOLOUR(KK).EQ.1)  CTRA(KK)=CT
            IF(KOLOUR(KK).EQ.2)  CTRA(KK)=1.E6*CT*WW*GA/P0
         ENDIF
 30   CONTINUE
C     
C     Set up arrays and variables for use in FFT routines
C     
      NTRWG=NFTWG*NHEM
      NTRGW=NFTGW*NHEM
      NTRGD=NFTGD*NHEM
      NTWG=(NTRWG-1)/NCRAY
      NTGW=(NTRGW-1)/NCRAY
      NTGD=(NTRGD-1)/NCRAY
      NRSTWG=NTRWG-NCRAY*NTWG
      NRSTGW=NTRGW-NCRAY*NTGW
      NRSTGD=NTRGD-NCRAY*NTGD
C     
C     Calculate auxiliary values required by FFT991
C     
      print*,'calling set99'
c     There is a potential bounds problem in the set99 routine which 
c       needs to be sorted out (djl)
      CALL SET99(TRIG,IFAX,MG)
      print*,'called set99'
C     
C     Set up CMPA array to calculate x-derivative of half transforms
C     
      DO 40 I=1,IGL
         CMPA(I)=0.0
 40   CONTINUE
      NROW=0
      DO 41 MP=1,MFP,MOCT
         NROW=NROW+1
         CMPA(NROW)=CMPLX(0.,REAL(MP-1))
 41   CONTINUE
      IF(NHEM.EQ.2)THEN
         NROW=0
         DO 42 MP=1,MFP,MOCT
            NROW=NROW+1
            CMPA(NROW+IDL)=CMPA(NROW)
 42      CONTINUE
      END IF
C     
      RETURN
      END
