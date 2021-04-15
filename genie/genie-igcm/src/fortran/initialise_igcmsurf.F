      SUBROUTINE initialise_igcmsurf
      implicit none
C     
C     Sets up various variables and arrays. Sets NAMELIST variables
C     to their default settings, then reads NAMELIST
C     
#include "precision.inc"
      include 'igcm_lsurf.cmn'
      include 'igcm_flux.cmn'
      include 'igcm_rough.cmn'
C     
#include "igcm_land_resolution.inc"
      integer jg
      parameter(jg=jgg/2)

      include 'igcm_area.cmn'

      real pi
      parameter(pi=3.141592654)

      real radea_local
      parameter(radea_local=6371e3)
      real ww_local
      parameter(ww_local=2.0*pi/(24*3600.0))
      real cv_local
      parameter(cv_local=radea_local*ww_local)
      real ga_local
      parameter(ga_local=9.80665)

c     For the weighting bit:
      integer i,j
      real ax
      real aboxedge1_lon_atm(mg+1)
      real aboxedge1_lat_atm(jgg+1)
      real weightcheck

      real plumin

      integer ios

      NAMELIST/INILSURF/ albedo_fix,snolook,szlook,salook,shsstar,
     :         outputdir_name_igcmlandsurf,dt_restart_igcmlandsurf,
     :         fname_restart_igcmlandsurf,
     :         outputdir_name_igcmoceansurf,dt_restart_igcmoceansurf,
     :         fname_restart_igcmoceansurf,flag_checkwater_land,
     :         coldstart_lsurf,
     :         snowalbfix

      NAMELIST/INIFLUX/ blvad,fluxfactor,
     :                    enhancestable,enhancelim,blrh,
     :                    txblcnst,tyblcnst
c     surf namelist
      NAMELIST/INIROUGH/rough_ocean

C     

c     Default values for the INILSURF namelist (in igcm_lsurf.cmn)......

      albedo_fix=0.05
      snowalbfix=0.0
C     
c      DATA SNOLOOK /0.8, 0.8, 0.2, 0.2, 0.3, 0.3,
c     $     0.3, 0.2, 0.3, 0.5, 0.5, 0.5,
c     $     0.7, 0.6, 0.8, 0.7, 0.7, 0.7,
c     $     0.7, 0.7, 0.4, 0.8, 0.7, 0.8/

      snolook(1)=0.8
      snolook(2)=0.8
      snolook(3)=0.2
      snolook(4)=0.2
      snolook(5)=0.3
      snolook(6)=0.3
      snolook(7)=0.3
      snolook(8)=0.2
      snolook(9)=0.3
      snolook(10)=0.5
      snolook(11)=0.5
      snolook(12)=0.5
      snolook(13)=0.7
      snolook(14)=0.6
      snolook(15)=0.8
      snolook(16)=0.7
      snolook(17)=0.7
      snolook(18)=0.7
      snolook(19)=0.7
      snolook(20)=0.7
      snolook(21)=0.4
      snolook(22)=0.8
      snolook(23)=0.7
      snolook(24)=0.8

c      DATA SZLOOK /0.001,1.0E-4,3.0E-4,1.0,1.2,1.0,
c     $     1.0,1.2,1.0,0.4,0.4,0.4,
c     $     0.12,0.12,0.12,0.12,0.12,0.12,
c     $     0.12,0.12,1.5,0.12,0.12,3.0E-3/

      szlook(1)=0.001
      szlook(2)=1.0e-4
      szlook(3)=3.0e-4
      szlook(4)=1.0
      szlook(5)=1.2
      szlook(6)=1.0
      szlook(7)=1.0
      szlook(8)=1.2
      szlook(9)=1.0
      szlook(10)=0.4
      szlook(11)=0.4
      szlook(12)=0.4
      szlook(13)=0.12
      szlook(14)=0.12
      szlook(15)=0.12
      szlook(16)=0.12
      szlook(17)=0.12
      szlook(18)=0.12
      szlook(19)=0.12
      szlook(20)=0.12
      szlook(21)=1.5
      szlook(22)=0.12
      szlook(23)=0.12
      szlook(24)=3.0e-3

c      DATA SALOOK /0.1, 0.75, 0.06, 0.14, 0.12, 0.13,
c     $     0.13, 0.13, 0.13, 0.17, 0.16, 0.16,
c     $     0.19, 0.2, 0.2, 0.12, 0.17, 0.19,
c     $     0.19, 0.25, 0.18, 0.15, 0.12, 0.35/

      salook(1)=0.1
      salook(2)=0.75
      salook(3)=0.06
      salook(4)=0.14
      salook(5)=0.12
      salook(6)=0.13
      salook(7)=0.13
      salook(8)=0.13
      salook(9)=0.13
      salook(10)=0.17
      salook(11)=0.16
      salook(12)=0.16
      salook(13)=0.19
      salook(14)=0.2
      salook(15)=0.2
      salook(16)=0.12
      salook(17)=0.17
      salook(18)=0.19
      salook(19)=0.19
      salook(20)=0.25
      salook(21)=0.18
      salook(22)=0.15
      salook(23)=0.12
      salook(24)=0.35

      shsstar=0.3

      outputdir_name_igcmlandsurf='xxx'
      dt_restart_igcmlandsurf=720
      fname_restart_igcmlandsurf='xxx'

      outputdir_name_igcmoceansurf='xxx'
      dt_restart_igcmoceansurf=720
      fname_restart_igcmoceansurf='xxx'


c     Default values for the INIFLUX namelist (in igcm_flux.cmn)......

      blvad=3.0

      fluxfactor=2.5

      enhancestable=0.2

      enhancelim=4.0

      blrh=100.0
      
      txblcnst=1.0
      tyblcnst=1.0

      rough_ocean=0.001

      flag_checkwater_land=.false.

      coldstart_lsurf=.false.

c     open NAMELIST file
      open(unit=7,file='data_igcm',status='old',iostat=ios)
      if (ios /= 0) then
         print*,'ERROR: could not open IGCM namelist file'
         stop
      end if

C     Read NAMELISTs, overwrite defaults and write them out
C     
      read(UNIT=7,NML=INILSURF,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INILSURF namelist'
         stop
      end if

      read(UNIT=7,NML=INIFLUX,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIFLUX namelist'
         stop
      end if

      read(UNIT=7,NML=INIROUGH,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INIROUGH namelist'
         stop
      end if

      WRITE(6,INILSURF)
      WRITE(6,INIFLUX)
      WRITE(6,INIROUGH)
c
c     adjust snow albedo
      snolook=snolook+snowalbfix
C
c     Now dimensional
c      blvad=blvad/cv_local
      blrh=blrh/100.0

c     NOTE: ROUGHNESS LENGTH is dedimensional and also INCLUDES FACTOR OF g
c      rough_ocean=ga_local/
c     :        (ww_local*ww_local*radea_local)*rough_ocean/radea_local
c     This has now been removed....

c     ****************************************
c     This bit copied from initialise_atmos.F
      ax=360.0/real(mg)
      do i=1,mg+1
         aboxedge1_lon_atm(i)=(i-1.5)*ax
      enddo

      print*,'in initialise_igcmsurf, aboxedge1_lon_atm:'
      print*,aboxedge1_lon_atm


      call gwtbox(aboxedge1_lat_atm,jg)
c     ****************************************

c     ****************************************
c     This bit copied from ini_weights.f
c     First of all, make a weight field for the atmosphere....
      if (aboxedge1_lat_atm(1).gt.aboxedge1_lat_atm(jgg)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0     
      do j=1,jgg
        do i=1,mg
          weight_atm(i,j)=plumin*(sin(aboxedge1_lat_atm(j)*2*pi/360.)-
     &              sin(aboxedge1_lat_atm(j+1)*2*pi/360.))*
     &              (aboxedge1_lon_atm(i+1)/360.-
     &              aboxedge1_lon_atm(i)/360.)/2.
          weightcheck=weightcheck+weight_atm(i,j)
        enddo
      enddo
      print*,'Check for weightings atm from ini_weights = ',weightcheck

      RETURN
      END
