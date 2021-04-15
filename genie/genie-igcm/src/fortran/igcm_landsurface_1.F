      subroutine igcm_landsurface_1(i,jj,plg,
     :   hsnow,    
     :   smstar,
     :   tscur,
     :   qcur,sqcur,
     :   rough,salb,runoff,
     :   fracice,
     :   fracsnowice,
     :   fracsnowveg,
     :   albice)

c     ++++++++++++++++++++++++++++++++++++++++++++++++++++
c     + Subroutine arguments:                            +
c     +                                                  +
c     +  i       - current x-location                IN  +
c     +  jj      - current y-location                IN  +
c     +  plg     - pressure at lowest model level    IN  +
c     +  hsnow   - snow depth at this point          IN  +
c     +  smstar  - soil moisture at this point    INOUT  +
c     +  tscur   - surface temperature at this point IN  +
c     +  qcur    - Surface specific humidity        OUT  +
c     +  sqcur   - Saturated vapur pressure         OUT  +
c     +  rough   - Surface roughness at this point  OUT  +
c     +  salb    - Surface albedo at this point     OUT  +
c     +  runoff  - Surface runoff field           INOUT  +
c     +  fracice - Ice fraction at this point        IN  +
c     +  fracsnowice - Snow-covered ice fraction     IN  +
c     +  fracsnowveg - Snow-covered veg fraction     IN  +
c     +  albice  - Ice albedo at this point          IN  +
c     +                                                  +
c     ++++++++++++++++++++++++++++++++++++++++++++++++++++

      implicit none
c    
      include 'igcm_lsurf.cmn'
      include 'files.cmn'
c      
#include "precision.inc"
c
c     **Input to this routine:
      integer i,jj
      real(rk_in) :: plg
      real hsnow   
      real(rk_in) :: tscur
      real fracice
      real fracsnowice
      real fracsnowveg
      real albice

c    **Input and modified and output      
      real smstar

c     **The igcm grid
#include "igcm_land_resolution.inc"

c    **Output from this routine
      real(rk_in) :: qcur
      real sqcur
      real(rk_in) :: rough
      real(rk_in) :: salb
      real runoff(mg,jgg)

      include 'igcm_area.cmn'

c     **For netcdf
      integer ncid,ifail
      integer ifirst
      DATA IFIRST/1/

c     **Arrays
      real svege(mg,jgg)
      save svege

c     **Non-dimensionalisation
      real radea
      parameter(radea=6371e3)
      include 'timestep.inc'
      real p0
      parameter(p0=100000.0)
      real gascon
      parameter(gascon=287.0)
      real cv
      parameter(cv=radea*ww)
      real cg
      parameter(cg=cv*cv)
      real ct
      parameter(ct=cg/gascon)

c     **Variables
      real srh
      real pqsat_prec
      real(rk_in) pqsat_input
      integer iii,jjj

c     For the new icesheet
      real salb_veg
c     Surface albedo of vegetation
      real salb_ice
c     Surface albedo of ice
      real salb_snowice
c     Albedo of snow-covered ice
      real salb_snowveg
c     Albedo of snow-covered veg
      real rough_veg
c     Roughness of vegetation at this point
      real rough_ice
c     Roughness of ice at this point

c     **PHYSICAL PARAMETERS**

      real ssmc
c     This is the depth of the soil moisture bucket model.
      parameter(ssmc=0.5)

      real ssmc23,ssmc13
c     These are fractions of the bucket depth
      parameter(ssmc23=ssmc*(2./3.),ssmc13=ssmc*(1./3.))      

c     Read the vegetation file and the runoff mask if it's 
c       the first call to this routine.
c      *********************************
      if (ifirst.eq.1) then

      print*,'opening ',fname_vegetation(1:ifname_vegetation)
      call open_file_nc(fname_vegetation(1:ifname_vegetation), ncid)
      call get2d_data_nc(ncid,'svege',mg,jgg,svege,ifail)
      call close_file_nc(fname_vegetation(1:ifname_vegetation), ncid)
      ifirst=0
      if (ifail.ne.0) then
         print*,'svege cannot be read.....'
         stop
      endif

      print*,'opening ',fname_runoff(1:ifname_runoff)
      call open_file_nc(fname_runoff(1:ifname_runoff), ncid)
      call get3d_data_nc(ncid,'destination_indices',mg,jgg,2,
     :                runoff_index,ifail)
      call close_file_nc(fname_runoff(1:ifname_runoff), ncid)
      ifirst=0
      if (ifail.ne.0) then
         print*,'runoff cannot be read.....'
         stop
      endif
c     Change points where runoff=0,0 to point to
c     that point
      do iii=1,size(runoff_index,1)
         do jjj=1,size(runoff_index,2)
            if ((runoff_index(iii,jjj,1).eq.0.0).or.
     :           (runoff_index(iii,jjj,2).eq.0.0)) then
               runoff_index(iii,jjj,1)=iii
               runoff_index(iii,jjj,2)=jjj
            endif
         enddo
      enddo
      endif
c      *********************************

c     Define the roughness length.
c     This was originally in blsurf.f
c      *********************************
      rough_veg=SZLOOK(NINT(SVEGE(i,Jj)))
      rough_ice=szlook(2)
c      *********************************
c     +++++++++++++++++++++++++++++++++++++++
c     This bit for the new icesheet routines....
      rough=rough_veg*(1.0-fracice-fracsnowice)
     :     +rough_ice*(fracice+fracsnowice)
c     +++++++++++++++++++++++++++++++++++++++


c     Define the albedo and the snow height. 
c      *********************************
      salb_veg=SALOOK(NINT(SVEGE(I,jj)))
      salb_ice=albice
      if (hsnow.gt.0) then
         salb_snowveg=salb_veg+(snolook(NINT(SVEGE(i,Jj)))-salb_veg)
     :                 *hsnow/(hsnow+shsstar)
         salb_snowice=salb_ice+(snolook(2)-salb_ice)
     :                 *hsnow/(hsnow+shsstar)
      else
         salb_snowveg=0.0
         salb_snowice=0.0
      endif
      salb_veg=salb_veg+albedo_fix
      salb_ice=salb_ice+albedo_fix
      salb_snowveg=salb_snowveg+albedo_fix
      salb_snowice=salb_snowice+albedo_fix
c      *********************************
c     +++++++++++++++++++++++++++++++++++++++
c     This bit for the new icesheet routines....
      salb=salb_veg*(1.0-fracice-fracsnowice-fracsnowveg)
     :     +salb_ice*fracice
     :     +salb_snowveg*fracsnowveg
     :     +salb_snowice*fracsnowice
c     +++++++++++++++++++++++++++++++++++++++
 
      if (smstar.gt.ssmc) then
        srh=1.
c       The runoff is simply the amount of water in the soil 
c         above the saturation point.  It is re-routed, and then
c         weighted by the relative area of the original and 
c         destination gridbox.   

        call igcm_land_runoff(runoff,(smstar-ssmc)*1000.0,i,jj)

        smstar=ssmc

      else if ((smstar.gt.ssmc23)
     $                    .or.(hsnow.gt.0.)) then
        srh=1.
      else if (smstar.gt.ssmc13) then
        srh=(smstar/ssmc13)-1.
      else
        srh=0.
      endif

c     Done like this because of precision reasons.....

#ifdef IGCMPQSATTEST
      if (tscur.lt.-50.0) then
         pqsat_input=(-50.0+273.16)/ct
      else if (tscur.gt.50.0) then
         pqsat_input=(50.0+273.16)/ct
      else
         pqsat_input=(tscur+273.16)/ct     
      end if
#else
      pqsat_input=(tscur+273.16)/ct
#endif

      sqcur=pqsat_prec(pqsat_input)/(plg/p0)

      qcur=srh*sqcur

c      *********************************


      return
      end



