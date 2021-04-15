      subroutine igcm_landsurface_2(deltu,
     :   precip,
     :   netsolar,
     :   netlong,
     :   hsnow,    
     :   smstar,
     :   tscur,tdcur,
     :   shbl,slbl,i,jj,itspd,surfitsll,
     :   evap,runoff,do_snow_calc)

      implicit none
c    
      include 'igcm_lsurf.cmn'
      include 'files.cmn'
 
#include "precision.inc"
c      
c     **The igcm grid
#include "igcm_land_resolution.inc"

c     **Input to this routine:
      real deltu
      real(rk_in) :: precip
      real(rk_in) :: netsolar,netlong    
      real(rk_in) :: shbl,slbl
      logical :: do_snow_calc

c    **Input and modified and output      

      real hsnow   
      real smstar
      real(rk_in) :: tscur
      real tdcur
      real runoff(mg,jgg)

c     **Output**
      
      real(rk_in) :: evap

c     for weighting...
      include 'igcm_area.cmn'

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
      real sfc
      real rkappa
      real pc1,pc2
      real hc1,hc2
      real rct1,rct2
      real trans
      real dwater

c     **PHYSICAL PARAMETERS**

      real sd1,sd2
c     sd1 is the upper soil level, sd2 the lower soil level
      parameter(sd1=0.06,sd2=2.2999)

      real skse
      parameter(skse=1.072818)

      real srsum
c     srsum is the total soil conductivity [K per W/m2]
      parameter(srsum=(sd1+sd2)/skse)

      real sksn
c     sksn is the thermal conductivity of snow
      parameter(sksn=0.24)

      real shcs 
c     shcs is the density for a 50% saturated soil
      parameter(shcs=2085.0E3)

      real shcsp
c     shcsp is equ. heat cap. freezing of soil water
      parameter(shcsp=15.312E6)

      real shcsn
c     shcsn is rho*&c for snow
      parameter(shcsn=627.0E3)

      real shsmax
      parameter(shsmax=1.4)

      real slhf
c     slhf is latent heat of fusion of ice
      parameter(slhf=3.5E5)        

      real snow_density
      parameter(snow_density=300.0)

      real water_density
      parameter(water_density=1000.0)

      real sdsn
c     density of snow (kg/m3) de-dimensionalised
      parameter(sdsn=snow_density)       

      real tp1,tz,tm3
c     tz is freezing point of water, tm3, tp1 are minus3 and plus1
      parameter(tz=0.0,tm3=tz-3.0,tp1=tz+1.0)

      real snowlim
c     default maximum snow depth of 10m
      parameter(snowlim=10.0)

      real deltasnow

      integer i,jj

c     For timestep info:
      integer surfitsll,itspd

c     *******************************************************
      dwater=( evap*3600*(24/real(itspd))/surfitsll + 
     :          precip*3600*24/real(itspd) ) 
c     *******************************************************

c     I need to check that this still has a factor of /2 in for
c       the forward-time timestep.

      if ( (hsnow.gt.0.).or.
     $           ((tscur.lt.tz).and.(dwater.gt.0.)) ) then
        if (do_snow_calc) hsnow=hsnow+dwater/snow_density
      else
        smstar=smstar+dwater/water_density
      endif

      sfc=shbl+slbl+netsolar+netlong

      rkappa=2./(srsum+hsnow/sksn)

      pc1=shcs
      pc2=shcs

c     The original had pc1 in the second line, which I think is wrong.
      if ((tscur.gt.tm3).and.(tscur.lt.tp1)) pc1=pc1+shcsp
      if ((tdcur.gt.tm3).and.(tdcur.lt.tp1)) pc2=pc2+shcsp

      hc1=pc1*sd1+min(hsnow,shsmax)*shcsn
      hc2=pc2*sd2

      trans=rkappa*(tscur-tdcur)

      rct1=(sfc-trans)/hc1
      rct2=trans/hc2

      tscur=tscur+(rct1*deltu)
      tdcur=tdcur+(rct2*deltu)

      if (tscur.gt.60.0) then
        tscur=60.0
c        print*,'temperature too high in landsurface...corrected'
      endif

      if (tscur.lt.-100.0) then
        tscur=-100.0
c        print*,'temperature too low in landsurface...corrected'
      endif

      if (tscur.gt.tz) then
        if (hsnow.gt.0.) then
C     
C     The factor of 2 is *REALLY* meant to be here, since we
C     are dealing with a forward time single level variable (hsnow)
C     within an (effectively) centre time timestep - thus each
C     segment of time is covered twice in this routine...
C     
          if (do_snow_calc) then
            deltasnow=-1.0*(tscur-tz)*hc1/(slhf*sdsn)/2.
c
          if (hsnow+deltasnow.lt.0) then
            deltasnow=-1*hsnow
          endif
          hsnow=hsnow+deltasnow
c
c     Put the melted snow into runoff!

        runoff(NINT(runoff_index(i,jj,1)),
     $         NINT(runoff_index(i,jj,2)))=
     $  runoff(NINT(runoff_index(i,jj,1)),
     $         NINT(runoff_index(i,jj,2)))-
     $    deltasnow*snow_density*
     $    weight_atm(i,jj)/
     $    weight_atm(NINT(runoff_index(i,jj,1))
     &              ,NINT(runoff_index(i,jj,2)))

          tscur=tz
          endif
        endif
      endif

      if (hsnow.gt.snowlim.and.do_snow_calc) then
C     
c     Put the icebergs into runoff!!

         call igcm_land_runoff(runoff,(hsnow-snowlim)*snow_density,
     :        i,jj)

          hsnow=snowlim

      endif


      return
      end



