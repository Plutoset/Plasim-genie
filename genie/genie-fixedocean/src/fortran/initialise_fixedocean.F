      subroutine initialise_fixedocean(tstar,albedo,
     :                seaice_frac,iland)

      implicit none

#include "precision.inc"

c     ****FOR COMMON BLOCK??****
      
      integer mg,jg
c     mg is longitudes, jg is latitudes per hemisphere
      parameter(mg=64,jg=16)

      integer nhem
c     nhem is number of hemispheres
      parameter(nhem=2)

      integer jgg
c     jgg is total latitudes
      parameter(jgg=nhem*jg)

c     
c     ****PASSED TO/FROM THIS ROUTINE

      real(rk_in) :: tstar(mg,jgg)
      real(rk_in) :: seaice_frac(mg,jgg)
      real(rk_in) :: dtcarry_ocn_ice(mg,jgg)
      real(rk_in) :: energycarry_ocn_ice(mg,jgg)
      real(rk_in) :: albedo(mg,jgg)
      integer iland(mg,jgg)

      integer istep

c     All this routine does is to call the fixedocean routine
c       and set istep=0

      istep=0

      call fixedocean(istep,
     :   tstar,
     :   seaice_frac,
     :   energycarry_ocn_ice,
     :   dtcarry_ocn_ice,
     :   albedo,iland)

      return
      end
