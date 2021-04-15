      subroutine initialise_slabocean(tstar,albedo,seaice_frac,
     :           temptop,
     :           iland,
     :           test_energy,
     :           test_water)

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

      real(rk_in) test_energy
      real(rk_in) test_water

c
c     ****PASSED TO THIS ROUTINE

      real(rk_in) :: tstar(mg,jgg)
      real(rk_in) :: latent(mg,jgg)
      real(rk_in) :: sensible(mg,jgg)
      real(rk_in) :: netsolar(mg,jgg)
      real(rk_in) :: netlong(mg,jgg)
      real(rk_in) :: prec(mg,jgg)
      real(rk_in) :: evap(mg,jgg)
      real(rk_in) :: runoff(mg,jgg)
      real(rk_in) :: seaice_frac(mg,jgg)
      real(rk_in) :: temptop(mg,jgg)
      real(rk_in) :: conductflux(mg,jgg)
      real(rk_in) :: albedo(mg,jgg)
      integer iland(mg,jgg)

      integer istep

c     All this routine does is to call the slabocean routine
c       and set istep=0

      istep=0.0

      call slabocean(istep,
     :   tstar,
     :   latent,
     :   sensible,
     :   netsolar,netlong,
     :   prec,evap,runoff,
     :   seaice_frac,
     :   temptop,
     :   conductflux,
     :   albedo,iland,
     :   test_energy,
     :   test_water)

      return
      end
