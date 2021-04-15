      subroutine initialise_slabseaice(tstar,albedo,seaice_frac,
     &           conductflux,   
     &           iland,test_energy,test_water,ksic_loop)

      implicit none

#include "precision.inc"

#include "resolution_slabseaice.inc"
      
      real(rk_in) test_energy
      real(rk_in) test_water

      integer ksic_loop

c     
c     ****PASSED TO THIS ROUTINE

      real(rk_in) :: tstar(mg,jgg)
      real(rk_in) :: latent(mg,jgg)
      real(rk_in) :: sensible(mg,jgg)
      real(rk_in) :: netsolar(mg,jgg)
      real(rk_in) :: netlong(mg,jgg)
      real(rk_in) :: latent_inst(mg,jgg)
      real(rk_in) :: sensible_inst(mg,jgg)
      real(rk_in) :: netsolar_inst(mg,jgg)
      real(rk_in) :: netlong_inst(mg,jgg)
      real(rk_in) :: seaice_frac(mg,jgg)
      real(rk_in) :: temptop(mg,jgg)
      real(rk_in) :: conductflux(mg,jgg)
      real(rk_in) :: albedo(mg,jgg)
      integer iland(mg,jgg)

      integer istep

c     All this routine does is to call the slabseaice routine
c       and set istep=0

      istep=0.0

      call slabseaice(istep,
     :   tstar,
     :   latent,
     :   sensible,
     :   netsolar,netlong,
     :   latent_inst,
     :   sensible_inst,
     :   netsolar_inst,netlong_inst,
     :   seaice_frac,
     :   temptop,
     :   conductflux,
     :   albedo,iland,
     :   test_energy,
     :   test_water,ksic_loop)

      return
      end
