      subroutine initialise_fixedchem(co2,n2o,ch4)

      implicit none

#include "precision.inc"
c
#include "resolution_fixedchem.inc"
c     
c     ****PASSED FROM THIS ROUTINE
      real(rk_in) :: co2(mg,jgg)
      real(rk_in) :: n2o(mg,jgg)
      real(rk_in) :: ch4(mg,jgg)

      integer istep
      integer iconv

c     All this routine does is to call the fixedicesheet routine
c       and set istep=0

      istep=0
      iconv=0

      call fixedchem(istep,co2,n2o,ch4,iconv)

      return
      end
