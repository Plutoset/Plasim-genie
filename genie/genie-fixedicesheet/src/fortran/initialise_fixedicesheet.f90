module initialise_fixedicesheet_mod
  
contains
  
  subroutine initialise_fixedicesheet(iland_atm, &
       orog_atm,albedo_atm,icefrac_atm)

    implicit none

#include "precision.inc"

#include "resolution_fixedicesheet.inc"

!     ****PASSED TO THIS ROUTINE
      real(rk_in) :: icefrac_atm(mg,jgg)
      real(rk_in) :: orog_atm(mg,jgg)
      real(rk_in) :: albedo_atm(mg,jgg)
      integer iland_atm(mg,jgg)

      integer istep

      integer iconv

!     All this routine does is to call the fixedicesheet routine
!       and set istep=0

      istep=0
      iconv=0
      
      call fixedicesheet(istep, &
           iland_atm,orog_atm,albedo_atm,icefrac_atm,iconv)

      return
    end subroutine initialise_fixedicesheet
    
  end module initialise_fixedicesheet_mod
