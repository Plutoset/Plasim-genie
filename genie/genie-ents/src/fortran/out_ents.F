cmsw
cmsw Writes output for restarts
cmsw
      subroutine out_ents(unit,
     :     land_snow_lnd                     !< land snow cover
     :     )

c      include '../genie-cgoldstein/var.cmn'
c      include '../genie-simpleland/var_ents.cmn'
#include "genie_ents.cmn"
#include "precision.inc"
#include "var_ents.cmn"

      integer i,j,unit
     
c land snow cover
      real(rk_in),dimension(maxi,maxj),intent(inout)::land_snow_lnd

      write(unit,*)((photo(i,j),i=1,imax),j=1,jmax) 
      write(unit,*)((respveg(i,j),i=1,imax),j=1,jmax)
      write(unit,*)((leaf(i,j),i=1,imax),j=1,jmax)
      write(unit,*)((respsoil(i,j),i=1,imax),j=1,jmax)

      write(unit,*)((Cveg(i,j),i=1,imax),j=1,jmax)
      write(unit,*)((Csoil(i,j),i=1,imax),j=1,jmax)
      write(unit,*)((fv(i,j),i=1,imax),j=1,jmax)

      write(unit,*)((tqld(1,i,j),i=1,imax),j=1,jmax)
      write(unit,*)((tqld(2,i,j),i=1,imax),j=1,jmax)

      write(unit,*)((land_snow_lnd(i,j),i=1,imax),j=1,jmax)

      write(unit,*)pco2ld

      end
