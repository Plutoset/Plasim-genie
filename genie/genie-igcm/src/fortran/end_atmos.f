      subroutine end_atmos
      implicit none
#include "param1.cmn"
      include 'outcon.cmn'
c
      if (lnetcdf) then
         if (ldaily) call end_netcdf(1)
         if (lmonthly) call end_netcdf(2)
         if (lannual.and.lannual_restart) 
     :      call end_netcdf(3)
         if (ldecadal.and.ldecadal_restart) 
     :      call end_netcdf(4)
      end if
c
      return
      end
