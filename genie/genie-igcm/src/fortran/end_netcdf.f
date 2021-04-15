      SUBROUTINE END_NETCDF(imode)
      implicit none
      include 'netdata.cmn'
c
      integer imode
c
      print*,' calling end netcdf for file = ',imode
      call closenc(nc(imode))
c
      return
      end
