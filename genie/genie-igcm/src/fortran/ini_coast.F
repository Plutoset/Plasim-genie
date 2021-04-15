      subroutine ini_coast

      use genie_util, ONLY: check_unit, check_iostat

      implicit none
c
      include 'coasts.cmn'
      include '../genie/genie_control.cmn'
c
      integer ilen,lnsig1,ios
c
      ilen=lnsig1(coastline_file)
      print*,' Reading coastline data ',coastline_file(1:ilen)
      call check_unit(10,__LINE__,__FILE__)
      open(unit=10,file=coastline_file(1:ilen),iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      call zcstrd(10,inseg,idt1,cstpnt,idt2)
      close(unit=10,iostat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
c
      return
      end
