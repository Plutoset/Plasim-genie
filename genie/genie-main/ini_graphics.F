#ifdef lgraph
      SUBROUTINE INI_GRAPHICS(coastline_file)
#else
      SUBROUTINE INI_GRAPHICS()
#endif

#ifdef lgraph
      use precision
      implicit none

      character coastline_file*(*)

      real(rk_libzxp) rar,xlim,ylim
      real(rk_libzxp) gr_1,gr_2,gr_3,gr_4,gr_5,gr_6,gr_7
      real(rk_libzxp) gr_8,gr_9,gr_10
      real(rk_libzxp) gr_11,gr_12,gr_13,gr_14

      rar=0.45

      gr_1=1.0
      gr_2=0.0
      gr_3=1.9
      gr_4=1.5
      gr_5=0.0
      gr_6=1.0
      gr_7=0.0
      gr_8=1.0
      gr_9=1.0
      gr_10=2.0
      gr_11=2.0
      gr_12=30.0
      gr_13=30.0
      gr_14=80.0
c
      call disset(23)
      call winini1
      call xdevic
      call xdspac(gr_1)
      call xpspac(gr_2,gr_3/gr_4,gr_5,gr_6)
      call xspace(3,3,gr_7,xlim,ylim)
      call xpscal(gr_8,gr_9)
      call zjinit
      call zstret(gr_10*rar,rar)
      call zstmol(rar)
      call zstamp(gr_11*rar)
      call zstlnt(gr_12,gr_13,gr_14)
      call xpages(-1,0)
c
      call ini_coast(coastline_file)
c
      return
#endif
      end
c
#ifdef lgraphics
      subroutine ini_coast(coastline_file)
#else
      subroutine ini_coast()
#endif

#ifdef lgraphics
      use coasts
      implicit none
      character coastline_file*(*)
c
      integer ilen,lnsig1,ios
c
      ilen=lnsig1(coastline_file)
      print*,' Reading coastline data ',coastline_file(1:ilen)
      open(unit=10,file=coastline_file(1:ilen),iostat=ios)
      if (ios/=0) then
         print*, "ERROR opening file ",  trim(coastline_file(1:ilen))
         print*, "ERROR on line ", __LINE__, " of file ", __FILE__
         stop
      end if
      call zcstrd(10,inseg,idt1,cstpnt,idt2)
      close(unit=10)
c
      return
#endif
      end

#ifdef lgraphics
      subroutine frame_graphics(iptyp)
#else
      subroutine frame_graphics()
#endif

#ifdef lgraphics
      use precision
      use coasts
      implicit none

      real(rk_libzxp) gr_15

      integer iptyp
c     
      gr_15=0.45

c     cstpnt in coasts.cmn should be at real(rk_libzxp), but it 
c       seems to work anyway somehow.

      call drawbox(iptyp,gr_15,0)
c     
      if (iptyp.ne.11.and.iptyp.ne.15) then
         call xlcol(3,4,4,-8)
         call zcstpl(inseg,idt1,cstpnt,idt2)
      end if
c     
      call xclear
c     
      return

#endif
      end

