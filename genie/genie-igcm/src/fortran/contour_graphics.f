      subroutine contour_graphics(zg,ix,iy,cinc,itype,iptyp,
     :     anewlat,vertcoord,text1,day)
      implicit none

C     
C======================================================================
C
C     Do contour plot - line drawing method
C
C======================================================================
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
c     
      real xcoord(mg+1),ycoord(jgg)
      real cnt(100),vertcoord(*),anewlat(*)
      integer iwk((mg+1)*jgg)
      character text1*(*)
      character text2*43,text3*43
c     
      integer ix,iy,iptyp,i,j,l,ncnt,itype,ival,ilen,lnsig1
      real zg(ix,iy)
      real fmin,fmax,cinc,finc,day,xoff,yoff

      if (ix.gt.mg+1) then
         print*,' Error in contour graphics ix,mg',ix,mg
         stop 1
      end if
      if (iy.gt.jgg) then
         print*,' Error in contour graphics iy,jgg ',iy,jgg
         stop 1
      end if
c     
      if (iptyp.eq.1) then
         do i=1,ix
            xcoord(i)=(i-1)*pi2/real(mg)
         end do
         do j=1,iy
            ycoord(j)=pi/180.0*alat(j)
         end do
      else if (iptyp.eq.11) then
         do i=1,ix
            xcoord(i)=alat(i)/90.0
         end do
         do l=1,iy
            ycoord(l)=1.0-sigma(iy+1-l)
         end do
      else if (iptyp.eq.15) then
         do i=1,ix
            xcoord(i)=anewlat(i)/90.0
         end do
         do l=1,iy
            ycoord(l)=1.-vertcoord(l)/5000.0
         end do
      end if
c     
      call maxmin(zg,ix*iy,fmin,fmax)
c     
      ncnt=100
      call setcv(FMIN,FMAX,CINC,CNT,NCNT,ITYPE,FINC)
      text2='min =  999.999, max=  999.999'
      write(text2(7:14),'(f8.3)')fmin
      write(text2(22:29),'(f8.3)')fmax
      text3='day =  ??????.??, ci=  999.999'
      write(text3(8:16),'(f9.2)')day
      write(text3(23:30),'(f8.3)')finc
c     
      do ival=1,ncnt
         if (cnt(ival).gt.0.1*FINC) then
            call xlcol(2,4,4,0)
         else if (cnt(ival).lt.-0.1*FINC) then
            call xlcol(4,4,4,0)
         else
            call xlcol(6,4,4,0)
         end if
         CALL ZGCNTR (ZG,xcoord,ycoord,IWK,IX,IX,IY,CNT(IVAL))
      end do
      call xchmag(0.017)
      if (iptyp.eq.1) then
         xoff=-0.9
         yoff=-0.45
      else if (iptyp.eq.11) then
         xoff=-1.0
         yoff=0.0
      else if (iptyp.eq.15) then
         xoff=-1.0
         yoff=0.0
      end if
      ilen=lnsig1(text1)
      call xcharl(xoff,yoff-0.06,text1(1:ilen))
      ilen=lnsig1(text2)
      call xcharl(xoff,yoff-0.13,text2(1:ilen))
      ilen=lnsig1(text3)
      call xcharl(xoff,yoff-0.20,text3(1:ilen))
      call xclear
c     
      return
      end
