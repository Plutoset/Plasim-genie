      subroutine plotfields(ipage)

      implicit none

c
#include "param1.cmn"
      include 'param2.cmn'
      REAL RAD
      PARAMETER(RAD=180./PI)
      include 'blank.cmn'
      include 'bats.cmn'
      include 'gridss.cmn'
      include 'legau.cmn'
      include 'outcon.cmn'
c     
      INTEGER J,IHEM,IOF,JJ,IOF1,I,L,IPAGE
      REAL    ASUM

      REAL LUG(MG+1,JGG),LTG(MG+1,JGG)
      REAL LZUG(JGG,NL),LZTG(JGG,NL)
c     
      do j=1,jg
         do ihem=1,nhem
            iof=(ihem-1)*mgpp
            if (ihem.eq.1) then
               jj=j
            else
               jj=jggp-j
            end if
            iof1=iof+(nl-1)*igc
            do i=1,mg
               lug(i,jj)=ug1(i+iof1,j)
            end do
            lug(mg+1,jj)=lug(1,jj)
            do l=1,nl
               iof1=iof+(l-1)*igc
               asum=0.0
               do i=1,mg
                  asum=asum+ug1(i+iof1,j)
               end do
               lzug(jj,nl+1-l)=asum/real(mg)
            end do
            iof1=iof+(nl-1)*igc
            do i=1,mg
               ltg(i,jj)=tg1(i+iof1,j)
            end do
            ltg(mg+1,jj)=ltg(1,jj)
            do l=1,nl
               iof1=iof+(l-1)*igc
               asum=0.0
               do i=1,mg
                  asum=asum+tg1(i+iof1,j)
               end do
               lztg(jj,nl+1-l)=asum/real(mg)
            end do
         end do
      end do
c     
      call xnwpic
#ifdef lgraph
      call frame_graphics(1)
#endif
      call contour_graphics(ltg,mg+1,jgg,-0.1,1,1,
     :     alat,sigma,
     :     'Temperature at bottom level ',day)
      call xnwpic
#ifdef lgraph
      call frame_graphics(1)
#endif
      call contour_graphics(lug,mg+1,jgg,-0.1,0,1,
     :     alat,sigma,
     :     'Zonal Wind at bottom level ',day)
c     
      call xnwpic
#ifdef lgraph
      call frame_graphics(11)
#endif
      call contour_graphics(lztg,jgg,nl,-0.1,1,11,
     :     alat,sigma,
     :     'Zonal Cross_Section of Temperature ',day)
      call xnwpic
#ifdef lgraph
      call frame_graphics(11)
#endif
      call contour_graphics(lzug,jgg,nl,-0.1,0,11,
     :     alat,sigma,
     :     'Zonal Cross_Section of Zonal Wind ',day)
c     
      if (ipage.eq.1) call xpages(-1,0)
c     
      return
      end
