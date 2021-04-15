      subroutine plotfields2(ipage)

      implicit none

#include "param1.cmn"
      include 'param2.cmn'
      REAL RAD
      PARAMETER(RAD=180./PI)
      include 'blank.cmn'
      include 'bats.cmn'
      include 'gridpp.cmn'
      include 'legau.cmn'
      include 'outcon.cmn'
      
      INTEGER J,IHEM,IOF,JJ,I,IPAGE

c
      REAL LUG(MG+1,JGG),LTG(MG+1,JGG),LTSTAR(MG+1,JGG)
c
      do j=1,jg
         do ihem=1,nhem
            iof=(ihem-1)*mgpp
            if (ihem.eq.1) then
               jj=j
            else
               jj=jggp-j
            end if
            do i=1,mg
               lug(i,jj)=arrcr(i+iof,j)*real(itspd)
            end do
            lug(mg+1,jj)=lug(1,jj)
            do i=1,mg
               ltg(i,jj)=arrlr(i+iof,j)*real(itspd)
            end do
            ltg(mg+1,jj)=ltg(1,jj)
            do i=1,mg
               ltstar(i,jj)=ct*atstar(i+iof,j)-273.16
            end do
            ltstar(mg+1,jj)=ltstar(1,jj)
         end do
      end do
c
      call xnwpic
#ifdef lgraph
      call frame_graphics(1)
#endif
      call contour_graphics(lug,mg+1,jgg,-0.1,1,1,
     :     alat,sigma,
     :     'Convective Rainfall ',day)
      call xnwpic
#ifdef lgraph
      call frame_graphics(1)
#endif
      call contour_graphics(ltg,mg+1,jgg,-0.1,0,1,
     :     alat,sigma,
     :     'Large scale Rainfall ',day)
      call xnwpic
#ifdef lgraph
      call frame_graphics(1)
#endif
      call contour_graphics(ltstar,mg+1,jgg,-0.1,0,1,
     :     alat,sigma,
     :     'Surface Temperature ',day)
c
c     next bit plots ocean
c     *******************************************************
c      call plotgraph(ipage,day)
       ipage=ipage
c      OK, that is just so that we don't get a warning about
c        unused variables.
c     *******************************************************

c     *******************************************************
c     instead, re-write the same atmos. fields so we keep the 
c     same number of plots...... 
c     will change this to energy fluxes soon......
c     take this out when goldstein is ready for genie.f
      call xnwpic
#ifdef lgraph
      call frame_graphics(1)
#endif
      call contour_graphics(ltg,mg+1,jgg,-0.1,0,1,
     :     alat,sigma,
     :     'Large scale Rainfall ',day)
      call xnwpic
#ifdef lgraph
      call frame_graphics(1)
#endif
      call contour_graphics(ltstar,mg+1,jgg,-0.1,0,1,
     :     alat,sigma,
     :     'Surface Temperature ',day)
c     *******************************************************


c
      return
      end
