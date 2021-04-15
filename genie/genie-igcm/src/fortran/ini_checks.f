      subroutine ini_checks(doy,kstart,ktotal,itspd,imonth,
     :     kits,idoy,delt,pi2,lrstrt,kounth,
     :     kountr)
c     
c     This routine checks to see that model finishes at end of month
c     AND sets a few starting constants
c     
      implicit none
      real doy,delt,pi2
      integer kstart,ktotal,itspd,imonth,kits,idoy,
     :     kounth,kountr
      logical lrstrt
c     
      integer ktest,ifail
      real rtest
c     
      ifail=0
c     
      ktest=kstart-30*itspd*nint(kstart/(30.0*itspd))
      if (ktest.ne.0) then
         print*,' Selected kstart is not at start of month '
         print*,' Number of time steps per day = ',itspd
         print*,' Start kount (kstart) = ',kstart
         ifail=1
      end if
c     
      ktest=ktotal-30*itspd*nint(ktotal/(30.0*itspd))
      if (ktest.ne.0) then
         print*,' Selected ktotal is not at start of month '
         print*,' Number of time steps per day = ',itspd
         print*,' End kount (ktotal) = ',ktotal
         ifail=1
      end if
c     
      if (kounth.ne.-999) then
         ktest=kounth-30*itspd*nint(kounth/(30.0*itspd))
         if (ktest.ne.0) then
            print*,' Selected kounth is not multiple of month '
            print*,' Number of time steps per day = ',itspd
            print*,' End kount (kounth) = ',kounth
            ifail=1
         end if
      end if
c     
      if (kountr.ne.-999) then
         ktest=kountr-30*itspd*nint(kountr/(30.0*itspd))
         if (ktest.ne.0) then
            print*,' Selected kountr is not multiple of month '
            print*,' Number of time steps per day = ',itspd
            print*,' End kount (kountr) = ',kountr
            ifail=1
         end if
      end if
c     
      rtest=doy-30.0*nint(doy/30.0)
      if (rtest.gt.1.e-6) then
         print*,' doy must be at start of next month '
         print*,' doy = ',doy
         ifail=1
      end if
c     
      if (kits.ne.0) then
         print*,' Short time steps are not allowed '
         ifail=1
      end if
c     
      if (.not.lrstrt) then
         idoy=nint(doy/(delt/pi2))
         imonth=int(int(doy)/30.0+1)
         if (imonth.gt.12) then
            print*,' doy should not be greater than (or =) to 361.0 '
            ifail=1
         end if
      end if
c     
      if (ifail.ne.0) then
         print*,' Please reset options and resubmit '
         stop 1
      end if
c     
      return
      end
