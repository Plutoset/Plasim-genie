*DECK COLAMT
C**********************************************************
C             SUBROUTINE COLAMT
C**********************************************************
      subroutine colamt(o3mod,h2omod)

      implicit none

#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'bats.cmn'
      include 'physca.cmn'

      real o3mod(nl,mg,nhem,jg),h2omod(nl,mg,nhem,jg)
      real sum                      ! Used in calculating column
c      real column                   ! amounts.
      integer b,f,d                 ! indices for where column
                                    ! amount is calculated.
      integer ccount                ! Counter of number of times
                                    ! col. amt. calculated.
      integer l
c
c
c ------------------------------- Test of o3 and h2o: column amounts
c
c Column amount in cm at STP using mass mixing ratios (r)is given by
c C(cm_STP)=100/rho_STP g (integral r dp) where rho_STP is of the
c constituent. Approximate integral r dp crudely using r delta p at
c model levels.
c Do this in DU for o3 (C as above but times 1e3) and in cm for h2o.
c

      b=10                                ! Longitude index
      f=1                                 ! Hemisphere index
      d=2                                 ! Latitude index



      do  ccount=1,7                   ! Start of loop to do column
                                          ! calculation a few times.

c ------------------------------- First for ozone

        sum=sigmah(1)*1.0e5*o3mod(1,b,f,d)      ! Top
                                                ! N.B. Pressure in Pa!
        do l=1,nl-2                             ! Middle
          sum=sum+((sigmah(l+1)-sigmah(l))*1.0e5*o3mod(l+1,b,f,d))
        enddo
        sum=sum+((1.0-sigmah(nlm))*1.0e5*o3mod(nl,b,f,d))    ! Bottom
c        column=(sum*1.0e5)/(GA*2.14)         ! N.B. in Dobson units!!
! 2.14 is density of ozone at STP
c       write(55,*)
c       write(55,*)'Location (long,hem,lat indices): ',b,f,d
c       write(55,*)
c       write(55,*)'Ozone column amount (DU): ',column
c       write(55,*)


c --------------- Same again for water ...

       sum=sigmah(1)*1.0e5*h2omod(1,b,f,d)                   ! Top
       do l=1,nl-2                                           ! Middle
         sum=sum+((sigmah(l+1)-sigmah(l))*1.0e5*h2omod(l+1,b,f,d))
       enddo
       sum=sum+((1.0-sigmah(nlm))*1.0e5*h2omod(nl,b,f,d))    ! Bottom
c       column=(sum*100.0)/(GA*1000.0)        ! N.B. in cm!!

c       write(55,*)'Water column amount (cm): ',column
c       write(55,*)

       b=b+8                     ! Look at column amt. in other
                                 ! places in NH.
       d=d+2

      enddo                  ! End of loop over column calculation.


      return
      end
