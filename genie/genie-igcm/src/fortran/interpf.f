*DECK INTERP
C**********************************************************
C             SUBROUTINE INTERPF
C**********************************************************
      subroutine interpf(fclim,fmod,ps)
c
c This subroutine takes in a profile from climatology (15 levels)
c and interpolates to model vertical grid (nl.gt.15), passing back
c profile fmod.
c N.B. The fclim profile must correspond to the climatology pressure
c levels given in this routine (ocliml(15)).
c  SMR 03-06-97
c
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'bats.cmn'
      include 'physca.cmn'

      real fclim(15)                ! Climatology profile. Must
                                    ! correspond to
                                    ! pressure levels given below!
      real fmod(nl)                 ! Model (interpolated) profile.
      integer l                     ! model level counter.
      integer lo                    ! climatology level counter.
      real pressu                   ! pressure in mb of current
                                    ! level being interpolated.
      real ocliml(15)               ! ozone climatology levels in mb.
      real ps                       ! Surface pressure.


      save
      data ocliml/1.0,3.0,10.0,30.0,50.0,70.0,100.0,150.0,200.0,
     &              250.0,300.0,400.0,500.0,700.0,850.0/




c
c -------------------------- Start of interpolation

      do 224 l=1,nl                  ! Loop over model levels.
         pressu=sigma(l)*ps/100.0    ! Pressure in mb of model level
                                     ! where ozone required.
         do 226 lo=1,14              ! Loop over climatology levels.

            if (pressu.lt.1.0) then                 ! Top
              fmod(l)=fclim(1)
            elseif (pressu.eq.ocliml(lo)) then   ! if on a
              fmod(l)=fclim(lo)                  ! climatology level.
            elseif (pressu.gt.ocliml(lo).and.pressu.lt.ocliml(lo+1))
     &        then                               ! need to interpolate
              fmod(l)=exp(
     &             log(fclim(lo)) +
     &             (log(fclim(lo+1)) -
     &              log(fclim(lo)))*
     &             (log(pressu) - log(ocliml(lo))) /
     &             (log(ocliml(lo+1)) - log(ocliml(lo))))
            elseif (pressu.ge.850.0) then         ! Bottom
              fmod(l)=fclim(15)
            endif

 226  continue                       ! End of loop over climatology.
 224  continue                       ! End of loop over model levels.
c
c --------------------------- End of interpolation
c
      return
      end
