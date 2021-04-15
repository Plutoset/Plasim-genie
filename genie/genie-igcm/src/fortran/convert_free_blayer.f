      subroutine convert_free_blayer(xg,plg,xzm,z,sigma,tstar)

      implicit none

c     for mg, nl
#include "param1.cmn"  
      include 'param2.cmn'  ! for jgg, igc, igd

      real xg(igd)          ! the input 3-D vector
      real plg(igc)         ! the surface pressure
      real z                ! the height for output
      real xzm(igc)         ! the output 2-D vector
      real sigma(nl)        ! the fractional vertical levels
      real tstar(igc)       ! the surface temperature
   
      integer i             ! for looping
      real x1,x2            ! the input values at level 1 and 2
      real p1,p2            ! the pressure at level 1 and 2
      real pzm              ! the pressure at the output level

      real g                ! gravity
      real R                ! gas constant
      parameter(g=9.81,R=287.0)

      do i=1,igc

      ! find the input values at the first and
      !   second levels of the free atmosphere
        x1=xg((nl-1)*igc+i)
        x2=xg((nl-2)*igc+i)

      ! find the pressure at the first and
      !   second levels of the free atmosphere
        p1=plg(i)*sigma(nl)
        p2=plg(i)*sigma(nl-1)

      ! find the pressure of the output height from 
      !   hydrostatic balance, 
      ! dp/dz=-(rho)g
        pzm=plg(i)-(plg(i)*g*z/(R*tstar(i)))

      ! finally, find the output value at the output height,
      !   by linaerally extrapolating from the lowest 2 
      !   model levels
        xzm(i)=(x1*(p2-pzm)-x2*(p1-pzm)) / (p2-p1)

      enddo





      return
      end











