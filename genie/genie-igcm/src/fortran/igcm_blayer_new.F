c      subroutine igcm_blayer_new(i,jj,jh,dsigma,plg,
      subroutine igcm_blayer_new(dsigma,plg,
     :                        tnlg,unlg,vnlg,qnlg,
     :                        surfsensible,
     :                        surfstressx,surfstressy,surfevap)

      implicit none

#include "precision.inc"

c     ****INPUT:
c     dsigma is the thickness of the lowest model layer in
c       sigma coordinates. 
      real dsigma
c     plg is the lowest atmospheric level pressure
      real(rk_in) :: plg
c     these are the sensible and latent heats
      real(rk_in) :: surfsensible
c     these are the wind stresses
      real(rk_in) :: surfstressx,surfstressy
c     this is the evaporation
      real(rk_in) :: surfevap


c     ****OUTPUT:
c     these are the change in temp,u,v,q
      real tnlg,unlg,vnlg,qnlg


c     ****INTERNAL VARIABLES:

c     For the igcm grid
      integer jg
      parameter(jg=16)

c     Physical parameters
      real gascon
      parameter(gascon=287.0)
      real akap
c     if heat capacity ratio, gamma, =7/5, kappa=(gamma-1)/gamma
c     kappa is 2/7=akap
      parameter(akap=0.286)
      real ga
c     ga is accel due to gravity
      parameter(ga=9.80665)

c     the u wind change....
      unlg=-1.0*surfstressx*ga/(dsigma*plg)

c     the v wind change....
      vnlg=-1.0*surfstressy*ga/(dsigma*plg)

c     the temp change....
      tnlg=-1.0*surfsensible*ga*akap/(gascon*dsigma*plg)

c     The humidity change, based on EVAP....
      qnlg=-1.0*surfevap*ga/(dsigma*plg)

      return
      end
