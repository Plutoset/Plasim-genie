c      subroutine igcm_surflux(i,jj,jh,sigma,dsigma,
      subroutine igcm_surflux(sigma,
     :                        plg,tscur,qcur,sqcur,rough,
     :                        ug,vg,tg,qg,
     :                        txbl,tybl,shbl,slbl,blvb,blcd,evap)

      implicit none
      include 'igcm_flux.cmn'

#include "precision.inc"

c     ****INPUT:
c     sigma is the lowest sigma level
      real sigma
c     plg is the lowest atmospheric level pressure
      real(rk_in) :: plg
c     tscur is the surface temperature
      real(rk_in) :: tscur
c     sqcur is the surface saturated humidity
      real sqcur
c     rough is the roughness length
      real(rk_in) :: rough
c     ug,vg is the lowest-level wind velocity
      real(rk_in) :: ug,vg
c     tg is the lowest atmospheric level temperature
      real(rk_in) :: tg
c     qg is the lowest-level humidity
      real(rk_in) :: qg

c     *****OUTPUT:
c     txbl,tybl are the surface stresses
      real(rk_in) :: txbl,tybl
c     shbl, slbl are the sensible and latent heats
      real(rk_in) :: shbl,slbl
c     blvb,blcd are bulk aerodynamic coefficients
      real blvb,blcd
c     qcur is the surface humidity
      real(rk_in) :: qcur
c     evap is the evaporation
      real(rk_in) :: evap

c     ****INTERNAL VARIABLES:

      include 'timestep.inc'

c     Variables
      real thbar
      real rzz0
      real rlzz02
      real vm,vmp
      real thnl
      real dth,dq

c     for first time only   
      integer ifirst
      DATA IFIRST/1/
      save ifirst
      real rsigf
      save rsigf

c     Physical parameters

c     this is R/Mr, or Cp-Cv
      real gascon
      parameter(gascon=287.0)

      real akap
c     if heat capacity ratio, gamma, =7/5, kappa=(gamma-1)/gamma
c     kappa is 2/7=akap
      parameter(akap=0.286)

      real clatnt,clatnti
c     latent heat for water and ice
      parameter(clatnt=2.5E6,clatnti=2.834E6)

      real ga
c     ga is accel due to gravity
      parameter(ga=9.80665)

      real tz
c     freezing point of water
      parameter(tz=-0.01)

c     lowestlh is the height of the lowest model level
      real lowestlh

c     density is the surface air density
      real density

c     THE CODE.........

      if (ifirst.eq.1) then
c     THIS BIT WORKS OUT THE HEIGHT OF THE LOWEST MODEL LEVEL, RSIGF*THBAR
c     rsigf is a constant used for working out the height of the lowest level
      rsigf=2.*(1-SIGMA)/(1+SIGMA)
      ifirst=0
      endif

c     thnl is the potential temperature at the lowest model level
      THNL=(TG+273.16)/(sigma**akap)

c     thbar is the mean potential temperature of the lower half of the 
c     lowest model level 
      THBAR=0.5*(THNL+(TSCUR+273.16))

c     calculation for lowestlh:
      lowestlh=RSIGF*THBAR*gascon/ga
c     I would have done:
c     lowestlh=(1-sigma)*(tscur+273.16)*gascon/ga

c     rzz0 is the ratio of z to z0
      RZZ0=lowestlh/rough

c     rlzz02 is the log-squared of rzz0
      RLZZ02=(LOG(RZZ0))**2

c     blcd is the bulk aerodynamic constant
C     blCD=(K/LN(Z/Z0))**2, K=0.41
c     see equation A1 in Forster et al, 2000
      blCD=0.1681/RLZZ02

c     the density....
      density=plg/(gascon*(tscur+273.16))

c     vm is the wind speed.
      VM=SQRT(Ug*Ug+Vg*Vg)

c     vmp is the wind speed plus the 'gustiness parameter'
      VMP=VM+BLVAD

c     WIND STRESSES
c     see Forster et al 2000, eqn. A2
c     ****************************************************
      txbl=txblcnst*blcd*density*vmp*ug
      tybl=tyblcnst*blcd*density*vmp*vg
c     ****************************************************

c     dth is the difference between surface and lowest-level potential temp.
      DTH=(TSCUR+273.16)-THNL
c     blvb is zero if the atmosphere is stable (dth -ve).


c     Otherwise, blvb=(1/blcd)*sqrt(dtheta/theta)*sqrt(g*z0)
c     There is a maximum of blvb=enhancelim*vm (=4.*vm  current default)
c     I should tidy up by replacing 5.95 and 0.1681 with reciprocol constants 
      blvb=0.
      IF (DTH.GT.0.) THEN
        BLVB=5.95*RLZZ02*SQRT(rough*ga*DTH/THBAR)
        IF (BLVB/VM.GT.enhancelim) BLVB=VM*enhancelim
      ENDIF

c     vmp is the wind speed plus blvb (gustiness parameter)*tuneable param.
      VMP=enhancestable*(VM+fluxfactor*BLVB)

c     SENSIBLE HEAT:
c     see Forster et al 2000, eqn. A3
c     gascon/akap=cp
c     ****************************************************
      SHBL=-1*blcd*density*vmp*(dth*gascon/akap)
c     ****************************************************

c     DQ is the difference between lowest-level and surface humidity
      QCUR=MIN(SQCUR,MAX(QCUR,Qg))
      DQ=BLRH*(QCUR-qg)

c     LATENT HEAT AND EVAP:
c     see Forster et al 2000, eqn. A4
c     ****************************************************
      evap=-1*blcd*density*vmp*dq
c     ****************************************************
      IF (TSCUR.GT.TZ) THEN
        slbl=evap*clatnt
      ELSE
        slbl=evap*clatnti
      ENDIF

      return
      end
