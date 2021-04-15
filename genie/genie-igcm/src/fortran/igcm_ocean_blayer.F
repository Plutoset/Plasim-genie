      subroutine igcm_ocean_blayer(iter,
     :     surfitsll_atm,
     :     surfdelt2_atm,
     :     surfdsigma_atm,
     :     surfiland_atm,
     :     surfsensible_atm,
     :     surfstressx_atm,surfstressy_atm,
     :     surfevap_atm,
     :     lowestlu_atm,lowestlv_atm,lowestlt_atm,
     :     lowestlq_atm,lowestlp_atm)


      implicit none

c     THIS MODULE TAKES THE FLUXES OF SENSIBLE AND LATENT HEAT, AND CHANGES THE
C       LOWEST MODEL LEVEL TEMPERATURE AND HUMIDITY APPROPRIATELY.  IT ALSO TAKES
C       THE SURFACE MOMENTUM FLUXES, AND CHANGES THE LOWEST MODEL LEVEL U AND V.
C       THE FLUXES ARE CALCULATED IN A SEPARATE LAND-SURFLUX MODULE.

c     Input only :
c     ------------
c     surfdsigma_atm - thickness of lowest model level
c     surfitsll_atm - number of mini-timesteps per surfdelt2 for land
c     lowestlp_atm - p at lowest atmos level
c     surflatent_atm - latent heat flux
c     surfsensible_atm - sensible heatflux
c     surfstressx_atm - zonal wind sheer
c     surfstressy_atm - meridional wind sheer

c     Input and modified :
c     --------------------
c     lowestlu_atm - u at lowest atmos level
c     lowestlv_atm - v at lowest atmos level
c     lowestlt_atm - t at lowest atmos level
c     lowestlq_atm - q at lowest atmos level
c     unlgs_atm - u at lowest atmos level
c     vnlgs_atm - v at lowest atmos level
c     tnlgs_atm - t at lowest atmos level
c     qnlgs_atm - q at lowest atmos level


c     Output only (ie 0 on entry or just after entry):
c     ------------
c     tendlowestlu_atm - change in lowest atmos level u
c     tendlowestlv_atm - change in lowest atmos level v
c     tendlowestlq_atm - change in lowest atmos level q
c     tendlowestlt_atm - change in lowest atmos level t

#include "param1.cmn"
      include 'param2.cmn'
      include 'balan.cmn'
      include 'bats.cmn'
      include 'blank.cmn'
      include 'legau.cmn'

      include 'tendencies.cmn'

c     Internal variables
c      integer igc,jg,nhem,mgpp,jgg,mg
c      parameter(igc=132,jg=16,nhem=2,mgpp=66,jgg=32,mg=64)
      integer ih,ihem,jj,i
      real deltl

      integer iter

#include "precision.inc"

      real(rk_in) :: lowestlu_atm(mg,jgg)
      real(rk_in) :: lowestlv_atm(mg,jgg)
      real(rk_in) :: lowestlq_atm(mg,jgg)
      real(rk_in) :: lowestlt_atm(mg,jgg)
      real(rk_in) :: lowestlp_atm(mg,jgg)
      real(rk_in) :: surfsensible_atm(mg,jgg)
      real(rk_in) :: surfstressx_atm(mg,jgg)
      real(rk_in) :: surfstressy_atm(mg,jgg)
      real(rk_in) :: surfevap_atm(mg,jgg)
      integer :: surfiland_atm(mg,jgg)
      real(rk_in) :: surfdsigma_atm
      integer :: surfitsll_atm
      real(rk_in) :: surfdelt2_atm

      real tendlowestlu(mg,jgg)
      real tendlowestlv(mg,jgg)
      real tendlowestlq(mg,jgg)
      real tendlowestlt(mg,jgg)

      real surfdsigma
      real surfitsll

      real rsurfitsll,rct,rcv,rww

c     scalars
      surfdsigma=surfdsigma_atm
      surfitsll=surfitsll_atm

c     for speed....
      rsurfitsll=1.0/surfitsll
      rct=1.0/ct
      rcv=1.0/cv
      rww=1.0/ww

      DELTl=2.0*surfDELT2_atm*rsurfitsll

      DO IH=1,JG
         JH=IH
         do ihem=1,nhem
            if (ihem.eq.1) then
               jj=jh
            else
               jj=jgg+1-jh
            end if
            do i=1,mg
 
            if (surfiland_atm(i,jj).eq.0) then

            if (iter.eq.1) then

               ocean_tendlowestlt_atm(i,jj)=0.0
               ocean_tendlowestlu_atm(i,jj)=0.0
               ocean_tendlowestlv_atm(i,jj)=0.0
               ocean_tendlowestlq_atm(i,jj)=0.0

            endif

c     ******************************************************
                  call igcm_blayer_new(surfdsigma,
     :                 lowestlp_atm(i,jj),
     :                 tendlowestlt(i,jj),
     :                 tendlowestlu(i,jj),
     :                 tendlowestlv(i,jj),
     :                 tendlowestlq(i,jj),
     :                 surfsensible_atm(i,jj),
     :                 surfstressx_atm(i,jj),surfstressy_atm(i,jj),
     :                 surfevap_atm(i,jj) )

                  lowestlt_atm(i,jj)=lowestlt_atm(i,jj)+
     :                 DELTl*tendlowestlt(i,jj)
                  lowestlu_atm(i,jj)=lowestlu_atm(i,jj)+
     :                 DELTl*tendlowestlu(i,jj)
                  lowestlv_atm(i,jj)=lowestlv_atm(i,jj)+
     :                 DELTl*tendlowestlv(i,jj)
                  lowestlq_atm(i,jj)=lowestlq_atm(i,jj)+
     :                 DELTl*tendlowestlq(i,jj)

c     Unfortunately, this bit needs to be de-dimensionalised as it goes
c       straight into the igcm:
                  ocean_tendlowestlq_atm(i,jj)=
     :            ocean_tendlowestlq_atm(i,jj)+
     :              tendlowestlq(i,jj)*rSURFITSLL*rww
                  ocean_tendlowestlt_atm(i,jj)=
     :            ocean_tendlowestlt_atm(i,jj)+
     :              tendlowestlt(i,jj)*rSURFITSLL*rct*rww
                  ocean_tendlowestlu_atm(i,jj)=
     :            ocean_tendlowestlu_atm(i,jj)+
     :              tendlowestlu(i,jj)*rSURFITSLL*rcv*cs(jh)*rww
                  ocean_tendlowestlv_atm(i,jj)=
     :            ocean_tendlowestlv_atm(i,jj)+
     :              tendlowestlv(i,jj)*rSURFITSLL*rcv*cs(jh)*rww

c     ******************************************************

            endif

            enddo
           enddo
         enddo
         
      return
      end
