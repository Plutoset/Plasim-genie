      subroutine igcm_ocean_surflux(istep_atm,
     :     iter,surfitsll_atm,
     :     surfsigma_atm,
     :     iland,
     :     surfnetsolar_atm,surfnetlong_atm,
     :     surfprec_atm,
     :     lowestlu_atm,lowestlv_atm,lowestlt_atm,
     :     lowestlq_atm,lowestlp_atm,
     :     albedo_atm,tstar_atm,
     :     surflatent_atm,surfsensible_atm,
     :     surfstressx_atm,surfstressy_atm,
     :     surfevap_atm,
     :     surflatentavg_atm,surfsensibleavg_atm,
     :     surfstressxavg_atm,surfstressyavg_atm,
     :     surfevapavg_atm,
     :     tscur_atm,
     :     rough_atm,
     :     surfqstar_atm,
     :     surfsalb_atm)


      implicit none

c     These includes are just temporary so i can have the igcm
c       constants ct and cv and t0 and cs.  
c     To get perfect restarts I require that the 
c       constant ct be from the same common block as that used in
c       initialise_atmos.F, in the line:
c                  tstar_atm(i,jj)=tstar(i+iof,j)*ct-273.16


#include "param1.cmn"
      include 'param2.cmn'
      include 'balan.cmn'
      include 'bats.cmn'
      include 'blank.cmn'
      include 'legau.cmn'

      include 'igcm_rough.cmn'

c      integer igc,jg,nhem,mgpp,jgg,mg
c      parameter(igc=132,jg=16,nhem=2,mgpp=66,jgg=32,mg=64)
c      integer ih,jh,ihem,jj,i,j

      integer ih,ihem,jj,i

c      real deltl
      integer iter
      integer istep_atm

#include "precision.inc"

      real(rk_in) :: surfnetsolar_atm(mg,jgg)
      real(rk_in) :: surfnetlong_atm(mg,jgg)
      real(rk_in) :: surfprec_atm(mg,jgg)
      real(rk_in) :: surflatent_atm(mg,jgg)
      real(rk_in) :: surfsensible_atm(mg,jgg)
      real(rk_in) :: surfstressx_atm(mg,jgg)
      real(rk_in) :: surfstressy_atm(mg,jgg)
      real(rk_in) :: surfevap_atm(mg,jgg)
      real(rk_in) :: surflatentavg_atm(mg,jgg)
      real(rk_in) :: surfsensibleavg_atm(mg,jgg)
      real(rk_in) :: surfstressxavg_atm(mg,jgg)
      real(rk_in) :: surfstressyavg_atm(mg,jgg)
      real(rk_in) :: surfevapavg_atm(mg,jgg)
      integer :: iland(mg,jgg)
c      real(rk_in) :: surfdelt2_atm
      integer :: surfitsll_atm
      real(rk_in) :: tscur_atm(mg,jgg)

      real :: surftdeepo_atm(mg,jgg)
      real :: surftdeep_atm(mg,jgg)
      save surftdeepo_atm,surftdeep_atm
      real :: surftstaro_atm(mg,jgg)
      save surftstaro_atm

      real surfpnu_atm
      parameter(surfpnu_atm=0.02)
 
      real(rk_in) :: surfsigma_atm
      real(rk_in) :: lowestlu_atm(mg,jgg),lowestlv_atm(mg,jgg)
      real(rk_in) :: lowestlt_atm(mg,jgg),lowestlq_atm(mg,jgg)
      real(rk_in) :: lowestlp_atm(mg,jgg)
      real(rk_in) :: rough_atm(mg,jgg)
      real(rk_in) :: surfqstar_atm(mg,jgg)

      real(rk_in) :: surfsalb_atm(mg,jgg)
      real(rk_in) :: albedo_atm(mg,jgg)
      real(rk_in) :: tstar_atm(mg,jgg)

      real surfhsnow(mg,jgg)
      save surfhsnow

      real surfsmstar(mg,jgg)
      save surfsmstar

      real(rk_in) :: surfitsll

c      real :: shbls(mg,jgg),slbls(mg,jgg)
c      real :: txbls(mg,jgg),tybls(mg,jgg)
c      save shbls,slbls,txbls,tybls

c      real :: tdcur(mg,jgg)
c      save tdcur

      real :: surfsigma
      real :: surfblcd(mg,jgg),surfblvb(mg,jgg)

      real :: sqcur(mg,jgg)
      save sqcur

      real :: surfsqstar(mg,jgg)
      real :: surftstar(mg,jgg)

      real pqsat_prec
      real(rk_in) pqsat_input

      real rsurfitsll
      real rct,rp0


c     For de-dimensionalising...
c      THES ARE NOW IN THE IGCM INCLUDES ABOVE

c      real radea
c      parameter(radea=6371e3)
c      real gascon
c      parameter(gascon=287.0)
c      real ww
c      parameter(ww=7.292E-5)
c      real cv
c      parameter(cv=radea*ww)
c      real cg
c      parameter(cg=cv*cv)
c      real ct
c      parameter(ct=cg/gascon)
c      real p0
c      parameter(p0=100000.0)

c     scalars
        surfitsll=surfitsll_atm
        surfsigma=surfsigma_atm

c     for speed....
      rsurfitsll=1.0/surfitsll
      rct=1.0/ct
      rp0=1.0/p0

c      DELTL=ww*2.0*surfDELT2_atm*rsurfitsll


      if (iter.eq.1) then
               call write_oceansurf_restart(itspd,istep_atm,
     :                 surfhsnow,surfsmstar,
     :                 surfsqstar,
     :                 surftdeep_atm,surftdeepo_atm,
     :                 surftstar,surftstaro_atm,
     :                 surfsalb_atm,
     :                 surfnetsolar_atm,surfnetlong_atm,
     :                 surfprec_atm,
     :                 iland,tstar_atm,albedo_atm)
      endif


      DO IH=1,JG
         JH=IH
         do ihem=1,nhem
            if (ihem.eq.1) then
               jj=jh
            else
               jj=jgg+1-jh
            end if
            do i=1,mg
 
               if (iland(i,jj).eq.0) then

               if (iter.eq.1) then

c               if (iland(i,jj).eq.1) then
c                  TSCUR(i,jj)=surfTSTARO_atm(i,Jj)
c               else
                  surfsalb_atm(i,jj)=albedo_atm(i,jj)
                  surftstar(i,jj)=tstar_atm(i,jj)
                  TSCUR_atm(i,jj)=surfTSTAR(i,Jj)
c               endif

               surfstressxavg_atm(i,jj)=0.0
               surfstressyavg_atm(i,jj)=0.0
               surfsensibleavg_atm(i,jj)=0.0
               surflatentavg_atm(i,jj)=0.0
               surfevapavg_atm(i,jj)=0.0

c               if (iland(i,jj).eq.1) then
c                  TDCUR(i,jj)=surfTDEEPO_atm(i,Jj)
c               endif


               endif


c     ******************************************************
c                  if (iland(i,jj).eq.1) then

c                     call igcm_landsurface_1(i,jj,
c     :                    lowestlp(i,jj),
c     :                    surfhsnow(i,jj),
c     :                    surfsmstar(i,jj),
c     :                    tscur(i,jj),
c     :                    qcur(i,jj),sqcur(i,jj),
c     :                    rough(i,jj),surfsalb(i,jj) )
c                  else

                     rough_atm(i,jj)=rough_ocean
c     Done like this because of precision reasons.....
                     pqsat_input=(tscur_atm(i,jj)+273.16)*rct
                     sqcur(i,jj)=
     :          pqsat_prec(pqsat_input)/
     :                   (lowestlp_atm(i,jj)*rp0)

                     surfqstar_atm(i,jj)=sqcur(i,jj)

c                  endif
c     ******************************************************

c     ******************************************************
                     call igcm_surflux(surfsigma,
     :                    lowestlp_atm(i,jj),
     :                    tscur_atm(i,jj),
     :                    surfqstar_atm(i,jj),sqcur(i,jj),
     :                    rough_atm(i,jj),
     :                    lowestlu_atm(i,jj),lowestlv_atm(i,jj),
     :                    lowestlt_atm(i,jj),lowestlq_atm(i,jj),
     :                    surfstressx_atm(i,jj),surfstressy_atm(i,jj),
     :                    surfsensible_atm(i,jj),surflatent_atm(i,jj),
     :                    surfblvb(i,jj),surfblcd(i,jj),
     :                    surfevap_atm(i,jj))
c     ******************************************************

c     ******************************************************
c                  if (iland(i,jj).eq.1) then

c                     call igcm_landsurface_2(deltl,
c     :                    surfprec_larg(i,jj)/surfitsll,
c     :                    surfprec_conv(i,jj)/surfitsll,
c     :                    surfnetsolar(i,jj),
c     :                    surfnetlong(i,jj),
c     :                    surfhsnow(i,jj),
c     :                    surfsmstar(i,jj),
c     :                    tscur(i,jj),tdcur(i,jj),
c     :                    surfsensible(i,jj),surflatent(i,jj) )
c                  endif


               surfstressxavg_atm(i,jj)=surfstressxavg_atm(i,jj)+
     :             surfstressx_atm(i,jj)*rSURFITSLL
               surfstressyavg_atm(i,jj)=surfstressyavg_atm(i,jj)+
     :             surfstressy_atm(i,jj)*rSURFITSLL
               surfsensibleavg_atm(i,jj)=surfsensibleavg_atm(i,jj)+
     :              surfsensible_atm(i,jj)*rSURFITSLL
               surflatentavg_atm(i,jj)=surflatentavg_atm(i,jj)+
     :              surflatent_atm(i,jj)*rSURFITSLL
               surfevapavg_atm(i,jj)=surfevapavg_atm(i,jj)+
     :              surfevap_atm(i,jj)*rSURFITSLL

c     ******************************************************



               if (iter.eq.6) then

c               if (iland(i,jj).eq.1) then
c                  surftdeepo_atm(i,jj)=
c     :                 surftdeep_atm(i,jj)*(1.-2.*surfpnu_atm)
c     $                 +surfpnu_atm*(surftdeepo_atm(i,jj)+tdcur(i,jj))
c                  surftdeep_atm(i,jj)=tdcur(i,jj)
c                  surftstaro_atm(i,jj)=
c     :                 surftstar(i,jj)*(1.-2.*surfpnu_atm)
c     $                 +surfpnu_atm*(surftstaro_atm(i,jj)+tscur(i,jj))
c                  surftstar(i,jj)=tscur(i,jj)
c               endif


               endif

            endif

            enddo
           enddo
         enddo

      return
      end
