      subroutine igcm_land_surflux(istep_atm,
     :     iter,surfitsll_atm,
     :     surfdelt2_atm,
     :     surfsigma_atm,
     :     iland,
     :     surfnetsolar_atm,surfnetlong_atm,
     :     surfprec_atm,
     :     lowestlu_atm,lowestlv_atm,lowestlt_atm,
     :     lowestlq_atm,lowestlp_atm,
     :     surflatent_atm,surfsensible_atm,
     :     surfstressx_atm,surfstressy_atm,
     :     surfevap_atm,
     :     surflatentavg_atm,surfsensibleavg_atm,
     :     surfstressxavg_atm,surfstressyavg_atm,
     :     surfevapavg_atm,
     :     tscur_atm,
     :     rough_atm,
     :     surfqstar_atm,
     :     surfsalb_atm,
     :     runoff_atm,
     :     test_water_land,
     :     fracice_atm,
     :     fracsnowice_atm,
     :     fracsnowveg_atm,
     :     snowdepth_atm,
     :     albice_atm,
     :     glim_covmap,
     :     glim_snow_model,
     :     glim_coupled)

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
      include 'igcm_lsurf.cmn'
      include 'igcm_prec.cmn'
#include "precision.inc"

C     =====
C     args
C     =====
      integer iter
      integer istep_atm
      integer :: iland(mg,jgg)
      integer :: surfitsll_atm

      real(rk_in) :: surfnetsolar_atm(mg,jgg)
      real(rk_in) :: surfnetlong_atm(mg,jgg)
      real(rk_in) :: surfprec_atm(mg,jgg)
      real(rk_in) :: surflatent_atm(mg,jgg)
      real(rk_in) :: surfsensible_atm(mg,jgg)
      real(rk_in) :: surfstressx_atm(mg,jgg)
      real(rk_in) :: surfstressy_atm(mg,jgg)
      real(rk_in) :: surflatentavg_atm(mg,jgg)
      real(rk_in) :: surfevap_atm(mg,jgg)
      real(rk_in) :: surfsensibleavg_atm(mg,jgg)
      real(rk_in) :: surfstressxavg_atm(mg,jgg)
      real(rk_in) :: surfstressyavg_atm(mg,jgg)
      real(rk_in) :: surfevapavg_atm(mg,jgg)
      real(rk_in) :: surfdelt2_atm
      real(rk_in) :: tscur_atm(mg,jgg)
      real(rk_in) :: surfsigma_atm
      real(rk_in) :: lowestlu_atm(mg,jgg),lowestlv_atm(mg,jgg)
      real(rk_in) :: lowestlt_atm(mg,jgg),lowestlq_atm(mg,jgg)
      real(rk_in) :: lowestlp_atm(mg,jgg)
      real(rk_in) :: rough_atm(mg,jgg)
      real(rk_in) :: surfqstar_atm(mg,jgg)
      real(rk_in) :: surfsalb_atm(mg,jgg)
      real(rk_in) :: runoff_atm(mg,jgg)
      real(rk_in) :: fracice_atm(mg,jgg)
      real(rk_in) :: fracsnowice_atm(mg,jgg)
      real(rk_in) :: fracsnowveg_atm(mg,jgg)
      real(rk_in) :: snowdepth_atm(mg,jgg)
      real(rk_in) :: albice_atm(mg,jgg)
      real(rk_in) :: glim_covmap(mg,jgg)
      real(rk_in) :: test_water_land

      logical glim_snow_model
c     glim_coupled is true if glimmer is running at this point
      logical glim_coupled

C     ==========
C     local vars
C     ==========

C      SAVE

      real surfpnu_atm
      parameter(surfpnu_atm=0.02)
c     Need this alternative definition for consistency 
c       with goldstein & genie.F energy and water terms.
      real radearth
      parameter(radearth=6370e3)

      integer ih,ihem,jj,i,j
      integer iof
      real deltl

      real :: surftdeepo_atm(mg,jgg)
      real :: surftdeep_atm(mg,jgg)
      save surftdeepo_atm,surftdeep_atm
      real :: surftstaro_atm(mg,jgg)
      save surftstaro_atm
      real surfhsnow(mg,jgg)
      save surfhsnow
      real surfsmstar(mg,jgg)
      save surfsmstar

      real(rk_in) :: surfitsll

      real :: tdcur(mg,jgg)
      save tdcur

      real :: surfsigma
      real :: surfblcd(mg,jgg),surfblvb(mg,jgg)

      real :: sqcur(mg,jgg)
      save sqcur

      real :: surfsqstar(mg,jgg)
      real :: surftstar(mg,jgg)
      save surftstar

      real fracice(mg,jgg)
      real fracsnowice(mg,jgg)
      real fracsnowveg(mg,jgg)
      real albice(mg,jgg)

c     do_snow_calc is set to specify whether
c     we do the snow-depth calculation at
c     a given grid-box
      logical do_snow_calc

      real runoff(mg,jgg)
      real runoff_sum(mg,jgg)
      save runoff_sum

      real rsurfitsll

      integer ifirst
      data ifirst/1/
      save ifirst

      real tot_water_land
      real ini_water_land
      save ini_water_land
      real tot_water_snow
      real ini_water_snow
      save ini_water_snow
      real tot_water_soil
      real ini_water_soil
      save ini_water_soil

      real test_precip
      real test_runoff
      real test_evap
      save test_precip
      save test_runoff
      save test_evap

      include 'igcm_area.cmn'

c     copy snow depth in...

      surfhsnow=snowdepth_atm

c     scalars
      surfitsll=surfitsll_atm
      surfsigma=surfsigma_atm

c     for speed....
      rsurfitsll=1.0/surfitsll

      DELTL=2.0*surfDELT2_atm*rsurfitsll

      if (iter.eq.1) then
               call write_landsurf_restart(itspd,istep_atm,
     :                 surfhsnow,surfsmstar,
     :                 surfsqstar,
     :                 surftdeep_atm,surftdeepo_atm,
     :                 surftstar,surftstaro_atm,
     :                 surfsalb_atm,
     :                 surfnetsolar_atm,surfnetlong_atm,
     :                 surfprec_atm) 
      endif

c     This bit needs to come after the restart.....
      if ((ifirst.le.2).and.(iter.eq.1)) then
        ini_water_snow=0.0
        ini_water_soil=0.0
        do j=1,jgg
        do i=1,mg
          ini_water_snow=ini_water_snow+
     :              surfhsnow(i,j)*300.0*
     :              4.0*pi*radearth*radearth*weight_atm(i,j)
          ini_water_soil=ini_water_soil+
     :              surfsmstar(i,j)*1000.0*
     :              4.0*pi*radearth*radearth*weight_atm(i,j)
        enddo
        enddo
        ini_water_land=ini_water_snow+ini_water_soil
        test_precip=0.0
        test_runoff=0.0
        test_evap=0.0
        ifirst=ifirst+1
      endif

      if (iter.eq.1) then
        runoff_sum(:,:)=0.0
      endif

      runoff(:,:)=0.0

      DO IH=1,JG
         JH=IH
         do ihem=1,nhem
               iof=(ihem-1)*mgpp
            if (ihem.eq.1) then
               jj=jh
            else
               jj=jgg+1-jh
            end if
            do i=1,mg
               j=(ihem-1)*mgpp+i
 
               if (iland(i,jj).eq.1) then

        fracice(i,jj)=fracice_atm(i,jj)
        albice(i,jj)=albice_atm(i,jj)
c       need this fix for making sure we get a snow fraction without glimmer
        if (surfhsnow(i,jj).gt.0.0.and.(.not.glim_snow_model)) then
           fracsnowice(i,jj)=fracice(i,jj)
           fracsnowveg(i,jj)=1.0-fracice(i,jj)
           fracice(i,jj)=0.0
        else
           fracsnowice(i,jj)=fracsnowice_atm(i,jj)
           fracsnowveg(i,jj)=fracsnowveg_atm(i,jj)
        endif

               if (iter.eq.1) then

c               if (iland(i,jj).eq.1) then
                  TSCUR_atm(i,jj)=surfTSTARO_atm(i,Jj)
c               else
c                  surfsalb(i,jj)=albedo_atm(i,jj)
c       This next line has to be /ct and not *rct for perfect restarts.....
c                  surftstar(i,jj)=(tstar_atm(i,jj)+273.16)/ct
c                  TSCUR(i,jj)=surfTSTAR(i,Jj)
c               endif

               surfstressxavg_atm(i,jj)=0.0
               surfstressyavg_atm(i,jj)=0.0
               surfsensibleavg_atm(i,jj)=0.0
               surflatentavg_atm(i,jj)=0.0
               surfevapavg_atm(i,jj)=0.0

c               if (iland(i,jj).eq.1) then
                  TDCUR(i,jj)=surfTDEEPO_atm(i,Jj)
c               endif


               endif

c     ******************************************************
c     Determine the value of snow calc flag
               if (glim_covmap(i,jj).gt.0.0.and.
     :              glim_snow_model.and.
     :              glim_coupled) then
                  do_snow_calc=.false.
               else
                  do_snow_calc=.true.
               endif
c     ******************************************************
c                  if (iland(i,jj).eq.1) then

                     call igcm_landsurface_1(i,jj,
     :                    lowestlp_atm(i,jj),
     :                    surfhsnow(i,jj),
     :                    surfsmstar(i,jj),
     :                    tscur_atm(i,jj),
     :                    surfqstar_atm(i,jj),sqcur(i,jj),
     :                    rough_atm(i,jj),surfsalb_atm(i,jj),
     :                    runoff,
     :                    fracice(i,jj),
     :                    fracsnowice(i,jj),
     :                    fracsnowveg(i,jj),
     :                    albice(i,jj))
c                  else

c                     rough(i,jj)=rough_ocean
c                     sqcur(i,jj)=pqsat(tscur(i,jj))/lowestlp(i,jj)
c                     qcur(i,jj)=sqcur(i,jj)

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

                   if (lownprecip) then
                     surfprec_atm(i,jj)=(prec_larg(i+iof,jh)+
     :                                  prec_conv(i+iof,jh))/
     :               3600.0*itspd/real(24)
                   endif


                     call igcm_landsurface_2(deltl,
     :                    surfprec_atm(i,jj)/surfitsll,
     :                    surfnetsolar_atm(i,jj),
     :                    surfnetlong_atm(i,jj),
     :                    surfhsnow(i,jj),
     :                    surfsmstar(i,jj),
     :                    tscur_atm(i,jj),tdcur(i,jj),
     :                    surfsensible_atm(i,jj),surflatent_atm(i,jj),
     :                    i,jj,itspd,surfitsll_atm,
     :                    surfevap_atm(i,jj),runoff,do_snow_calc)
c                  endif

c
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

c
c     ******************************************************

               if (iter.eq.6) then

c               if (iland(i,jj).eq.1) then
                  surftdeepo_atm(i,jj)=
     :                 surftdeep_atm(i,jj)*(1.-2.*surfpnu_atm)
     $                 +surfpnu_atm*(surftdeepo_atm(i,jj)+tdcur(i,jj))
                  surftdeep_atm(i,jj)=tdcur(i,jj)
                  surftstaro_atm(i,jj)=
     :                 surftstar(i,jj)*(1.-2.*surfpnu_atm)
     $                 +surfpnu_atm*(surftstaro_atm(i,jj)+
     :                  tscur_atm(i,jj))
                  surftstar(i,jj)=tscur_atm(i,jj)
c               endif

               endif
        
            endif

            enddo
           enddo
         enddo



c     Need to do these bits at the end because the runoff is 
c       not updated in gridpoint-by-gridpoint order.
      runoff_sum(:,:)=runoff_sum(:,:)+runoff(:,:)


      if (iter.eq.6) then

        runoff_atm(:,:)=runoff_sum(:,:)*(itspd/real(24))/3600.0

      if (flag_checkwater_land) then
        tot_water_snow=0.0
        tot_water_soil=0.0
        do j=1,jgg
        do i=1,mg
          tot_water_snow=tot_water_snow+
     :              surfhsnow(i,j)*300.0*
     :              4.0*pi*radearth*radearth*weight_atm(i,j)
          tot_water_soil=tot_water_soil+
     :              surfsmstar(i,j)*1000.0*
     :              4.0*pi*radearth*radearth*weight_atm(i,j)

          test_precip=test_precip+
     :      surfprec_atm(i,j)*
     :      3600.0*weight_atm(i,j)*4.0*pi*radearth*radearth*24/
     :      real(itspd)

          test_evap=test_evap+
     :      surfevapavg_atm(i,j)*
     :      3600.0*weight_atm(i,j)*4.0*pi*radearth*radearth*24/
     :      real(itspd)

          test_runoff=test_runoff-
     :      runoff_atm(i,j)*
     :      3600.0*weight_atm(i,j)*4.0*pi*radearth*radearth*24/
     :      real(itspd)

        enddo
        enddo
      tot_water_land=tot_water_snow+tot_water_soil
      test_water_land=tot_water_land-ini_water_land
      print*,'Total land water=',test_water_land
      print*,'Snow land water=',tot_water_snow-ini_water_snow
      print*,'Soil land water=',tot_water_soil-ini_water_soil
      print*,'Total precip on land=',test_precip
      print*,'Total runoff on land=',test_runoff
      print*,'Total evap on land=',test_evap
      print*,'Net p+e+r on land,=',test_precip+test_evap+test_runoff
      endif
      endif   

c     copy snow depth out again

      snowdepth_atm=surfhsnow

      return
      end
