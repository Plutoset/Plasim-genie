

      program GENIE

c     (DL) It would be good to get some consistency with variable
c       naming conventions.
c
c     (DL) We need to sort out the timings as well, and have something more
c     instructive than koverall.
c
c     (DL) OK, lowestlp is a bit of a misnomer because it is
c       actually the surface pressure!

c     use statements for modules

      use genie_util, only : die
#ifdef glimmeron
      use glint_main, glimmer_pi => pi
      use glimmer_global
#endif
      use fixed_atmosphere
      use fake_atmosphere
      use fixed_land
      use write_netcdf
      use genie_control

      use genie_global
      use genie_ini_wrappers
      use genie_loop_wrappers
      use genie_end_wrappers

      use check_fluxes

      implicit none
c SG > This is (hopefully!) a temp measure for ENTS
#include "../genie-ents/src/fortran/var_ents.cmn"
c SG <

      write(*,*)
      write(*,*) '**************************************************'
      print '("       Welcome to GENIE      version:",I5)',genie_version
      write(*,*) '**************************************************'
      write(*,*)
      
      call initialise_genie
      call ini_averages

#ifdef lgraph
      if (lgraphics) call ini_graphics(coastline_file)
#endif
#ifdef glimmeron
c     Need to make sure the fixed icesheet is turned off if 
c     Glimmer is being used
        if (flag_icesheet) then
                flag_fixedicesheet=.false.
        endif
#endif

c ======================================================================
c     Initialise the fixed icesheet (also needed if glimmer is running)
#ifdef glimmeron
      if (flag_icesheet.or.flag_fixedicesheet) then
#else
      if (flag_fixedicesheet) then
#endif
         call ini_fixedicesheet_wrapper
      endif
c ======================================================================

c ======================================================================
c     Initialise the fixed atmospheric chemistry
      if (flag_fixedchem) then
         call initialise_fixedchem_wrapper
      endif
c ======================================================================
c     Initialise the IGCM atmosphere model
      if (flag_igcmatmos) then
c        This is a call to the igcm landsurface initialisation
         call initialise_igcmsurf
c        This sets up the IGCM grid, and includes one call to the igcm.
c        This should be renamed to initialise_igcm.
         call initialise_atmos_wrapper
      endif
c ======================================================================
c     Initialise the fixed atmosphere model
      if (flag_fixedatmos) then
         call initialise_fixedatmos_wrapper
      endif
c ======================================================================
c     Initialise the fake atmosphere model
      if (flag_fakeatmos) then
         call initialise_fakeatmos_wrapper
      endif
c ======================================================================
c     Initialise the fixed sea-ice model
      if (flag_fixedseaice) then
         call initialise_fixedseaice_wrapper
      endif
c ======================================================================
c     Initialise the slab sea-ice model
      if (flag_slabseaice) then
         call initialise_slabseaice_wrapper
      endif
c ======================================================================
c     Initialise the fixed ocean model
      if (flag_fixedocean) then
         call initialise_fixedocean_wrapper
      endif
c ======================================================================
c     Initialise the slab ocean model
      if (flag_slabocean) then
         call initialise_slabocean_wrapper
      endif
c ======================================================================
c     Initialise GOLDSTEIN ocean model
      if (flag_goldsteinocean) then
         call initialise_goldocean_wrapper
c              Execute interpolation routines if EMBM not used
c    do not exectute if fixedatmos and grid_type=2     
             if (.not. (flag_fakeatmos.or.flag_ebatmos.or.
     :  (flag_fixedatmos.and.(grid_type_fixedatmos.eq.2)))) then
c
                call interp_ocn_atm(
     :               aboxedge1_lon_atm,aboxedge1_lat_atm,
     :               aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :               ilandmask1_atm,ilandmask1_ocn,
     :               iwork1_atm,iwork1_ocn,
     :               interpmask_atm,weighttot_atm,
     :               interpmask_ocn,weighttot_ocn,
     :               dummy_atm,tstar_ocn,
     :               -2)

c     **COULD HIDE THIS IN INTERP_OCN_ATM**
               do j=1,ilat1_atm
                 do i=1,ilon1_atm   
                   if ((ilandmask1_atm(i,j).eq.0).and.
c     SHOULD THIS BE seaicefrac_atm_meanocn??
     :             (seaicefrac_atm(i,j).eq.0)) then
                     tstar_atm(i,j)=dummy_atm(i,j)
                   endif
                   temptop_atm(i,j)=dummy_atm(i,j)
                enddo
              enddo
c
              call interp_ocn_atm(
     :             aboxedge1_lon_atm,aboxedge1_lat_atm,
     :             aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :             ilandmask1_atm,ilandmask1_ocn,
     :             iwork1_atm,iwork1_ocn,
     :             interpmask_atm,weighttot_atm,
     :             interpmask_ocn,weighttot_ocn,
     :             dummy_atm,albedo_ocn,
     :             -2)
              do j=1,ilat1_atm
                do i=1,ilon1_atm   
                  if ((ilandmask1_atm(i,j).eq.0).and.
c     SHOULD THIS BE seaicefrac_atm_meanocn??
     :               (seaicefrac_atm(i,j).eq.0)) then
                    albedo_atm(i,j)=dummy_atm(i,j)
                  endif
                enddo
              enddo
      endif
      endif
c ======================================================================
c     Initialise EMBM atmosphere model
      if (flag_ebatmos) then
         call initialise_embm_wrapper
      endif
c ======================================================================
c Initialise wind module
c This module obtains the grid definitions from
c the atmospheric module, therefore this call has to be made after the
c atmospheric module has been initialised
      if (flag_wind) then
          call initialise_wind_wrapper
      endif
c
c ======================================================================
c If GOLDSTEIN and EMBM active, copy wind stress fields from
c atmospheric to ocean grid
c
      if ((flag_wind).and.(flag_ebatmos).and.(flag_goldsteinocean)) then
c No regridding required for genie-wind in conjunction with GOLDSTEIN
c and EMBM, as all three modules use the same grid definitions
         if ((ilat1_atm.ne.ilat1_ocn).or.
     &        (ilon1_atm.ne.ilon1_ocn).or.
     &        (ilat2_atm.ne.ilat3_atm).or.
     &        (ilon2_atm.ne.ilon3_atm)) then
            call die("Incompatible grids!",__LINE__,__FILE__)
         endif
         do j=1,ilat2_atm
            do i=1,ilon2_atm
               ocean_stressx2_ocn(i,j) = surf_stressx2_atm(i,j)
               ocean_stressy2_ocn(i,j) = surf_stressy2_atm(i,j)
               ocean_stressx3_ocn(i,j) = surf_stressx3_atm(i,j)
               ocean_stressy3_ocn(i,j) = surf_stressy3_atm(i,j)
            enddo
         enddo
      endif
c ======================================================================
c     Initialise c-GOLDSTEIN sea-ice model
      if (flag_goldsteinseaice) then
         call ini_goldsteinseaice_wrapper
      endif
c
c AY (23/09/04) : calculate average ocean grid cell temperature and
c                 albedo (for IGCM3-GOLDOCN-GOLDSIC runs)
c
      if (flag_goldsteinocean.and.flag_goldsteinseaice
     :     .and.(.not.flag_ebatmos)) then
         call gold_ocnsic_avg(
     :        frac_sic,
     :        tstar_ocn, temp_sic,
     :        albedo_ocn, albd_sic,
     :        tmpavg_ocn, albavg_ocn)
      endif
c ======================================================================
c Initialise land-surface scheme
      if (flag_land.and.(.not.flag_igcmatmos)) then
#ifdef USE_mosestriffid 
         call initialise_land_wrapper
#endif
      elseif (flag_land.and.flag_igcmatmos) then
#ifdef USE_mosestriffid 
         CALL reini_lnd_account
#endif
      elseif (flag_fixedland) then
         call initialise_fixedland_wrapper
         print*,'FIXEDLAND %% Finished initialise_fixedland'
      elseif (flag_ents) then
         call initialise_ents_wrapper
      endif
c     ======================================================================
c     *** GEM model - START
      if (flag_atchem.or.flag_biogem.or.flag_sedgem.or.flag_rokgem) then
         call initialise_gem_wrapper
      endif
c     ======================================================================
c     *** BIOGEM model - START
      if (flag_biogem) then
         call initialise_biogem_wrapper
         call biogem_climate_wrapper
      endif
c     ======================================================================
c     *** ATCHEM model - START
      if (flag_atchem) then
         call initialise_atchem_wrapper
         call cpl_comp_atmocn_wrapper
         call cpl_comp_EMBM_wrapper
      endif
c     ======================================================================
c     *** SEDGEM model - START
      if (flag_sedgem) then
         call initialise_sedgem_wrapper
         call cpl_flux_sedocn_wrapper
         call cpl_comp_sedocn_wrapper
      endif
c     ======================================================================
c     *** ROKGEM model - START
      if (flag_rokgem) then
          call initialise_rokgem_wrapper
c         call cpl_flux_rokocn_wrapper
      endif
c     ======================================================================
c Initialise ichem
      if (flag_ichem) then
          call initialise_ichem_wrapper
      endif
c=======================================================================
c     Initialise the Land Ice Model (GLIMMER)
#ifdef glimmeron
      if (flag_icesheet) then
         call initialise_glimmer_wrapper
c     Retrieve glimmer coverage map ====================================
         if (glint_coverage_map(glimmer_p,glim_covmap,
     :       glim_covmap_orog)/=0) then
            print*,'Problem with glimmer coverage map retrieval'
            stop
         endif
c+++++++++++++ GLIMMER land-scheme coupling - initial ++++++++++++++++++
c Note that we only do this if glimmer is coupled in immediately, and
c not delayed.
         if (kicesheet_start==0) then
            landicefrac_atm=glim_icefrac*glim_covmap+
     :           landicefrac_atm*(1-glim_covmap)
            if (glim_snow_model) then
               landsnowdepth_atm=glim_snowdepth*ratio_sd_dense*
     :              glim_covmap+landsnowdepth_atm*(1-glim_covmap)
            endif
         endif
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      endif
#endif

c ======================================================================
c     Initialise the weights routine (used for spatial interpolation)
      if ( 
     &  (flag_fixedatmos.or.flag_igcmatmos) .and.
     &  flag_goldsteinocean
     &   ) then
        call ini_weights_wrapper
      endif
c ======================================================================
c
      istep_atm=0
      istep_ocn=0
      istep_sic=0
      istep_lic=0
      istep_gsurf=0
      istep_che=0
c
c ADB I dont think the fake atmosphere needs this (not sure what the 
c purpose is) I'll put a if loop around to skip this step.

      if (.not. flag_fakeatmos) then

      if (aboxedge1_lat_atm(1).gt.aboxedge1_lat_atm(ilat1_atm)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0
      do j=1,ilat1_atm
        do i=1,ilon1_atm
          weight_atm(i,j)=plumin*(sin(aboxedge1_lat_atm(j)*2*pi/360.)-
     &              sin(aboxedge1_lat_atm(j+1)*2*pi/360.))*
     &              (aboxedge1_lon_atm(i+1)/360.-
     &              aboxedge1_lon_atm(i)/360.)/2.
          weightcheck=weightcheck+weight_atm(i,j)
        enddo
      enddo
      print*,'Check for weightings from genie atm = ',weightcheck
c
      
      if (aboxedge1_lat_ocn(1).gt.aboxedge1_lat_ocn(ilat1_ocn)) then
        plumin=1.0
      else
        plumin=-1.0
      endif
      weightcheck=0.0
      do j=1,ilat1_ocn
        do i=1,ilon1_ocn
          weight_ocn(i,j)=plumin*(sin(aboxedge1_lat_ocn(j)*2*pi/360.)-
     &              sin(aboxedge1_lat_ocn(j+1)*2*pi/360.))*
     &              (aboxedge1_lon_ocn(i+1)/360.-
     &              aboxedge1_lon_ocn(i)/360.)/2.
          weightcheck=weightcheck+weight_ocn(i,j)
        enddo
      enddo
      print*,'Check for weightings from genie ocn = ',weightcheck
      
      endif   ! if not flag_fakeatmos
      
      print*
      print*,'*******************************************************'
      print*,' Initialisation complete, simulation starting'
      print*,'*******************************************************'
      print*
c     
c     do the looping.....
c     koverall is now in atmospheric steps (the most dynamic module)
c     (= hours in the standard igcm version as in genie_example.job)
c     

      print*,'do the looping.....'

c ======================================================================
c     *** BIOGEM model - headers and initialization run-time reporting 
      if (flag_biogem) then
         call biogem_climate_wrapper
         call diag_biogem_wrapper
      endif

      do koverall=1,koverall_total

c       increment the clock which accumulates total time
        call increment_genie_clock

c       update Earth system boundary conditions
cjda comment out this unnecessary call
c        if (mod(koverall,100*kocn_loop).eq.0) then
c           call radfor_wrapper
c        endif

c
c     ============================================================
c      Earth system modules
c     ============================================================
c
c ======================================================================
c        *** IGCM atmosphere model ***
        if (flag_igcmatmos) then
          if (mod(koverall,katm_loop).eq.0) then
            istep_atm=istep_atm+1
            if (mod(istep_atm,24).eq.0.or.istep_atm.le.24) 
     :              print*,'calling the IGCM atmosphere model'
            do katmos=1,katmos_int
c
c ======================================================================
C This is the Adiabatic (i.e. advecting only - no physics)
C part of the igcm.                                                       
               call igcm3_adiab_wrapper
c
c ======================================================================
c  ichem module called here. Same time step as atmosphere
       if (flag_ichem) then
              call ichem_wrapper
       endif
c
c ======================================================================

              land_lowestlu_atm(:,:)=atmos_lowestlu_atm(:,:)
              land_lowestlv_atm(:,:)=atmos_lowestlv_atm(:,:)
              land_lowestlt_atm(:,:)=atmos_lowestlt_atm(:,:)
              land_lowestlq_atm(:,:)=atmos_lowestlq_atm(:,:)
              ocean_lowestlu_atm(:,:)=atmos_lowestlu_atm(:,:)
              ocean_lowestlv_atm(:,:)=atmos_lowestlv_atm(:,:)
              ocean_lowestlt_atm(:,:)=atmos_lowestlt_atm(:,:)
              ocean_lowestlq_atm(:,:)=atmos_lowestlq_atm(:,:)
              ocean_lowestlh_atm(:,:)=atmos_lowestlh_atm(:,:)
              ocean_lowestlp_atm(:,:)=atmos_lowestlp_atm(:,:)
c     Set the ocean and land lowest level variables equal to the 
c       equivalent values from the atmosphere.
c
c ======================================================================
c If using glimmer and igcm land surface, adjust fluxes to maintain
c water balance (hopefully)
c              if(flag_icesheet.and.(.not.flag_land)) then
c                 precip_atm=precip_atm-glim_prcp_adj
c                 where (precip_atm<0.0)
c                    glim_runoff_adj=precip_atm
c                    precip_atm=0.0
c                 elsewhere
c                    glim_runoff_adj=0.0
c                 endwhere
c              endif
c
c ======================================================================
c This is the mini-timestep loop, needed for the igcm land-surface.
c For the igcm, land_niter_tim=6, so normally loop is over 10minutes.
              do surf_iter_tim=1,land_niter_tim
c               This is the igcm land-surface and surface flux routine.
                IF(flag_land) THEN
#ifdef USE_mosestriffid 
                   call genie_land_surflux_wrapper
                   land_stressx_atm(:,:)  = land_stressxinst_atm(:,:)
                   land_stressy_atm(:,:)  = land_stressyinst_atm(:,:)
                   land_latent_atm(:,:)   = land_latentinst_atm(:,:)
                   land_sensible_atm(:,:) = land_sensibleinst_atm(:,:)
                   land_evap_atm(:,:)     = land_evapinst_atm(:,:)
#endif
                ELSE
                   call igcm_land_surflux_wrapper
                ENDIF
                call igcm_land_blayer_wrapper
              enddo
c
c ======================================================================
c Need to readjust runoff according to glimmer
c              if(flag_icesheet.and.(.not.flag_land)) then
c              do i=1,ilon1_atm
c                 do j=1,ilat1_atm
c                    call igcm_land_runoff(land_runoff_atm,
c     :                   glim_runoff_adj,i,j)
c                 enddo
c              enddo
c              endif
c
c ======================================================================

                 IF (flag_goldsteinocean.and.flag_goldsteinseaice) THEN
                    call genie_restarts_wrapper(1)
                 endif
c
c ======================================================================
              do surf_iter_tim=1,ocean_niter_tim
c     This is the mini-timestep loop, needed for the igcm ocean-surface.
c     For the igcm, ocean_niter_tim=6, so normally loop is over 10minutes.
                 IF (flag_goldsteinocean.and.flag_goldsteinseaice) THEN
c
                 else
                    call igcm_ocean_surflux_wrapper
                 endif
                 call igcm_ocean_blayer_wrapper
              enddo
c
c ======================================================================

              surf_latent_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_latent_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_latent_atm(:,:)
              surf_sensible_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_sensible_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_sensible_atm(:,:)
              surf_stressx_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_stressx_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_stressx_atm(:,:)
              surf_stressy_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_stressy_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_stressy_atm(:,:)
              surf_evap_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_evap_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_evap_atm(:,:)

              surf_tstarinst_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_tstarinst_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_tstarinst_atm(:,:)
              surf_rough_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_rough_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_rough_atm(:,:)
              surf_qstar_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_qstar_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_qstar_atm(:,:)
              surf_salb_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_salb_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_salb_atm(:,:)

              atmos_lowestlu_atm(:,:) = 
     &             ilandmask1_atm(:,:)*land_lowestlu_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_lowestlu_atm(:,:)
              atmos_lowestlv_atm(:,:) = 
     &             ilandmask1_atm*land_lowestlv_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_lowestlv_atm(:,:)
              atmos_lowestlt_atm(:,:) = 
     &             ilandmask1_atm*land_lowestlt_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_lowestlt_atm(:,:)
              atmos_lowestlq_atm(:,:) = 
     &             ilandmask1_atm*land_lowestlq_atm(:,:)
     &             + (1-ilandmask1_atm(:,:))*ocean_lowestlq_atm(:,:)
c
c     Make composite atmos and surf variables from the land and ocean values.
c
c ======================================================================
c     ***************************************
c     Use this line if you want to check the energy or
c       water budget for the surface
              if (flag_checkfluxes_surf) then
                 call check_fluxes_surf_l_wrapper
              endif
c     ***************************************  
              call igcm3_diab_wrapper
c
c ======================================================================
   


            enddo
c
c     End of the atmospheric and land loop
c
          endif                ! End of if atmos step
        endif                  ! End of if flag_igcmatmos
c     
c ======================================================================
c
c
c
c        *** Fixed atmosphere model
        if (flag_fixedatmos) then
           if(mod(koverall,katm_loop).eq.0) then
              if (.not.(flag_igcmatmos.or.flag_ebatmos)) then
                istep_atm=istep_atm+1
              endif
              if (mod(istep_atm,8640).eq.0.or.istep_atm.le.24)    
     :             print*,'calling the fixed atmosphere model' 
              call fixedatmos_wrapper
           endif
        endif
c
c ======================================================================
c        *** Fake atmosphere model (surface boundary conditions for goldstein)
        if (flag_fakeatmos) then
           if(mod(koverall,katm_loop).eq.0) then
              istep_atm=istep_atm+1
              if (mod(istep_atm,8640).eq.0.or.istep_atm.le.24)    
     :             print*,'calling the fake atmosphere model' 
              call fakeatmos_wrapper
           endif
        endif

c ======================================================================
c ======================================================================


        if (flag_goldsteinocean.and.flag_goldsteinseaice
     :       .and.(.not.flag_ebatmos)) then
c
c
c AY (27/07/04) : average these fields down to the ocean timestep
           do j=1,ilat1_atm
              do i=1,ilon1_atm
c
c Atmospheric properties
                 lowestlu_atm_meanocn(i,j) = 
     :                lowestlu_atm_meanocn(i,j) + 
     :                ocean_lowestlu_atm(i,j)*katm_loop/(1.*kocn_loop)
                 lowestlv_atm_meanocn(i,j) = 
     :                lowestlv_atm_meanocn(i,j) + 
     :                ocean_lowestlv_atm(i,j)*katm_loop/(1.*kocn_loop)
                 lowestlt_atm_meanocn(i,j) = 
     :                lowestlt_atm_meanocn(i,j) + 
     :                ocean_lowestlt_atm(i,j)*katm_loop/(1.*kocn_loop)
                 lowestlq_atm_meanocn(i,j) = 
     :                lowestlq_atm_meanocn(i,j) + 
     :                ocean_lowestlq_atm(i,j)*katm_loop/(1.*kocn_loop)
                 lowestlp_atm_meanocn(i,j) = 
     :                lowestlp_atm_meanocn(i,j) + 
     :                ocean_lowestlp_atm(i,j)*katm_loop/(1.*kocn_loop)
                 lowestlh_atm_meanocn(i,j) = 
     :                lowestlh_atm_meanocn(i,j) + 
     :                ocean_lowestlh_atm(i,j)*katm_loop/(1.*kocn_loop)
c     
c Heat fluxes
                 netsolar_atm_meanocn(i,j) = 
     :                netsolar_atm_meanocn(i,j) + 
     :                netsolar_atm(i,j)*katm_loop/(1.*kocn_loop)
                 netlong_atm_meanocn(i,j) = 
     :                netlong_atm_meanocn(i,j) + 
     :                netlong_atm(i,j)*katm_loop/(1.*kocn_loop)
c     
c Freshwater fluxes
                 precip_atm_meanocn(i,j) = 
     :                precip_atm_meanocn(i,j) + 
     :                precip_atm(i,j)*katm_loop/(1.*kocn_loop)
c DJLAY (8/11/2004) added runoff
                 runoff_atm_meanocn(i,j) =
     :                runoff_atm_meanocn(i,j) + 
     :                land_runoff_atm(i,j)*katm_loop/(1.*kocn_loop)
              enddo
           enddo
        endif                   ! End of if flag_goldstein ...
c
c ======================================================================

c     ** Average the fluxes output fron the atmosphere onto the 
c       seaice/ocean timestep **
c     In fact, stressx, stressy, and precip are not used by
c       the slabseaice.
c     THIS SHOULD REALLY BE HIDDEN IN A SUBROUTINE

        if ( (flag_igcmatmos.or.flag_fixedatmos) .and.
     &       (flag_slabseaice) ) then
           if (mod(koverall,katm_loop).eq.0) then
              do i=1,ilon1_atm
                 do j=1,ilat1_atm
                    latent_atm_meansic(i,j)=
     :                   latent_atm_meansic(i,j)+
     :                   surf_latent_atm(i,j)*katm_loop/(1.*ksic_loop)
                    sensible_atm_meansic(i,j)=
     :                   sensible_atm_meansic(i,j)+
     :                   surf_sensible_atm(i,j)*katm_loop/(1.*ksic_loop)
                    netsolar_atm_meansic(i,j)=
     :                   netsolar_atm_meansic(i,j)+
     :                   netsolar_atm(i,j)*katm_loop/(1.*ksic_loop)
                    netlong_atm_meansic(i,j)=
     :                   netlong_atm_meansic(i,j)+
     :                   netlong_atm(i,j)*katm_loop/(1.*ksic_loop)
                    stressx_atm_meansic(i,j)=
     :                   stressx_atm_meansic(i,j)+
     :                   surf_stressx_atm(i,j)*katm_loop/(1.*ksic_loop)
                    stressy_atm_meansic(i,j)=
     :                   stressy_atm_meansic(i,j)+
     :                   surf_stressy_atm(i,j)*katm_loop/(1.*ksic_loop)
                    precip_atm_meansic(i,j)=
     :                   precip_atm_meansic(i,j)+
     :                   (precip_atm(i,j))
     :                   *katm_loop/(1.*kocn_loop)
              enddo
            enddo
         endif
      endif                     ! End of atmos slabseaice interaction
c
c ======================================================================
c        *** Fixed sea-ice model ***
      if (flag_fixedseaice) then
         if (mod(koverall,ksic_loop).eq.0) then
            istep_sic=istep_sic+1
            if (mod(istep_sic,120).eq.0.or.istep_sic.le.6) 
     :           print*,'calling the fixed sea-ice model'
            call fixedseaice_wrapper
            do i=1,ilon1_atm
               do j=1,ilat1_atm
                  seaicefrac_atm_meanocn(i,j)=
     :                 seaicefrac_atm_meanocn(i,j)+
     :                 seaicefrac_atm(i,j)*
     :                 ksic_loop/(1.*kocn_loop)
               enddo
            enddo
         endif
      endif
c ======================================================================
c        *** Slab sea-ice model ***
      if (flag_slabseaice) then
         if (mod(koverall,ksic_loop).eq.0) then
            istep_sic=istep_sic+1
            if (mod(istep_sic,5).eq.0.or.istep_sic.le.10) 
     :           print*,'calling the slab sea-ice model'
            call slabseaice_wrapper
            do i=1,ilon1_atm
               do j=1,ilat1_atm
                  seaicefrac_atm_meanocn(i,j)=
     :                 seaicefrac_atm_meanocn(i,j)+
     :                 seaicefrac_atm(i,j)*
     :                 ksic_loop/(1.*kocn_loop)
c     NOTE THE MINUS SIGN HERE!!!!!!!!
                  conductflux_atm_meanocn(i,j)=
     :                 conductflux_atm_meanocn(i,j)+
     :                 conductflux_atm(i,j)*
     :                 ksic_loop/(1.*kocn_loop)
               enddo
            enddo
          endif
        endif                  ! End of the slab sea-ice model
c
c          Now need to correct the fluxes which the ocean has received, 
c            if the ocean has changed to seaice.......
c ======================================================================
c
c AY (23/09/04) : I don't think these adjustments of netsolar, etc.
c                 for sea-ice and ocean are appropriate for the
c                 IGCM3-GOLDSTEIN-GOLDSTEINSEAICE model, so have
c                 altered flag list to stop this section being
c                 called if this version is being used - this is far
c                 from satisfactory
c
c      if ( (flag_igcmatmos.or.flag_fixedatmos) .and.
c    &         (flag_goldsteinocean.or.flag_slabocean) ) then
c
        if ( (flag_igcmatmos.or.flag_fixedatmos) .and.
     : (flag_goldsteinocean.or.flag_slabocean.or.flag_fixedocean) .and.
     : (.not.flag_goldsteinseaice) ) then
c
           do i=1,ilon1_atm
              do j=1,ilat1_atm
c
            latent_atm_meanocn(i,j)=
     :          latent_atm_meanocn(i,j)+(1-seaicefrac_atm(i,j))*
     :          surf_latent_atm(i,j)*katm_loop/(1.*kocn_loop)
            sensible_atm_meanocn(i,j)=
     :          sensible_atm_meanocn(i,j)+(1-seaicefrac_atm(i,j))*
     :          surf_sensible_atm(i,j)*katm_loop/(1.*kocn_loop)
            netsolar_atm_meanocn(i,j)=
     :          netsolar_atm_meanocn(i,j)+(1-seaicefrac_atm(i,j))*
     :          netsolar_atm(i,j)*katm_loop/(1.*kocn_loop)
            netlong_atm_meanocn(i,j)=
     :          netlong_atm_meanocn(i,j)+(1-seaicefrac_atm(i,j))*
     :          netlong_atm(i,j)*katm_loop/(1.*kocn_loop)
            stressx_atm_meanocn(i,j)=
     :          stressx_atm_meanocn(i,j)+(1-seaicefrac_atm(i,j))*
     :          surf_stressx_atm(i,j)*katm_loop/(1.*kocn_loop)
            stressy_atm_meanocn(i,j)=
     :          stressy_atm_meanocn(i,j)+(1-seaicefrac_atm(i,j))*
     :          surf_stressy_atm(i,j)*katm_loop/(1.*kocn_loop)
            precip_atm_meanocn(i,j)=precip_atm_meanocn(i,j)+
     :          (precip_atm(i,j))
     :          *katm_loop/(1.*kocn_loop)
            evap_atm_meanocn(i,j)=evap_atm_meanocn(i,j)+
     :          surf_evap_atm(i,j)
     :          *katm_loop/(1.*kocn_loop)
            runoff_atm_meanocn(i,j)=runoff_atm_meanocn(i,j)+
     :          land_runoff_atm(i,j)
     :          *katm_loop/(1.*kocn_loop)

c     These bits are currently in fixedatmos but should really 
c       be in fixedseaice
                if (flag_fixedatmos) then
              if (.not.(flag_igcmatmos.or.flag_ebatmos)) then                  
                   conductflux_atm_meanocn(i,j)=
     :      conductflux_atm_meanocn(i,j)+(1-seaicefrac_atm(i,j))*
     :      conductflux_atm(i,j)*katm_loop/(1.*kocn_loop)
                  waterflux_atm_meanocn(i,j)=
     :      waterflux_atm_meanocn(i,j)+(1-seaicefrac_atm(i,j))*
     :      waterflux_atm(i,j)*katm_loop/(1.*kocn_loop)
                endif
              endif               
            enddo
         enddo
      endif                     ! End of flux adjustments for slab sea-ice
c
c ======================================================================
c        *** Fixed ocean model ***
      if (flag_fixedocean) then
         if (mod(koverall,kocn_loop).eq.0) then
            istep_ocn=istep_ocn+1
            if (mod(istep_ocn,5).eq.0.or.istep_ocn.le.10) 
     :           print*,'calling the fixed ocean model'
            call fixedocean_wrapper

         endif
      endif                     ! End of the fixed ocean model
c ======================================================================
c        *** Slab ocean model ***
      if (flag_slabocean) then
         if (mod(koverall,kocn_loop).eq.0) then
            istep_ocn=istep_ocn+1
            if (mod(istep_ocn,5).eq.0.or.istep_ocn.le.10) 
     :           print*,'calling the slab ocean model'
            call slabocean_wrapper
         endif
      endif                     ! End of the slab ocean model
c ======================================================================
c
c        *** GOLDSTEIN-SEAICE surface flux routine
c     
        if (flag_goldsteinocean.and.flag_goldsteinseaice
     :       .and.(.not.flag_ebatmos)) then
c AY (23/09/04) : unlike the other surflux routine, the number below
c                 should be zero, not one
           if(mod(koverall,kocn_loop).eq.0) then
              istep_gsurf=istep_gsurf+1
c    
c    Execute interpolation routines but not if fixedatmos and grid_type=2
              if (.not. (flag_fixedatmos.and.
     :             (grid_type_fixedatmos.eq.2))) then
c     
c                Lowest level atmospheric temperature
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                lowestlt_atm_meanocn,ocean_lowestlt_ocn,
     :                2)
c
c                Lowest level atmospheric humidity
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                lowestlq_atm_meanocn,ocean_lowestlq_ocn,
     :                2)
c
c                Lowest level atmospheric pressure
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                lowestlp_atm_meanocn,ocean_lowestlp_ocn,
     :                2)
c
c                Lowest level atmospheric height
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                lowestlh_atm_meanocn,ocean_lowestlh_ocn,
     :                2)
c
c                Lowest level atmospheric height
c                Not implemented yet
c                Fixed value of 100 m assumed in subroutine (bad!)
c
c                Wind speed x on the ocean's u point
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge2_lon_ocn,aboxedge2_lat_ocn,
     :                ilandmask1_atm,ilandmask2_ocn,
     :                iwork1_atm,iwork2_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                lowestlu_atm_meanocn,ocean_lowestlu2_ocn,
     :                2)
c
c                Wind speed y on the ocean's u point
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge2_lon_ocn,aboxedge2_lat_ocn,
     :                ilandmask1_atm,ilandmask2_ocn,
     :                iwork1_atm,iwork2_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                lowestlv_atm_meanocn,ocean_lowestlv2_ocn,
     :                2)
c
c                Wind speed x on the ocean's v point
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge3_lon_ocn,aboxedge3_lat_ocn,
     :                ilandmask1_atm,ilandmask3_ocn,
     :                iwork1_atm,iwork3_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                lowestlu_atm_meanocn,ocean_lowestlu3_ocn,
     :                2)
c
c                Wind speed y on the ocean's v point
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge3_lon_ocn,aboxedge3_lat_ocn,
     :                ilandmask1_atm,ilandmask3_ocn,
     :                iwork1_atm,iwork3_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                lowestlv_atm_meanocn,ocean_lowestlv3_ocn,
     :                2)
c
c                Net short-wave heat flux
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                netsolar_atm_meanocn,ocean_atm_netsolar_ocn,
     :                4)
c
c                Net long-wave heat flux
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                netlong_atm_meanocn,ocean_atm_netlong_ocn,
     :                4)
c
c                Precipitation (not actually needed until goldstein.F)
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                precip_atm_meanocn,ocean_precip_ocn,
     :                4)
c
c                Runoff (not actually needed until goldstein.F)
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                runoff_atm_meanocn,ocean_runoff_ocn,
     :                4)
              else
c
c No regridding required (on same grid as ocean)
c Might become an option with fixed atmos at some point
                 do j=1,ilat1_atm
                    do i=1,ilon1_atm
                  ocean_lowestlt_ocn(i,j)=lowestlt_atm_meanocn(i,j)
                  ocean_lowestlq_ocn(i,j)=lowestlq_atm_meanocn(i,j)
                  ocean_lowestlp_ocn(i,j)=lowestlp_atm_meanocn(i,j)
                  ocean_lowestlu2_ocn(i,j)=lowestlu_atm_meanocn(i,j)
                  ocean_lowestlv2_ocn(i,j)=lowestlv_atm_meanocn(i,j)
                  ocean_lowestlu3_ocn(i,j)=lowestlu_atm_meanocn(i,j)
                  ocean_lowestlv3_ocn(i,j)=lowestlv_atm_meanocn(i,j)
                  ocean_atm_netsolar_ocn(i,j)=netsolar_atm_meanocn(i,j)
                  ocean_atm_netlong_ocn(i,j)=netlong_atm_meanocn(i,j)
                  ocean_precip_ocn(i,j)=precip_atm_meanocn(i,j)
                  ocean_runoff_ocn(i,j)=runoff_atm_meanocn(i,j)
                    enddo
                 enddo
c
              endif
c           

c             =========================
              call surf_ocn_sic_wrapper
c             =========================
c
c              Execute interpolation routines
c    do not exectute if fixedatmos and grid_type=2     
              if (.not. (flag_fixedatmos.and.
     :             (grid_type_fixedatmos.eq.2))) then
c
c                Ocean roughness
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                ocean_rough_atm,rough_ocn,
     :                -2)
c
c                Ocean-to-atmosphere latent heat flux
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                ocean_latent_atm,atmos_latent_ocn,
     :                -4)
c
c                Ocean-to-atmosphere sensible heat flux
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                ocean_sensible_atm,atmos_sensible_ocn,
     :                -4)
c
c                Ocean-to-atmosphere evaporation FW flux
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :                ilandmask1_atm,ilandmask1_ocn,
     :                iwork1_atm,iwork1_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                ocean_evap_atm,atmos_evap_ocn,
     :                -4)
c
c                Set instantaneous flux variable to equal flux variable
                 ocean_evapinst_atm(:,:) = ocean_evap_atm(:,:)
c
c                Wind stress x on the ocean's u point 
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge2_lon_ocn,aboxedge2_lat_ocn,
     :                ilandmask1_atm,ilandmask2_ocn,
     :                iwork1_atm,iwork2_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                ocean_stressx_atm,ocean_stressx2_ocn,
     :                -2)
c
c                Wind stress y on the ocean's u point
                 call interp_ocn_atm(
     :                aboxedge1_lon_atm,aboxedge1_lat_atm,
     :                aboxedge2_lon_ocn,aboxedge2_lat_ocn,
     :                ilandmask1_atm,ilandmask2_ocn,
     :                iwork1_atm,iwork2_ocn,
     :                interpmask_atm,weighttot_atm,
     :                interpmask_ocn,weighttot_ocn,
     :                ocean_stressy_atm,ocean_stressy2_ocn,
     :                -2)
              else
c No regridding required (on same grid as ocean)
                 do j=1,ilat1_atm
                    do i=1,ilon1_atm
                       ocean_rough_atm(i,j)=rough_ocn(i,j)
                       ocean_latent_atm(i,j)=atmos_latent_ocn(i,j)
                       ocean_sensible_atm(i,j)=atmos_sensible_ocn(i,j)
                       ocean_evap_atm(i,j)=atmos_evap_ocn(i,j)
                       ocean_stressx_atm(i,j)=ocean_stressx2_ocn(i,j)
                       ocean_stressy_atm(i,j)=ocean_stressy2_ocn(i,j)
                    enddo
                 enddo
              endif
c
c AY (03/08/04) : blank meanocn fields (for refilling next time)
            do j=1,ilat1_atm
               do i=1,ilon1_atm
                  lowestlu_atm_meanocn(i,j) = 0.
                  lowestlv_atm_meanocn(i,j) = 0.
                  lowestlt_atm_meanocn(i,j) = 0.
                  lowestlq_atm_meanocn(i,j) = 0.
                  lowestlp_atm_meanocn(i,j) = 0.
                  lowestlh_atm_meanocn(i,j) = 0.
                  netsolar_atm_meanocn(i,j) = 0.
                  netlong_atm_meanocn(i,j) = 0.
                  precip_atm_meanocn(i,j) = 0.
                  runoff_atm_meanocn(i,j) = 0.
               enddo
            enddo
c
c AY (23/09/04) : not sure what the difference between "normal" and "inst"
c                 variables is for, so copying what Phil does with the
c                 land fluxes for now
            ocean_latentinst_atm(:,:)   = ocean_latent_atm(:,:)
            ocean_sensibleinst_atm(:,:) = ocean_sensible_atm(:,:)
            ocean_stressxinst_atm(:,:)  = ocean_stressx_atm(:,:)
            ocean_stressyinst_atm(:,:)  = ocean_stressy_atm(:,:)

         endif
      endif                     ! End of GOLDSTEIN-SEAICE surface fluxes
c     
c ======================================================================
c
c        *** c-GOLDSTEIN surface flux routine
c
        if (flag_ebatmos.and.flag_goldsteinocean) then
c AY (03/05/04) : the next line should read ".eq.1" so that c-GOLDSTEIN
c                 surflux is executed on the first time-step
           if(mod(koverall,kocn_loop).eq.1) then
              istep_ocn=istep_ocn+1
              call surflux_wrapper
          endif

          if((flag_land).and.(mod(koverall,klnd_loop).eq.0)) then
!------------------------------------------------------------------
! Note that the output from surflux() is using the wrong arrays:
! netsolar_atm should be net surface shortwave (+ve towards surface) 
! but is atmospheric shortwave heating.  Ditto for longwave.
!------------------------------------------------------------------
            land_lowestlu_atm(:,:)  = 2.0
            land_lowestlv_atm(:,:)  = 1.0
            atmos_lowestlp_atm(:,:) = 1e5
            land_lowestlq_atm(:,:)  = surf_qstar_atm(:,:)
            land_lowestlt_atm(:,:)  = tstar_atm(:,:)

#ifdef USE_mosestriffid 
            call genie_land_surflux_wrapper2
#endif

          endif

          if (flag_land) then 
#ifdef USE_mosestriffid 
            CALL land_embm_combine(surf_sensible_atm,      !INOUT
     &             evap_atm,runoff_ocn,                                    !INOUT
     &             land_sensibleinst_atm,              !IN
     &             land_evap_atm,land_runoff_atm)                          !IN
#endif
          endif
        endif                  ! End of c-GOLDSTEIN surface fluxes
c     
c ======================================================================
c        *** EMBM atmosphere model
        if (flag_ebatmos) then
c       AY (03/05/04) : the next line should read ".eq.0" so that the embm
c                 is executed every time-step
           if(mod(koverall,katm_loop).eq.0) then
              istep_atm=istep_atm+1
              call embm_wrapper
           endif
        endif                   ! end of the EMBM atmosphere model
c ======================================================================
c wind module
       if (flag_wind) then
         call wind_wrapper
       endif
c
c ======================================================================
c If GOLDSTEIN and EMBM active, copy wind stress fields from
c atmospheric to ocean grid
c
      if ((flag_wind).and.(flag_ebatmos).and.(flag_goldsteinocean)) then
c No regridding required for genie-wind in conjunction with GOLDSTEIN
c and EMBM, as all three modules use the same grid definitions
         if ((ilat1_atm.ne.ilat1_ocn).or.
     &        (ilon1_atm.ne.ilon1_ocn).or.
     &        (ilat2_atm.ne.ilat3_atm).or.
     &        (ilon2_atm.ne.ilon3_atm)) then
            call die("Incompatible grids!",__LINE__,__FILE__)
         endif
         do j=1,ilat2_atm
            do i=1,ilon2_atm
               ocean_stressx2_ocn(i,j) = surf_stressx2_atm(i,j)
               ocean_stressy2_ocn(i,j) = surf_stressy2_atm(i,j)
               ocean_stressx3_ocn(i,j) = surf_stressx3_atm(i,j)
               ocean_stressy3_ocn(i,j) = surf_stressy3_atm(i,j)
            enddo
         enddo
      endif
c ======================================================================
c        *** c-GOLDSTEIN sea-ice model
        if (flag_goldsteinseaice) then
c AY (03/05/04) : the next line should read ".eq.1" so that gold_seaice
c                 (and goldstein and surflux) is executed on the first
c                 time-step
c AY (23/09/04) : no longer true - now set to 0 (this works 'cos the
c                 ocean and sea-ice can be updated more arbitrarily
c                 than the EMBM and c-GOLDSTEIN surflux)
           if(mod(koverall,ksic_loop).eq.0) then
              istep_sic=istep_sic+1
              call gold_seaice_wrapper
           endif
        endif                   ! End of the c-GOLDSTEIN sea-ice model
c ======================================================================
c
c        *** GOLDSTEIN ocean model
c
        if (flag_goldsteinocean) then
c AY (03/05/04) : the next line should read ".eq.1" to reproduce c-goldstein
c                 exactly, so that goldstein
c                 (and surflux and gold_seaice) is executed on the first
c                 time-step
c
c AY (30/07/04) : the above is no longer true - now set to 0 (this 
c                 works 'cos the ocean and sea-ice can be updated more 
c                 arbitrarily than the EMBM and c-GOLDSTEIN surflux)
           if(mod(koverall,kocn_loop).eq.0) then
c              Only step the ocean counter here if the EMBM is not
c              being used (if it is, the counter is stepped by surflux.F)
             if (.not.flag_ebatmos) istep_ocn=istep_ocn+1
c     
c    Execute interpolation routines if EMBM not used,
c    do not exectute if fixedatmos and grid_type=2        
c            if (.not. (flag_ebatmos.or.
c    :  (flag_fixedatmos.and.(grid_type_fixedatmos.eq.2)))) then
c
c AY (23/09/04) : not sure about the conditionality above - is this
c                 better?  I might not have thought of all of the
c                 possible flag combinations.

c ADB (5/8/07), add flag for fake atmosphere because we do not want to 
c do any interpolation when using it. 

        if ( (.not.flag_fakeatmos) .and.
     :       (.not.flag_ebatmos) .and.
     :       (.not.(flag_fixedatmos.and.
     :             (grid_type_fixedatmos.eq.2))) .and.
     :       (.not.flag_goldsteinseaice) ) then
c
c              Sea-ice fractional cover
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              seaicefrac_atm_meanocn,seaicefrac_ocn,
     :              2)
c
c              Sea-ice conduction heat flux
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              conductflux_atm_meanocn,conductflux_ocn,
     :              4)
c
c              Latent heat flux
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              latent_atm_meanocn,latent_ocn,
     :              4)
c

c              Net shortwave heat flux
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              netsolar_atm_meanocn,netsolar_ocn,
     :              4)
c
c              Net longwave heat flux
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              netlong_atm_meanocn,netlong_ocn,
     :              4)
c
c              Sensible heat flux
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              sensible_atm_meanocn,sensible_ocn,
     :              4)
c
c AY (21/09/04) : The following interpolations are incorrect since
c                 wind stress should be interpolated to the ocean's
c                 u and v points rather than its tracer point.  The
c                 interpolations remain unchanged at this time so
c                 that the model remains compliant with ongoing work.
c
c              Should be using alon2_ocn, etc. (u point)
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              stressx_atm_meanocn,ocean_stressx2_ocn,
     :              2)
c              Should be using alon3_ocn, etc. (v point)
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              stressx_atm_meanocn,ocean_stressx3_ocn,
     :              2)
c              Should be using alon2_ocn, etc. (u point)
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              stressy_atm_meanocn,ocean_stressy2_ocn,
     :              2)
c              Should be using alon3_ocn, etc. (v point)
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              stressy_atm_meanocn,ocean_stressy3_ocn,
     :              2)
c
c              Precipitation freshwater flux
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              precip_atm_meanocn,precip_ocn,
     :              4)
c
c              Evaporation freshwater flux
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              evap_atm_meanocn,evap_ocn,
     :              4)
c
c              Runoff freshwater flux
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              runoff_atm_meanocn,runoff_ocn,
     :              4)
            elseif (flag_fixedatmos.and.
     :              (grid_type_fixedatmos.eq.2)) then
c
c     If we have fixedatmos on the goldstein grid, then set 
c       *_atm=*_ocn
c     This only works if the atmospheric and ocean grids are the same,
c       hence the check on grid_type_fixedatmos
c
               do j=1,ilat1_atm
                  do i=1,ilon1_atm
                     netsolar_ocn(i,j)=netsolar_atm_meanocn(i,j)
                     netlong_ocn(i,j)=netlong_atm_meanocn(i,j)
                     sensible_ocn(i,j)=sensible_atm_meanocn(i,j)
                     latent_ocn(i,j)=latent_atm_meanocn(i,j)
c AY (23/09/04) : the next lines are incorrect - see above note
                     ocean_stressx2_ocn(i,j)=stressx_atm_meanocn(i,j)
                     ocean_stressy2_ocn(i,j)=stressy_atm_meanocn(i,j)
                     ocean_stressx3_ocn(i,j)=stressx_atm_meanocn(i,j)
                     ocean_stressy3_ocn(i,j)=stressy_atm_meanocn(i,j)
                     conductflux_ocn(i,j)=conductflux_atm_meanocn(i,j)
                     waterflux_ocn(i,j)=waterflux_atm_meanocn(i,j)
                     precip_ocn(i,j)=precip_atm_meanocn(i,j)
                     evap_ocn(i,j)=evap_atm_meanocn(i,j)
                     runoff_ocn(i,j)=runoff_atm_meanocn(i,j)
                  enddo
               enddo
c
            elseif (flag_goldsteinocean.and.flag_goldsteinseaice
     :              .and.flag_igcmatmos) then
c
c AY (27/07/04) : This sets up the fluxes for IGCM3-GOLDOCN-GOLDSIC runs
               do j=1,ilat1_ocn
                  do i=1,ilon1_ocn
                     latent_ocn(i,j)=ocean_latent_ocn(i,j)
                     sensible_ocn(i,j)=ocean_sensible_ocn(i,j)
                     netsolar_ocn(i,j)=ocean_netsolar_ocn(i,j)
                     netlong_ocn(i,j)=ocean_netlong_ocn(i,j)
                     evap_ocn(i,j)=ocean_evap_ocn(i,j)
                     precip_ocn(i,j)=ocean_precip_ocn(i,j)
                     runoff_ocn(i,j)=ocean_runoff_ocn(i,j)
                  enddo
               enddo
c
            endif
c
c     I think that modules should output only where their mask is not 
c       equal to 0.
c     They should give 1e20 elsewhere, as a check.  The model will crash 
c       (hopefully) if any module is given 1e20 for any of its fluxes.
c
c             if (mod(koverall,8640).eq.0) then
c               print*,'calling the goldstein ocean model'
c             endif
c
c     *********************************************
c     for water-budgeting      
c     AY (12/07/05) : removed surplus argument to function
       if (flag_goldsteinocean.and.flag_goldsteinseaice
     :             .and.(.not.flag_ebatmos)) then
         call goldstein_delayfluxes(
     :     lrestart_genie,
     :     evap_ocn,
     :     latent_ocn,
     :     sensible_ocn
     :                             )
       endif

c     *********************************************

c            PRINT*,'>>> GOLDSTEIN @ ',koverall,kocn_loop
       call goldstein_wrapper
c            PRINT*,'<<<'

c
c AY (24/09/04) : calculate average ocean grid cell temperature and
c                 albedo (for IGCM3-GOLDOCN-GOLDSIC runs) then set
c                 up dummy arguments for ocean temperature and
c                 albedo regridding
c
            if (flag_goldsteinocean.and.flag_goldsteinseaice
     :           .and.(.not.flag_ebatmos)) then
c              For IGCM3-GOLDOCN-GOLDSIC model
               call gold_ocnsic_avg(
     :              frac_sic,
     :              tstar_ocn, temp_sic,
     :              albedo_ocn, albd_sic,
     :              tmpavg_ocn, albavg_ocn)
c
               dumtmp_ocn(:,:) = tmpavg_ocn(:,:)
               dumalb_ocn(:,:) = albavg_ocn(:,:)
            else
c              For IGCM3-GOLDOCN-SLABSIC model, etc.
               dumtmp_ocn(:,:) = tstar_ocn(:,:)
               dumalb_ocn(:,:) = albedo_ocn(:,:)
            endif
c
c              Execute interpolation routines if EMBM not used
c    do not exectute if fakeatmos or fixedatmos and grid_type=2     
            if (.not. (flag_fakeatmos.or.flag_ebatmos.or.
     :         (flag_fixedatmos.and.(grid_type_fixedatmos.eq.2)))) then
c               
c              Ocean temperature (or "average" temperature)
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              dummy_atm,dumtmp_ocn,
     :              -2)

            if (flag_goldsteinocean.and.flag_goldsteinseaice
     :           .and.(.not.flag_ebatmos)) then
                tstar_atm(:,:)=dummy_atm(:,:)
                ocean_tstarinst_atm(:,:)=dummy_atm(:,:)
            else
c     **COULD HIDE THIS IN INTERP_OCN_ATM**
               do j=1,ilat1_atm
                  do i=1,ilon1_atm   
c     SHOULD THIS BE seaicefrac_atm_meanocn??
                     if ((ilandmask1_atm(i,j).eq.0).and.
     :                    (seaicefrac_atm(i,j).eq.0)) then
                        tstar_atm(i,j)=dummy_atm(i,j)
                     endif
                     temptop_atm(i,j)=dummy_atm(i,j)
                  enddo
               enddo
            endif ! end goldocn,goldsic, not eb

c
c              Ocean albedo (or average albedo)
               call interp_ocn_atm(
     :              aboxedge1_lon_atm,aboxedge1_lat_atm,
     :              aboxedge1_lon_ocn,aboxedge1_lat_ocn,
     :              ilandmask1_atm,ilandmask1_ocn,
     :              iwork1_atm,iwork1_ocn,
     :              interpmask_atm,weighttot_atm,
     :              interpmask_ocn,weighttot_ocn,
     :              dummy_atm,dumalb_ocn,
     :              -2)

            if (flag_goldsteinocean.and.flag_goldsteinseaice
     :           .and.(.not.flag_ebatmos)) then
                albedo_atm(:,:)=dummy_atm(:,:)
            else
               do j=1,ilat1_atm
                  do i=1,ilon1_atm   
c     SHOULD THIS BE seaicefrac_atm_meanocn??
                     if ((ilandmask1_atm(i,j).eq.0).and.
     :                    (seaicefrac_atm(i,j).eq.0)) then
                        albedo_atm(i,j)=dummy_atm(i,j)
                     endif
                  enddo
               enddo
            endif ! end goldocn,goldsic, not eb



            endif   ! end if not embm or fixedatmos(ocn grid)

         endif   ! end of if ocn loop
      endif                     ! end of the GOLDSTEIN ocean model
c     
c     ======================================================================
c     *** MOSES/TRIFFID land model
      if(mod(koverall,klnd_loop).eq.0) then
         if (flag_ents) then
                                !Simple carbon-cycle land scheme
                                !           call genie_simpleland()
         elseif (flag_fixedland) then
                                !Fixed land scheme
            call genie_fixedland_wrapper
         endif
      endif
c     ======================================================================
c     *** ENTS land model
      if(mod(koverall,klnd_loop).eq.0) then
         if (flag_ents) then
            call ents_wrapper
            call cpl_flux_lndatm_wrapper
         endif
      endif
c     ======================================================================
c     *** ROKGEM model - UPDATE
      if (flag_rokgem) then
         if (mod(koverall,conv_kocn_krokgem*kocn_loop).eq.0) then
c            PRINT*,'>>> ROKEGM @ ',koverall,conv_kocn_krokgem*kocn_loop
c            call cpl_flux_atmrok_wrapper 
             call rokgem_wrapper
c            PRINT*,'<<<'
c            PRINT*,'* cpl_flux_rokatm_wrapper'
             call cpl_flux_rokatm_wrapper
c            PRINT*,'* cpl_flux_rokocn_wrapper'
             call cpl_flux_rokocn_wrapper
         endif
      endif
c     ======================================================================
c     *** BIOGEM model - UPDATE
      if (flag_biogem) then
         if (mod(koverall,conv_kocn_kbiogem*kocn_loop).eq.0) then
c            PRINT*,'>>> BIOGEM @ ',koverall,conv_kocn_kbiogem*kocn_loop
            call biogem_wrapper
c            PRINT*,'<<<'
c            PRINT*,'* biogem_climate_wrapper'
            call biogem_climate_wrapper
            call diag_biogem_timeslice_wrapper
            call diag_biogem_timeseries_wrapper
            call cpl_flux_ocnatm_wrapper
            call cpl_flux_ocnsed_wrapper
            call cpl_comp_ocnsed_wrapper
            call diag_biogem_wrapper
         endif
      endif
c     ======================================================================
c     *** ATCHEM model - UPDATE
      if (flag_atchem) then
         if (mod(koverall,conv_kocn_katchem*kocn_loop).eq.0) then
c            PRINT*,'>>> ATCHEM @ ',koverall,conv_kocn_katchem*kocn_loop
            call atchem_wrapper
c            PRINT*,'<<<'
c            PRINT*,'* cpl_comp_atmocn_wrapper'
            call cpl_comp_atmocn_wrapper
            call cpl_comp_EMBM_wrapper
            call cpl_comp_atmlnd_wrapper
            call cpl_comp_lndEMBM_wrapper
         endif
      endif
c     ======================================================================
c     *** SEDGEM model - UPDATE
      if (flag_sedgem) then
         if (mod(koverall,conv_kocn_ksedgem*kocn_loop).eq.0) then
c            PRINT*,'>>> SEDEGM @ ',koverall,conv_kocn_ksedgem*kocn_loop
            call sedgem_wrapper
c            PRINT*,'<<<'
            call cpl_flux_sedocn_wrapper
            call cpl_comp_sedocn_wrapper
         endif
      endif
cc     ======================================================================
cc     *** BIOGEM model - RESTARTS
c      if (flag_biogem) then
c         if (mod(istep_ocn,go_iwstp).eq.0) then
c            PRINT*,'>>> BIOGEM RESTART @ ',istep_ocn,go_iwstp
c            call biogem_restart_wrapper
c            PRINT*,'<<<'
c         end if
c      endif
cc     ======================================================================
cc     *** ATCHEM model - RESTARTS
c      if (flag_atchem) then
c         if (mod(istep_ocn,go_iwstp).eq.0) then
c            PRINT*,'>>> ATCHEM @ ',istep_ocn,go_iwstp
c            call atchem_wrapper
c            PRINT*,'<<<'
c            PRINT*,'>>> ATCHEM RESTART @ ',istep_ocn,go_iwstp
c            call atchem_restart_wrapper
c            PRINT*,'<<<'
c         end if
c      endif
cc     ======================================================================
cc     *** SEDGEM model - RESTARTS
c      if (flag_sedgem) then
c         if (mod(istep_ocn,go_iwstp).eq.0) then
c            PRINT*,'>>> SEDGEM RESTART @ ',istep_ocn,go_iwstp
c            call sedgem_restart_wrapper
c            PRINT*,'<<<'
c         end if
c      endif
cc     ======================================================================
cc     *** ROKGEM model - RESTARTS
c      if (flag_rokgem) then
c         if (mod(istep_ocn,go_iwstp).eq.0) then
c            PRINT*,'>>> ROKGEM RESTART @ ',istep_ocn,go_iwstp
c            call rokgem_restart_wrapper
c            PRINT*,'<<<'
c         end if
c      endif
c     ======================================================================
c     *** Fixed atmospheric chemistry model
      if (flag_fixedchem) then
        if (mod(koverall,kchem_loop).eq.0) then
          istep_che=istep_che+1
          call fixedchem_wrapper
        endif
      endif
c     ======================================================================
c     *** Fixed land-ice model
      if (flag_fixedicesheet) then
        if (mod(koverall,kicesheet_loop).eq.0) then
          istep_lic=istep_lic+1
          call fixedicesheet_wrapper
          iconv4lnd = iconv_ice
        endif
      endif
c     ======================================================================
c     *** GLIMMER land-ice model
#ifdef glimmeron
        if (flag_icesheet.and.koverall>=kicesheet_start) then
          if (mod(koverall,kicesheet_loop).eq.0) then
             glim_tempmask=((surf_tstarinst_atm.gt.0.0).and.
     :            (landsnowdepth_atm.gt.0.0).and.
     :            (landsnowvegfrac_atm+
     :            landsnowicefrac_atm.gt.0.0).and.
     :            glim_covmap.gt.0.0)
             istep_lic=istep_lic+glim_timestep
             call glimmer_wrapper
            if (glim_flag) then
               call fixedicesheet_wrapper
c++++++++++ GLIMMER orography coupling +++++++++++++++++++++++++++++++++
               surf_orog_atm=glim_orog*glim_covmap_orog+
     :            surf_orog_atm*(1-glim_covmap_orog)
c++++++++++ GLIMMER land-scheme coupling +++++++++++++++++++++++++++++++
               where (glim_covmap>0.0)
                  landicefrac_atm=glim_icefrac
                  landsnowicefrac_atm=glim_snowicefrac
                  landsnowvegfrac_atm=glim_snowvegfrac
               endwhere
               if (glim_snow_model) then
                  landsnowdepth_atm=
     :                 glim_snowdepth*ratio_sd_dense*glim_covmap+
     :                 landsnowdepth_atm*(1-glim_covmap)
c++++++++++ ADJUST SURFACE AIR TEMP APPROPRIATELY ++++++++++++++++++++++
                  where (glim_tempmask)
                     surf_tstarinst_atm=0.0
                  endwhere
               endif
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
               iconv_ice=1
               iconv4lnd=1
            endif
          endif
        endif                  ! end of the GLIMMER land-ice model
#endif
c ======================================================================
c
c     ============================================================
c      Data output
c     ============================================================
        if ((flag_igcmatmos).or.(flag_fixedatmos)) then
          if(mod(koverall,kocn_loop).eq.0) then
             call write_averages_wrapper
          endif
        endif
c
c     ****************************************
c      An example call to write_netcdf_genie:
c
c        allocate (ncwritedata(ilon1_atm,ilat1_atm,2))
c        ncwritedata(:,:,1) = precip_atm
c        ncwritevarnames(1) = 'precip_atm'
c        ncwritedata(:,:,2) = tstar_atm
c        ncwritevarnames(2) = 'tstar_atm'
c        call write_netcdf_genie('/home/ggdagw/precip.nc',
c     :       ncwritedata,ncwritevarnames,alon1_atm,alat1_atm)
c        deallocate (ncwritedata)
c
c     ****************************************
c
c     ============================================================
c      Housekeeping
c     ============================================================
c
c     ****************************************
c     Use this line if you want to check the energy or
c       water budget for ocean
        if (mod(koverall,kocn_loop).eq.0) then
           if (flag_checkfluxes_ocn) then
              call check_fluxes_ocn_wrapper
           endif
        endif
c     ****************************************
c      
c     ****************************************
c     Use this line if you want to check the energy or
c       water budget for the surface
        if (flag_checkfluxes_surf) then
           call check_fluxes_surf_o_wrapper
        endif
c     ****************************************
c
c     ****************************************
c     Use this line if you want to check the
c       water budget for the atlantic
        if (flag_checkfluxes_atlantic) then
           call check_fluxes_atlantic_wrapper
        endif
c     ****************************************

c     Reset flux fields between ocean/sea-ice timesteps
c
        if ((flag_igcmatmos).or.(flag_fixedatmos)) then
c
c          Sea-ice fields
c
           if(mod(koverall,ksic_loop).eq.0) then
c
c        BEWARE, THIS BIT OF CODE HAS CHANGED AND WILL AFFECT PREVIOUS RESULTS.
c        used to be if(mod(koverall,kocn_loop).eq.0)
c        THAT WAS A BUG I THINK.
c
              do i=1,ilon1_atm
                 do j=1,ilat1_atm
c                   Mean seaice fluxes from atm reset
                    latent_atm_meansic(i,j)=0.
                    sensible_atm_meansic(i,j)=0.
                    netsolar_atm_meansic(i,j)=0.
                    netlong_atm_meansic(i,j)=0.
                    stressx_atm_meansic(i,j)=0.
                    stressy_atm_meansic(i,j)=0.
                    precip_atm_meansic(i,j)=0.                     
                 enddo
              enddo
           endif
c
c          Ocean fields
c
           if(mod(koverall,kocn_loop).eq.0) then
              do i=1,ilon1_atm
                 do j=1,ilat1_atm
c                   Mean ocean fluxes from atm reset
                    latent_atm_meanocn(i,j)=0.
                    sensible_atm_meanocn(i,j)=0.
                    netsolar_atm_meanocn(i,j)=0.
                    netlong_atm_meanocn(i,j)=0.
                    stressx_atm_meanocn(i,j)=0.
                    stressy_atm_meanocn(i,j)=0.
                    precip_atm_meanocn(i,j)=0.
                    evap_atm_meanocn(i,j)=0.
                    runoff_atm_meanocn(i,j)=0.
c                   Mean ocean fluxes from seaice reset
                    seaicefrac_atm_meanocn(i,j)=0.
                    conductflux_atm_meanocn(i,j)=0.
                    waterflux_atm_meanocn(i,j)=0.
c                   Extra fields used in GOLDOCN-GOLDSIC surflux
                    lowestlu_atm_meanocn(i,j) = 0.
                    lowestlv_atm_meanocn(i,j) = 0.
                    lowestlt_atm_meanocn(i,j) = 0.
                    lowestlq_atm_meanocn(i,j) = 0.
                    lowestlp_atm_meanocn(i,j) = 0.
                 enddo
              enddo
           endif
c
        endif
c

c     AJR; genie_clock updating now at statr of loop
c       increment the clock which accumulates total time
c        call increment_genie_clock

c ************************
         if (flag_goldsteinocean.and.flag_goldsteinseaice
     :              .and.flag_igcmatmos) then
           call genie_restarts_wrapper(2)

         endif
c ************************

c     End of timestep loop
      enddo

c     ============================================================
c     Flux tests
c     ============================================================
      if ((flag_igcmatmos).and.(flag_checkfluxes_surf)) then
         call compare_water_fluxes
      end if

c     ============================================================
c     Temporary code for biogeochemistry restart saving
c     ============================================================
c     *** BIOGEM model - RESTARTS
      if (flag_biogem) then
         PRINT*,'>>> SAVING BIOGEM RESTART <<<'
         call biogem_restart_wrapper
      endif
c     ======================================================================
c     *** ATCHEM model - RESTARTS
      if (flag_atchem) then
c         PRINT*,'>>> ATCHEM @ ',koverall
         call atchem_wrapper
c         PRINT*,'<<<'
         PRINT*,'>>> SAVING ATCHEM RESTART <<<'
         call atchem_restart_wrapper
      endif
c     ======================================================================
c     *** SEDGEM model - RESTARTS
      if (flag_sedgem) then
         PRINT*,'>>> SAVING SEDEGM RESTART <<<'
         call sedgem_restart_wrapper
      endif
c     ======================================================================
c     *** ROKGEM model - RESTARTS
      if (flag_rokgem) then
         PRINT*,'>>> SAVING ROKGEM RESTART <<<'
         call rokgem_restart_wrapper
      endif
c     ======================================================================

c
c     ============================================================
c      Earth system module shutdown
c     ============================================================
c
      print*
      print*,'*******************************************************'
      print*,' Simulation complete, shutdown starting'
      print*,'*******************************************************'
      print*
c         
c     End IGCM3 atmosphere
      if (flag_igcmatmos) then
         call end_atmos
      endif
c
c     End GOLDSTEIN ocean
      if (flag_goldsteinocean) then
         call end_goldstein
      endif
c
c     End EMBM atmosphere
      if (flag_ebatmos) then
         call end_embm
      endif
c
c     End GOLDSTEIN sea-ice
      if (flag_goldsteinseaice) then
         call end_seaice
      endif
c
c     End MOSES/TRIFFID land
      if (flag_land) then
#ifdef USE_mosestriffid 
        call end_land
#endif
c
c     End SimpleLand
c     elseif (flag_simpleland) then
c       call end_simpleland
c
c     End Fixed land
      elseif (flag_fixedland) then
        call end_fixedland
      endif
c
c     End Fixed atmos
      if (flag_fixedatmos) then
        call end_fixedatmos
      endif
c
c     ======================================================================
c     *** BIOGEM model - END
      if (flag_biogem) then
         call end_biogem_wrapper
      endif
c     ======================================================================
c     *** ATCHEM model - END
      if (flag_atchem) then
         call end_atchem_wrapper
      endif
c     ======================================================================
c     *** SEDGEM model - END
      if (flag_sedgem) then
         call end_sedgem_wrapper
      endif
c     ======================================================================
c     *** ROKGEM model - END
      if (flag_rokgem) then
         call end_rokgem_wrapper
      endif
c     ======================================================================
c
c     End GLIMMER icesheet
#ifdef glimmeron
      if (flag_icesheet) then
        call end_glint(glimmer_p)
      endif
#endif
c
      if (flag_wind) then
        call end_wind_wrapper
      endif
c
#ifdef lgraph
      if (lgraphics) call end_graphics
#endif
c
c     End GENIE module
      call end_genie
c     
      print*
      print*,'*******************************************************'
      print*,' Shutdown complete, au revoir'
      print*,'*******************************************************'
      print*
c
      stop
      end
