      program GENIE

      use genie_util, only : die
      use write_netcdf
      use genie_control
      use genie_global
      use genie_ini_wrappers
      use genie_loop_wrappers
      use genie_end_wrappers

      implicit none
c     SG > This is (hopefully!) a temp measure for ENTS
#include "../genie-ents/src/fortran/var_ents.cmn"
c     SG <

      print*
      print*,'*******************************************************'
      print '("      Welcome to cGENIE -- version:",I5)',genie_version
      print*,'*******************************************************'
      print*
      
      call initialise_genie

c     ==================================================================
c     Initialise GOLDSTEIN ocean model
      if (flag_goldsteinocean) then
         call initialise_goldocean_wrapper
      endif
c     ==================================================================
c     Initialise EMBM atmosphere model
      if (flag_ebatmos) then
         call initialise_embm_wrapper
      endif
c     ==================================================================
c     Initialise PLASIM atmosphere model
      if (flag_plasimatmos) then
         call initialise_plasim_wrapper
      endif
c     ==================================================================
c     Initialise wind module
c     This module obtains the grid definitions from
c     the atmospheric module, therefore this call has to be made after
c     the atmospheric module has been initialised
      if (flag_wind) then
         call initialise_wind_wrapper
c     If GOLDSTEIN and EMBM active, copy wind stress fields from
c     atmospheric to ocean grid
         if ((flag_ebatmos).and.(flag_goldsteinocean)) then
c     No regridding required for genie-wind with GOLDSTEIN
c     and EMBM, as all three modules use the same grid definitions
            if ((ilat1_atm.ne.ilat1_ocn).or.
     &           (ilon1_atm.ne.ilon1_ocn).or.
     &           (ilat2_atm.ne.ilat3_atm).or.
     &           (ilon2_atm.ne.ilon3_atm)) then
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
      end if
c     ==================================================================
c     Initialise c-GOLDSTEIN sea-ice model
      if (flag_goldsteinseaice) then
         call ini_goldsteinseaice_wrapper
      endif
c     ==================================================================
c     Initialise land-surface scheme
      if (flag_ents) then
         call initialise_ents_wrapper
      endif
c     ==================================================================
c     *** GEM model - START
      if (flag_atchem.or.flag_biogem.or.flag_sedgem.or.flag_rokgem) then
         call initialise_gem_wrapper
      endif
c     ==================================================================
c     *** BIOGEM model - START
      if (flag_biogem) then
         call initialise_biogem_wrapper
         call biogem_climate_wrapper
      endif
c     ==================================================================
c     *** ATCHEM model - START
      if (flag_atchem) then
         call initialise_atchem_wrapper
         call cpl_comp_atmocn_wrapper
         call cpl_comp_EMBM_wrapper
         call cpl_comp_atmlnd_wrapper
         call cpl_comp_lndEMBM_wrapper
      endif
c     ==================================================================
c     *** SEDGEM model - START
      if (flag_sedgem) then
         call initialise_sedgem_wrapper
         call cpl_flux_sedocn_wrapper
         call cpl_comp_sedocn_wrapper
      endif
c     ==================================================================
c     *** ROKGEM model - START
      if (flag_rokgem) then
         call initialise_rokgem_wrapper
c     call cpl_flux_rokocn_wrapper
      endif
c     ==================================================================
c     
      istep_atm=0
      istep_ocn=0
      istep_sic=0
c     
      print*
      print*,'*******************************************************'
      print*,' Initialisation complete, simulation starting'
      print*,'*******************************************************'
c     
c     ==================================================================
c     *** BIOGEM model - headers and initialization run-time reporting 
      if (flag_biogem) then
         call biogem_climate_wrapper
         call diag_biogem_wrapper
      endif
c     ==================================================================

c     do the looping.....
c     koverall is now in atmospheric steps (the most dynamic module)
c     (= hours in the standard igcm version as in genie_example.job)
      do koverall=1,koverall_total
c     increment the clock which accumulates total time
         call increment_genie_clock
c     
c     ==================================================================
c     Earth system modules
c     ==================================================================
c     
c     ==================================================================
c     *** c-GOLDSTEIN surface flux routine
         if (flag_ebatmos.and.flag_goldsteinocean) then
c     the next line should read ".eq.1" so that c-GOLDSTEIN
c     surflux is executed on the first time-step
            if(mod(koverall,kocn_loop).eq.1) then
               istep_ocn=istep_ocn+1
               call surflux_wrapper
            endif
         endif 
c     ==================================================================
c     *** GOLDSTEIN & PLASIM advance timestep
         if (flag_plasimatmos.and.flag_goldsteinocean) then
            if(mod(koverall,kocn_loop).eq.0) then
               istep_ocn=istep_ocn+1
            endif
         endif 
c     ==================================================================
c     *** EMBM atmosphere model
         if (flag_ebatmos) then
            if(mod(koverall,katm_loop).eq.0) then
               istep_atm=istep_atm+1
               call embm_wrapper
            endif
         endif
c     ==================================================================
c     *** PLASIM atmosphere model
         if (flag_plasimatmos) then
            if(mod(koverall,katm_loop).eq.0) then
               istep_atm=istep_atm+1
!               print*,"calling plasim" 
               call plasim_wrapper
!AVERAGING
! for goldstein ocean
               if(mod(koverall-1,kocn_loop).eq.0) then
!                 print*,"resetting plasim averages for goldstein ocean",koverall
                 latent_ocn=0.0
                 sensible_ocn=0.0
                 netsolar_ocn=0.0
                 netlong_ocn=0.0
                 latent_coeff_atm=0.0
                 sensible_coeff_atm=0.0
                 evap_ocn=0.0
                 precip_ocn=0.0
                 runoff_ocn=0.0
                 ocean_stressx2_ocn=0.0
                 ocean_stressy2_ocn=0.0
                 ocean_stressx3_ocn=0.0
                 ocean_stressy3_ocn=0.0
                 go_solfor=0.0
               endif
               latent_ocn=latent_ocn+surf_latent_atm
               latent_coeff_atm=latent_coeff_atm+surf_latent_coeff_atm
               sensible_ocn=sensible_ocn+surf_sensible_atm
               sensible_coeff_atm=sensible_coeff_atm+surf_sensible_coeff_atm
               netsolar_ocn=netsolar_ocn+netsolar_atm
               netlong_ocn=netlong_ocn+netlong_atm
               evap_ocn=evap_ocn+evap_atm
               precip_ocn=precip_ocn+precip_atm
               runoff_ocn=runoff_ocn+land_runoff_atm
               ocean_stressx2_ocn=ocean_stressx2_ocn+surf_stressx2_atm
               ocean_stressy2_ocn=ocean_stressy2_ocn+surf_stressy2_atm
               ocean_stressx3_ocn=ocean_stressx3_ocn+surf_stressx3_atm
               ocean_stressy3_ocn=ocean_stressy3_ocn+surf_stressy3_atm
               go_solfor=go_solfor+solfor_atm
               if(mod(koverall,kocn_loop).eq.0) then
!                 print*,"averaging plasim for goldstein ocean",koverall
                 latent_ocn=latent_ocn/float(kocn_loop)
                 latent_coeff_atm=latent_coeff_atm/float(kocn_loop)
                 sensible_ocn=sensible_ocn/float(kocn_loop)
                 sensible_coeff_atm=sensible_coeff_atm/float(kocn_loop)
                 netsolar_ocn=netsolar_ocn/float(kocn_loop)
                 netlong_ocn=netlong_ocn/float(kocn_loop)
                 evap_ocn=evap_ocn/float(kocn_loop)
                 precip_ocn=precip_ocn/float(kocn_loop)
                 runoff_ocn=runoff_ocn/float(kocn_loop)
                 ocean_stressx2_ocn=ocean_stressx2_ocn/float(kocn_loop)
                 ocean_stressy2_ocn=ocean_stressy2_ocn/float(kocn_loop)
                 ocean_stressx3_ocn=ocean_stressx3_ocn/float(kocn_loop)
                 ocean_stressy3_ocn=ocean_stressy3_ocn/float(kocn_loop)
                 go_solfor=go_solfor/float(kocn_loop)
               endif
! for goldstein sea ice
               if(mod(koverall-1,ksic_loop).eq.0) then
!                 print*,"resetting plasim averages for goldstein seaice",koverall
                 insolar_sic=0.0
                 inlong_sic=0.0
                 netheat_sic=0.0
                 surft_atm_sic=0.0
                 surfq_atm_sic=0.0
                 surfp_atm_sic=0.0
                 surf_windspeed_sic=0.0
               endif
               insolar_sic=insolar_sic+insolar_atm
               inlong_sic=inlong_sic+inlong_atm
               netheat_sic=netheat_sic+netheat_atm
               surft_atm_sic=surft_atm_sic+surft_atm
               surfq_atm_sic=surfq_atm_sic+surfq_atm
               surfp_atm_sic=surfp_atm_sic+surfp_atm
               surf_windspeed_sic=surf_windspeed_sic+surf_windspeed_atm
               if(mod(koverall,ksic_loop).eq.0) then
!                 print*,"averaging plasim for goldstein seaice",koverall
                 insolar_sic=insolar_sic/float(ksic_loop)
                 go_fxsw=insolar_sic !same variable but for biogem
                 inlong_sic=inlong_sic/float(ksic_loop)
                 netheat_sic=netheat_sic/float(ksic_loop)
                 surft_atm_sic=surft_atm_sic/float(ksic_loop)
                 surfq_atm_sic=surfq_atm_sic/float(ksic_loop)
                 surfp_atm_sic=surfp_atm_sic/float(ksic_loop)
                 surf_windspeed_sic=surf_windspeed_sic/float(ksic_loop)
               endif
!!END AVERAGING
            endif
         endif
c     ==================================================================
c     wind module
         if (flag_wind) then
            call wind_wrapper
c     If GOLDSTEIN and EMBM active, copy wind stress fields from
c     atmospheric to ocean grid
            if ((flag_ebatmos).and.(flag_goldsteinocean)) then
c     No regridding required for genie-wind in conjunction with GOLDSTEIN
c     and EMBM, as all three modules use the same grid definitions
               if ((ilat1_atm.ne.ilat1_ocn).or.
     &              (ilon1_atm.ne.ilon1_ocn).or.
     &              (ilat2_atm.ne.ilat3_atm).or.
     &              (ilon2_atm.ne.ilon3_atm)) then
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
         end if
c     ==================================================================
c     ==================================================================
c     *** c-GOLDSTEIN sea-ice model
         if (flag_goldsteinseaice) then
            if(mod(koverall,ksic_loop).eq.0) then
               istep_sic=istep_sic+1
               if(flag_plasimatmos) then
!                 print*,"calling icesurflux and goldseaice",koverall
                 call plasim_surflux_wrapper
               endif
               call gold_seaice_wrapper
            endif
         endif
c     ==================================================================
c     *** GOLDSTEIN ocean model
         if (flag_goldsteinocean) then
            if(mod(koverall,kocn_loop).eq.0) then
!               print*,"calling goldstein",koverall
               call goldstein_wrapper
            endif
         endif
c     ==================================================================
c     *** ENTS land model
         if(mod(koverall,klnd_loop).eq.0) then
            if (flag_ents) then
               call ents_wrapper
               call cpl_flux_lndatm_wrapper
            endif
         endif
c     ==================================================================
c     *** ROKGEM model - UPDATE
         if (flag_rokgem) then
            if (mod(koverall,conv_kocn_krokgem*kocn_loop).eq.0) then
               call rokgem_wrapper
               call cpl_flux_rokatm_wrapper
               call cpl_flux_rokocn_wrapper
            endif
         endif
c     ==================================================================
c     *** BIOGEM model - UPDATE
         if (flag_biogem) then
            if (mod(koverall,conv_kocn_kbiogem*kocn_loop).eq.0) then
               call biogem_wrapper
               call biogem_climate_wrapper
               call diag_biogem_timeslice_wrapper
               call diag_biogem_timeseries_wrapper
               call cpl_flux_ocnatm_wrapper
               call cpl_flux_ocnsed_wrapper
               call cpl_comp_ocnsed_wrapper
               call diag_biogem_wrapper
            endif
         endif
c     ==================================================================
c     *** ATCHEM model - UPDATE
         if (flag_atchem) then
            if (mod(koverall,conv_kocn_katchem*kocn_loop).eq.0) then
               call atchem_wrapper
               call cpl_comp_atmocn_wrapper
               call cpl_comp_EMBM_wrapper
               call cpl_comp_atmlnd_wrapper
               call cpl_comp_lndEMBM_wrapper
            endif
         endif
c     ==================================================================
c     *** SEDGEM model - UPDATE
         if (flag_sedgem) then
            if (mod(koverall,conv_kocn_ksedgem*kocn_loop).eq.0) then
               call sedgem_wrapper
               call cpl_flux_sedocn_wrapper
               call cpl_comp_sedocn_wrapper
            endif
         endif

c     End of timestep loop
      enddo
c     ==================================================================

c     ==================================================================
c     *** BIOGEM model - RESTARTS
      if (flag_biogem) then
         call biogem_restart_wrapper
      endif
c     ==================================================================
c     *** ATCHEM model - RESTARTS
      if (flag_atchem) then
         call atchem_wrapper
         call atchem_restart_wrapper
      endif
c     ==================================================================
c     *** SEDGEM model - RESTARTS
      if (flag_sedgem) then
         call sedgem_restart_wrapper
      endif
c     ==================================================================
c     *** ROKGEM model - RESTARTS
      if (flag_rokgem) then
         call rokgem_restart_wrapper
      endif
c     ==================================================================
c     
c     ==================================================================
c     Earth system module shutdown
c     ==================================================================
c     
      print*
      print*,'*******************************************************'
      print*,' Simulation complete, shutdown starting'
      print*,'*******************************************************'
      print*

c     ==================================================================
c     End GOLDSTEIN ocean
      if (flag_goldsteinocean) then
         call end_goldstein
      endif
c     ==================================================================
c     End EMBM atmosphere
      if (flag_ebatmos) then
         call end_embm
      endif
c     ==================================================================
c     End PLASIM atmosphere
      if (flag_plasimatmos) then
          call epilog
          call mpstop
      endif
c     ==================================================================
c     End GOLDSTEIN sea-ice
      if (flag_goldsteinseaice) then
         call end_seaice
      endif
c     ==================================================================
c     *** BIOGEM model - END
      if (flag_biogem) then
         call end_biogem_wrapper
      endif
c     ==================================================================
c     *** ATCHEM model - END
      if (flag_atchem) then
         call end_atchem_wrapper
      endif
c     ==================================================================
c     *** SEDGEM model - END
      if (flag_sedgem) then
         call end_sedgem_wrapper
      endif
c     ==================================================================
c     *** ROKGEM model - END
      if (flag_rokgem) then
         call end_rokgem_wrapper
      endif
c     ==================================================================
c     
      if (flag_wind) then
         call end_wind_wrapper
      endif
c     ==================================================================
c     
c     End GENIE module
      call end_genie
c     
      print*
      print*,'*******************************************************'
      print*,' Shutdown complete; home time'
      print*,'*******************************************************'
      print*
c     
      stop
      end
