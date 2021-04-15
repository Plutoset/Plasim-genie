

MODULE genie_loop_wrappers

  use genie_global

contains

#ifdef USE_mosestriffid 
  subroutine genie_land_surflux_wrapper
    implicit none
    call genie_land_surflux( &
         co2_atm,netsolar_atm,netlong_atm,land_lowestlu_atm, &   !IN
         land_lowestlv_atm,precip_atm, &                         !IN
         land_lowestlt_atm,land_lowestlq_atm, &                  !IN
         atmos_lowestlp_atm,atmos_lowestlh_atm, &                !IN
         land_tstarinst_atm, &                                   !OUT
         land_salb_atm,land_evapinst_atm,land_latentinst_atm, &  !OUT
         land_sensibleinst_atm,land_stressxinst_atm, &           !OUT
         land_stressyinst_atm,land_rough_atm,land_qstar_atm, &   !OUT
         land_runoff_atm,land_tice_ice,landicealbedo_atm, &      !OUT
         land_fxco2_atm,test_water_land)                         !OUT
  end subroutine genie_land_surflux_wrapper
#endif

  !! **REDFLAG** These 2 routines MUST be merged
#ifdef USE_mosestriffid 
  subroutine genie_land_surflux_wrapper2
    implicit none
    CALL genie_land_surflux( &
         co2_atm,netsolar_ocn,netlong_ocn,land_lowestlu_atm, &    !IN
         land_lowestlv_atm,precip_atm, &                          !IN
         land_lowestlt_atm,land_lowestlq_atm, &                   !IN
         atmos_lowestlp_atm,atmos_lowestlh_atm, &                 !IN
         land_tstarinst_atm, &                                    !OUT
         land_salb_atm,land_evap_atm,land_latentinst_atm, &       !OUT
         land_sensibleinst_atm,land_stressxinst_atm, &            !OUT
         land_stressyinst_atm,land_rough_atm,land_qstar_atm, &    !OUT
         land_runoff_atm,land_tice_ice,land_albice_ice, &         !OUT
         land_fxco2_atm,test_water_land)                          !OUT
  end subroutine genie_land_surflux_wrapper2
#endif

  !!
  subroutine fixedatmos_wrapper
    use fixed_atmosphere
    implicit none
    call fixedatmos( &
         istep_atm, &
         tstar_atm, &
         precip_atm, &
         surf_latent_atm, &
         surf_sensible_atm, &
         netsolar_atm, &
         netlong_atm, &
         surf_stressx_atm, &
         surf_stressy_atm, &
         land_runoff_atm, &
         surf_evap_atm, &
         conductflux_atm, &
         waterflux_atm, &
         atmos_lowestlq_atm,atmos_lowestlu_atm, &
         atmos_lowestlv_atm,atmos_lowestlp_atm, &
         atmos_lowestlh_atm, &
         interpmask_atm, &
         weighttot_atm, &
         surf_orog_atm)
  end subroutine fixedatmos_wrapper

  !!
  subroutine fakeatmos_wrapper
    use fake_atmosphere
    implicit none

  print*,''
  print*,'before fakeatmos: trest and srest, heat and fwflux at x=-165 and y=54n'
  print*, trest_ocn(9,32), srest_ocn(9,32), conductflux_ocn(9,32), waterflux_ocn(9,32)
  print*,'before fakeatmos: tstar_ocn and sstar_ocn + 34.9 at x=-165 and y=54n'
  print*, tstar_ocn(9,32), (sstar_ocn(9,32)+34.9)

    call fakeatmos(  &
         conductflux_ocn, &
         waterflux_ocn, &
         tstar_ocn, &
         sstar_ocn, &
         trest_ocn, &
         srest_ocn)

  print*,''
  print*,'after fakeatmos: trest and srest, heat and fwflux at x=-165 and y=54n'
  print*, trest_ocn(9,32), srest_ocn(9,32), conductflux_ocn(9,32), waterflux_ocn(9,32)
  print*,'after fakeatmos: tstar_ocn and sstar_ocn+34.9 at x=-165 and y=54n'
  print*, tstar_ocn(9,32), (sstar_ocn(9,32)+34.9)


  end subroutine fakeatmos_wrapper

  !!
  subroutine fixedseaice_wrapper
    ! I (djl) should change the arguments to this routine, as not all 
    ! of them are used.
    ! DJL
    ! Well, the seaicefrac_atm should really be called seaicefrac_sic
    ! for consistency.  I will change this later.  Same for albedo.
    ! Also should re-order arguments sensibly into input,input&output,output.
    ! The 1.* should also be changed to real().  This applies to everywhere 
    ! in this code.
    implicit none
    call fixedseaice( &
         istep_sic, &
         tstar_atm, &
         seaicefrac_atm, &
         energycarry_sic_ocn, &
         energycarry_ocn_sic,dtcarry_ocn_sic, &
         albedo_atm,ilandmask1_atm)
  end subroutine fixedseaice_wrapper

  !!
  subroutine slabseaice_wrapper
    implicit none
    call slabseaice( &
         istep_sic, &
         tstar_atm, &
         latent_atm_meansic, &
         sensible_atm_meansic, &
         netsolar_atm_meansic,netlong_atm_meansic, &
         surf_latent_atm, &
         surf_sensible_atm, &
         netsolar_atm,netlong_atm, &
         seaicefrac_atm, &
         temptop_atm, &
         conductflux_atm, &
         albedo_atm,ilandmask1_atm, &
         test_energy_seaice, &
         test_water_seaice,ksic_loop)
  end subroutine slabseaice_wrapper

  !!
  subroutine fixedocean_wrapper
    implicit none
    ! I (djl) should change the arguments to this routine, as not all 
    !  of them are used.
    call fixedocean( &
         istep_ocn, &
         tstar_atm, &
         seaicefrac_atm, &
         energycarry_ocn_sic,dtcarry_ocn_sic, &
         albedo_atm,ilandmask1_atm)
  end subroutine fixedocean_wrapper

  !!
  subroutine slabocean_wrapper
    implicit none
    call slabocean( &
         istep_ocn, &
         tstar_atm,latent_atm_meanocn, &
         sensible_atm_meanocn, &
         netsolar_atm_meanocn,netlong_atm_meanocn, &
         precip_atm_meanocn, &
         evap_atm_meanocn, &
         runoff_atm_meanocn, &
         seaicefrac_atm, &
         temptop_atm, &
         conductflux_atm_meanocn, &
         albedo_atm,ilandmask1_atm, &
         test_energy_ocean, &
         test_water_ocean)
  end subroutine slabocean_wrapper

  !!
  subroutine genie_fixedland_wrapper
    use fixed_land
    implicit none
    call genie_fixedland( &
         atmos_lowestlu_atm,atmos_lowestlv_atm, &
         precip_atm, &
         atmos_lowestlt_atm, &
         atmos_lowestlh_atm, &
         tstar_gb_land,albedo_land,evap_land,fx_le_land,fx_sen_land, &
         fx_momx_land,fx_momy_land,land_runoff_atm,land_tice_ice, &
         land_albice_ice)
  end subroutine genie_fixedland_wrapper

  !!
  subroutine fixedchem_wrapper
    implicit none
    call fixedchem( &
         istep_che, &
         co2_atm,n2o_atm,ch4_atm, &
         iconv_che)
  end subroutine fixedchem_wrapper

  !!
  subroutine ichem_wrapper
   use ichem_main
   implicit none
   call ichem(atmos_dt_tim, &
         co2_atm,mass14co2,massair, &
         alon1_atm,alat1_atm,psigma,ddtmass14co2)
 end subroutine ichem_wrapper

  !!
  subroutine fixedicesheet_wrapper
    implicit none
    call fixedicesheet( &
         istep_lic,ilandmask1_atm,surf_orog_atm, &
         landicealbedo_atm,landicefrac_atm,iconv_ice)
  end subroutine fixedicesheet_wrapper

  !!
#ifdef glimmeron
  subroutine glimmer_wrapper
    use glint_main
    use glimmer_global
    implicit none
    ! Call is different depending on
    ! whether we force with precip or 
    ! not.
    if (flag_glim_pforce) then
       call glint( &
            glimmer_p, &
            istep_lic, &
            surf_tstarinst_atm, &
            max(precip_atm,0.0), &
            surf_orog_atm, &
            output_flag=glim_flag, &
            orog_out=glim_orog, &
            albedo=glim_albedo, &
            ice_frac=glim_icefrac, &
            veg_frac=glim_vegfrac, &
            snowice_frac=glim_snowicefrac, &
            snowveg_frac=glim_snowvegfrac, &
            snow_depth=glim_snowdepth, &
            water_in=glim_waterin, &
            water_out=glim_waterout, &
            ice_tstep=glim_icets)
    else
       call glint( &
            glimmer_p, &
            istep_lic, &
            surf_tstarinst_atm, &
            max(precip_atm+land_evap_atm,0.0), &
            surf_orog_atm, &
            output_flag=glim_flag, &
            orog_out=glim_orog, &
            albedo=glim_albedo, &
            ice_frac=glim_icefrac, &
            veg_frac=glim_vegfrac, &
            snowice_frac=glim_snowicefrac, &
            snowveg_frac=glim_snowvegfrac, &
            snow_depth=glim_snowdepth, &
            water_in=glim_waterin, &
            water_out=glim_waterout, &
            ice_tstep=glim_icets)
    end if
  end subroutine glimmer_wrapper
#endif

  !!
  subroutine surf_ocn_sic_wrapper
    implicit none
    ! Surflux module : GOLDSTEIN-GOLDSEAICE (parentage = c-GOLDSTEIN)
    !
    ! Inputs :  tstar_ocn                ocean surface temperature
    !           sstar_ocn                ocean surface salinity
    !           albedo_ocn               ocean albedo
    !           tstar_atm                surface temperature
    !           surf_qstar_atm           surface specific humidity
    !           surf_pres_atm            surface pressure
    !           surf_hght_atm            surface height (atmosphere)
    !           hght_sic                 sea-ice height
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature
    !           albd_sic                 sea-ice albedo (also an output)
    !           ocean_lowestlu2_ocn      surface wind speed (x) at u point
    !           ocean_lowestlv2_ocn      surface wind speed (y) at u point
    !           ocean_lowestlu3_ocn      surface wind speed (x) at v point
    !           ocean_lowestlv3_ocn      surface wind speed (y) at v point
    !           netsolar_ocnsic          net short-wave to ocean + sea-ice
    !           netlong_ocnsic           net long-wave to ocean + sea-ice
    !           albavg_ocn               average ocean grid cell albedo
    !           rough_ocn                ocean roughness
    !           ocean_stressx2_ocn       surface wind stress (x) at u point
    !           ocean_stressy2_ocn       surface wind stress (y) at u point
    !           ocean_stressx3_ocn       surface wind stress (x) at v point
    !           ocean_stressy3_ocn       surface wind stress (y) at v point
    !           latent_ocn               latent heat flux
    !           sensible_ocn             sensible heat flux
    !           netsolar_ocn             net short-wave heat flux to ocean only
    !           netlong_ocn              net long-wave heat flux to ocean only
    !           evap_ocn                 evaporation
    !           surf_latent_atm          latent heat flux
    !           surf_sensible_atm        sensible heat flux
    !           evap_atm                 evaporation
    !           dhght_sic                change in sea-ice height
    !           dfrac_sic                change in sea-ice fractional cover
    call surf_ocn_sic( &
         istep_gsurf, &               !
         tstar_ocn, sstar_ocn, albedo_ocn, &        ! input
         ocean_lowestlt_ocn, ocean_lowestlq_ocn, &  ! input
         ocean_lowestlp_ocn, ocean_lowestlh_ocn, &  ! input
         hght_sic, frac_sic, temp_sic, albd_sic, &  ! input (temp_sic is input AND output!)
         ocean_lowestlu2_ocn,ocean_lowestlv2_ocn, & ! input
         ocean_lowestlu3_ocn,ocean_lowestlv3_ocn, & ! input
         ocean_atm_netsolar_ocn, &                  ! input
         ocean_atm_netlong_ocn, &                   ! input
         albavg_ocn, &                              ! input
         rough_ocn, &                               ! output
         ocean_stressx2_ocn, ocean_stressy2_ocn, &  ! output
         ocean_stressx3_ocn, ocean_stressy3_ocn, &  ! output
         ocean_latent_ocn,ocean_sensible_ocn, &     ! input/output
         ocean_netsolar_ocn,ocean_netlong_ocn, &    ! output
         ocean_evap_ocn, &                          ! output
         atmos_latent_ocn,atmos_sensible_ocn, &     ! output
         atmos_evap_ocn, &                          ! output
         dhght_sic,dfrac_sic, &                     ! output
         test_energy_seaice, &                      ! input/output
         weight_ocn)                                ! input
  end subroutine surf_ocn_sic_wrapper

  !!
  subroutine surflux_wrapper
    implicit none
    ! Surflux module : GOLDSTEIN-EMBM-GOLDSEAICE (parentage = c-GOLDSTEIN)
    !
    ! Inputs :  tstar_ocn                ocean surface temperature
    !           sstar_ocn                ocean surface salinity
    !           tstar_atm                surface temperature
    !           surf_qstar_atm           surface specific humidity
    !           hght_sic                 sea-ice height
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature
    !           albd_sic                 sea-ice albedo
    !           ocean_stressx2_ocn       surface wind stress (x) at u point
    !           ocean_stressy2_ocn       surface wind stress (y) at u point
    !           ocean_stressx3_ocn       surface wind stress (x) at v point
    !           ocean_stressy3_ocn       surface wind stress (y) at v point
    ! Outputs : albedo_ocn               ocean albedo (excl. sea-ice)
    !           latent_ocn               latent heat flux
    !           sensible_ocn             sensible heat flux
    !           netsolar_ocn             net short-wave heat flux
    !           netlong_ocn              net long-wave heat flux
    !           evap_ocn                 evaporation
    !           precip_ocn               precipitation
    !           runoff_ocn               runoff
    !           surf_latent_atm          latent heat flux
    !           surf_sensible_atm        sensible heat flux
    !           netsolar_atm             net short-wave heat flux
    !           netlong_atm              net long-wave heat flux
    !           evap_atm                 evaporation
    !           precip_atm               precipitation (not used normally)
    !           dhght_sic                change in sea-ice height
    !           dfrac_sic                change in sea-ice fractional cover
    !           atmos_lowestlh_atm       Height of lowest atmos level (m)
    !           atmos_lowestlu2_atm      zonal component of wind speed
    !           atmos_lowestlv3_atm      meridional component of wind speed
    !
    call surflux( &
         istep_ocn, &                             !
         tstar_ocn, sstar_ocn, &                  ! input
         tstar_atm, surf_qstar_atm, &             ! input
         hght_sic,frac_sic,temp_sic,albd_sic, &   ! input
         ocean_stressx2_ocn,ocean_stressy2_ocn, & ! input
         ocean_stressx3_ocn,ocean_stressy3_ocn, & ! input
         albedo_ocn,latent_ocn,sensible_ocn, &    ! output
         netsolar_ocn,netlong_ocn, &              ! output
         evap_ocn,precip_ocn,runoff_ocn, &        ! output
         surf_latent_atm,surf_sensible_atm, &     ! output
         netsolar_atm,netlong_atm, &              ! output
         evap_atm,precip_atm, &                   ! output
         dhght_sic,dfrac_sic, &                   ! output
         atmos_lowestlh_atm, &                    ! output
         go_solfor,go_fxsw, &                     ! output (to BIOGEM)
         intrac_atm_max, &                        ! input
         genie_sfcatm1, &                         ! input (from ATCHEM)
         eb_ca,global_daysperyear, &
         eb_fx0a,eb_fx0o,eb_fxsen,eb_fxlw, &
         eb_evap,eb_pptn,eb_relh, &
         genie_solar_constant, &
         co2_atm,ch4_atm,n2o_atm, &               ! output
         surf_orog_atm, &                         ! in/output
         landice_slicemask_lic, &                 ! in/output
         albs_atm, &                              ! in/output
         land_albs_snow_lnd, &                    ! input
         land_albs_nosnow_lnd, &                  ! input
         land_snow_lnd, &                         ! in/output
         land_bcap_lnd, &                         ! in/output
         land_z0_lnd, &                           ! in/output
         land_temp_lnd, &                         ! in/output
         land_moisture_lnd, &                     ! in/output
         flag_ents, &                             ! input
         atmos_lowestlu2_atm, &                   ! in/output
         atmos_lowestlv3_atm &                    ! in/output
         )
    !
  end subroutine surflux_wrapper

  !!
  subroutine plasim_surflux_wrapper
    implicit none
    call surflux_goldstein_seaice(              &
!input
         hght_sic, frac_sic,                    &
         tstar_ocn,sstar_ocn,                   &
         surft_atm_sic,surfq_atm_sic,           &
         surfp_atm_sic,                         &
         insolar_sic,inlong_sic,                &
         netheat_sic,                           &
         surf_windspeed_sic,                    &
         evap_ocn,                              &
         latent_coeff_atm,                      &
         sensible_coeff_atm,                    &
!input and output
         latent_ocn,sensible_ocn,               &
         netsolar_ocn,netlong_ocn,              &
!output
         dhght_sic,dfrac_sic,temp_sic,albd_sic, &
         delta_flux                             &
     )
    !
  end subroutine plasim_surflux_wrapper

  !!
  subroutine embm_wrapper
    implicit none
    ! Atmosphere module : EMBM (parentage = c-GOLDSTEIN)
    !
    ! Inputs  : surf_latent_atm          latent heat flux
    !           surf_sensible_atm        sensible heat flux
    !           surf_netsolar_atm        net short-wave heat flux
    !           surf_netlong_atm         net long-wave heat flux
    !           evap_atm                 evaporation
    !           precip_atm               precipitation (not used)
    ! Outputs : ocean_stressx2_ocn       surface wind stress (x) at u point
    !           ocean_stressy2_ocn       surface wind stress (y) at u point
    !           ocean_stressx3_ocn       surface wind stress (x) at v point
    !           ocean_stressy3_ocn       surface wind stress (y) at v point
    !           tatar_atm                surface temperature
    !           surf_qstar_atm           surface specific humidity
    !           atmos_lowestlu2_atm      zonal component of wind speed
    !           atmos_lowestlv3_atm      meridional component of wind speed
    call embm( &
         istep_atm, &                             ! 
         surf_latent_atm,surf_sensible_atm, &     ! input
         netsolar_atm,netlong_atm, &              ! input
         evap_atm,precip_atm, &                   ! input
         ocean_stressx2_ocn,ocean_stressy2_ocn, & ! output
         ocean_stressx3_ocn,ocean_stressy3_ocn, & ! output
         tstar_atm,surf_qstar_atm, &              ! output
         koverall, &                              !
         torog_atm, &                             ! output
         surf_orog_atm,&                          ! input
         flag_ents, &                             ! input
         atmos_lowestlu2_atm, &                   ! in/output
         atmos_lowestlv3_atm &                    ! in/output
         )
  end subroutine embm_wrapper

  !!
  subroutine plasim_wrapper
    implicit none
     call master(istep_atm,                                &
!input
       tstar_ocn,                                          &
       temp_sic,hght_sic,frac_sic,albd_sic,                &
       delta_flux,                                         &
       genie_sfcatm_lnd(3,1,1),                            & !co2 from atchem
!output
       surf_latent_atm,surf_latent_coeff_atm,              &
       surf_sensible_atm,surf_sensible_coeff_atm,          &
       netsolar_atm,netlong_atm,                           &
       insolar_atm,inlong_atm,netheat_atm,                 &
       surft_atm,surfq_atm,surfp_atm,                      &
       evap_atm,precip_atm,land_runoff_atm,                &
       surf_stressx2_atm,surf_stressy2_atm,                &
       surf_stressx3_atm,surf_stressy3_atm,                &
       surf_windspeed_atm,solfor_atm                       &
     )
     !
  end subroutine plasim_wrapper

  !!
  subroutine radfor_wrapper
    implicit none
    ! 
    call radfor(istep_atm, global_daysperyear, &
         genie_solar_constant &
         & )                                      ! input
  end subroutine radfor_wrapper

  !!
  subroutine gold_seaice_wrapper
    implicit none
    ! Sea-ice module : GOLDSTEIN sea-ice (parentage = c-GOLDSTEIN)
    !
    ! Inputs  : dhght_sic                change in sea-ice height
    !           dfrac_sic                change in sea-ice fractional cover
    !           ustar_ocn                surface ocean velocity (u)
    !           vstar_ocn                surface ocean velocity (v)
    ! Outputs : hght_sic                 sea-ice height
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature } technically
    !           albd_sic                 sea-ice albedo              } inputs
    !           waterflux_ocn            freshwater flux to ocean (melting)
    !           conductflux_ocn          heat flux to ocean (melting)
    !
    call gold_seaice( &
         istep_sic, &                            !
         dhght_sic,dfrac_sic, &                  ! input
         ustar_ocn,vstar_ocn, &                  ! input
         hght_sic,frac_sic,temp_sic,albd_sic, &  ! output
         waterflux_ocn,conductflux_ocn, &        ! output
         test_energy_seaice,test_water_seaice, & ! output
         koverall)                               !
  end subroutine gold_seaice_wrapper

  !!
  subroutine goldstein_wrapper
    ! Ocean module : GOLDSTEIN (parentage = c-GOLDSTEIN)
    ! Inputs  : latent_ocn               latent heat flux
    !           sensible_ocn             sensible heat flux
    !           netsolar_ocn             net short-wave heat flux
    !           netlong_ocn              net long-wave heat flux
    !           conductflux_ocn          heat flux to ocean (sea-ice melting)
    !           evap_ocn                 evaporation
    !           precip_ocn               precipitation
    !           runoff_ocn               runoff
    !           waterflux_ocn            freshwater flux to ocean (sea-ice melting)
    !           stressx_ocn              surface wind stress (x) on ocean
    !           stressy_ocn              surface wind stress (y) on ocean
    !           frac_sic                 sea-ice fractional cover
    !           temp_sic                 sea-ice surface temperature
    ! Outputs : tstar_ocn                ocean surface temperature
    !           sstar_ocn                ocean surface salinity
    !           ustar_ocn                surface ocean velocity (u)
    !           vstar_ocn                surface ocean velocity (v)
    !
    implicit none
    call goldstein( &
         istep_ocn, &                              !
         latent_ocn,sensible_ocn, &                ! input
         netsolar_ocn,netlong_ocn, &               ! input
         conductflux_ocn, &                        ! input
         evap_ocn,precip_ocn,runoff_ocn, &         ! input
         waterflux_ocn, &                          ! input
         ocean_stressx2_ocn,ocean_stressy2_ocn, &  ! input
         ocean_stressx3_ocn,ocean_stressy3_ocn, &  ! input
         tstar_ocn,sstar_ocn, &                    ! output
         ustar_ocn,vstar_ocn, &                    ! output
         albedo_ocn, &                             ! output
         test_energy_ocean,test_water_ocean, &     ! output
         koverall, &                               ! input
         go_ts,go_ts1, &                           ! output (to BIOGEM)
         go_cost,go_u,go_tau, &                     ! output (to BIOGEM)
!ccSR
         go_mldta, &                           ! output (to BIOGEM)
!ccSR
         go_rho)                                   ! output (to ENTS)
  end subroutine goldstein_wrapper

  !!
  subroutine write_averages_wrapper
    ! The seaice variables (3rd array for each variable-type)
    ! are yet to be sorted out.
    implicit none
    call write_averages( &
    istep_ocn,alon1_atm,alat1_atm, &
    alon1_ocn,alat1_ocn, &
    alon1_sic,alat1_sic, &
    netsolar_atm_meanocn,netsolar_ocn,netsolar_sic, &
    netlong_atm_meanocn,netlong_ocn,netlong_sic, &
    sensible_atm_meanocn,sensible_ocn,sensible_sic, &
    latent_atm_meanocn,latent_ocn,latent_sic, &
    stressx_atm_meanocn,ocean_stressx2_ocn,stressx_sic, &
    stressy_atm_meanocn,ocean_stressy2_ocn,stressy_sic, &
    conductflux_atm_meanocn,conductflux_ocn,conductflux_sic, &
    evap_atm_meanocn,evap_ocn,evap_sic, &
    precip_atm_meanocn,precip_ocn,precip_sic, &
    runoff_atm_meanocn,runoff_ocn,runoff_sic, &
    waterflux_atm_meanocn,waterflux_ocn,waterflux_sic, &
    seaicefrac_atm_meanocn,seaicefrac_ocn,seaicefrac_sic, &
    tstar_atm,tstar_ocn,tstar_sic, &
    albedo_atm,albedo_ocn,albedo_sic)
  end subroutine write_averages_wrapper

  !!
  subroutine ents_wrapper
    implicit none
    call ents(istep_ocn,go_nyear, &
         torog_atm, &
         co2_atm, &
         go_rh0sc,go_rhosc,go_rsc,go_ds,go_dphi, &
         go_dsc,go_saln0,go_dz,go_ec,go_rho, &
         eb_fx0a,eb_fx0o,eb_fxsen,eb_fxlw, &
         eb_evap,eb_pptn,eb_relh,go_istep0, &
           el_photo,el_respveg,el_respsoil,el_leaf, & ! GHC - added these for use with rokgem
         landice_slicemask_lic, &
         albs_atm, &
         land_albs_snow_lnd, &
         land_albs_nosnow_lnd, &
         land_snow_lnd, &
         land_bcap_lnd, &                         ! output
         land_z0_lnd, &                           ! output
         land_temp_lnd, &                         ! input
         land_moisture_lnd, &                     ! input
         intrac_atm_max, &
         genie_sfcatm_lnd, &
         genie_sfxatm_lnd &
         )
  end subroutine ents_wrapper

  !!
  subroutine biogem_wrapper
    implicit none
    call biogem(                                             &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & go_ts,                                            & ! input/output
         & go_ts1,                                           & ! input/output
         & genie_sfcatm1,                                    & ! input/output
         & genie_sfxatm1,                                    & ! input/output
         & genie_sfcocn1,                                    & ! input/output
         & genie_sfxocn1,                                    & ! input/output
         & genie_sfcsed1,                                    & ! input/output
         & genie_sfxsed1,                                    & ! input/output
         & genie_sfxsumrok1                                  & ! input/output
         )
  end subroutine biogem_wrapper

  !!
  subroutine biogem_climate_wrapper
    implicit none
    call biogem_climate(                                     &
         & hght_sic,                                         & ! input
         & frac_sic,                                         & ! input
         & go_cost,                                          & ! input
         & go_solfor,                                        & ! input
         & go_fxsw,                                          & ! input
         & go_u,                                             & ! input
         & go_tau,                                           & ! input
         & go_mldta,                                         & ! input
         & genie_solar_constant                              & ! input/output
         )
  end subroutine biogem_climate_wrapper

  !!
  subroutine atchem_wrapper
    implicit none
    call atchem(                                             &
         & real(conv_kocn_katchem*kocn_loop)*genie_timestep, & ! input
         & genie_sfxsumatm,                                  & ! input/output
         & genie_sfcatm                                      & ! input/output
         & )
  end subroutine atchem_wrapper
  
  !!
  subroutine cpl_flux_ocnatm_wrapper
    implicit none
    call cpl_flux_ocnatm(                                    &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & intrac_atm_max,                                   & ! input
         & ilon1_atm,ilat1_atm,                              & ! input
         & ilon1_ocn,ilat1_ocn,                              & ! input
         & genie_sfxatm1,                                    & ! input/output
         & genie_sfxsumatm                                   & ! input/output
         & )
  end subroutine cpl_flux_ocnatm_wrapper
  
  !!
  subroutine cpl_flux_lndatm_wrapper
    implicit none
    call cpl_flux_lndatm(                                    &
         & real(klnd_loop)*genie_timestep,                   & ! input
         & intrac_atm_max,                                   & ! input
         & ilon1_atm,ilat1_atm,                              & ! input
         & ilon1_lnd,ilat1_lnd,                              & ! input
         & genie_sfxatm_lnd,                                 & ! input/output
         & genie_sfxsumatm                                   & ! input/output
         & )
  end subroutine cpl_flux_lndatm_wrapper
  
  !!
  subroutine cpl_comp_atmocn_wrapper
    implicit none
    call cpl_comp_atmocn(       &
         & intrac_atm_max,      & ! input
         & ilon1_atm,ilat1_atm, & ! input
         & ilon1_ocn,ilat1_ocn, & ! input
         & genie_sfcatm,        & ! input
         & genie_sfcatm1        & ! input/output
         & )
  end subroutine cpl_comp_atmocn_wrapper
  
  !!
  subroutine cpl_comp_EMBM_wrapper
    implicit none
    call cpl_comp_EMBM(         &
         & intrac_atm_max,      & ! input
         & ilon1_atm,ilat1_atm, & ! input
         & ilon1_ocn,ilat1_ocn, & ! input
         & tstar_atm,           & ! input (surface temperature)
         & surf_qstar_atm,      & ! input (surface specific humidity)
         & genie_sfcatm1        & ! input/output
         & )
  end subroutine cpl_comp_EMBM_wrapper

  !!
  subroutine cpl_comp_atmlnd_wrapper
    implicit none
    call cpl_comp_atmlnd(       &
         & intrac_atm_max,      & ! input
         & ilon1_atm,ilat1_atm, & ! input
         & ilon1_lnd,ilat1_lnd, & ! input
         & genie_sfcatm,        & ! input
         & genie_sfcatm_lnd     & ! input/output
         & )
  end subroutine cpl_comp_atmlnd_wrapper
  
  !!
  subroutine cpl_comp_lndEMBM_wrapper
    implicit none
    call cpl_comp_EMBM(         &
         & intrac_atm_max,      & ! input
         & ilon1_atm,ilat1_atm, & ! input
         & ilon1_lnd,ilat1_lnd, & ! input
         & tstar_atm,           & ! input (surface temperature)
         & surf_qstar_atm,      & ! input (surface specific humidity)
         & genie_sfcatm_lnd     & ! input/output
         & )
  end subroutine cpl_comp_lndEMBM_wrapper

  !!
  subroutine sedgem_wrapper
    implicit none
    call sedgem(                                             &
         & real(conv_kocn_ksedgem*kocn_loop)*genie_timestep, & ! input
         & genie_sfxsumsed,                                  &
         & genie_sfcsumocn,                                  &
         & genie_sfcsed,                                     &
         & genie_sfxocn                                      &
         & )
  end subroutine sedgem_wrapper
  
  !!
  subroutine cpl_flux_ocnsed_wrapper
    implicit none
    call cpl_flux_ocnsed(                                    &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & intrac_sed_max,                                   & ! input
         & ilon1_ocn,ilat1_ocn,                              & ! input
         & ilon1_sed,ilat1_sed,                              & ! input
         & genie_sfxsed1,                                    & ! input/output
         & genie_sfxsumsed                                   & ! input/output
         & )
  end subroutine cpl_flux_ocnsed_wrapper
  
  !!
  subroutine cpl_flux_sedocn_wrapper
    implicit none
    call cpl_flux_sedocn(       &
         & intrac_ocn_max,      & ! input
         & ilon1_ocn,ilat1_ocn, & ! input
         & ilon1_sed,ilat1_sed, & ! input
         & genie_sfxocn1,       & ! input/output
         & genie_sfxocn         & ! output
         & )
  end subroutine cpl_flux_sedocn_wrapper
  
  !!
  subroutine cpl_comp_ocnsed_wrapper
    implicit none
    call cpl_comp_ocnsed(                       &
         & int(koverall/kocn_loop),             &
         & conv_kocn_kbiogem,conv_kocn_ksedgem, &
         & intrac_ocn_max,                      & ! input
         & ilon1_ocn,ilat1_ocn,                 & ! input
         & ilon1_sed,ilat1_sed,                 & ! input
         & genie_sfcocn1,                       & ! input
         & genie_sfcsumocn                      & ! input/output
         & )
  end subroutine cpl_comp_ocnsed_wrapper
  
  !!
  subroutine cpl_comp_sedocn_wrapper
    implicit none
    call cpl_comp_sedocn(       &
         & intrac_sed_max,      & ! input
         & ilon1_ocn,ilat1_ocn, & ! input
         & ilon1_sed,ilat1_sed, & ! input
         & genie_sfcsed1,       & ! input
         & genie_sfcsed         & ! input/output
         & )
  end subroutine cpl_comp_sedocn_wrapper
  
  !!
  subroutine rokgem_wrapper
    implicit none
     call rokgem(               &
           & genie_sfcatm1,       & ! input
           & precip_atm,      & ! input
             & el_photo,        & ! input
             & el_respveg,      & ! input
           & genie_sfxrok,    & ! input/output
           & genie_sfxatm1        & ! input/output
           & )
  end subroutine rokgem_wrapper

  subroutine cpl_flux_rokatm_wrapper
    implicit none
    call cpl_flux_rokatm(       &
           & real(conv_kocn_krokgem*kocn_loop)*genie_timestep,&
         & intrac_atm_max,      & ! input
         & ilon1_rok,ilat1_rok, & ! input
         & ilon1_atm,ilat1_atm, & ! input
         & genie_sfxatm1,         & ! input
         & genie_sfxsumatm     & ! input/output
         & )
  end subroutine cpl_flux_rokatm_wrapper

  !!
  subroutine cpl_flux_rokocn_wrapper
    implicit none
    call cpl_flux_rokocn(       &
           & real(conv_kocn_krokgem*kocn_loop)*genie_timestep,&
         & intrac_ocn_max,      & ! input
         & ilon1_rok,ilat1_rok, & ! input
         & ilon1_ocn,ilat1_ocn, & ! input
         & genie_sfxrok,        & ! input
         & genie_sfxsumrok1     & ! input/output
         & )
  end subroutine cpl_flux_rokocn_wrapper
  
  !!
  subroutine cpl_comp_rokEMBM_wrapper
!    call cpl_comp_rokEMBM(      &
!         & )
  end subroutine cpl_comp_rokEMBM_wrapper
  
  !!
  subroutine biogem_restart_wrapper
    implicit none
    call rest_biogem()
  end subroutine biogem_restart_wrapper
 
  !!
  subroutine atchem_restart_wrapper
    implicit none
    call rest_atchem()
  end subroutine atchem_restart_wrapper
 
  !!
  subroutine sedgem_restart_wrapper
    implicit none
    call rest_sedgem()
  end subroutine sedgem_restart_wrapper

  !!
  subroutine rokgem_restart_wrapper
    implicit none
    call rest_rokgem()
  end subroutine rokgem_restart_wrapper

  !!
  subroutine diag_biogem_wrapper
    implicit none
    call diag_biogem(    &
         & genie_clock,  & ! input
         & genie_sfcatm1 & ! input
         )
  end subroutine diag_biogem_wrapper

  !!
  subroutine diag_biogem_timeslice_wrapper
    implicit none
    call diag_biogem_timeslice(                              &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & genie_sfcatm1,                                    & ! input
         & genie_sfxatm1,                                    & ! input
         & genie_sfxocn1,                                    & ! input
         & genie_sfcsed1,                                    & ! input
         & genie_sfxsed1                                     & ! input
         )
  end subroutine diag_biogem_timeslice_wrapper

  !!
  subroutine diag_biogem_timeseries_wrapper
    implicit none
    call diag_biogem_timeseries(                             &
         & real(conv_kocn_kbiogem*kocn_loop)*genie_timestep, & ! input
         & genie_clock,                                      & ! input
         & genie_sfcatm1,                                    & ! input
         & genie_sfxatm1,                                    & ! input
         & genie_sfxocn1,                                    & ! input
         & genie_sfcsed1,                                    & ! input
         & genie_sfxsed1                                     & ! input
         )
  end subroutine diag_biogem_timeseries_wrapper

  !!
  subroutine genie_restarts_wrapper(restart_type)
    implicit none
    integer restart_type
    call genie_restarts(koverall, &
         lrestart_genie, &
         alon1_atm,alat1_atm, &
         ocean_sensible_atm, &
         ocean_latent_atm, &
         ocean_stressx_atm, &
         ocean_stressy_atm, &
         ocean_sensibleinst_atm, &
         ocean_stressxinst_atm, &
         ocean_stressyinst_atm, &
         ocean_evapinst_atm, &
         ocean_latentinst_atm, &
         ocean_tstarinst_atm, &
         ocean_rough_atm, &
         ocean_qstar_atm, &
         ocean_salb_atm, &
         tstar_atm, &
         albedo_atm, &
         restart_type)
  end subroutine genie_restarts_wrapper

  subroutine wind_wrapper
    implicit none
  end subroutine wind_wrapper

END MODULE genie_loop_wrappers
