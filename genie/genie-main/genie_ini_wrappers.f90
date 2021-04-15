

MODULE genie_ini_wrappers

  ! ===========================================================
  ! This module contains wrapper subroutines to hide arg lists
  !             __Initialisation Routines Only__
  ! ===========================================================

  use genie_global

contains

  !!
  subroutine ini_fixedicesheet_wrapper
    use initialise_fixedicesheet_mod
    implicit none
    call initialise_fixedicesheet( &
         ilandmask1_atm, &
         surf_orog_atm, &
         landicealbedo_atm, &
         landicefrac_atm)
  end subroutine ini_fixedicesheet_wrapper

  !!
  subroutine initialise_fixedchem_wrapper
    implicit none
    call initialise_fixedchem( &
         co2_atm, &
         n2o_atm, &
         ch4_atm)
  end subroutine initialise_fixedchem_wrapper

  !!
  subroutine initialise_fixedatmos_wrapper
    use fixed_atmosphere
    implicit none
    ! Note that 13 in this call is the file unit to use.
    ! For the 1st timestep, we don't know weighttot yet, so set it =1.0
    weighttot_atm=1.0
    call initialise_fixedatmos(fixedatmos_file,13, &
         alon1_atm,alat1_atm, &
         aboxedge1_lon_atm,aboxedge1_lat_atm, &
         ilandmask1_atm, &
         atmos_dt_tim, &
         tstar_atm, &
         precip_atm, &
         surf_latent_atm, &
         surf_sensible_atm, &
         netsolar_atm, &
         netlong_atm, &
         surf_stressx_atm, &
         surf_stressy_atm, &
         land_runoff_atm, &
         evap_atm, &
         conductflux_atm, &
         waterflux_atm, &
         atmos_lowestlq_atm,atmos_lowestlu_atm, &
         atmos_lowestlv_atm,atmos_lowestlp_atm, &
         atmos_lowestlh_atm, &
         surf_orog_atm, &
         interpmask_atm, &
         weighttot_atm,grid_type_fixedatmos)
  end subroutine initialise_fixedatmos_wrapper

  !!
  subroutine initialise_fakeatmos_wrapper
    use fake_atmosphere
    implicit none

    call initialise_fakeatmos(ocean_stressx2_ocn, &
         ocean_stressy2_ocn, &
         ocean_stressx3_ocn, &
         ocean_stressy3_ocn, &
         latent_ocn, &
         sensible_ocn, &
         netsolar_ocn, &
         netlong_ocn, &
         precip_ocn, &
         evap_ocn, &
         runoff_ocn, &
         conductflux_ocn, &
         waterflux_ocn, &
         trest_ocn, &
         srest_ocn)

  end subroutine initialise_fakeatmos_wrapper

  !!
  subroutine initialise_fixedseaice_wrapper
    implicit none
    call initialise_fixedseaice( &
         tstar_atm,albedo_atm, &
         seaicefrac_atm, &
         ilandmask1_atm)
  end subroutine initialise_fixedseaice_wrapper

  !!
  subroutine initialise_slabseaice_wrapper
    implicit none
    call initialise_slabseaice( &
         tstar_atm,albedo_atm, &
         seaicefrac_atm,conductflux_atm, &
         ilandmask1_atm, &
         test_energy_seaice, &
         test_water_seaice,ksic_loop)
  end subroutine initialise_slabseaice_wrapper

  !!
  subroutine initialise_fixedocean_wrapper
    implicit none
    call initialise_fixedocean( &
         tstar_atm,albedo_atm, &
         seaicefrac_atm,ilandmask1_atm)
  end subroutine initialise_fixedocean_wrapper

  !!
  subroutine initialise_slabocean_wrapper
    implicit none
    call initialise_slabocean( &
         tstar_atm,albedo_atm, &
         seaicefrac_atm,temptop_atm, &
         ilandmask1_atm, &
         test_energy_ocean, &
         test_water_ocean)
  end subroutine initialise_slabocean_wrapper

  !! Goldstein ocean has some question marks...

  !!
  subroutine initialise_embm_wrapper(reinit)
    implicit none
    logical,optional::reinit
    logical::reinit_
    reinit_=.false.
    if (present(reinit)) reinit_=reinit
    call initialise_embm( &
         alon1_atm,alat1_atm, &
         alon2_atm,alat2_atm,alon3_atm,alat3_atm, &
         aboxedge1_lon_atm,aboxedge1_lat_atm, &
         aboxedge2_lon_atm,aboxedge2_lat_atm, &
         aboxedge3_lon_atm,aboxedge3_lat_atm, &
         ilandmask1_atm,ilandmask2_atm,ilandmask3_atm, &
         ias_out,iaf_out,ips_out,ipf_out,jsf_out, &
         tstar_ocn, &
         koverall_total, &
         co2_atm,ch4_atm,n2o_atm, &
         ocean_stressx2_ocn,ocean_stressy2_ocn, &
         ocean_stressx3_ocn,ocean_stressy3_ocn, &
         tstar_atm,surf_qstar_atm,atmos_dt_tim, &
         genie_solar_constant, &
         eb_rmax,eb_dphi,eb_rdtdim,eb_ca, &
         global_daysperyear, &
         torog_atm, &
         surf_orog_atm, &
         landice_slicemask_lic, &
         go_syr, &
         flag_ents, &
         atmos_lowestlu2_atm, &
         atmos_lowestlv3_atm, &
         flag_wind, &
         reinit_ &
         )
  end subroutine initialise_embm_wrapper

  !!
  subroutine initialise_plasim_wrapper
    implicit none
    call mpstart
    call prolog(go_nyear,kocn_loop,genie_sfcatm_lnd(3,1,1))
    call ini_steps(tstar_ocn,temp_sic,hght_sic,frac_sic,albd_sic)
!    call initialise_plasim( &
!         alon1_atm,alat1_atm, &
!         alon2_atm,alat2_atm,alon3_atm,alat3_atm, &
!         aboxedge1_lon_atm,aboxedge1_lat_atm, &
!         aboxedge2_lon_atm,aboxedge2_lat_atm, &
!         aboxedge3_lon_atm,aboxedge3_lat_atm, &
!         ilandmask1_atm,ilandmask2_atm,ilandmask3_atm, &
!         ias_out,iaf_out,ips_out,ipf_out,jsf_out, &
!         tstar_ocn, &
!         koverall_total, &
!         co2_atm,ch4_atm,n2o_atm, &
!         ocean_stressx2_ocn,ocean_stressy2_ocn, &
!         ocean_stressx3_ocn,ocean_stressy3_ocn, &
!         tstar_atm,surf_qstar_atm,atmos_dt_tim, &
!         genie_solar_constant, &
!         eb_rmax,eb_dphi,eb_rdtdim,eb_ca, &
!         global_daysperyear, &
!         torog_atm, &
!         surf_orog_atm, &
!         landice_slicemask_lic, &
!         go_syr, &
!         flag_ents, &
!         atmos_lowestlu2_atm, &
!         atmos_lowestlv3_atm, &
!         flag_wind &
!         )
  end subroutine initialise_plasim_wrapper

  !!
  subroutine ini_goldsteinseaice_wrapper(reinit)
    implicit none
    logical,optional::reinit
    logical::reinit_
    reinit_=.false.
    if (present(reinit)) reinit_=reinit
    call initialise_seaice( &
         alon1_sic,alat1_sic, &
         alon2_sic,alat2_sic,alon3_sic,alat3_sic, &
         aboxedge1_lon_sic,aboxedge1_lat_sic, &
         aboxedge2_lon_sic,aboxedge2_lat_sic, &
         aboxedge3_lon_sic,aboxedge3_lat_sic, &
         ilandmask1_sic,ilandmask2_sic, &
         ilandmask3_sic, &
         koverall_total, &
         hght_sic,frac_sic,temp_sic,albd_sic, &
         test_energy_seaice, &
         reinit_ &
         )
  end subroutine ini_goldsteinseaice_wrapper

  !!
#ifdef USE_mosestriffid
  subroutine initialise_land_wrapper
    implicit none
    call initialise_land( &
         ilandmask1_atm, &
         atmos_dt_tim,klnd_loop, &
         alon1_atm,alat1_atm,ilon1_atm,ilat1_atm, &
         aboxedge1_lon_atm,aboxedge1_lat_atm)
  end subroutine initialise_land_wrapper
#endif

  !!
  subroutine initialise_fixedland_wrapper
    use fixed_land
    implicit none
    call initialise_fixedland( &
         ilon1_atm, &
         ilat1_atm, &
         ilandmask1_atm)
  end subroutine initialise_fixedland_wrapper

  !! Only compile if asked for
#ifdef glimmeron
  subroutine initialise_glimmer_wrapper
    use glint_main
    use glimmer_log
    use glimmer_vers
    implicit none
    call open_log(unit=101)  
    ! Calculate calling timestep
    glim_timestep=nint(atmos_dt_tim/3600.0)*kicesheet_loop
    ! Check we have the right version
    if (glimmer_v_number<10002) then
       print*,'ERROR: Using incompatible version of Glimmer'
       print*,'       Must upgrade to v.1.0.2 or later'
       stop
    end if
    ! Initialise model
    call initialise_glint( &
         glimmer_p, &
         alat1_atm, &
         alon1_atm, &
         glim_timestep, &
         (/glimmer_file/), &
         orog=surf_orog_atm, &
         ice_frac=glim_icefrac, &
         snow_depth=glim_snowdepth, &
         snow_model=glim_snow_model)
! For coupling with igcm:
!         orog_lats=hrlats_atm, &
!         orog_longs=hrlons_atm, &
!         orog_latb=hrlatsedge_atm, &
!         orog_lonb=hrlonsedge_atm)
  end subroutine initialise_glimmer_wrapper
#endif

  !!
  subroutine initialise_goldocean_wrapper(reinit)
    implicit none
    logical,optional::reinit
    logical::reinit_
    reinit_=.false.
    if (present(reinit)) reinit_=reinit
    call initialise_goldstein( &
         alon1_ocn,alat1_ocn, &
         alon2_ocn,alat2_ocn,alon3_ocn,alat3_ocn, &
         aboxedge1_lon_ocn,aboxedge1_lat_ocn, &
         aboxedge2_lon_ocn,aboxedge2_lat_ocn, &
         aboxedge3_lon_ocn,aboxedge3_lat_ocn, &
         depth1_ocn,depth2_ocn, &
         ilandmask1_ocn,ilandmask2_ocn, &
         ilandmask3_ocn,koverall_total, &
         tstar_ocn,sstar_ocn,ustar_ocn,vstar_ocn,albedo_ocn, &
         ias_out,iaf_out,ips_out,ipf_out,jsf_out, &
         lrestart_genie, & 
         go_saln0,go_rhoair,go_cd,go_ds,go_dphi,go_ips,go_ipf, &
         go_usc,go_dsc,go_fsc,go_rh0sc, &
         go_rhosc,go_cpsc,go_scf, &
         go_k1,go_dz,go_dza, &
         go_ias,go_iaf,go_jsf,go_c,go_cv,go_s,go_sv, &
         go_ts,go_ts1, &
         go_rsc,go_syr,go_nyear,go_lin,go_ec,go_istep0, &
         reinit_ &
         )
  end subroutine initialise_goldocean_wrapper

  !!
!  subroutine ini_weights_wrapper
!    implicit none
!    call ini_weights( &
!         aboxedge1_lon_atm,aboxedge1_lat_atm, &
!         aboxedge1_lon_ocn,aboxedge1_lat_ocn, &
!         ilandmask1_atm,ilandmask1_ocn, &
!         interpmask_atm,weighttot_atm, &
!         interpmask_ocn,weighttot_ocn)
!  end subroutine ini_weights_wrapper

  !!
  subroutine initialise_gem_wrapper
    implicit none
    call initialise_gem ()
  end subroutine initialise_gem_wrapper

  !!
  subroutine initialise_biogem_wrapper(reinit)
    implicit none
    logical,optional::reinit
    logical::reinit_
    reinit_=.false.
    if (present(reinit)) reinit_=reinit
    call initialise_biogem(                                                                        &
         & go_saln0,go_rhoair,go_cd,go_ds,go_dphi,                                                 &
         & go_usc,go_dsc,go_fsc,go_rh0sc,                                                          &
         & go_rhosc,go_cpsc,genie_solar_constant,go_scf,                                           &
         & go_ips(1:ilat1_ocn),go_ipf(1:ilat1_ocn),go_ias(1:ilat1_ocn),go_iaf(1:ilat1_ocn),go_jsf, &
         & go_k1(1:ilon1_ocn,1:ilat1_ocn),                                                         &
         & go_dz(1:inl1_ocn),go_dza(1:inl1_ocn),                                                   &
         & go_c(0:ilat1_ocn),go_cv(0:ilat1_ocn),go_s(0:ilat1_ocn),go_sv(0:ilat1_ocn),              &
         & go_ts(1:intrac_ocn,1:ilon1_ocn,1:ilat1_ocn,1:inl1_ocn),                                 &
         & go_ts1(1:intrac_ocn,1:ilon1_ocn,1:ilat1_ocn,1:inl1_ocn),                                &
         & genie_sfcatm1,                                                                          &
         & genie_sfxatm1,                                                                          &
         & genie_sfcocn1,                                                                          &
         & genie_sfxocn1,                                                                          &
         & genie_sfcsed1,                                                                          &
         & genie_sfxsed1,                                                                          &
         & reinit_                                                                                 &
         & )
  end subroutine initialise_biogem_wrapper

  !!
  subroutine initialise_atchem_wrapper
    implicit none
    call initialise_atchem ( &
         & genie_sfxsumatm,  &
         & genie_sfcatm      &
         & )
   end subroutine initialise_atchem_wrapper

  !!
  subroutine initialise_sedgem_wrapper
    implicit none
    call initialise_sedgem ( &
         & genie_timestep,   & 
         & genie_sfxsumsed,  &
         & genie_sfcsumocn,  &
         & genie_sfcsed,     &
         & genie_sfxocn      &
         & )
  end subroutine initialise_sedgem_wrapper

  !! SG Initialising ENTS module
  subroutine initialise_ents_wrapper
    implicit none
    call initialise_ents( &
         go_lin,go_rsc,go_syr,go_nyear, &
         go_ds,go_dphi,inl1_ocn, &
         go_k1(1:ilon1_ocn,1:ilat1_ocn), &
         eb_rmax,eb_rdtdim, &
         tstar_atm,surf_qstar_atm,eb_ca,co2_atm, &
!	 global_daysperyear, &
         global_daysperyear,alat1_ocn, &
         landice_slicemask_lic, &
         albs_atm, &
         land_albs_snow_lnd, &
         land_albs_nosnow_lnd, &
         land_snow_lnd, &
         land_bcap_lnd, &
         land_z0_lnd, &
         land_temp_lnd, &                         ! output
         land_moisture_lnd, &                     ! output
         intrac_atm_max, &
         genie_sfcatm_lnd, &
         genie_sfxatm_lnd &
         )
  end subroutine initialise_ents_wrapper

  !!
  subroutine initialise_rokgem_wrapper
    implicit none
    call initialise_rokgem (            &
         & genie_timestep,                  & 
         & genie_sfxrok,                         &
         & genie_sfxsumrok1             )
   end subroutine initialise_rokgem_wrapper

 subroutine initialise_ichem_wrapper
   use ichem_main
   implicit none
   call initialise_ichem
 end subroutine initialise_ichem_wrapper

 subroutine initialise_wind_wrapper
   implicit none
   call initialise_wind(     &
        & alon1_atm,         &
        & aboxedge1_lon_atm, &
        & alat1_atm,         &
        & aboxedge1_lat_atm, &
        & alon2_atm,         &
        & aboxedge2_lon_atm, &
        & alat2_atm,         &
        & aboxedge2_lat_atm, &
        & alon3_atm,         &
        & aboxedge3_lon_atm, &
        & alat3_atm,         &
        & aboxedge3_lat_atm, &
        & atmos_lowestlu2_atm, &
        & atmos_lowestlv3_atm, &
        & surf_stressx2_atm,surf_stressy2_atm, &
        & surf_stressx3_atm,surf_stressy3_atm &
        & )
 end subroutine initialise_wind_wrapper

END MODULE genie_ini_wrappers
