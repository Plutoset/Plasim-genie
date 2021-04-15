!***************************************************************************
! File: genie_land_surflux.f90
!
! Description: 
!
! Interface between top-level GENIE code and GENIE-land module.  This subroutine
! takes in surface radiation fluxes and lowest atmospheric level state variables
! and outputs surface state, turbulent fluxes, runoff and CO2 exchange.
!
! 1) Regrids input variables from GENIE atmosphere lon/lat grid to vector
!    of land-points.
!
! 2) If icesheet state has changed, calls update_landice() to update 
!    GENIE-land associated variables.
!
! 3) If necessary, call land_restart_write().
!
! 4) Calls tstep_land() for a single land timestep.
!
! 5) If necessary, tstep_triffid() called every per_trif ocean steps.
!
! 6) If necessary, tstep_phenology() updated every per_phen ocean steps.
!
! 7) Update and output GENIE-land diagnostics.
!
! 8) Regrids GENIE-land output variables from vectors of land-points to 
!    GENIE atmosphere lon/lat grid.
!
! 9) Route runoff from land to ocean gridboxes.
!
!10) Update land energy, water and carbon accounts.
!
! subroutine: genie_land_surflux
!
! input:
!             co2_atm - Gridbox mean atmospheric CO2 concentration (???)
!             netsolar_atm - Gridbox mean net surface shortwave radiation (W/m2)
!             netlong_atm - Gridbox mean net surface longwave radiation (W/m2)
!             lowestlu - Gridbox mean lowest atmos level zonal wind speed (m/s)
!             lowestlv - Gridbox mean lowest atmos level meridional wind speed (m/s)
!             precip_atm - Gridbox mean precipitation rate (kg/m2/s)
!             lowestlt - Gridbox mean lowest atmos level air temperature (deg C)
!             lowestlq - Gridbox mean lowest atmos level air specific humidity (kg/kg)
!             lowestlp - Gridbox mean surface air pressure (Pa)
!             lowestlh - Height of lowest atmos level (m)
!
! input/output:
!
! output:
!             land_tstarinst_atm - Gridbox mean surface temperature (deg C)
!             land_salb_atm - Gridbox mean surface albedo (dimensionless)
!             land_evap_atm - Gridbox mean surface evaporation rate (kg/m2/s)
!             land_latentinst_atm - Gridbox mean surface latent heat flux to land (W/m2)
!             land_sensibleinst_atm - Gridbox mean surface sensible heat flux to land (W/m2)
!             land_stressxinst_atm - Gridbox mean surface zonal momentum flux to atmos (kg/m/s2)
!             land_stressyinst_atm - Gridbox mean surface meridional momentum flux to atmos (kg/m/s2)
!             land_rough_atm - Gridbox mean surface roughness length (m)
!             land_qstar_atm - Gridbox mean near-surface specific humidity (kg/kg)
!             land_runoff_atm - Gridbox mean runoff rate (kg/m2/s)
!             land_tice_ice - Surface temperature of land-ice tile (deg C)
!             land_albice_ice - Surface albedo of land-ice tile (dimensionless)
!             land_fxco2_atm - Net land-to-atmosphere CO2 flux (mol/m2/s)
!             test_water_land - Change in total land water since start of simulation (kg H2O)
!***************************************************************************
SUBROUTINE genie_land_surflux(co2_atm,netsolar_atm,netlong_atm,               &
                              lowestlu,lowestlv,precip_atm,                   &
                              lowestlt,lowestlq,lowestlp,lowestlh,            &
                              land_tstarinst_atm,land_salb_atm,land_evap_atm, &
                              land_latentinst_atm,land_sensibleinst_atm,      &
                              land_stressxinst_atm,land_stressyinst_atm,      &
                              land_rough_atm,land_qstar_atm,                  &
                              land_runoff_atm,land_tice_ice,land_albice_ice,  &
                              land_fxco2_atm,test_water_land                  )

  USE phys_const, only : con_zeroc, con_sboltz, con_latfus
  USE land_const
  USE land_var
  USE land_diags
  USE land_netcdf
  USE land_restart
  USE land_runoff
  USE land_accounting

  IMPLICIT NONE

!**********************************************************************
! DECLARE INPUT VARIABLES
! GBM="Grid box mean"
! LAL="Lowest atmospheric level"
!**********************************************************************
!  INTEGER,INTENT(in) :: istot                        !Atmosphere model step number
!  INTEGER            :: iconv4lnd                    !Flag for whether icesheet has been updated
!  INTEGER,INTENT(in) :: klnd_loop                    !Frequency of land calls in koverall steps
  REAL(kr_g),INTENT(in) :: co2_atm(nlon,nlat)        !Atm CO2 concentration (atms?)
  REAL(kr_g),INTENT(in) :: netsolar_atm(nlon,nlat)   !GBM net surface solar radiation (W/m2)
  REAL(kr_g),INTENT(in) :: netlong_atm(nlon,nlat)    !GBM net surface longwave radiation (W/m2)
  REAL(kr_g),INTENT(in) :: lowestlu(nlon,nlat)       !LAL zonal wind component (m/s)
  REAL(kr_g),INTENT(in) :: lowestlv(nlon,nlat)       !LAL meridional wind component (m/2)
  REAL(kr_g),INTENT(in) :: precip_atm(nlon,nlat)     !precip rate (kg/m2/s)
  REAL(kr_g),INTENT(in) :: lowestlt(nlon,nlat)       !LAL air temperature (deg C)
  REAL(kr_g),INTENT(in) :: lowestlq(nlon,nlat)       !LAL specific humidity (kg/kg)
  REAL(kr_g),INTENT(in) :: lowestlp(nlon,nlat)       !LAL surface air pressure (Pa)
  REAL(kr_g),INTENT(in) :: lowestlh(nlon,nlat)       !Height of LAL (m)
!  REAL(kr_g),INTENT(in) :: icefrac_atm(nlon,nlat)    !Gridbox ice fraction (-)

!**********************************************************************
! DECLARE OUTPUT VARIABLES
!**********************************************************************
  REAL(kr_g),INTENT(out) :: land_tstarinst_atm(nlon,nlat)    !Surface radiative temperature (K)
  REAL(kr_g),INTENT(out) :: land_salb_atm(nlon,nlat)         !Surface albedo - no zenith angle correction (-)
  REAL(kr_g),INTENT(out) :: land_evap_atm(nlon,nlat)         !Surface evaporation rate (kg/m2/s)
  REAL(kr_g),INTENT(out) :: land_latentinst_atm(nlon,nlat)   !Surface latent heat flux (W/m2)
  REAL(kr_g),INTENT(out) :: land_sensibleinst_atm(nlon,nlat) !Surface sensible heat flux (W/m2)
  REAL(kr_g),INTENT(out) :: land_stressxinst_atm(nlon,nlat)  !Surface zonal momentum flux (kg/m/s2)
  REAL(kr_g),INTENT(out) :: land_stressyinst_atm(nlon,nlat)  !Surface meridional momentum flux (kg/m/s2)
  REAL(kr_g),INTENT(out) :: land_rough_atm(nlon,nlat)        !Surface effective roughness length (m)
  REAL(kr_g),INTENT(out) :: land_qstar_atm(nlon,nlat)        !Near-surface humidity specific humidity (kg/kg)
  REAL(kr_g),INTENT(out) :: land_runoff_atm(nlon,nlat)       !Surface runoff rate (kg/m2/s)
  REAL(kr_g),INTENT(out) :: land_tice_ice(nlon,nlat)         !Time mean ice-tile surface temperature (K)
  REAL(kr_g),INTENT(out) :: land_albice_ice(nlon,nlat)       !Albedo of ice tile (-)
  REAL(kr_g),INTENT(out) :: land_fxco2_atm(nlon,nlat)        !Net land-to-atm CO2 flux (mol/m2/s)
  REAL(kr_g),INTENT(out) :: test_water_land                  !Change in total land water since start of run (kg H2O)

!**********************************************************************
! DECLARE WORK VARIABLES
!**********************************************************************
  INTEGER :: n                ! WORK step of year

  REAL :: lw(land_pts)        ! WORK Downward longwave (W/m2)
  REAL :: sw(land_pts)        ! WORK Downward shortwave (W/m2)
  REAL :: q(land_pts)         ! WORK Bottom atm level air humidity (kg/kg)
  REAL :: qsurf(land_pts)     ! WORK Near surface humidity (kg/kg)
  REAL :: tair(land_pts)      ! WORK Bottom atm level air temperature (K)
  REAL :: rain(land_pts)      ! WORK Rainfall rate (kg/m2/s)
  REAL :: snow(land_pts)      ! WORK Snowfall rate (kg/m2/s)
  REAL :: windx(land_pts)     ! WORK Zonal windspeed (m/s)
  REAL :: windy(land_pts)     ! WORK Meridional windspeed (m/s)
  REAL :: z1(land_pts)        ! WORK Reference height LAL (m)
  REAL :: albice(land_pts)    ! WORK albedo of land-ice tile (-)
  REAL :: swn(land_pts)       ! WORK Net shortwave radiation from land scheme(W/m2)
  REAL :: radnet(land_pts)    ! WORK Net allwave radiation (W/m2)
  REAL :: gpp(land_pts,npft)  ! WORK Gross primary productivity (kgC/m2/s)
  REAL :: npp(land_pts,npft)  ! WORK Net primary productivity (kgC/m2/s)
  REAL :: resp_s(land_pts)    ! WORK Soil respiration (kgC/m2)
  REAL :: fx_mom(land_pts,2)  ! WORK Surface momentum flux (N/m2) 
  REAL :: co2_atm_l(land_pts) ! WORK Atmospheric CO2 concentration (atms?)
  REAL :: fx_co2(land_pts)    ! WORK Net atmos-to-land CO2 flux (kg CO2/m2/s)
  REAL :: fx_surf(land_pts)   ! WORK Surface heat flux (W/m2)
  REAL :: gc_gb(land_pts)     ! WORK Canopy conductance (m/s)
  REAL(kr_g) :: land_drain_temp(nlon,nlat)       ! Temporary array for surface runoff rate (kg/m2/s)
  REAL :: fx_fusion(land_pts) ! WORK Heat release on rain->snow phase change (W/m2)

  INTEGER :: i,j              ! Loop counters
  INTEGER :: ifield           ! Number of land points

  !**********************************************************************
  ! Update the land-ice boundary conditions in genie-land if they have
  ! been changed by an icesheet model
  !**********************************************************************
!bowie  ices_steps_since_ice = ices_steps_since_ice + 1
!bowie  IF(iconv4lnd.eq.1) CALL update_landice(icefrac_atm,net_water_in_atm,iconv4lnd)

  !**********************************************************************
  ! Convert forcing data from atmosphere grid/units to
  ! LAND grid/units, and cast from GENIE precision to genie-land precision
  !**********************************************************************
  fx_fusion(:) = 0.0
  DO ifield=1,land_pts
    i=ij_land(ifield,1)
    j=ij_land(ifield,2)

    ! ATMOS CO2 CONC (atm --> kg(CO2)/kg(air))
    co2_atm_l(ifield) = REAL(co2_atm(i,j))

    ! SURFACE NET SHORTWAVE RADIATION (W/m2)
    sw(ifield)      = REAL(netsolar_atm(i,j))

    ! AIR TEMP (degC --> K)
    tair(ifield) = REAL(lowestlt(i,j)) + con_zeroc

    ! SURFACE NET LONGWAVE RADIATION (W/m2)
    lw(ifield) = REAL(netlong_atm(i,j))

    !AIR HUMIDITY (kg/kg)
    q(ifield) = MAX(REAL(lowestlq(i,j)),0.0)

    ! PRECIP (kg/m2/s=mm/s)
    ! Allocate pptn as RAIN or SNOW, according to soil temp above/below 0 deg C,
    IF(tsub1(ifield).GT.snow_thresh) THEN
      rain(ifield) = REAL(precip_atm(i,j))
      snow(ifield) = 0.0
    ELSE
      rain(ifield) = 0.0
      snow(ifield) = REAL(precip_atm(i,j))
      fx_fusion(ifield) = con_latfus*snow(ifield)
    ENDIF

    ! WIND (m/s)
    windx(ifield) = REAL(lowestlu(i,j))
    windy(ifield) = REAL(lowestlv(i,j))

    ! SURFACE PRESSURE (Pa)
    pstar(ifield) = REAL(lowestlp(i,j))

    ! REFERENCE HEIGHT (m)
    z1(ifield) = MAX(1.0,REAL(lowestlh(i,j)))

  ENDDO

  !**********************************************************************
  ! Restart dump call
  !**********************************************************************
  IF(MOD(nstep_land,irest_land).EQ.0) THEN
    igcm_swnet(:) = sw(:)
    igcm_lwnet(:) = lw(:)
    igcm_prec(:)  = rain(:) + snow(:)
    igcm_z1(:)    = z1(:)
    CALL land_restart_write(trim(out_dir))
  ENDIF

  !**********************************************************************
  ! Calculate the step-of-year number for use by daylength correction
  !**********************************************************************
  n = MOD(nstep_land, nstepyear) + 1

  !*********************************************************************
  ! Call main LAND routine
  !*********************************************************************
  CALL tstep_land(land_index,tile_pts,tile_index,timestep,dayfrac(:,n),   &
            co2_atm_l,lw,sw,pstar,q,tair,rain,snow,windx,windy,           &
            cs,frac,ht,lai,lying_snow,msoil,tsub1,tstar,albsnc,           &
            albsnf,z0,z1,g_leaf_acc,gpp_dr,npp_dr,resp_s_dr,resp_w_dr,    &
            evap,esub,fx_le,tstar_gb,gpp,npp,                             &
            resp_s,gravdr,snowmelt_acc,albedo_gb,swn,radnet,fx_sen,       &
            fx_ground,fx_mom,albice,qsurf,fx_surf,gc_gb,albedo_tiles)

  nstep_land = nstep_land + 1

  !**********************************************************************
  ! Update vegetation
  !**********************************************************************
  fx_co2(:) = 0.0
  IF(gland_switches%trif_on) THEN
    IF((gland_switches%phen_on) .AND. (MOD(nstep_land,iphen_land).EQ.0)) CALL tstep_phenology
    IF(MOD(nstep_land,itrif_land).EQ.0) CALL tstep_triffid(fx_co2)
  ENDIF

  !**********************************************************************
  ! Increment the land diagnostics
  !**********************************************************************
  CALL update_land_diags(sw,lw,tair,q,windx,rain, &
                         pstar,swn,lw,radnet,fx_ground,frac,fx_surf,gpp,npp,gc_gb)

  !**********************************************************************
  ! Diagnostic write call
  !**********************************************************************
  IF(MOD(nstep_land,idiag_land).EQ.0) THEN
    CALL ini_netcdf_land(nstep_land,1,trim(out_dir))
    CALL write_nc_land(1)
    CALL end_nc_land(1)
    CALL zero_land_diags
  ENDIF

  !**********************************************************************
  ! If we're using a dynamic icesheet then
  !**********************************************************************
  IF(gland_switches%ices_on) THEN
    DO ifield=1,land_pts
      i=ij_land(ifield,1)
      j=ij_land(ifield,2)
      ices_sum_precip(ifield) = ices_sum_precip(ifield) + rain(ifield) + snow(ifield)
      ices_sum_evap(ifield)   = ices_sum_evap(ifield)   + evap(ifield) + esub(ifield) 
      gravdr(ifield)          = ices_mean_drainage(ifield)
    ENDDO
  ENDIF

  !**********************************************************************
  ! Regrid land output variables to atmos grid, and cast from genie-land
  ! precision to GENIE precision.
  ! Note that both radiative and turbulent energy fluxes are positive 
  ! TOWARDS surface
  !**********************************************************************
  land_tstarinst_atm(:,:)    = REAL(0.0, KIND=kr_g)
  land_salb_atm(:,:)         = REAL(0.0, KIND=kr_g)
  land_latentinst_atm(:,:)   = REAL(0.0, KIND=kr_g)
  land_sensibleinst_atm(:,:) = REAL(0.0, KIND=kr_g)
  land_stressxinst_atm(:,:)  = REAL(0.0, KIND=kr_g)
  land_stressyinst_atm(:,:)  = REAL(0.0, KIND=kr_g)
  land_evap_atm(:,:)         = REAL(0.0, KIND=kr_g)
  land_drain_temp(:,:)       = REAL(0.0, KIND=kr_g)
  land_tice_ice(:,:)         = REAL(0.0, KIND=kr_g)
  land_albice_ice(:,:)       = REAL(0.0, KIND=kr_g)
  land_qstar_atm(:,:)        = REAL(0.0, KIND=kr_g)
  land_rough_atm(:,:)        = REAL(0.0, KIND=kr_g)
  land_fxco2_atm(:,:)        = REAL(0.0, KIND=kr_g)

  DO ifield=1,land_pts
    i=ij_land(ifield,1)
    j=ij_land(ifield,2)

    land_tstarinst_atm(i,j)    = REAL(tstar_gb(ifield)-con_zeroc, KIND=kr_g)
    land_salb_atm(i,j)         = REAL(albedo_gb(ifield), KIND=kr_g)
    land_latentinst_atm(i,j)   = REAL(-fx_le(ifield), KIND=kr_g)
    land_sensibleinst_atm(i,j) = REAL(-fx_sen(ifield)-fx_fusion(ifield), KIND=kr_g)
    land_stressxinst_atm(i,j)  = REAL(fx_mom(ifield,1), KIND=kr_g)
    land_stressyinst_atm(i,j)  = REAL(fx_mom(ifield,2), KIND=kr_g)
    land_evap_atm(i,j)         = REAL(-(evap(ifield) + esub(ifield)), KIND=kr_g)
    land_drain_temp(i,j)       = REAL(gravdr(ifield), KIND=kr_g)
    land_tice_ice(i,j)         = REAL(tstar(ifield,soil+1), KIND=kr_g)  !TODO ice-sheet step mean
    land_albice_ice(i,j)       = REAL(albice(ifield), KIND=kr_g)
    land_qstar_atm(i,j)        = REAL(qsurf(ifield), KIND=kr_g)
    land_rough_atm(i,j)        = REAL(z0_gb(ifield), KIND=kr_g)
    land_fxco2_atm(i,j)        = REAL(fx_co2(ifield), KIND=kr_g)
  ENDDO
  CALL runoff_route(land_drain_temp,land_runoff_atm,land_evap_atm)

  !**********************************************************************
  ! Temporary accounting call
  !**********************************************************************
  CALL update_lnd_account(precip_atm,land_evap_atm,land_runoff_atm          &
                         ,netsolar_atm,netlong_atm,land_latentinst_atm,land_sensibleinst_atm &
                         ,land_fxco2_atm)

  IF(MOD(nstep_land,iacc_land).EQ.0) CALL check_lnd_account
  CALL get_inc_lnd_account(test_water_land)

  RETURN

END SUBROUTINE genie_land_surflux
