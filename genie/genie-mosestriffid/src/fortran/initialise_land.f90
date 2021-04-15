!$Id: initialise_land.f90 4215 2008-05-19 11:42:44Z gethin-williams $
!***************************************************************************
! File: initialise_land.f90
!
! Description:
! 
! Initialisation routine for GENIE-land.
!
! 1) Sets up land grid variables based on atmosphere grid.
!
! 2) Sets default values for land variables.
!
! 3) Reads land control NAMELIST file.
!
! 4) If requested, reads the specified GENIE-land restart file.
!
! 5) Reads specified runoff destination file.
!
! 6) Calculates derived quantities constistent with initialisation.
!
! 7) Initialises land energy, water and carbon accounts.
!
! 8) Calculates normalised daylength for each land gridpoint through an annual cycle.
!
! subroutine: initialise_land
! 
! input:
!
!             landseamask_atm - Atmosphere model land/sea mask (1=land,0=ocean)
!             dt_atm - Timestep length of atmosphere model (s)
!             klnd_loop - Number of land timesteps per GENIE loop (dimensionless)
!             alon1_atm - Longitudes of atmosphere gridbox mid-points (degrees)
!             alat1_atm - Latitudes of atmosphere gridbox mid-points (degrees)
!             ilon1_atm - Number of atmosphere gridboxes in longitude direction (dimensionless)
!             ilat1_atm - Number of atmosphere gridboxes in latitude direction (dimensionless)
!             aboxedge1_lon_atm - Longitudes of atmosphere gridbox edges (degrees)
!             aboxedge1_lat_atm - Latitudes of atmosphere gridbox edges (degrees)     
!
! input/output:
!
! output:
!
!***************************************************************************
SUBROUTINE initialise_land(landseamask_atm,dt_atm,klnd_loop, &
                 alon1_atm,alat1_atm,ilon1_atm,ilat1_atm,    &
                 aboxedge1_lon_atm,aboxedge1_lat_atm         )

  USE phys_const
  USE land_const
  USE land_var
  USE land_diags
  USE land_netcdf
  USE land_restart
  USE land_runoff
  USE land_accounting
  USE genie_util, ONLY: check_unit, check_iostat

  IMPLICIT NONE

  !###################################
  ! Input via subroutine arguments
  !###################################
  INTEGER,INTENT(in) :: ilon1_atm,ilat1_atm                           !Atmos no of gridboxes longitude/latitude (-)
  INTEGER,INTENT(in),DIMENSION(ilon1_atm,ilat1_atm)::landseamask_atm  !Atmos grid landsea mask (-)
  INTEGER,INTENT(in) :: klnd_loop                                     !Frequency of land calls in GENIE steps
  REAL(kr_g),INTENT(in) :: dt_atm                                     !Atmosphere timestep (s)
  REAL(kr_g),INTENT(in),DIMENSION(ilat1_atm)   :: alat1_atm           !Atmos gridbox midpoint latitudes (degrees)
  REAL(kr_g),INTENT(in),DIMENSION(ilon1_atm)   :: alon1_atm           !Atmos gridbox midpoint longitudes (degrees)
  REAL(kr_g),INTENT(in),DIMENSION(ilat1_atm+1) :: aboxedge1_lat_atm   !Atmos gridbox edges latitude (degrees)
  REAL(kr_g),INTENT(in),DIMENSION(ilon1_atm+1) :: aboxedge1_lon_atm   !Atmos gridbox edges longitude (degrees)

  !###################################
  ! Input via namelist
  !###################################
  LOGICAL            :: l_restart !IN .T. if land restart file is to be read
  CHARACTER(len=200) :: c_restart !IN name of land restart file inc. path
  CHARACTER(len=200) :: c_runoff_fl
  NAMELIST/GENIELAND_CONTROL/init_doy_angle,dayinyear_land,               &
                idiag_land, irest_land, iacc_land,                        &
                l_trif_on, l_phen_on, l_equil_on, itrif_land, iphen_land, &
                l_restart, c_restart,                                     &
                c_runoff_fl
  NAMELIST/GENIELAND_TUNABLE/albsnf_nvg,hcap_nvg,hcon_nvg,albsnf_max,     &
                hcap_veg,msat,snow_thresh,kaps,q10,                       &
                str_tune,ene_tune,alb_tune

  !###################################
  ! Work variables
  !###################################
  LOGICAL        :: lexist                      ! Flag for existence of data file.
  INTEGER        :: i,j,l                       ! Loop counters
  REAL,PARAMETER :: init_temp = 0.0 + con_zeroc ! Uniform initial temperature (K)
  REAL           :: tv                          ! Temporary time variable
  REAL,PARAMETER :: alb_tol = 0.1               ! +/- multiplicative tolerance on albedo tuning 
  INTEGER        :: ios                         ! for file checks etc.

  PRINT*
  PRINT*,'LAND %% INITIALISING GENIE-LAND.'
  PRINT*,'LAND %%'

  !----------------------------------------------------------------------
  ! Allocate arrays in land_var and land_diags modules based on input
  ! landsea mask
  !----------------------------------------------------------------------
  land_pts = COUNT(landseamask_atm>0)
  nlon         = ilon1_atm
  nlat         = ilat1_atm
  CALL allocate_land_var
  CALL allocate_land_diags
  vlons(:)     = REAL(alon1_atm(:))
  vlats(:)     = REAL(alat1_atm(:))

  !----------------------------------------------------------------------
  ! Define atmosphere grid for land
  !----------------------------------------------------------------------
  ! Set up land arrays based on atmos grid
  l = 0
  DO j=1,nlat
    DO i=1,nlon
      ! if (i,j) is a land point
      IF(landseamask_atm(i,j) > 0) THEN
        l = l + 1

        !Store an array relating i,j points to vector land points
        ij_land(l,1) = i
        ij_land(l,2) = j
        latitudes(l) = vlats(j)*con_pi180

        ! NB This isnt the same as the description of tile index in land.f
        land_index(l) = l

        ! Indices of land points which include the nth vegetation type.
        ! NB All land points include all vegetation types
        tile_index(l,:) = l
      ENDIF
    ENDDO
  ENDDO

  ! set up no. of land points with the nth vegetation type
  ! NB All land points include all vegetation types
  tile_pts(:) = land_pts

  !----------------------------------------------------------------------
  ! Set default values
  !----------------------------------------------------------------------
  CALL initialise_land_default(init_temp)

  !----------------------------------------------------------------------
  ! Read genie-land data file for this run
  !----------------------------------------------------------------------
  INQUIRE(file='data_mosestriffid',EXIST=lexist)
  IF(lexist) THEN
    call check_unit(14,__LINE__,__FILE__)
    OPEN(14,file='data_mosestriffid',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(14,GENIELAND_CONTROL,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    READ(14,GENIELAND_TUNABLE,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    CLOSE(14,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
  ELSE
    PRINT*,'LAND %% Cannot find data_LAND, so cannot initialise genie-land.'
    STOP
  ENDIF

  !----------------------------------------------------------------------
  ! Setup land and TRIFFID time-stepping and flow control variables and 
  ! ensure self-consistency.
  !----------------------------------------------------------------------
  ! The following definitions of land timestep assumes that the
  ! atmos is the most frequently called component of GENIE
  ! i.e. katm_loop=1  
  timestep     = REAL(dt_atm)*REAL(klnd_loop)
  dt_land      = REAL(dt_atm)*REAL(klnd_loop)
  yearlen_land = dayinyear_land*con_secday
  tv           = yearlen_land/dt_land
  nstepyear    = NINT(tv)
  dt_doy_angle = 2.0*con_pi/tv

  ! Setup model switches
  gland_switches%trif_on    = l_trif_on
  gland_switches%phen_on    = l_phen_on
  gland_switches%trif_eq_on = l_equil_on
  gland_switches%ices_on    = .FALSE. !flag_icesheet !TODO [Take in flag as argument]
  steps_since_ice = 0  !TODO

  ! Diagnostics output every idiag_land GENIE steps,
  ! but land called every klnd_loop GENIE steps
  idiag  = idiag_land
  ridiag = REAL(klnd_loop)/REAL(idiag_land)
  PRINT*,'LAND %% land timestep',timestep,' seconds'
  PRINT*,'LAND %% land thinks there are ',dayinyear_land,' days in the year and ',nstepyear,' land steps per year'
  PRINT*,'LAND %% land thinks initial time-of-year circular angle is ',init_doy_angle,' relative to perihelion.'
  PRINT*,'LAND %% GENIE circular anglestep ',dt_doy_angle
  IF((tv-FLOOR(tv)).gt.1E-3) PRINT*,'LAND %% Warning : there may be a conflict between dt_land and dayinyear_land'
  ! NOTE: GOLDSTEIN starts simulations from perihelion (~3 Jan, init_doy_angle=0)
  !       IGCM starts simulations from perihelion unless exact_orbit=.true., 
  !        then init_doy_angle=-0.0405 rad

  ! Calculate the TRIFFID and phenology timestep
  dt_trif = REAL(itrif_land)*dt_atm
  dt_phen = REAL(iphen_land)*dt_atm
  IF(l_trif_on) THEN
    PRINT*,'LAND %% PHEN period (s) =',dt_phen
    PRINT*,'LAND %% TRIFFID period (s) =',dt_trif
    IF(l_equil_on) THEN
      PRINT*,'LAND %% Dynamic vegetation: ON (equilibrium mode)'
    ELSE
      PRINT*,'LAND %% Dynamic vegetation: ON (transient mode)'
    ENDIF
    IF(l_phen_on) PRINT*,'LAND %% Phenology: ON'
  ELSEIF(l_phen_on) THEN
    PRINT*,'LAND %% Note: Phenology will not work with TRIFFID OFF'
    l_phen_on = .FALSE.
    PRINT*,'LAND %% Phenology has been switched OFF'
  ELSE
    PRINT*,'LAND %% Dynamic vegetation: OFF'
  ENDIF

  !----------------------------------------------------------------------
  ! If requested, read restart dump and set land_netcdf module variable
  !----------------------------------------------------------------------
  IF(l_restart) THEN
    CALL land_restart_read(c_restart)
    istep0 = nstep_land + 1
  ENDIF
  CALL read_runoff_dests(c_runoff_fl,landseamask_atm,1)

  !----------------------------------------------------------------------
  ! Adjust surface albedos.  This is for use with EnKF tuning and 
  ! alb_tune should be set to 0.5 to retain the default parameters.
  !----------------------------------------------------------------------
  tv = ( 1.0 + alb_tol*(alb_tune - 0.5)/0.5 )
  ALBSNC_NVG(:) = ALBSNC_NVG(:) * tv
  ALBSNF_NVG(:) = ALBSNF_NVG(:) * tv
  ALBSNC_MAX(:) = ALBSNC_MAX(:) * tv
  ALBSNC_MIN(:) = ALBSNC_MIN(:) * tv
  ALBSNF_MAX(:) = ALBSNF_MAX(:) * tv

  ALBSNC_NVG(:) = MAX(MIN(ALBSNC_NVG(:),1.0), 0.0)
  ALBSNF_NVG(:) = MAX(MIN(ALBSNF_NVG(:),1.0), 0.0)
  ALBSNC_MAX(:) = MAX(MIN(ALBSNC_MAX(:),1.0), 0.0)
  ALBSNC_MIN(:) = MAX(MIN(ALBSNC_MIN(:),1.0), 0.0)
  ALBSNF_MAX(:) = MAX(MIN(ALBSNF_MAX(:),1.0), 0.0)

  !----------------------------------------------------------------------
  ! Calculate derived quantities constistent with initialisation
  !----------------------------------------------------------------------
  CALL initialise_land_adjust()

  !----------------------------------------------------------------------
  ! Initialise the energy/mass accounting
  !----------------------------------------------------------------------
  CALL init_lnd_account(REAL(aboxedge1_lon_atm),REAL(aboxedge1_lat_atm))
  CALL check_lnd_account

  !----------------------------------------------------------------------
  ! Calculate daylength array
  !----------------------------------------------------------------------
  CALL calc_daylength

END SUBROUTINE initialise_land


!#######################################################################################
!#######################################################################################
!
!#######################################################################################
!#######################################################################################
SUBROUTINE initialise_land_default(init_temp)

  USE phys_const, only : con_sboltz, con_zeroc
  USE land_const, only : lai_min, msat, a_wl, a_ws, eta_sl, b_wl
  USE land_var
  IMPLICIT NONE

  REAL,INTENT(in) :: init_temp ! Uniform initial temperature (K)
  INTEGER         :: ipft, i

  DO i=1,land_pts
    !----------------------------------------------------------------------
    ! Default land cover is bare soil everywhere with 'seeding' fraction
    ! of each veg type
    !----------------------------------------------------------------------
    frac(i,:)      = 0.0
    frac(i,1:npft) = frac_min
    veg_frac(i)    = SUM(frac(i,1:npft))
    frac(i,soil)   = 1.0 - veg_frac(i)
    frac(i,soil+1) = 0.0
    frac_vs(i)     = veg_frac(i) + frac(i,soil)

    !----------------------------------------------------------------------
    ! Initialise various physical fields
    !----------------------------------------------------------------------
    pstar(i)      = 1.0133e5                  ! Surface air pressure (Pa)
    cs(i)         = 10.0                      ! Soil carbon (kg C/m2) 
    msoil(i)      = msat                      ! Soil moisture content (kg H2O/m2)
    tsub1(i)      = init_temp                 ! Sub-surface temperature (K)
    tstar(i,:)    = init_temp                 ! Tile surface temperature (K)
    tstar_gb(i)   = SUM(frac(i,:)*tstar(i,:)) ! Grid-box mean surface temperature (K)
    lying_snow(i) = 0.0                       ! Snow mass (kg/m2)
    drain(ij_land(i,1),ij_land(i,2)) = 0.0    ! Runoff rate (kg/m2/s)

    !----------------------------------------------------------------------
    ! Set initial LAI to the allowed minimum for each PFT
    ! Set initial heights based on LAI
    !----------------------------------------------------------------------
    DO ipft=1,npft
      lai(i,ipft) = lai_min(ipft)
      ht(i,ipft)  = (a_wl(ipft)/(a_ws(ipft)*eta_sl(ipft)))*(lai_min(ipft)**(b_wl(ipft)-1.0))
    ENDDO

    !----------------------------------------------------------------------
    ! Zero the carbon accumulations used by TRIFFID
    !----------------------------------------------------------------------
    g_leaf_phen_acc(i,:) = 0.0
    g_leaf_acc(i,:)      = 0.0
    gpp_dr(i,:)          = 0.0
    npp_dr(i,:)          = 0.0
    resp_w_dr(i,:)       = 0.0
    resp_s_dr(i)         = 0.0
  ENDDO

  RETURN

END SUBROUTINE initialise_land_default

!#######################################################################################
!#######################################################################################
! Subroutine to calculate derived quantities following restart read.
!#######################################################################################
!#######################################################################################
SUBROUTINE initialise_land_adjust()

  USE land_const, only : ntype, npft, land_pts, frac_min
  USE land_var
  USE genie_util, only : check_iostat 
  IMPLICIT NONE

  INTEGER                       :: ipft, land_pts_chk, l
  INTEGER                       :: alloc_stat
  REAL,DIMENSION(:),ALLOCATABLE :: dummy_arr,albsoil,lai_bal,leaf,root,wood

  !----------------------------------------------------------------------
  ! Check that number of land points on atmos landsea mask agrees with restart
  !----------------------------------------------------------------------
  land_pts_chk = COUNT(tsub1>0.0)
  IF(land_pts_chk==land_pts) THEN
    PRINT*,'LAND %% No of land points ',land_pts_chk,'is okay'
  ELSE
    PRINT*,'LAND %% *** Land points inconsistency ***'
    PRINT*,'LAND %% ',land_pts,' land points in atmos land mask'
    PRINT*,'LAND %% ',land_pts_chk,' land points in restart file'
    STOP
  ENDIF

  !----------------------------------------------------------------------
  ! Allocate local work arrays
  !----------------------------------------------------------------------
  ALLOCATE(dummy_arr(land_pts),stat=alloc_stat)
  call check_iostat(alloc_stat,__LINE__,__FILE__)
  ALLOCATE(albsoil(land_pts),stat=alloc_stat)
  call check_iostat(alloc_stat,__LINE__,__FILE__)
  ALLOCATE(leaf(land_pts),stat=alloc_stat)
  call check_iostat(alloc_stat,__LINE__,__FILE__)
  ALLOCATE(root(land_pts),stat=alloc_stat)
  call check_iostat(alloc_stat,__LINE__,__FILE__)
  ALLOCATE(wood(land_pts),stat=alloc_stat)
  call check_iostat(alloc_stat,__LINE__,__FILE__)
  ALLOCATE(lai_bal(land_pts),stat=alloc_stat)
  call check_iostat(alloc_stat,__LINE__,__FILE__)

  !----------------------------------------------------------------------
  ! Reset ALBSNC,ALBSNF,Z0 based on height and LAI
  !----------------------------------------------------------------------
  albedo_gb(:) = 0.0
  albsoil(:)   = albsnf_nvg(1)
  cv(:)        = 0.0
  DO ipft=1,npft
    CALL pft_sparm(land_pts,ipft,tile_index(:,ipft),tile_pts(ipft), &
                   albsoil,ht(:,ipft),lai(:,ipft),                  &
                   albsnc(:,ipft),albsnf(:,ipft),dummy_arr,         &
                   z0(:,ipft)                                       )

    !----------------------------------------------------------------------
    ! Calculate gridbox mean vegetation carbon
    !----------------------------------------------------------------------
    lai_bal(:)   = 0.0
    leaf(:)      = 0.0
    root(:)      = 0.0
    wood(:)      = 0.0
    lai_bal(:)   = (a_ws(ipft)*eta_sl(ipft)*ht(:,ipft)/a_wl(ipft))**(1.0/(b_wl(ipft)-1))
    leaf(:)      = sigl(ipft)*lai_bal(:)
    root(:)      = leaf(:)
    wood(:)      = a_wl(ipft)*(lai_bal(:)**b_wl(ipft))
    cv(:)        = cv(:) + frac(:,ipft)*(leaf(:) + root(:) + wood(:))
  ENDDO

  !----------------------------------------------------------------------
  ! Calculate any GBM values needed before first land call.
  !----------------------------------------------------------------------
  CALL calc_albedos(frac,tstar,lying_snow,albsnf,albsnc,albedo_tiles,albedo_gb)
  CALL calc_z0_gbm(z0_gb)
  DO l=1,land_pts
    tstar_gb(l) = SUM(frac(l,:)*tstar(l,:))
  ENDDO

  !----------------------------------------------------------------------
  ! Get rid of excess water from bucket to prevent unreasonably large
  ! amounts of runoff/endorheic evap on first timestep.
  !----------------------------------------------------------------------
  land_pts_chk = COUNT(msoil>msat)
  IF(land_pts_chk > 0) THEN
    PRINT*,'LAND %% Restart file msoil > msat for',land_pts_chk,'points.'
    PRINT*,'LAND %% Setting msoil = msat for these points.'
    WHERE(msoil > msat) msoil = msat
  ENDIF
  
  ! clean up memory
  if (ALLOCATED(dummy_arr)) DEALLOCATE(dummy_arr)
  if (ALLOCATED(albsoil))   DEALLOCATE(albsoil)
  if (ALLOCATED(leaf))      DEALLOCATE(leaf)
  if (ALLOCATED(root))      DEALLOCATE(root)
  if (ALLOCATED(wood))      DEALLOCATE(wood)
  if (ALLOCATED(lai_bal))   DEALLOCATE(lai_bal)

  RETURN

END SUBROUTINE initialise_land_adjust 

!#######################################################################################
!#######################################################################################
! Subroutine to return the extra variables required for perfect restarts with
! IGCM.  It copies 4 atmospheric variables on land points only onto the 
! land grid boxes of the 2-D atmos array.
!#######################################################################################
!#######################################################################################
SUBROUTINE initialise_land_extra(igcm_swneto,igcm_lwneto,igcm_preco,igcm_z1o)
  USE land_const, only : land_pts,nlon,nlat
  USE land_var, only : igcm_swnet,igcm_lwnet,igcm_prec,igcm_z1,ij_land
  IMPLICIT NONE
  REAL,DIMENSION(nlon,nlat),INTENT(inout) :: igcm_swneto
  REAL,DIMENSION(nlon,nlat),INTENT(inout) :: igcm_lwneto
  REAL,DIMENSION(nlon,nlat),INTENT(inout) :: igcm_preco
  REAL,DIMENSION(nlon,nlat),INTENT(inout) :: igcm_z1o
  INTEGER :: l, ij(2)

  DO l=1,land_pts
    ij(:) = ij_land(l,:)
    igcm_swneto(ij(1),ij(2)) = igcm_swnet(l)
    igcm_lwneto(ij(1),ij(2)) = igcm_lwnet(l)
    igcm_preco(ij(1),ij(2))  = igcm_prec(l)
    igcm_z1o(ij(1),ij(2))    = igcm_z1(l)
  ENDDO

  RETURN
END SUBROUTINE initialise_land_extra
