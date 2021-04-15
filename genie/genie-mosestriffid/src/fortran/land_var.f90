!#######################################################################################
!#######################################################################################
! Module containing variables used by GENIE-land
! 19/11/03 PPH
!#######################################################################################
!#######################################################################################
MODULE land_var

  USE land_const
  IMPLICIT NONE

  ! Fixed LAND fields in vector format
  INTEGER,DIMENSION(:),ALLOCATABLE      :: land_index
  INTEGER,DIMENSION(:,:),ALLOCATABLE    :: tile_index
  INTEGER,DIMENSION(:),ALLOCATABLE      :: tile_pts
  INTEGER,DIMENSION(:,:),ALLOCATABLE    :: ij_land

  ! Step counter for land
  INTEGER :: nstep_land = 0

  REAL,DIMENSION(:),ALLOCATABLE :: vlats       ! Latitudes of gridbox midpoints (degrees)
  REAL,DIMENSION(:),ALLOCATABLE :: vlons       ! Longitudes of gridbox midpoints (degrees)
  REAL,DIMENSION(:),ALLOCATABLE :: latitudes   ! Latitudes of land points (radians)
  REAL,DIMENSION(:),ALLOCATABLE :: lnd_weights ! Area weightings of land points as a fraction 
                                               ! of Earth total spherical surface area (-)
  REAL,DIMENSION(:,:),ALLOCATABLE :: lnd_weights2(:,:) ! As lnd_weights, but on atmos grid (-)

  LOGICAL :: l_trif_on,l_phen_on,l_equil_on

  TYPE gland_model_switches
    LOGICAL :: trif_on               ! .t. to switch TRIFFID on
    LOGICAL :: phen_on               ! .t. to use TRIFFID phenology scheme
    LOGICAL :: trif_eq_on            ! .t. to put TRIFFID in spin-up mode
    LOGICAL :: ices_on               ! .t. if running with dynamic icesheet
  END TYPE gland_model_switches
  TYPE (gland_model_switches) :: gland_switches

  !For GLIMMER coupling to aid water budgeting with ocean
  REAL,DIMENSION(:),ALLOCATABLE :: ices_sum_evap
  REAL,DIMENSION(:),ALLOCATABLE :: ices_sum_precip
  REAL,DIMENSION(:),ALLOCATABLE :: ices_msoil0
  REAL,DIMENSION(:),ALLOCATABLE :: ices_lying_snow0
  REAL,DIMENSION(:),ALLOCATABLE :: ices_mean_drainage
  INTEGER :: ices_steps_since_ice

  ! Land fields in vector format
  REAL :: timestep, dt_land, dt_trif, dt_phen                               ! Timesteps (s)
  REAL :: init_doy_angle, dt_doy_angle
  REAL,DIMENSION(:),ALLOCATABLE    :: cs, resp_s_dr, veg_frac, frac_vs, pstar
  REAL,DIMENSION(:),ALLOCATABLE    :: lying_snow, msoil, evap, esub, fx_le, fx_sen, fx_ground, gravdr, snowmelt_acc
  REAL,DIMENSION(:),ALLOCATABLE    :: tsub1, tstar_gb, albedo_gb, z0_gb
  REAL,DIMENSION(:,:),ALLOCATABLE  :: ht, lai, g_leaf_acc, g_leaf_phen_acc  !(npts,npft)
  REAL,DIMENSION(:,:),ALLOCATABLE  :: gpp_dr, npp_dr, resp_w_dr             !(npts,npft)
  REAL,DIMENSION(:,:),ALLOCATABLE  :: albsnc, albsnf, z0                    !(npts,npft)
  REAL,DIMENSION(:,:),ALLOCATABLE  :: frac, tstar                           !(npts,ntype)
  REAL,DIMENSION(:,:),ALLOCATABLE  :: drain                                 !(nlon,nlat)
  REAL,DIMENSION(:,:),ALLOCATABLE  :: albedo_tiles                          !(land_pts,ntype)
  REAL,DIMENSION(:,:),ALLOCATABLE  :: dayfrac                               !(land_pts,dayinyear_land)

  !TRIFFID output
  REAL,DIMENSION(:),ALLOCATABLE   :: cv, lit_c_t
  REAL,DIMENSION(:,:),ALLOCATABLE :: c_veg, lit_c

  !For EMBM coupling
  REAL,DIMENSION(:,:),ALLOCATABLE :: albplan

  !For perfect restarts with IGCM
  REAL,DIMENSION(:),ALLOCATABLE :: igcm_swnet
  REAL,DIMENSION(:),ALLOCATABLE :: igcm_lwnet
  REAL,DIMENSION(:),ALLOCATABLE :: igcm_prec
  REAL,DIMENSION(:),ALLOCATABLE :: igcm_z1

CONTAINS

  SUBROUTINE allocate_land_var

    USE genie_util, only : check_iostat
    
     IMPLICIT NONE

     ! locals
     INTEGER :: alloc_stat  ! memory checks

     ALLOCATE(vlats(nlat),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(vlons(nlon),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(land_index(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(latitudes(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(lnd_weights(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(tile_index(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(ij_land(land_pts,2),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(tile_pts(npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(cs(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(resp_s_dr(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(veg_frac(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(frac_vs(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(pstar(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(lying_snow(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(msoil(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(evap(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(esub(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(fx_le(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(fx_sen(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(fx_ground(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(gravdr(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(snowmelt_acc(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(tsub1(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(tstar_gb(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(z0_gb(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(albedo_gb(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(ht(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(lai(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(g_leaf_acc(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(g_leaf_phen_acc(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(gpp_dr(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(npp_dr(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(resp_w_dr(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(albsnc(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(albsnf(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(z0(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(frac(land_pts,ntype),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(tstar(land_pts,ntype),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(drain(nlon,nlat),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(lnd_weights2(nlon,nlat),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(albplan(nlon,nlat),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(cv(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(c_veg(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(lit_c(land_pts,npft),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(lit_c_t(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(albedo_tiles(land_pts,ntype),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(ices_sum_evap(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(ices_sum_precip(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(ices_msoil0(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(ices_lying_snow0(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(ices_mean_drainage(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     ALLOCATE(igcm_swnet(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(igcm_lwnet(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(igcm_prec(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)
     ALLOCATE(igcm_z1(land_pts),stat=alloc_stat)
     call check_iostat(alloc_stat,__LINE__,__FILE__)

     RETURN

  END SUBROUTINE allocate_land_var

  SUBROUTINE deallocate_land_var

    IMPLICIT NONE

    if(ALLOCATED(vlats)) DEALLOCATE(vlats)
    if(ALLOCATED(vlons)) DEALLOCATE(vlons)
    if(ALLOCATED(land_index)) DEALLOCATE(land_index)
    if(ALLOCATED(latitudes)) DEALLOCATE(latitudes)
    if(ALLOCATED(lnd_weights)) DEALLOCATE(lnd_weights)
    if(ALLOCATED(tile_index)) DEALLOCATE(tile_index)
    if(ALLOCATED(ij_land)) DEALLOCATE(ij_land)
    if(ALLOCATED(tile_pts)) DEALLOCATE(tile_pts)
    if(ALLOCATED(cs)) DEALLOCATE(cs)
    if(ALLOCATED(resp_s_dr)) DEALLOCATE(resp_s_dr)
    if(ALLOCATED(veg_frac)) DEALLOCATE(veg_frac)
    if(ALLOCATED(frac_vs)) DEALLOCATE(frac_vs)
    if(ALLOCATED(pstar)) DEALLOCATE(pstar)
    if(ALLOCATED(lying_snow)) DEALLOCATE(lying_snow)
    if(ALLOCATED(msoil)) DEALLOCATE(msoil)
    if(ALLOCATED(evap)) DEALLOCATE(evap)
    if(ALLOCATED(esub)) DEALLOCATE(esub)
    if(ALLOCATED(fx_le)) DEALLOCATE(fx_le)
    if(ALLOCATED(fx_sen)) DEALLOCATE(fx_sen)
    if(ALLOCATED(fx_ground)) DEALLOCATE(fx_ground)
    if(ALLOCATED(gravdr)) DEALLOCATE(gravdr)
    if(ALLOCATED(snowmelt_acc)) DEALLOCATE(snowmelt_acc)
    if(ALLOCATED(tsub1)) DEALLOCATE(tsub1)
    if(ALLOCATED(tstar_gb)) DEALLOCATE(tstar_gb)
    if(ALLOCATED(z0_gb)) DEALLOCATE(z0_gb)
    if(ALLOCATED(albedo_gb)) DEALLOCATE(albedo_gb)
    if(ALLOCATED(ht)) DEALLOCATE(ht)
    if(ALLOCATED(lai)) DEALLOCATE(lai)
    if(ALLOCATED(g_leaf_acc)) DEALLOCATE(g_leaf_acc)
    if(ALLOCATED(g_leaf_phen_acc)) DEALLOCATE(g_leaf_phen_acc)
    if(ALLOCATED(gpp_dr)) DEALLOCATE(gpp_dr)
    if(ALLOCATED(npp_dr)) DEALLOCATE(npp_dr)
    if(ALLOCATED(resp_w_dr)) DEALLOCATE(resp_w_dr)
    if(ALLOCATED(albsnc)) DEALLOCATE(albsnc)
    if(ALLOCATED(albsnf)) DEALLOCATE(albsnf)
    if(ALLOCATED(z0)) DEALLOCATE(z0)
    if(ALLOCATED(frac)) DEALLOCATE(frac)
    if(ALLOCATED(tstar)) DEALLOCATE(tstar)
    if(ALLOCATED(drain)) DEALLOCATE(drain)
    if(ALLOCATED(lnd_weights2)) DEALLOCATE(lnd_weights2)
    if(ALLOCATED(albplan)) DEALLOCATE(albplan)
    if(ALLOCATED(cv)) DEALLOCATE(cv)
    if(ALLOCATED(c_veg)) DEALLOCATE(c_veg)
    if(ALLOCATED(lit_c)) DEALLOCATE(lit_c)
    if(ALLOCATED(lit_c_t)) DEALLOCATE(lit_c_t)
    if(ALLOCATED(albedo_tiles)) DEALLOCATE(albedo_tiles)
    if(ALLOCATED(ices_sum_evap)) DEALLOCATE(ices_sum_evap)
    if(ALLOCATED(ices_sum_precip)) DEALLOCATE(ices_sum_precip)
    if(ALLOCATED(ices_msoil0)) DEALLOCATE(ices_msoil0)
    if(ALLOCATED(ices_lying_snow0)) DEALLOCATE(ices_lying_snow0)
    if(ALLOCATED(ices_mean_drainage)) DEALLOCATE(ices_mean_drainage)
    if(ALLOCATED(igcm_swnet)) DEALLOCATE(igcm_swnet)
    if(ALLOCATED(igcm_lwnet)) DEALLOCATE(igcm_lwnet)
    if(ALLOCATED(igcm_prec)) DEALLOCATE(igcm_prec)
    if(ALLOCATED(igcm_z1)) DEALLOCATE(igcm_z1)

  END SUBROUTINE DEALLOCATE_LAND_VAR

END MODULE land_var
