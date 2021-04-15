!#######################################################################################
! Module that contains land diagnostics
! To add a new diagnostic:
!  1.1) [land_diags.f90] change ndiags/ndiags_tile
!  1.2) [land_diags.f90] add increment to diag_var/diag_var_tile
!  1.3) [land_diags.f90] pass the variable to land_diag() if necessary
!  2.1) [land_netcdf.f90] change nall????
!  2.2) [land_netcdf.f90] add variable to setup_nc_land()
!#######################################################################################
MODULE land_diags

  IMPLICIT NONE

  INTEGER           :: idiag             ! Frequency of diagnostic output in GENIE steps
  REAL              :: ridiag            ! Fraction of each land step contributing to period mean
  INTEGER,PARAMETER :: ndiags      = 16  ! Number of GBM output fields
  INTEGER,PARAMETER :: ndiags_tile = 5   ! Number of tile output fields
  INTEGER,PARAMETER :: nforce      = 7   ! Number of GBM atmospheric forcing output fields 

  REAL,DIMENSION(:,:),ALLOCATABLE   :: diag_force
  REAL,DIMENSION(:,:),ALLOCATABLE   :: diag_var
  REAL,DIMENSION(:,:,:),ALLOCATABLE :: diag_var_tile

CONTAINS

  !#######################################################################################
  ! Allocate arrays
  !#######################################################################################
  SUBROUTINE allocate_land_diags
    USE land_const, only : land_pts,ntype
    USE genie_util, only : check_iostat
    IMPLICIT NONE

    ! local
    INTEGER :: alloc_stat

    ALLOCATE(diag_force(land_pts,nforce),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    ALLOCATE(diag_var(land_pts,ndiags),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    ALLOCATE(diag_var_tile(land_pts,ntype,ndiags_tile),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)
    CALL zero_land_diags

    RETURN
  END SUBROUTINE allocate_land_diags

  !#######################################################################################
  ! Add increment to time means
  !#######################################################################################
  SUBROUTINE update_land_diags(in_sw,in_lw,in_tair,in_qair &
                         ,in_wind,in_precip,in_airp,in_swnet,in_lwnet,in_rnet &
                         ,in_ground,in_frac,in_fxsurf,in_gpp,in_npp,in_gc_gb)

    USE phys_const, only : con_aearth
    USE land_const, only : land_pts,ntype
    USE land_var
    IMPLICIT NONE

    REAL,INTENT(in),DIMENSION(land_pts) :: in_sw,in_lw,in_tair,in_qair  &
                   ,in_wind,in_precip,in_airp,in_swnet,in_lwnet,in_rnet &
                   ,in_ground,in_fxsurf, in_gc_gb
    REAL,INTENT(in),DIMENSION(land_pts,ntype) :: in_frac
    REAL,INTENT(in),DIMENSION(land_pts,npft)  :: in_npp, in_gpp

    diag_force(:,1) = diag_force(:,1) + in_sw(:)*ridiag
    diag_force(:,2) = diag_force(:,2) + in_lw(:)*ridiag
    diag_force(:,3) = diag_force(:,3) + in_tair(:)*ridiag
    diag_force(:,4) = diag_force(:,4) + in_qair(:)*ridiag
    diag_force(:,5) = diag_force(:,5) + in_wind(:)*ridiag
    diag_force(:,6) = diag_force(:,6) + in_precip(:)*ridiag
    diag_force(:,7) = diag_force(:,7) + in_airp(:)*ridiag

    diag_var(:,1) = diag_var(:,1) + in_swnet(:)*ridiag
    diag_var(:,2) = diag_var(:,2) + in_lwnet(:)*ridiag
    diag_var(:,3) = diag_var(:,3) + in_rnet(:)*ridiag
    diag_var(:,4) = diag_var(:,4) + fx_le(:)*ridiag
    diag_var(:,5) = diag_var(:,5) + fx_sen(:)*ridiag
    diag_var(:,6) = diag_var(:,6) + in_ground(:)*ridiag
    diag_var(:,7) = diag_var(:,7) + in_fxsurf(:)*ridiag
    diag_var(:,8) = diag_var(:,8) + tstar_gb(:)*ridiag
    diag_var(:,9) = diag_var(:,9) + tsub1(:)*ridiag
    diag_var(:,10) = diag_var(:,10) + msoil(:)*ridiag
    diag_var(:,11) = diag_var(:,11) + lying_snow(:)*ridiag
    diag_var(:,12) = diag_var(:,12) + in_gc_gb(:)*ridiag
    diag_var(:,13) = cv(:)
    diag_var(:,14) = cs(:)
    diag_var(:,15) = lnd_weights(:)*con_aearth
    diag_var(:,16) = diag_var(:,16) + albedo_gb(:)*ridiag

    diag_var_tile(:,:,1) = in_frac(:,:)
    diag_var_tile(:,1:npft,2) = lai(:,:)
    diag_var_tile(:,1:npft,3) = ht(:,:)
    diag_var_tile(:,1:npft,4) = diag_var_tile(:,1:npft,4) + in_npp(:,:)*ridiag
    diag_var_tile(:,1:npft,5) = diag_var_tile(:,1:npft,5) + in_gpp(:,:)*ridiag

    RETURN
  END SUBROUTINE update_land_diags

  !#######################################################################################
  ! Reset/zero the land diagnostic arrays
  !#######################################################################################
  SUBROUTINE zero_land_diags

    IMPLICIT NONE

    PRINT*,'LAND %% Zeroing diagnostic arrays'
    diag_force(:,:)      = 0.0
    diag_var(:,:)        = 0.0
    diag_var_tile(:,:,:) = 0.0    

    RETURN

  END SUBROUTINE zero_land_diags

  !#######################################################################################
  ! deallocate land diagnostic arrays
  !#######################################################################################
  SUBROUTINE deallocate_land_diags

    IMPLICIT NONE

    if (ALLOCATED(diag_force))    DEALLOCATE(diag_force)
    if (ALLOCATED(diag_var))      DEALLOCATE(diag_var)
    if (ALLOCATED(diag_var_tile)) DEALLOCATE(diag_var_tile)

    RETURN

  END SUBROUTINE deallocate_land_diags

END MODULE land_diags
