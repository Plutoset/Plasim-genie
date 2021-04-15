!#######################################################################################
! Module containing runoff routines that can be called on-line
!#######################################################################################
MODULE land_runoff

  USE land_const, only : land_pts, nlon, nlat, kr_nc, kr_g
  USE land_var, only : ij_land, lnd_weights, lnd_weights2

  IMPLICIT NONE
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: ij_runoff

CONTAINS
  !###################################################
  ! Read a preprepared netCDF file of runoff 
  ! destinations.  File name specified in job script
  !###################################################
  SUBROUTINE read_runoff_dests(flname,landseamask,kmax)

    USE genie_util, only : check_iostat

    IMPLICIT NONE
    ! args
    CHARACTER(len=*),INTENT(in) :: flname
    INTEGER,INTENT(in),DIMENSION(nlon,nlat) :: landseamask
    INTEGER,INTENT(in) :: kmax
    ! locals
    INTEGER,PARAMETER :: nxy = 2
    INTEGER,DIMENSION(nlon,nlat,2) :: ij_runoff_xy
    REAL(kr_nc),DIMENSION(nlon,nlat,2) :: workreal
    INTEGER :: ifail,ncid                                    !netCDF handles
    INTEGER :: n,i,j                                         !Loop counters
    INTEGER :: im,jm                                         !Work variables
    INTEGER :: alloc_stat                                    !memory checks

    !=======================================================
    ! Set module stored variables
    !=======================================================
    ALLOCATE(ij_runoff(land_pts,nxy),stat=alloc_stat)
    call check_iostat(alloc_stat,__LINE__,__FILE__)

    !=======================================================
    ! Open existing netCDF file, get destination indices,
    ! close file.
    !=======================================================
    CALL open_file_nc(flname,ncid)
    CALL get3d_data_nc(ncid,'destination_indices',nlon,nlat,nxy,workreal,ifail)
    CALL closenc(ncid)

    ! Cast real stored variable to integer
    ij_runoff_xy(:,:,:) = NINT(workreal(:,:,:))

    n=0
    DO j=1,nlat
      DO i=1,nlon
        IF(landseamask(i,j)>=kmax) THEN
          n=n+1
          ij_runoff(n,1) = ij_runoff_xy(i,j,1)
          ij_runoff(n,2) = ij_runoff_xy(i,j,2)
        ENDIF
      ENDDO
    ENDDO

    im = MAXVAL(ij_runoff(:,1))
    jm = MAXVAL(ij_runoff(:,2))
    IF(im>nlon .or. jm>nlat) THEN
      PRINT*,'LAND %% Input file has runoff index outside of current atmos grid'
      PRINT*,'LAND %% ',im,jm
      STOP
    ENDIF

    RETURN

  END SUBROUTINE read_runoff_dests

  !###################################################
  ! Subroutine to route and weight runoff from land
  ! gridboxes.  Runoff rate calculated in land_state()
  ! is weighted by the ratio of source box area to 
  ! destination box area.  Runoff from endorheic land 
  ! points is saved to be added to evap mass flux.
  !###################################################
  SUBROUTINE runoff_route(drain,total_runoff_atm,land_evap_atm)

    IMPLICIT NONE
    REAL(kr_g),DIMENSION(nlon,nlat),INTENT(in)    :: drain            ! Runoff rate of land points (kg/m2/s)
    REAL(kr_g),DIMENSION(nlon,nlat),INTENT(out)   :: total_runoff_atm ! Total runoff rate on wet coastal points (kg/m2/s)
    REAL(kr_g),DIMENSION(nlon,nlat),INTENT(inout) :: land_evap_atm    ! Evaporation mass flux from land points (kg/m2/s)

    INTEGER,DIMENSION(2) :: tij, sij  ! Indices of destination and source points on atmos grid
    INTEGER :: n                      ! Loop counter

    total_runoff_atm(:,:)   = 0.0
    DO n=1,land_pts
      tij(:) = ij_runoff(n,:)
      sij(:) = ij_land(n,:)
      IF(ALL(tij==0)) THEN
        land_evap_atm(sij(1),sij(2)) = land_evap_atm(sij(1),sij(2)) - drain(sij(1),sij(2))
      ELSE
        total_runoff_atm(tij(1),tij(2)) = total_runoff_atm(tij(1),tij(2)) &
                   + drain(sij(1),sij(2)) * REAL(lnd_weights(n)/lnd_weights2(tij(1),tij(2)),KIND=kr_g)
      ENDIF
    ENDDO

    RETURN

  END SUBROUTINE runoff_route

  !###################################################
  ! Memory cleanup
  !###################################################
  SUBROUTINE deallocate_land_runoff

    implicit none

    if (ALLOCATED(ij_runoff)) DEALLOCATE(ij_runoff)

    RETURN

  END SUBROUTINE DEALLOCATE_LAND_RUNOFF

END MODULE land_runoff
