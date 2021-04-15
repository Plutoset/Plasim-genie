!##################################################################
! Program that forms a wrapper on the IGCM atmos grid at T21 
! resolution (64x32) for the generic subroutine make_new_restart().
!
! PPH 24/08/04
!##################################################################
PROGRAM get_atm_grid

  USE make_restart

  IMPLICIT NONE

  INTEGER,PARAMETER :: nlat_atm = 32
  INTEGER,PARAMETER :: nlon_atm = 64
  INTEGER,PARAMETER :: kmax     = 1
  REAL,DIMENSION(nlon_atm,nlat_atm) :: landseamask_atm
  REAL,DIMENSION(nlat_atm,2) :: lats_atm
  REAL,DIMENSION(nlon_atm,2) :: lons_atm
  INTEGER :: nco1,ifail1

  REAL,DIMENSION(nlat_atm) :: igcm_lats = &
    (/ 85.76059,  80.26877,  74.74454,  69.21297,  63.67863, 58.14296,              &
       52.60652,  47.06964,  41.53246,  35.99508,  30.45755, 24.91993, 19.38223,    &
       13.84448,   8.306703,  2.768903, -2.768903, -8.306703, -13.84448, -19.38223, &
      -24.91993, -30.45755, -35.99508, -41.53246, -47.06964, -52.60652,             &
      -58.14296, -63.67863, -69.21297, -74.74454, -80.26877, -85.76059             /)

  REAL,DIMENSION(nlon_atm) :: igcm_lons = &
    (/ 0.0, 5.625, 11.25, 16.875, 22.5, 28.125, 33.75, 39.375, 45.0,            &
     50.625, 56.25, 61.875, 67.5, 73.125, 78.75, 84.375, 90.0,95.625, 101.25,   &
     106.875, 112.5, 118.125, 123.75, 129.375, 135.0, 140.625, 146.25, 151.875, &
     157.5, 163.125, 168.75, 174.375, 180.0, 185.625, 191.25, 196.875, 202.5,   &
     208.125, 213.75, 219.375, 225.0, 230.625, 236.25, 241.875, 247.5, 253.125, &
     258.75, 264.375, 270.0, 275.625, 281.25, 286.875, 292.5, 298.125, 303.75,  &
     309.375, 315.0, 320.625, 326.25, 331.875, 337.5, 343.125, 348.75, 354.375 /)

  CALL open_file_nc('../../../../genie-igcm3/data/input/landmask_std_t21.nc',nco1)
  CALL get2d_data_nc(nco1,'lsm',nlon_atm,nlat_atm,landseamask_atm,ifail1)
  CALL closenc(nco1)

  CALL get_atm_bounds(lats_atm,lons_atm)
  print*,lons_atm(:,1)
  CALL make_new_restart(nlat_atm,nlon_atm,lats_atm,lons_atm,landseamask_atm)

CONTAINS

  SUBROUTINE get_atm_bounds(lats_atm,lons_atm)

    IMPLICIT NONE

    REAL,INTENT(out),DIMENSION(nlat_atm,2) :: lats_atm
    REAL,INTENT(out),DIMENSION(nlon_atm,2) :: lons_atm
    INTEGER :: i,j

    !####################################################
    ! Calc the IGCM gridbox bounds
    !####################################################
    lats_atm(1,1)        =  90.0
    lats_atm(nlat_atm,2) = -90.0
    DO j=2,nlat_atm
      lats_atm(j,1)   = 0.5*(igcm_lats(j-1)+igcm_lats(j))
      lats_atm(j-1,2) = lats_atm(j,1)
    ENDDO

    lons_atm(1,1)         = 0.0 - 360.0/REAL(nlon_atm)/2.0
    lons_atm(1,2)         = lons_atm(1,1) + 360.0/REAL(nlon_atm)
    DO i=2,nlon_atm
      lons_atm(i,1) = lons_atm(i-1,1) + 360.0/REAL(nlon_atm)
      lons_atm(i,2) = lons_atm(i-1,2) + 360.0/REAL(nlon_atm)
    ENDDO

    print*,nlon_atm

    RETURN

  END SUBROUTINE get_atm_bounds

END PROGRAM get_atm_grid
