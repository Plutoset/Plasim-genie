!##################################################################
!
!##################################################################
PROGRAM make_runoff_igcm

  USE make_runoff_map
  USE land_general_utils

  IMPLICIT NONE

  INTEGER,PARAMETER :: nx=64
  INTEGER,PARAMETER :: ny=32
  REAL,DIMENSION(nx,ny) :: lsmask
  INTEGER,DIMENSION(nx,ny) :: lsmaski
  REAL :: lats(ny,2), lons(nx,2)

  INTEGER :: i,j,nco1,ifail1

  !####################################################
  ! Read IGCM land/sea mask file
  !####################################################
  CALL open_file_nc('../../../../genie-igcm3/data/input/landmask_std_t21.nc',nco1)
  CALL get2d_data_nc(nco1,'lsm',nx,ny,lsmask,ifail1)
  CALL closenc(nco1)

  CALL calc_latlon(lats,lons)
  lsmaski=NINT(lsmask)
  lats=REFLECT(lats,2)
  print*,REFLECT(lats,2)
  CALL initialise_tethys(1,1,ny,nx,lsmaski,lats,lons,'runoff_mask_igcm64x32.nc')
  CALL makemaps('routeing_igcm.dat')

CONTAINS

  !##################################################################
  ! Calculate the longitudes/latitudes of gridbox edges for GOLDSTEIN
  ! grid.
  !##################################################################
  SUBROUTINE calc_latlon(lats,lons)

    IMPLICIT NONE
    REAL :: pi,ds,s0,s1
    REAL,INTENT(out) :: lats(ny,2), lons(nx,2)
    INTEGER :: i,j

    REAL,DIMENSION(ny) :: igcm_lats = &
    (/ 85.76059,  80.26877,  74.74454,  69.21297,  63.67863, 58.14296,              &
       52.60652,  47.06964,  41.53246,  35.99508,  30.45755, 24.91993, 19.38223,    &
       13.84448,   8.306703,  2.768903, -2.768903, -8.306703, -13.84448, -19.38223, &
      -24.91993, -30.45755, -35.99508, -41.53246, -47.06964, -52.60652,             &
      -58.14296, -63.67863, -69.21297, -74.74454, -80.26877, -85.76059             /)

    REAL,DIMENSION(nx) :: igcm_lons = &
    (/ 0.0, 5.625, 11.25, 16.875, 22.5, 28.125, 33.75, 39.375, 45.0,            &
     50.625, 56.25, 61.875, 67.5, 73.125, 78.75, 84.375, 90.0,95.625, 101.25,   &
     106.875, 112.5, 118.125, 123.75, 129.375, 135.0, 140.625, 146.25, 151.875, &
     157.5, 163.125, 168.75, 174.375, 180.0, 185.625, 191.25, 196.875, 202.5,   &
     208.125, 213.75, 219.375, 225.0, 230.625, 236.25, 241.875, 247.5, 253.125, &
     258.75, 264.375, 270.0, 275.625, 281.25, 286.875, 292.5, 298.125, 303.75,  &
     309.375, 315.0, 320.625, 326.25, 331.875, 337.5, 343.125, 348.75, 354.375 /)

    !####################################################
    ! Calc the IGCM gridbox bounds (:,1)=upper (:,2)=lower
    !####################################################
    lats(1,1)  =  90.0
    lats(ny,2) = -90.0
    DO j=2,ny
      lats(j,1)   = 0.5*(igcm_lats(j-1)+igcm_lats(j))
      lats(j-1,2) = lats(j,1)
    ENDDO

    lons(1,1) = 0.0 - 360.0/REAL(nx)/2.0
    lons(1,2) = lons(1,1) + 360.0/REAL(nx)
    DO i=2,nx
      lons(i,1) = lons(i-1,2) 
      lons(i,2) = lons(i,1) + 360.0/REAL(nx)
    ENDDO

    RETURN
  END SUBROUTINE calc_latlon

  !##################################################################
  ! Write out a list of lats/lons for each land point and it's 
  ! runoff destination for use in plotting nice pictures of the GENIE
  ! runoff routeing.
  !##################################################################
  SUBROUTINE makemaps(flname)
    
    IMPLICIT NONE    

    CHARACTER(len=*),INTENT(in)   :: flname
    
    REAL :: x_source,y_source,x_dest,y_dest
    INTEGER :: i

    OPEN(UNIT=88,file=flname)
    DO i=1,nout_lnd
      x_source = 0.5*SUM(lons(ij_lando(i,1),:))
      y_source = 0.5*SUM(lats(ij_lando(i,2),:))
      x_dest   = 0.5*SUM(lons(ij_runoff(i,1),:))
      y_dest   = 0.5*SUM(lats(ij_runoff(i,2),:))
      IF(ij_runoff(i,1)==0 .and. ij_runoff(i,2)==0) THEN
        x_dest = 1e4
        y_dest = 1e4
      ENDIF
      WRITE(88,'(4(E15.5,X))') x_source,y_source,x_dest,y_dest
    ENDDO
    CLOSE(88)

    RETURN

  END SUBROUTINE makemaps

END PROGRAM make_runoff_igcm
