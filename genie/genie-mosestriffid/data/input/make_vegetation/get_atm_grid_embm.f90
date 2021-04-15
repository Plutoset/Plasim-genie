!##################################################################
! Program that forms a wrapper on the EMBM atmos grid (36x36) for 
! the generic subroutine make_new_restart().
!
! PPH 24/08/04
!##################################################################
PROGRAM get_atm_grid

  USE make_restart

  IMPLICIT NONE

  INTEGER,PARAMETER :: nlat_atm = 36
  INTEGER,PARAMETER :: nlon_atm = 36
  INTEGER,PARAMETER :: kmax     = 90
  INTEGER,DIMENSION(0:nlon_atm+1,0:nlat_atm+1) :: lsmask
  REAL,DIMENSION(nlon_atm,nlat_atm) :: landseamask_atm
  REAL,DIMENSION(nlat_atm,2) :: lats_atm
  REAL,DIMENSION(nlon_atm,2) :: lons_atm
  INTEGER :: i,j

  OPEN(unit=55,file="../../../../genie-goldstein/data/input/worbe2.k1")  
  READ(55,"(38I3)") ((lsmask(i,j),i=0,nlon_atm+1),j=nlat_atm+1,0,-1)
  PRINT("(38I3)"),lsmask
  CLOSE(55)

  landseamask_atm(:,:) = 0.0
  WHERE(lsmask(1:nlon_atm,1:nlat_atm)>=kmax) landseamask_atm=1.0

  CALL get_atm_bounds(lats_atm,lons_atm)
  print*,lons_atm(:,1)
  CALL make_new_restart(nlat_atm,nlon_atm,lats_atm,lons_atm,landseamask_atm)

CONTAINS

  SUBROUTINE get_atm_bounds(lats_atm,lons_atm)

    IMPLICIT NONE

    REAL :: pi,ds,s0,s1
    REAL,INTENT(out),DIMENSION(nlat_atm,2) :: lats_atm
    REAL,INTENT(out),DIMENSION(nlon_atm,2) :: lons_atm
    INTEGER :: i,j

    !CALC LATITUDES
    pi=4.0*ATAN(1.0)
    s0=SIN(-pi/2)
    s1=SIN( pi/2)
    ds=(s1-s0)/REAL(nlat_atm)
    DO j=1,nlat_atm
      lats_atm(j,1)=-ASIN(s0+ds*(REAL(j)-1.0))*(180.0/pi)
      lats_atm(j,2)=-ASIN(s0+ds*(REAL(j)))*(180.0/pi)
    ENDDO
    lats_atm(:,:) = -1.0*lats_atm(:,:)

    !CALC LONGITUDES
    ds=360.0/REAL(nlon_atm)
    s0=-260.0
    DO i=1,nlon_atm
      lons_atm(i,1)=s0+REAL(i-1)*ds
      lons_atm(i,2)=s0+REAL(i)*ds
    ENDDO

    print*,nlon_atm

    RETURN

  END SUBROUTINE get_atm_bounds

END PROGRAM get_atm_grid
