!##################################################################
!
!##################################################################
PROGRAM make_runoff_embm

  USE make_runoff_map

  IMPLICIT NONE

  INTEGER,PARAMETER :: nx=36
  INTEGER,PARAMETER :: ny=36
  INTEGER,DIMENSION(0:nx+1,0:ny+1) :: lsmask
  REAL :: lats(ny,2), lons(nx,2)

  INTEGER :: i,j

  OPEN(unit=55,file="../../../../genie-goldstein/data/input/worbe2.k1")  
  READ(55,"(38I3)") ((lsmask(i,j),i=0,nx+1),j=ny+1,0,-1)
  PRINT("(38I3)"),lsmask
  CLOSE(55)

  CALL calc_latlon(lats,lons)
  CALL initialise_tethys(1,90,ny,nx,lsmask(1:nx,1:ny),lats,lons,'runoff_mask_gold36x36.nc')
  CALL makemaps('routeing_embm_trip.dat')
  DEALLOCATE(ij_lando,ij_runoff)
  CALL initialise_tethys(3,90,ny,nx,lsmask(1:nx,1:ny),lats,lons,'runoff_mask_gold36x36_orig.nc')
  CALL makemaps('routeing_embm_old.dat')

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

    !CALC LATITUDES
    pi=4.0*ATAN(1.0)
    s0=SIN(-pi/2)
    s1=SIN( pi/2)
    ds=(s1-s0)/REAL(ny)
    DO j=1,ny
      lats(j,1)=-ASIN(s0+ds*(REAL(j)-1.0))*(180.0/pi)
      lats(j,2)=-ASIN(s0+ds*(REAL(j)))*(180.0/pi)
    ENDDO
    lats(:,:) = -1.0*lats(:,:)

    !CALC LONGITUDES
    ds=360.0/REAL(nx)
    s0=-260.0
    DO i=1,nx
      lons(i,1)=s0+REAL(i-1)*ds
      lons(i,2)=s0+REAL(i)*ds
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

END PROGRAM make_runoff_embm
