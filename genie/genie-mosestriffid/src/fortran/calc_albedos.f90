!#######################################################################################
! Subroutine to calculate the gridbox mean and tile albedos.
!
! This was previously done by swrad.f, which also calculated net solar radiation over
! each tile.  The atmosphere calculates GBM net surface solar based on albedo calculated 
! by the land. The land then retrieves tile net solar based on tile albedo and GBM 
! albedo.
!
! PPH 20/10/04
!#######################################################################################
SUBROUTINE calc_albedos(frac, tstar, lying_snow, albsnf_pft, albsnc_pft, albedo_tiles, albedo_gb)

  USE land_const, only : land_pts, npft, nnvg, ntype, albsnf_nvg, albsnc_nvg
  IMPLICIT NONE

  REAL,DIMENSION(land_pts,ntype),INTENT(in)  :: frac                    ! Fractional cover of tiles (-)
  REAL,DIMENSION(land_pts,ntype),INTENT(in)  :: tstar                   ! Surface temperature on tiles (K)
  REAL,DIMENSION(land_pts),      INTENT(in)  :: lying_snow              ! Gridbox snow water equivalent (kg/m2)
  REAL,DIMENSION(land_pts,npft), INTENT(in)  :: albsnf_pft, albsnc_pft  ! Snow-free and -covered albedos of vegetation tiles (-)
  REAL,DIMENSION(land_pts,ntype),INTENT(out) :: albedo_tiles            ! Albedo of tiles accounting for snow (-)
  REAL,DIMENSION(land_pts),INTENT(out) :: albedo_gb                     ! Grid box mean albedo (-)

  REAL,DIMENSION(land_pts) :: albnvgf,albnvgc
  INTEGER :: i

  DO i=1,npft
    CALL calc_albedo_tile(albedo_tiles(:,i),albsnf_pft(:,i),albsnc_pft(:,i),tstar(:,i),lying_snow)
  ENDDO
  DO i=1,nnvg
    albnvgf(:) = albsnf_nvg(i)
    albnvgc(:) = albsnc_nvg(i)
    CALL calc_albedo_tile(albedo_tiles(:,npft+i),albnvgf,albnvgc,tstar(:,npft+i),lying_snow)
  ENDDO
  DO i=1,land_pts
    albedo_gb(i) = 0.0
    albedo_gb(i) = SUM(frac(i,:)*albedo_tiles(i,:))
  ENDDO

  RETURN

END SUBROUTINE calc_albedos


!#######################################################################################
! Subroutine to calculate the albedo of a tile taking into account the snow cover and
! the tile surface temperature.
! Based on the obselete routine swrad.f
!
! PPH 20/10/04
!#######################################################################################
SUBROUTINE calc_albedo_tile(albedo_tile,albsnf,albsnc,tstar,lying_snow)

  USE phys_const, only : con_zeroc
  USE land_const, only : land_pts
  IMPLICIT NONE
  REAL,DIMENSION(land_pts),INTENT(out) :: albedo_tile
  REAL,DIMENSION(land_pts),INTENT(in) :: albsnf
  REAL,DIMENSION(land_pts),INTENT(in) :: albsnc
  REAL,DIMENSION(land_pts),INTENT(in) :: tstar
  REAL,DIMENSION(land_pts),INTENT(in) :: lying_snow
  REAL :: albsnow, albsnow_min
  REAL,PARAMETER :: delt  = 2.0
  REAL,PARAMETER :: maskd = 5.0
  INTEGER :: i

  DO i=1,land_pts
    albedo_tile(i) = albsnf(i)
    albsnow        = albsnc(i)
    albsnow_min    = 0.7*albsnc(i) + 0.3*albsnf(i)
    IF (lying_snow(i) .GT. 0.0) THEN
      IF (tstar(i).GT.(con_zeroc - delt)) THEN
        albsnow = albsnc(i) + 0.3*(albsnf(i) - albsnc(i))*(tstar(i) - con_zeroc + delt)/delt
        albsnow = MAX(albsnow, albsnow_min)
      ENDIF
      albedo_tile(i) = albsnf(i) + (albsnow - albsnf(i)) *(1.0 - EXP(-lying_snow(i)/maskd))
    ENDIF
  ENDDO
  RETURN
END SUBROUTINE calc_albedo_tile
