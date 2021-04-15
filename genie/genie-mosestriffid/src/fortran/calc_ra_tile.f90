!#######################################################################################
! Subroutine to calculate aerodynamic resistance over a tile.
!
! PPH 22/07/04
!#######################################################################################
SUBROUTINE calc_ra_tile(windx,windy,z1,z0,chn,ra)

  USE phys_const, only : con_karman
  USE land_const, only : land_pts,ene_tune

  IMPLICIT NONE

  REAL,DIMENSION(land_pts),INTENT(in) :: windx, windy !*FD Zonal and meridional wind speeds (m/s)
  REAL,DIMENSION(land_pts),INTENT(in) :: z0           !*FD Roughness length (m)
  REAL,DIMENSION(land_pts),INTENT(in) :: z1           !*FD Reference height (m)

  REAL,DIMENSION(land_pts),INTENT(out) :: ra          !*FD Aerodynamic resistance (s/m)
  REAL,DIMENSION(land_pts),INTENT(out) :: chn         !*FD Coefficient (-)

  REAL,DIMENSION(land_pts) :: wind                    !*FD Wind speed (m/s)
  REAL,DIMENSION(land_pts) :: zetam, zetah            !*FD Work variables (-)
  INTEGER :: i                                        !*FD Loop counter

  DO i=1,land_pts
    chn(i) = 0.0
    ra(i)  = 0.0

    wind(i)  = SQRT(windx(i)**2 + windy(i)**2)
    zetam(i) = ALOG((z1(i) + z0(i)) / z0(i))
    zetah(i) = ALOG((z1(i) + z0(i)) / (0.1*z0(i)))
    chn(i)   = (con_karman * con_karman) / (zetah(i) * zetam(i))
!   ene_tune = 1.0 by default, but is now a tunable paramter.
    ra(i)    = ene_tune / ( chn(i) * wind(i) )
  ENDDO

  RETURN

END SUBROUTINE calc_ra_tile


