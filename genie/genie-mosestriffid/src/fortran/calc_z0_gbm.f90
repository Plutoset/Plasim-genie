!#######################################################################################
! Subroutine to calculate a gridbox representative value of roughness length following
! the method of Mason (1988), QJRMS, 114, pp399-420. Tile roughness length values are 
! aggregated at a blending height of 1/rl_blend.
!
! PPH 14/7/04
!#######################################################################################
SUBROUTINE calc_z0_gbm(z0_gb)

  USE land_const, only : land_pts,ntype,npft,nnvg,z0_nvg,rl_blend
  USE land_var, only : frac,z0

  IMPLICIT NONE

  REAL,DIMENSION(land_pts),INTENT(out)     :: z0_gb  !GBM roughness length (m)

  INTEGER :: n
  REAL,DIMENSION(land_pts)      :: work_sum
  REAL,DIMENSION(land_pts,npft) :: fact1
  REAL,DIMENSION(land_pts,nnvg) :: fact2

  work_sum(:) = 0.0
  fact1(:,:)  = 0.0
  fact2(:,:)  = 0.0

  DO n=1,land_pts
    fact1(n,:) = frac(n,1:npft)*(LOG(z0(n,:)*rl_blend))**(-2)
    fact2(n,:) = frac(n,npft+1:ntype)*(LOG(z0_nvg(:)*rl_blend))**(-2)
  ENDDO

  work_sum(:) = SUM(fact1(:,:),DIM=2)               !veg. tile contributions
  work_sum(:) = work_sum(:) + SUM(fact2(:,:),DIM=2) !non-veg. tile contributions

  z0_gb(:) = EXP(-1.0 / SQRT(work_sum(:))) / rl_blend

  RETURN

END SUBROUTINE calc_z0_gbm
