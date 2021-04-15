!-----------------------------------------------------------------------
! Update leaf phenological state
! If L_PHEN_ON=.false. then the simple leaf turnover rate calculated in 
!  leaf_lit() is accumulated and used to force TRIFFID (g_leaf_acc).
! If L_PHEN_ON=.true. then LAI is a prognostic variable and the simple
!  leaf turnover rate is adjusted to reflect changing LAI, accumulated
!  and used to force TRIFFID.
!-----------------------------------------------------------------------
SUBROUTINE tstep_phenology

  USE land_const, only : land_pts, npft, yearlen_land
  USE land_var, only : tile_pts, tile_index, g_leaf_acc, g_leaf_phen_acc, ht, lai, dt_phen

  IMPLICIT NONE

  INTEGER :: n                          ! Loop counter
  REAL    :: dtime_phen                 ! Time units conversion factor ()
  REAL    :: g_leaf_phen(land_pts,npft) ! Leaf turnover rate after this phenology step (/year)


  dtime_phen = dt_phen/yearlen_land

  !----------------------------------------------------------------------
  ! Units conversion from turnover --> turnover/yr
  !----------------------------------------------------------------------
  g_leaf_acc(:,:)  = g_leaf_acc(:,:)/dtime_phen
  g_leaf_phen(:,:) = 0.0

  !----------------------------------------------------------------------
  ! Update leaf phenology
  !----------------------------------------------------------------------
  DO n=1,npft
    CALL phenol(land_pts,tile_pts(n),tile_index(:,n),n, &
                g_leaf_acc(:,n),ht(:,n),dtime_phen,     &
                g_leaf_phen(:,n),lai(:,n)               )
  ENDDO

  !----------------------------------------------------------------------
  ! Update the phenological leaf turnover rates used by TRIFFID
  !----------------------------------------------------------------------
  g_leaf_phen_acc(:,:) = g_leaf_phen_acc(:,:) + g_leaf_phen(:,:)*dtime_phen
  g_leaf_acc(:,:)      = 0.0

  RETURN

END SUBROUTINE tstep_phenology
