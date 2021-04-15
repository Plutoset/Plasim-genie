!----------------------------------------------------------------------
! Update the vegetation areal coverages, structural parameters,
! and soil carbon.
!----------------------------------------------------------------------
SUBROUTINE tstep_triffid(fx_co2)

  USE land_const, only : land_pts, npft, yearlen_land, gamma_eq, iter_eq, albsnf_nvg, soil
  USE land_var, only : l_phen_on, dt_trif, tile_index, frac_vs, veg_frac, l_equil_on, tile_pts, &
                       g_leaf_acc, g_leaf_phen_acc, npp_dr, gpp_dr,                             &
                       resp_s_dr, resp_w_dr, cs, frac, ht, lai, c_veg, cv,                      &
                       lit_c, lit_c_t, albsnc, albsnf, z0, z0_gb, dt_land

  IMPLICIT NONE

  REAL,INTENT(out),DIMENSION(land_pts) :: fx_co2 ! (kg C/m2/s)

  INTEGER :: kiter
  INTEGER :: trif_pts
  INTEGER :: l, ii, n                 ! Loop counters
  REAL    :: forw, gamma, gam_trif
  INTEGER,DIMENSION(land_pts)   :: trif_index
  REAL,DIMENSION(land_pts)      :: fraca
  REAL,DIMENSION(land_pts)      :: catch, albsoil
  REAL,DIMENSION(land_pts,npft) :: g_leaf_dr
  REAL,DIMENSION(land_pts)      :: dcs,dcv

  print*,'LAND %% Calling TRIFFID!'

  !----------------------------------------------------------------------
  ! Only operate on land points with space for vegetation to grow
  !----------------------------------------------------------------------
  trif_pts = 0
  DO l=1,land_pts
    IF (frac_vs(l).GE.(npft*1.0E-6)) THEN
      trif_pts = trif_pts + 1
      trif_index(trif_pts) = l
    ENDIF
  ENDDO

  !----------------------------------------------------------------------
  ! Set parameters depending on whether in equilibrium or transient mode
  !----------------------------------------------------------------------
  gam_trif = yearlen_land/dt_trif
  IF(l_equil_on) THEN
    forw  = 1.0
    gamma = gamma_eq
    kiter = iter_eq
  ELSE
    forw  = 0.0
    gamma = gam_trif
    kiter = 1
  ENDIF

  !----------------------------------------------------------------------
  ! Units conversion for accumulated carbon fluxes kgC/m2 --> kgC/m2/yr
  !----------------------------------------------------------------------
  resp_s_dr(:)   = resp_s_dr(:)*gam_trif
  resp_w_dr(:,:) = resp_w_dr(:,:)*gam_trif
  npp_dr(:,:)    = npp_dr(:,:)*gam_trif

  ! If phenology option is being used then use leaf turnover rate from 
  ! phenol(), else use the basic one from leaf_lit().
  g_leaf_dr(:,:) = 0.0
  IF(l_phen_on) THEN
    g_leaf_dr(:,:) = g_leaf_phen_acc(:,:)*gam_trif
  ELSE
    g_leaf_dr(:,:) = g_leaf_acc(:,:)*gam_trif
  ENDIF

  !----------------------------------------------------------------------
  ! Save old reservoir values for calculating changes on this timestep
  ! for flux to the atmosphere
  !----------------------------------------------------------------------
  DO l=1,land_pts
    dcs(l) = cs(l)
    dcv(l) = cv(l)
  ENDDO

  !----------------------------------------------------------------------
  ! Call TRIFFID
  !----------------------------------------------------------------------
  fraca(:) = 0.0
  DO ii=1,kiter
    CALL triffid (land_pts,trif_pts,trif_index,forw,gamma, &
                  frac_vs,fraca,g_leaf_dr,                 &
                  npp_dr,resp_s_dr,resp_w_dr,              &
                  cs,frac,ht,lai,c_veg,cv,lit_c,lit_c_t    )
  ENDDO

  !----------------------------------------------------------------------
  ! Calculate net atmos-to-land CO2 flux for atmos CO2 update from
  ! changes to carbon reservoirs.  Divide by the land timestep as the 
  ! atmos will see carbon flux as a pulse over a single land step with
  ! zero flux on other land timesteps.
  !----------------------------------------------------------------------
  DO L=1,LAND_PTS
    DCS(L) = CS(L) - DCS(L)
    DCV(L) = CV(L) - DCV(L)
    fx_co2(L) = (DCV(L) + DCS(L))/dt_land
  ENDDO

  !----------------------------------------------------------------------
  ! Zero the accumulated driving variables
  !----------------------------------------------------------------------
  resp_s_dr(:)         = 0.0
  gpp_dr(:,:)          = 0.0
  npp_dr(:,:)          = 0.0
  resp_w_dr(:,:)       = 0.0
  g_leaf_acc(:,:)      = 0.0
  g_leaf_phen_acc(:,:) = 0.0

  !----------------------------------------------------------------------
  ! Derive vegetation parameters from the new areal fractions and
  ! structural properties.
  !----------------------------------------------------------------------
  albsoil(:) = albsnf_nvg(1)
  DO n=1,npft
    CALL pft_sparm (land_pts,n,tile_index(:,n),tile_pts(n),    &
                    albsoil,ht(:,n),lai(:,n),                  &
                    albsnc(:,n),albsnf(:,n),catch(:),z0(:,n)   )
  ENDDO

  ! Calc GBM surface roughness length
  CALL calc_z0_gbm(z0_gb)

  ! Define other vegetation parameters
  veg_frac(:) = 0.0
  frac_vs(:)  = 0.0
  veg_frac(:) = SUM(frac(:,1:npft),DIM=2)
  frac_vs(:)  = veg_frac(:) + frac(:,soil)

  RETURN

END SUBROUTINE tstep_triffid
