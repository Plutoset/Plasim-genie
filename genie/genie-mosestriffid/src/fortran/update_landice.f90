!***************************************************************************
! Subroutine to update the land tile fractions and vegetation carbon
! following an update of the icesheet model.
! This routine,
!  (i) [DONE] Updates ice fraction on land_pts and moves any lost vegetation carbon
!      to the soil carbon reservoir
! (ii) [TODO] Updates the list of "extra" land points corresponding to icesheet 
!      over ocean
!(iii) [TODO] Diagnoses the runoff rate applicable for until the next icesheet update
! 
! PPH 10/11/04
!***************************************************************************
SUBROUTINE update_landice(icefrac_atm,net_water_in_atm,iconv4lnd)

  USE land_const, only : kr_g, land_pts, nlat, nlon, npft, soil
  USE land_var, only : ij_land, frac, veg_frac, frac_vs, cs, cv,  &
                       lying_snow, msoil, dt_land,                &
                       ices_sum_evap,ices_sum_precip,ices_msoil0, &
                       ices_lying_snow0,ices_mean_drainage,       &
                       ices_steps_since_ice

  IMPLICIT NONE

  !**********************************************************************
  ! Input variables
  !**********************************************************************
  REAL(kr_g),DIMENSION(nlon,nlat),INTENT(in) :: icefrac_atm      ! Land-ice fraction from ice-sheet model
  REAL(kr_g),DIMENSION(nlon,nlat),INTENT(in) :: net_water_in_atm ! Freshwater flux assimilated by icesheet (m/s)
  INTEGER,INTENT(inout) :: iconv4lnd

  !**********************************************************************
  ! Work variables
  !**********************************************************************
  INTEGER :: i,j,l     ! Loop counters
  REAL    :: df        ! Increment in land-ice fraction
  REAL    :: d_ice, d_smc, d_snow ! Increment in icesheet, soil moisture and lying_snow masses (mm H2O)
  REAL    :: rtv       ! Temporary variable, used in various calculations
  INTEGER :: itv       ! Temporary variable, used in various calculations

  !**********************************************************************
  ! Diagnose the runoff rate the ocean sees for the next icesheet period
  ! as the difference between the net surface water mass flux (P minus E) 
  ! and the change in the three water reservoirs (icesheet, soil, snow).
  ! If this runoff rate is negative, then adjust (reduce) the soil moisture
  ! reservoir to balance the previous years water budget.  If there is
  ! insufficient soil moisture output an error message.
  !**********************************************************************
  rtv    = 0.0
  d_ice  = 0.0
  d_smc  = 0.0
  d_snow = 0.0
  DO l = 1,land_pts
    i  = ij_land(l,1)
    j  = ij_land(l,2)

    d_ice  = REAL(net_water_in_atm(i,j))*(REAL(ices_steps_since_ice)*dt_land*1e-3)
    d_smc  = msoil(l) - ices_msoil0(l)
    d_snow = lying_snow(l) - ices_lying_snow0(l)
    rtv    = ices_sum_precip(l) - ices_sum_evap(l) - (d_ice + d_smc + d_snow)
    IF(rtv < 0.0) THEN
      ices_mean_drainage(l) = 0.0
      IF(ABS(rtv) <= msoil(l)) THEN
        msoil(l) = msoil(l) + rtv
      ELSE
        PRINT*,'LAND %% ERROR %% Water source after icesheet update = ',msoil(l) + rtv,l
        msoil(l) = 0.0
      ENDIF
    ELSE
      ices_mean_drainage(l) = rtv
    ENDIF
    !******************************************************************
    ! Setup/reset variables for next call
    !******************************************************************
    ices_msoil0(l)      = msoil(l)
    ices_lying_snow0(l) = lying_snow(l)
    ices_sum_precip(l)  = 0.0
    ices_sum_evap(l)    = 0.0
  ENDDO

  !**********************************************************************
  ! Handle changes to the ice-sheet over 
  !**********************************************************************
  df  = 0.0
  rtv = 0.0
  DO l = 1,land_pts
    i  = ij_land(l,1)
    j  = ij_land(l,2)
    df = REAL(icefrac_atm(i,j)) - frac(soil+1,l)
    IF (df > 0.0) THEN
      !******************************************************************
      ! If ice-sheet expands areally in this gridbox, then reduce the 
      ! fractional coverage of the other tiles and transfer the lost 
      ! vegetation carbon in the soil carbon pool
      !******************************************************************
      rtv            = df/frac_vs(l)
      frac(1:soil,l) = (1.0 - rtv)*frac(1:soil,l)
      cs(l)          = cs(l) + rtv*cv(l)
      cv(l)          = (1.0 - rtv)*cv(l)
      frac(soil+1,l) = frac(soil+1,l) + df
    ELSE
      !******************************************************************
      ! If ice-sheet shrinks areally in this gridbox, then increase the
      ! bare soil fraction to fill the gap.
      !******************************************************************
      frac(soil,l)   = frac(soil,l)   - df
      frac(soil+1,l) = frac(soil+1,l) + df
    ENDIF
  ENDDO
  veg_frac(:) = SUM(frac(1:npft,:),DIM=1)
  frac_vs(:)  = SUM(frac(1:soil,:),DIM=1)

  !**********************************************************************
  ! TODO Handle those ocean points that are land-ice
  !**********************************************************************
!  DO l = 1,land_pts_extra
!  ENDDO
  itv = COUNT(icefrac_atm > 0.0)
  IF(itv > land_pts) THEN
    PRINT*,'LAND %% WARNING %% Looks like you have ',land_pts,' land points'
    PRINT*,'      and ', itv,' gridboxes with some icesheet.  This land'
    PRINT*,'      scheme does not handle iceheet over ocean points yet.'
  ENDIF

  ! Reset the flag now that the information from the icesheet update
  ! has been used.
  iconv4lnd = 0

  RETURN

END SUBROUTINE update_landice
