!**********************************************************************
! Routine to update land surface prognostic variables
! (sub-surface moisture, sub-surface temperature, lying snow mass).
! Also diagnoses runoff and snowmelt
!
! Original version written by P.Cox for K.Meissner @ UVic
!
! (27/08/2003) PPH Correction to prevent negative soil moisture
! (26/03/2004) PPH Added land-ice tile type
! (01/07/2004) PPH Corrected to conserve energy during snowmelt 
! (24/09/2004) PPH Quite a few changes to properly account for land-ice fraction
! 
!**********************************************************************
SUBROUTINE LAND_STATE (FRACICE,FX_G,RAIN,SNOW,EVAP,ESUB,   &
                       MSOIL,LYING_SNOW,TS1,RUNOFF,SNOWMELT_acc)


  USE phys_const, only : con_latfus, con_zeroc
  USE land_const
  USE land_var, only : timestep
  IMPLICIT NONE

  !-----------------------------------------------------------------------
  ! Input only
  !-----------------------------------------------------------------------  
  REAL,INTENT(in),DIMENSION(land_pts) :: FX_G            ! Ground heat flux (W/m2).
  REAL,INTENT(in),DIMENSION(land_pts) :: FRACICE         ! Areal coverage of land-ice in gridbox.
  REAL,INTENT(in),DIMENSION(land_pts) :: RAIN            ! Rainfall rate (kg/m2/s).
  REAL,INTENT(in),DIMENSION(land_pts) :: SNOW            ! Snowfall rate (kg/m2/s).

  !-----------------------------------------------------------------------
  ! Output and modified
  !-----------------------------------------------------------------------  
  REAL,INTENT(inout),DIMENSION(land_pts) :: EVAP           ! Evapotranspiration rate (kg/m2/s).
  REAL,INTENT(inout),DIMENSION(land_pts) :: ESUB           ! Sublimation rate (kg/m2/s).
  REAL,INTENT(inout),DIMENSION(land_pts) :: MSOIL          ! Soil moisture (kg/m2).
  REAL,INTENT(inout),DIMENSION(land_pts) :: LYING_SNOW     ! Lying snow water equivalent (kg/m2).
  REAL,INTENT(inout),DIMENSION(land_pts) :: TS1            ! Soil temperature (K).
  REAL,INTENT(out),DIMENSION(land_pts)   :: RUNOFF         ! Runoff (kg/m2/s).
  REAL,INTENT(out),DIMENSION(land_pts)   :: SNOWMELT_acc   ! Accumulated snowmelt (kg/m2/s).
  REAL,DIMENSION(land_pts)   :: SNOWMELT                   ! Snowmelt (kg/m2/s).

  !-----------------------------------------------------------------------
  ! Local parameters
  !-----------------------------------------------------------------------
  INTEGER :: L                    ! Loop counter.
  REAL :: HCAP                    ! Gridbox sub-surace heat capacity (J/m3/K)
  REAL :: dmass                   ! Snow/Soil moisture increment (kg/m2)
  REAL :: ttest                   ! Soil temperature if all soil heat flux applied (K)
  REAL :: drain                   ! WORK Runoff rate (kg/m2/s)

  dmass = 0.0
  ttest = 0.0

  runoff(:)   = 0.0
  snowmelt(:) = 0.0 
  DO L=1,LAND_PTS
    hcap = hcap_nvg(1)

    !----------------------------------------------------------------------
    ! Update the lying snow from snowfall and sublimation
    !----------------------------------------------------------------------
    dmass = (snow(l)-esub(l)) * timestep 
    IF((-dmass).GT.lying_snow(l)) THEN
      esub(l) = lying_snow(l)/timestep
      lying_snow(l) = 0.0
    ELSE
      lying_snow(l) = lying_snow(l) + dmass
    ENDIF

    !----------------------------------------------------------------------
    ! Update the soil moisture from non-ice frac rainfall and evaporation
    !   (i) Calculate the drainage rate
    !  (ii) Calculate the net flux of water into soil bucket
    ! (iii) If a net withdrawl exceeds bucket content then limit 
    !       evaporation to rainfall + bucket content
    !  (iv) Else increment bucket content and add drainage to runoff
    !----------------------------------------------------------------------
    drain = drain_max * MAX(0.0,(msoil(l)/msat - v_wilt)/(1.0 - v_wilt))
    dmass = ( (1.0-fracice(l))*rain(l)-evap(l) ) * timestep 
    IF(dmass.LT.(-msoil(l))) THEN
      evap(l) = (1.0-fracice(l))*rain(l) + msoil(l)/timestep
      msoil(l) = 0.0
    ELSE
      msoil(l)  = msoil(l) + dmass
    ENDIF

    dmass = -1*drain*timestep
    IF(dmass.LT.(-msoil(l))) THEN
      msoil(l)  = msoil(l) + dmass
      runoff(l) = runoff(l) + drain
    ENDIF

    !----------------------------------------------------------------------
    ! Add ice frac rainfall to runoff 
    !----------------------------------------------------------------------
    runoff(l) = runoff(l) + fracice(l) * rain(l)

    !----------------------------------------------------------------------
    ! Update the soil temperature and diagnose snowmelt.
    !----------------------------------------------------------------------
    !If there's no snow cover, use all ground heat flux 
    ! to calculate new soil temperature
    !If there's snow cover, use ground heat flux to 
    !  (i) raise sub-surface temperature to zero deg C
    ! (ii) melt snow
    !(iii) raise sub-surface temperature above zero deg C
    !----------------------------------------------------------------------

    ttest      = ts1(l) + timestep * fx_g(l) / (dz_soil*hcap)
!bowie    ts1(l)      = ts1(l) + timestep * fx_g(l) / (dz_soil*hcap)
    snowmelt(l) = 0.0
    IF ((lying_snow(l).GT.0.0) .AND. (ttest.GT.con_zeroc)) THEN
      snowmelt(l) = hcap*dz_soil*(ttest-con_zeroc) / (con_latfus*timestep)
      IF(snowmelt(l)*timestep.GT.lying_snow(l)) THEN
        snowmelt(l) = lying_snow(l)/timestep
        ts1(l)      = ts1(l) + timestep*(fx_g(l)-con_latfus*snowmelt(l))/(dz_soil*hcap)
!        ts1(l)      = con_zeroc + timestep*(fx_g(l)-con_latfus*snowmelt(l))/(dz_soil*hcap)
      ELSE
        ts1(l) = con_zeroc
      ENDIF
    ELSE
      ts1(l) = ttest
    ENDIF

    !----------------------------------------------------------------------
    ! Update the soil moisture and lying_snow to account for snowmelt.
    ! Snowmelt over ice frac goes straight to runoff
    ! Snowmelt over non-ice frac goes to soil bucket
    ! Excess moisture in soil bucket added to runoff
    !----------------------------------------------------------------------
    lying_snow(l) = lying_snow(l) - snowmelt(l)*timestep
    runoff(l)     = runoff(l) +    fracice(l) * snowmelt(l)
    msoil(l)      = msoil(l)  + (1-fracice(l))* snowmelt(l)*timestep
    IF(msoil(l) > msat) THEN
      runoff(l) = runoff(l) + (msoil(l) - msat)/timestep
      msoil(l)  = msat
    ENDIF

    !----------------------------------------------------------------------
    ! Make sure snow depth doesn't exceed a maximum value after snowfall 
    ! and snowmelt.  If it does then add excess to runoff rate.
    ! This prevents the land becoming a sink of water in any version of 
    ! GENIE with a closed water cycle.
    !----------------------------------------------------------------------
    IF(lying_snow(l) > lying_snow_max) THEN
      dmass           = lying_snow(l) - lying_snow_max
      lying_snow(l)   = lying_snow_max
      runoff(l)       = runoff(l) + dmass/timestep
      snowmelt_acc(l) = snowmelt_acc(l) + dmass
    ENDIF
    snowmelt_acc(l) = snowmelt_acc(l) + snowmelt(l)*timestep
  ENDDO

  RETURN

END SUBROUTINE land_state

!     hcap = 0.0
!     hcap = (1.0-fracice(l))*hcap_nvg(1) + fracice(l)*hcap_nvg(2)
!     hcap = (msoil(l) + lying_snow(l))*hcap_s_wat
