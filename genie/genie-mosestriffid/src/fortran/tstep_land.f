!$Id: tstep_land.f 2516 2005-07-15 10:41:46Z cvs-gw $
      SUBROUTINE TSTEP_LAND (LAND_INDEX,TILE_PTS,TILE_INDEX
     &,                TIMESTEP,dayfrac
     &,                co2_atm_l,LW,SW,PSTAR,Q,T,RAIN,SNOW,windx,windy
     &,                CS,FRAC,HT,LAI,LYING_SNOW,MSOIL,TS1,TSTAR
     &,                ALBSNC,ALBSNF,Z0,Z1
     &,                G_LEAF_ACC,GPP_DR,NPP_DR,RESP_S_DR,RESP_W_DR
     &,                E,ESUB,LE,TSTAR_GB,GPP,NPP,RESP_S
     &,                RUNOFF,SNOWMELT_ACC,ALBEDO_gb,swn_gb,RADNET,H,g
     &,                fx_mom_gb,albice,qsurf_gb,fx_surf,gc_gb
     &,                albedo_tiles)
!-----------------------------------------------------------------------
!
!      Subroutine containing simplified land model for Katrin (Jan 2001)
!
!      18/02/03 Include ALBSNOW, ALBSOIL as arguments (PMC)
!      19/02/03 Include G_LEAF_PHEN as argument to save between
!               timesteps (PMC)
!      19/02/03 Correct reseting of G_LEAF_ACC(1,N) to (L,N) (PMC)
!-----------------------------------------------------------------------
      USE phys_const, only : con_rhowat
      USE land_const

      IMPLICIT NONE

      INTEGER
     & LAND_INDEX(LAND_PTS)      ! IN Indices of land points.
     &,TILE_PTS(NPFT)            ! IN Number of land points which includ
C                                ! IN the nth vegetation type.
     &,TILE_INDEX(LAND_PTS,NPFT) ! IN Indices of land points which inclu
C                                !    the nth vegetation type.
!     &,TRIF_PTS                  ! WORK Number of land points where vegetation
!                                !    can grow
!     &,TRIF_INDEX(LAND_PTS)      ! WORK Indices of land points where vegetation
!                                !    can grow
     &,L,N                       ! Loop counters

!-----------------------------------------------------------------------
! Timesteps and logical flags
!-----------------------------------------------------------------------
      REAL
     & TIMESTEP                  ! IN Model timestep (s).
     &,dayfrac(land_pts)         ! IN Fraction of 24 hour when sun is above horizon (-)

!-----------------------------------------------------------------------
! Driving variables
!-----------------------------------------------------------------------
      REAL
     & co2_atm_l(land_pts)        ! IN Atm CO2 concentratrion (kg(CO2)/kg(air))
     &,LW(LAND_PTS)               ! IN Surface longwave radiation (W/m**2
     &,SW(LAND_PTS)               ! IN Surface shortwave radiation (W/m**
     &,PSTAR(LAND_PTS)            ! IN Surface pressure (Pa).
     &,Q(LAND_PTS)                ! IN Specific humidity (kg/kg).
     &,T(LAND_PTS)                ! IN Air temperature (K).
     &,RAIN(LAND_PTS)             ! IN Rainfall (kg/m2/s).
     &,SNOW(LAND_PTS)             ! IN Snowfall (kg/m2/s).
     &,windx(land_pts)            ! IN Zonal wind component (m/s).
     &,windy(land_pts)            ! IN Meridional wind component (m/s).
     &,PAR(LAND_PTS)              ! WORK Daily mean photosynthetically active radiation (W/m2).
     &,SWN(LAND_PTS)              ! OUT Net shortwave radiation (W/m**2)
     &,SWN_GB(LAND_PTS)           ! OUT Gridbox mean net shortwave radiation (W/m**2)

!-----------------------------------------------------------------------
! Disturbance prescription.
!-----------------------------------------------------------------------
      REAL Z1(LAND_PTS)            ! IN Reference height (m).
!     & ALBSNOW(LAND_PTS)          ! IN Snow albedo.
!     &,ALBSOIL(LAND_PTS)          ! IN Soil albedo.
!     &,FRACA(LAND_PTS)            ! IN Areal fraction of agriculture.
!     &,G_ANTH(LAND_PTS)           ! IN Anthropogenic disturbance rate (/y

!-----------------------------------------------------------------------
! Prognostic variables
!-----------------------------------------------------------------------
      REAL
     & CS(LAND_PTS)               ! INOUT Soil carbon (kg C/m2).
     &,FRAC(LAND_PTS,NTYPE)       ! INOUT Areal coverage.
     &,HT(LAND_PTS,NPFT)          ! INOUT Canopy height (m)
     &,LAI(LAND_PTS,NPFT)         ! INOUT Leaf area index.
     &,LYING_SNOW(LAND_PTS)       ! INOUT Snow mass (kg/m2).
     &,MSOIL(LAND_PTS)            ! INOUT Soil moisture (kg/m2).
     &,TS1(LAND_PTS)              ! INOUT Sub-surface temperature (K).
     &,TSTAR(LAND_PTS,NTYPE)      ! INOUT Surface temperature on veg tiles (K).

!-----------------------------------------------------------------------
! Other variables which need to be saved between calls
!-----------------------------------------------------------------------
      REAL
     & ALBSNC(LAND_PTS,NPFT)      ! INOUT Cold deep snow albedo.
     &,ALBSNF(LAND_PTS,NPFT)      ! INOUT Snow free albedo.
     &,Z0(LAND_PTS,NPFT)          ! INOUT Vegetative roughness length (m)
     &,G_LEAF_ACC(LAND_PTS,NPFT)  ! INOUT Daily mean leaf turnover rate (/yr).
     &,GPP_DR(LAND_PTS,NPFT)      ! INOUT Accumulated Gross Primary Productivity (kg C/m2/yr).
     &,NPP_DR(LAND_PTS,NPFT)      ! INOUT Accumulated Net Primary Productivity (kg C/m2/yr)
     &,RESP_S_DR(LAND_PTS)        ! INOUT Accumulated soil respiration rate (kg C/m2/yr).
     &,RESP_W_DR(LAND_PTS,NPFT)   ! INOUT Accumulated wood respiration rate (kg C/m2/yr).
!     &,VEG_FRAC(LAND_PTS)         ! INOUT Vegetated fraction.
!     &,FRAC_VS(LAND_PTS)          ! INOUT Total fraction of gridbox cover

!-----------------------------------------------------------------------
! Output diagnostics
!-----------------------------------------------------------------------
      REAL
     & E(LAND_PTS)                ! OUT Evapotranspiration (kg/m2/s).
     &,ESUB(LAND_PTS)             ! OUT Sublimation (kg/m2/s).
     &,LE(LAND_PTS)               ! OUT Latent heat flux (W/m2).
     &,TSTAR_GB(LAND_PTS)         ! OUT Effective radiative temperature
C                                 !     of gridbox (K).
     &,RUNOFF(LAND_PTS)           ! OUT Gravitational Drainage (kg/m2/s).
     &,SNOWMELT_ACC(LAND_PTS)     ! OUT Accumulated snowmelt (kg/m2).
     &,ALBICE(LAND_PTS)           ! OUT Albedo of land-ice tile (-)
     &,fx_mom_gb(land_pts,2)      ! OUT Zonal and meridonal surface momentum fluxes (kg/m/s2)
     &,fx_mom(land_pts,2)         !WORK Zonal and meridonal surface momentum fluxes
!                                 !     on a tile (kg/m/s2)
     &,qsurf_gb(land_pts)         ! OUT Near-surface specific humidity (kg/kg)
!     &,z0_gb(land_pts)            ! OUT Gridbox mean roughness length (m)
     &,gc_gb(land_pts)            ! OUT Gridbox mean canopy conductance (m/s)

      REAL :: albedo_gb(land_pts)        
      REAL :: albedo_tiles(land_pts,ntype)
!-----------------------------------------------------------------------
! Work Vegetation parameters
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Work Carbon variables.
!-----------------------------------------------------------------------
      REAL
     & GC(LAND_PTS,NPFT)          ! Canopy conductance (m/s).
     &,GPP(LAND_PTS,NPFT)         ! Gross Primary Productivity (kg C/m2/s
     &,G_LEAF(LAND_PTS,NPFT)      ! Leaf turnover rate (/yr).
!     &,LAI_BAL(LAND_PTS,NPFT)     ! Balanced growth LAI.
     &,NPP(LAND_PTS,NPFT)         ! Net Primary Productivity (kg C/m2/s).
     &,RESP_P(LAND_PTS,NPFT)      ! Plant respiration rate (kg C/m2/s).
     &,RESP_S(LAND_PTS)           ! Soil respiration rate (kg C/m2/s).
     &,RESP_W(LAND_PTS,NPFT)      ! Wood maintenance respiration rate
!                                 ! (kg C/m2/s).

!-----------------------------------------------------------------------
! Work Hydrology variables
!-----------------------------------------------------------------------
      REAL
     & ETRAN(LAND_PTS,NTYPE)      ! Transpiration (kg/m2/s).
     &,FSMC(LAND_PTS)             ! Moisture availability factor.
     &,G_T(LAND_PTS,NTYPE)        ! ground heat flux on tiles (W/m2).
     &,G(LAND_PTS)                ! GBM ground heat flux (W/m2).
     &,fx_surf_t(land_pts,ntype)  ! Surface heat flux on tiles (W/m2).
     &,fx_surf(land_pts)          ! GBM Surface heat flux (W/m2).
     &,H_T(LAND_PTS,NTYPE)        ! Sensible heat flux on veg tiles (W/m2
     &,H(LAND_PTS)                ! OUT Sensible heat flux (W/m2).
     &,LETRAN(LAND_PTS,NTYPE)     ! Latent heat of transpiration (W/m2).
     &,RADNET_T(LAND_PTS,NTYPE)   ! Net radiation on veg tiles (W/m2).
     &,RADNET(LAND_PTS)           ! Net Radiation (W/m2).
     &,V_ROOT(LAND_PTS)           ! Liquid volumetric soil moisture
C                                 ! concentration in the rootzone
C                                 ! (m3 H2O/m3 soil).


!-----------------------------------------------------------------------
! Other surface quantities
!-----------------------------------------------------------------------
      REAL
     & RA(LAND_PTS)               ! Aerodynamic resistance (s/m).
     &,RS(LAND_PTS)               ! Surface resistance (s/m).
     &,chn(land_pts)              ! Flux coefficient ()
     &,V_SAT(LAND_PTS)            ! Volumetric soil moisture
C                                 ! concentration at saturation
C                                 ! (m3 H2O/m3 soil).
     &,Z0S(LAND_PTS)              ! Soil roughness length (m).

      REAL,DIMENSION(LAND_PTS) :: tv !work variable

!-----------------------------------------------------------------------
! Approximate PAR, assume atmosphere is neutral and setup vector fields.
!-----------------------------------------------------------------------
      DO L=1,LAND_PTS
!-----------------------------------------------------------------------
! Estimate daily maximum PAR based on daylength and mean net solar
! Note that factor 2.0 * 0.5 = 1.0 is excluded from PAR calculation
!-----------------------------------------------------------------------
        tv(L)          = 1.0 / (1.0 - albedo_gb(L))
        PAR(L)         = SW(L) * tv(L) / MAX(dayfrac(l),0.1)
        V_SAT(L)       = 1.0
!-----------------------------------------------------------------------
! Zero arrays
!-----------------------------------------------------------------------
        RA(L)          = 0.0
!        FRACA(L)       = 0.0
!        G_ANTH(L)      = 0.0
        SWN_GB(L)      = 0.0
        FX_MOM_GB(L,:) = 0.0
        GC_GB(L)       = 0.0
        GC(L,:)        = 1.0E-6
!-----------------------------------------------------------------------
! Calculate the soil moisture stress factor.
!-----------------------------------------------------------------------
        V_ROOT(L) = MSOIL(L) / MSAT

        IF (V_ROOT(L) .GT. V_CRIT) THEN
          FSMC(L) = 1.0
        ELSEIF (V_ROOT(L) .LE. V_WILT) THEN
          FSMC(L) = 0.0
        ELSE
          FSMC(L) = (V_ROOT(L) - V_WILT)
     &            / (V_CRIT - V_WILT)
        ENDIF
      ENDDO

!***********************************************************************
!***********************************************************************
! CALCULATIONS ON VEGETATION TILE
!***********************************************************************
!***********************************************************************
      DO N=1,NPFT
!-----------------------------------------------------------------------
! Calculate net solar radiation over veg tile
!-----------------------------------------------------------------------
        DO L=1,LAND_PTS
          swn(L) = sw(L) * (1.0 - albedo_tiles(L,n))*tv(L)
          SWN_GB(L) = SWN_GB(L) + FRAC(L,N)*SWN(L)
        ENDDO
!-----------------------------------------------------------------------
! Calculate tile surface resistance and plant photosynthesis/respiration.
!-----------------------------------------------------------------------
        CALL calc_ra_tile(windx,windy,z1,z0(:,n),chn,ra)
        CALL SF_STOM (LAND_PTS,LAND_INDEX,LAND_PTS
     &,               TILE_PTS(N),TILE_INDEX(:,N),N,dayfrac
     &,               co2_atm_l,FSMC,HT(:,N),PAR,LAI(:,N),PSTAR
     &,               Q,RA,t(:)
     &,               GPP(:,N),NPP(:,N),RESP_P(:,N),RESP_W(:,N)
     &,               GC(:,N))

!-----------------------------------------------------------------------
! Calculate tile surface resistance
! If snow if lying assume no surface resistance
!-----------------------------------------------------------------------
        DO L=1,LAND_PTS
          RS(L) = 1.0 / GC(L,N)
cpph rs=0 causes instability
!          IF (LYING_SNOW(L) .GT. 0.0) RS(L)=1e6
        ENDDO

!-----------------------------------------------------------------------
! Solve the surface energy balance.
!-----------------------------------------------------------------------
        CALL energy_balance(LAND_PTS,timestep
     &,              HCON_NVG(1),RS,ra,chn
     &,              LW,SWN,PSTAR,Q,T,TS1,windx,windy
     &,              ETRAN(:,N),LETRAN(:,N),H_T(:,N),G_T(:,N)
     &,              RADNET_T(:,N),TSTAR(:,N),fx_mom
     &,              fx_surf_t(:,N))

! Add contribution to GBM momentum fluxes from this tile
        DO L=1,LAND_PTS
          FX_MOM_GB(L,1) = FX_MOM_GB(L,1) + FRAC(L,N)*FX_MOM(L,1)
          FX_MOM_GB(L,2) = FX_MOM_GB(L,2) + FRAC(L,N)*FX_MOM(L,2)
          gc_gb(L)       = gc_gb(L)       + frac(L,n)*gc(L,n)
        ENDDO
!-----------------------------------------------------------------------
! Calculate the leaf turnover rate.
!-----------------------------------------------------------------------
        CALL LEAF_LIT (LAND_PTS,TILE_PTS(N),TILE_INDEX(:,N),N
     &,                FSMC,TSTAR(:,N),G_LEAF(:,N))

      ENDDO ! FT Loop



!***********************************************************************
!***********************************************************************
! CALCULATIONS ON NON-VEGETATION TILES
!***********************************************************************
!***********************************************************************
      DO N=NPFT+1,NTYPE
!-----------------------------------------------------------------------
! Initialise globally uniform non-veg parameters
!-----------------------------------------------------------------------
        DO L=1,LAND_PTS
!          ALBSOIL(L) = ALBSNF_NVG(N-NPFT)
!          ALBSNOW(L) = ALBSNC_NVG(N-NPFT)
          RS(L)      = 1.0 / GS_NVG(N-NPFT)
          Z0S(L)     = Z0_NVG(N-NPFT)
        ENDDO

        CALL calc_ra_tile(windx,windy,z1,z0s,chn,ra)
!-----------------------------------------------------------------------
! Adjust soil surface resistance as a function of soil moisture
!-----------------------------------------------------------------------
        DO L=1,LAND_PTS
          RS(L) = 1.0E6
          IF(FSMC(L).GT.0.0) RS(L) = MIN(1.0E6,100.0/FSMC(L))
!-----------------------------------------------------------------------
! If snow if lying assume no surface resistance
!-----------------------------------------------------------------------
cpph rs=0 causes instability
c          IF (LYING_SNOW(L) .GT. 0.0) RS(L)=0.0
          IF (LYING_SNOW(L) .GT. 0.0) RS(L)=1e6

!-----------------------------------------------------------------------
! Calculate net solar radiation for non-veg tile
!-----------------------------------------------------------------------
          swn(L) = sw(L) * (1.0 - albedo_tiles(L,n)) * tv(L)

! Save land-ice albedo for ice-sheet model
          IF(N.eq.SOIL+1) ALBICE(L) = albedo_tiles(l,n)
          SWN_GB(L) = SWN_GB(L) + FRAC(L,N)*SWN(L)
        ENDDO
!-----------------------------------------------------------------------
! Solve the soil surface energy balance.
!-----------------------------------------------------------------------
        CALL energy_balance(LAND_PTS,timestep
     &,              HCON_NVG(N-NPFT),RS,ra,chn
     &,              LW,SWN,PSTAR,Q,T,TS1,windx,windy
     &,              ETRAN(:,N),LETRAN(:,N),H_T(:,N),G_T(:,N)
     &,              RADNET_T(:,N),TSTAR(:,N),fx_mom
     &,              fx_surf_t(:,n))

! Add contribution to GBM momentum fluxes from this tile
        DO L=1,LAND_PTS
          FX_MOM_GB(L,1) = FX_MOM_GB(L,1) + FRAC(L,N)*FX_MOM(L,1)
          FX_MOM_GB(L,2) = FX_MOM_GB(L,2) + FRAC(L,N)*FX_MOM(L,2)
        ENDDO
      ENDDO !End of NVG loop

!-----------------------------------------------------------------------
! Calculate gridbox mean fluxes and surface temperature.
!-----------------------------------------------------------------------
      DO L=1,LAND_PTS
        E(L)           = 0.0
        ESUB(L)        = 0.0
        LE(L)          = 0.0
        H(L)           = 0.0
        RADNET(L)      = 0.0
        G(L)           = 0.0
        TSTAR_GB(L)    = 0.0
        fx_surf(l)     = 0.0
        DO N=1,NTYPE
          E(L)        = E(L)        + FRAC(L,N) * ETRAN(L,N)
          LE(L)       = LE(L)       + FRAC(L,N) * LETRAN(L,N)
          TSTAR_GB(L) = TSTAR_GB(L) + FRAC(L,N) * TSTAR(L,N)
          H(L)        = H(L)        + FRAC(L,N) * H_T(L,N)
          G(L)        = G(L)        + FRAC(L,N) * G_T(L,N)
          fx_surf(l)  = fx_surf(l)  + frac(l,n) * fx_surf_t(l,n)
          RADNET(L)   = RADNET(L)   + FRAC(L,N) * RADNET_T(L,N)
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
! Calculate gridbox mean qsurf for the IGCM
! At the moment this is just set to qsat(tsurf_gb)
!-----------------------------------------------------------------------
      CALL QSAT(qsurf_gb,tstar_gb,pstar,land_pts)
      WHERE(qsurf_gb<q) qsurf_gb = q

!-----------------------------------------------------------------------
! Update carbon accumulations
!-----------------------------------------------------------------------

! Calculate the soil respiration
      CALL MICROBE (LAND_PTS,LAND_PTS,1
     &,             CS,V_ROOT,V_SAT,V_WILT,TS1,RESP_S)

! Update phenology accumulation variable.
      DO N=1,NPFT
        DO L=1,LAND_PTS
          G_LEAF_ACC(L,N) = G_LEAF_ACC(L,N) 
     &            + G_LEAF(L,N) * TIMESTEP/yearlen_land
        ENDDO
      ENDDO

! Accumulate TRIFFID driving variables
      DO L=1,LAND_PTS
        RESP_S_DR(L) = RESP_S_DR(L)+RESP_S(L)*TIMESTEP
        DO N=1,NPFT
          GPP_DR(L,N)    = GPP_DR(L,N) + GPP(L,N)*TIMESTEP
          NPP_DR(L,N)    = NPP_DR(L,N) + NPP(L,N)*TIMESTEP
          RESP_W_DR(L,N) = RESP_W_DR(L,N) + RESP_W(L,N)*TIMESTEP
        ENDDO
      ENDDO

!-----------------------------------------------------------------------
! Update the soil water/heat state and surface albedo
!-----------------------------------------------------------------------
      CALL LAND_STATE (FRAC(:,SOIL+1),G
     &,                RAIN,SNOW,E,ESUB
     &,                MSOIL,LYING_SNOW,TS1
     &,                RUNOFF,SNOWMELT_ACC)

      CALL calc_albedos(frac,tstar,lying_snow,albsnf,albsnc
     &                 ,albedo_tiles,albedo_gb)

      RETURN

      END
