C**********************************************************************
C Routine to calculate the evaporation using an extended version of
C the Penman-Monteith equation.
C
C Written by Peter Cox (March 1996)
C
C Modified for use by Katrin Meissner (Jan 2001)
C**********************************************************************
      SUBROUTINE energy_balance(LAND_PTS,timestep
     &,                  HCON_SOIL,RS,ra,chn
     &,                  LW,SWN,PSTAR,Q1,T1,TS1,windx,windy
     &,                  E,LE,H,G,RADNET,TSTAR,fx_mom
     &,                  fx_surf)

      USE phys_const, only : con_cp, con_epsil, con_gascon, con_lateva
     &                     , con_latfus
      USE land_const, only : hcap_veg, dz_soil
     &                     , str_tune
      IMPLICIT NONE

      INTEGER
     & LAND_PTS                   ! IN Number of land points.
     &,L                          ! WORK Loop counter.

! Surface parameters
      REAL
     & HCON_SOIL                  ! IN Soil heat conductivity (W/m/K).
     &,RS(LAND_PTS)               ! IN Surface resistance (s/m).
     &,ra(land_pts)               ! IN Aerodynamic resistance (s/m)
     &,chn(land_pts)              ! IN Neutral transfer coefficients.
!     &,Z0(LAND_PTS)               ! IN Roughness length (m).
     &,timestep                   ! IN Land timestep length (s)

! Driving parameters
      REAL
     & LW(LAND_PTS)               ! IN Downward LW (W/m2).
     &,SWN(LAND_PTS)              ! IN Net downward SW (W/m2).
     &,PSTAR(LAND_PTS)            ! IN Surface pressure (Pa).
     &,Q1(LAND_PTS)               ! IN Specific humidity (kg/kg).
     &,T1(LAND_PTS)               ! IN Atmospheric temperature (K).
     &,TS1(LAND_PTS)              ! IN Sub-surface temperature (K).
     &,WIND(LAND_PTS)             ! WORK Windspeed (m/s).
     &,windx(land_pts)            ! IN Zonal wind component (m/s)
     &,windy(land_pts)            ! IN Meridional wind component (m/s)
!     &,Z1                         ! IN Reference height (m).
!     &,lying_snow(land_pts)       ! IN Lying snow water equivalent (kg/m2)

! Outputs
      REAL
     & E(LAND_PTS)                ! OUT Evapotranspiration (kg/m2/s).
     &,LE(LAND_PTS)               ! OUT Latent heat flux (W/m2).
     &,H(LAND_PTS)                ! OUT Sensible heat flux (W/m2).
     &,G(LAND_PTS)                ! OUT Ground heat flux (W/M2).
     &,fx_surf(land_pts)          ! OUT Canopy heat flux (W/m2).
     &,RADNET(LAND_PTS)           ! OUT Surface net radiation (W/m2).
     &,TSTAR(LAND_PTS)            ! OUT Surface temperature (K).
     &,fx_mom(land_pts,2)         ! OUT Surface momentum flux zonal and meridional (kg/m/s2)
     &,cv_dt,tstar_old

! Work Varibles
      REAL
     & AHAT(LAND_PTS)             ! WORK "Available energy" (W/m2).
     &,AS1                        ! WORK 2*HCON_SOIL/DZ_SOIL (W/m2/K).
     &,DENOM                      ! WORK Denominator of PM equation.
     &,DQ1(LAND_PTS)              ! WORK Humidity deficit (kg/kg).
     &,DQS_DT(LAND_PTS)           ! WORK Rate of change of saturated
C                                 !      specific humidity with
C                                 !      temperature (kg/kg/K).
     &,DUM                        ! WORK Workspace variable.
     &,LAT(LAND_PTS)              ! WORK Latent heat constant (J/kg).
     &,NUMER                      ! WORK Numerator of PM equation.
     &,RESF                       ! WORK 1/(1+RS/RA).
     &,QS1(LAND_PTS)              ! WORK Saturated specific humidity
C                                 !      at (T1,PSTAR) (kg/kg).
     &,QS_old(LAND_PTS)           ! WORK Saturated specific humidity
C                                 !      at (Tstar,PSTAR) (kg/kg).
     &,RHOSTAR(LAND_PTS)          ! WORK Surface air density (kg/m3).

C-----------------------------------------------------------------------
C Local parameters
C-----------------------------------------------------------------------
      REAL,PARAMETER :: gustiness = 3.0

C----------------------------------------------------------------------
C Calculate the saturated specific humidity, its gradient w.r.t.
C temperature, and the humidity deficit.
C----------------------------------------------------------------------
      CALL QSAT(QS1,T1,PSTAR,LAND_PTS)
      CALL QSAT(QS_old,tstar,PSTAR,LAND_PTS)

!----------------------------------------------------------------------
! Calculate coefficient for ground heat flux.  Note that soil thermal
! conductivity is used even if there is lying snow or vegetation.
!----------------------------------------------------------------------
      AS1 = 2*HCON_SOIL/DZ_SOIL

      DO L=1,LAND_PTS
C----------------------------------------------------------------------
C Calculate the surface air density (use atmospheric rather than
C surface temperature).
C----------------------------------------------------------------------
        tstar_old = tstar(l)
        RHOSTAR(L) = PSTAR(L)/(con_gascon*T1(L))
        LAT(L) = CON_LATEVA

! Don't allow super-saturation or negative humidity deficits
        IF(qs1(l)<q1(l)) THEN
          qs1(l) = q1(l)
          dq1(l) = 0.0
          ! REDFLAG--must sort this out
          dq1 = dq1
        ELSE
          dq1(l) = MAX(qs1(l) - q1(l),0.0)
          ! REDFLAG--must sort this out
          dq1 = dq1
        ENDIF
        dqs_dt(l) = (con_epsil*lat(l)*qs_old(l))
     &             /(con_gascon*tstar_old*tstar_old)
!----------------------------------------------------------------------
! Calculate zonal and meridional momentum fluxes
!----------------------------------------------------------------------
        WIND(L) = SQRT(WINDX(L)**2 + WINDY(L)**2)
        FX_MOM(L,1) = CHN(L)*RHOSTAR(L)*(WIND(L)+GUSTINESS)*WINDX(L)
     &                *str_tune
        FX_MOM(L,2) = CHN(L)*RHOSTAR(L)*(WIND(L)+GUSTINESS)*WINDY(L)
     &                *str_tune
C-----------------------------------------------------------------------
C Calculate available energy for this tile 
!    = Net shortwave + downward longwave
C-----------------------------------------------------------------------
        AHAT(L) = SWN(L) + LW(L)
C----------------------------------------------------------------------
C Calculate the evaporation rate and diagnose the surface temperature.
C----------------------------------------------------------------------
        RESF = 1.0/(1.0+RS(L)/RA(L))

        cv_dt = hcap_veg/timestep !surface heat capacity over timestep
        dum = lat(l)*rhostar(l)*resf*DQS_DT(L)/ra(l)

!bowie  using netrad from atmos       numer = ahat(l) + 3.0*con_sboltz*(tstar_old**4) 
        numer = ahat(l) 
     &    + tstar_old*(cv_dt + dum) + as1*ts1(l) 
     &    + (rhostar(l)*con_cp/ra(l))*t1(l) 
     &    - (dum/DQS_DT(L))*(qs_old(l) - q1(l))

!bowie using netrad from atmos        denom = cv_dt + 4.0*con_sboltz*(tstar_old**3) + as1 + dum 
        denom = cv_dt + as1 + dum 
     &    + rhostar(l)*con_cp/ra(l)

        tstar(l) = numer/denom

        E(L) = (RESF*RHOSTAR(L)/RA(L))
     &        *(DQS_DT(L)*(tstar(l)-tstar_old) + qs_old(l) - q1(l))
        LE(l)= lat(l)*e(l)
        H(l) = (rhostar(l)*CON_CP/RA(l))*(tstar(l)-t1(l))
        G(l) = as1*(tstar(l)-ts1(l))
        fx_surf(l) = cv_dt*(tstar(l)-tstar_old)

        RADNET(L) = SWN(L) + LW(L) 
!bowie using netrad from atmos    &  - con_sboltz*(tstar_old**4 + 4.0*(tstar_old**3)*(tstar(l)-tstar_old))

!Calculate the near-surface humidity and don't allow condensation
!TODO the associated energy partition correction
        e(l)=MAX(e(l),0.0)

      ENDDO

      RETURN
      END
