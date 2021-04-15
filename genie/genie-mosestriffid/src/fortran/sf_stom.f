C *****************************COPYRIGHT******************************
C (c) CROWN COPYRIGHT 1997, METEOROLOGICAL OFFICE, All Rights Reserved.
C
C Use, duplication or disclosure of this code is subject to the
C restrictions as set forth in the contract.
C
C                Meteorological Office
C                London Road
C                BRACKNELL
C                Berkshire UK
C                RG12 2SZ
C
C If no contract has been raised with this copy of the code, the use,
C duplication or disclosure of it is strictly prohibited.  Permission
C to do so must first be obtained in writing from the Head of Numerical
C Modelling at the above address.
C ******************************COPYRIGHT******************************
!**********************************************************************
! Routine to calculate the bulk stomatal resistance and the canopy
! CO2 fluxes
!
! Written by Peter Cox (June 1997)
! Adapted for MOSES II tile model by Richard Essery (July 1997)
!**********************************************************************
      SUBROUTINE SF_STOM  (LAND_FIELD,LAND_INDEX,P_FIELD
     &,                    VEG_PTS,VEG_INDEX,FT
     &,                    dayfrac,CO2,FSMC,HT,IPAR,LAI,PSTAR
     &,                    Q1,RA,TSTAR
     &,                    GPP,NPP,RESP_P,RESP_W,GC)


      USE land_const
      IMPLICIT NONE

      INTEGER
     & LAND_FIELD                 ! IN Total number of land points.
     &,LAND_INDEX(LAND_FIELD)     ! IN Index of land points on the
!                                 !    P-grid.
     &,P_FIELD                    ! IN Total number of P-gridpoints.
     &,VEG_PTS                    ! IN Number of vegetated points.
     &,VEG_INDEX(LAND_FIELD)      ! IN Index of vegetated points
!                                 !    on the land grid.

      INTEGER
     & FT                         ! IN Plant functional type.

      REAL
     & CO2(LAND_FIELD)            ! IN Atmospheric CO2 concentration
!                                 !    (kg CO2/kg air).
     &,dayfrac(land_field)        ! IN Fraction of 24 hours that sun is above horizon (-)
     &,FSMC(LAND_FIELD)           ! IN Soil water factor.
     &,HT(LAND_FIELD)             ! IN Canopy height (m).
     &,IPAR(P_FIELD)              ! IN Incident PAR (W/m2).
     &,LAI(LAND_FIELD)            ! IN Leaf area index.
     &,PSTAR(LAND_FIELD)          ! IN Surface pressure (Pa).
     &,Q1(P_FIELD)                ! IN Specific humidity of level 1
!                                 !    (kg H2O/kg air).
     &,RA(LAND_FIELD)             ! IN Aerodynamic resistance (s/m).
     &,TSTAR(LAND_FIELD)          ! IN Surface temperature (K).
     &,GPP(LAND_FIELD)            ! OUT Gross Primary Productivity
!                                 !     (kg C/m2/s).
     &,NPP(LAND_FIELD)            ! OUT Net Primary Productivity
!                                 !     (kg C/m2/s).
     &,RESP_P(LAND_FIELD)         ! OUT Plant respiration rate
!                                 !     (kg C/m2/sec).
     &,RESP_W(LAND_FIELD)         ! OUT Wood respiration rate
!                                 !     (kg C/m2/sec).
     &,GC(LAND_FIELD)             ! INOUT Canopy resistance to H2O
!                                 !       (m/s).

      REAL
     & ANETC(LAND_FIELD)          ! WORK Net canopy photosynthesis
!                                 !     (mol CO2/m2/s).
     &,CO2C(LAND_FIELD)           ! WORK Canopy level CO2 concentration
!                                 !      (kg CO2/kg air).
     &,CI(LAND_FIELD)             ! WORK Internal CO2 pressure (Pa).
     &,DQ(LAND_FIELD)             ! WORK Specific humidity deficit
!                                 !      (kg H2O/kg air).
     &,DQC(LAND_FIELD)            ! WORK Canopy level specific humidity
!                                 !      deficit (kg H2O/kg air).
     &,FPAR(LAND_FIELD)           ! WORK PAR absorption factor.
     &,LAI_BAL(LAND_FIELD)        ! WORK Leaf area index in balanced
!                                 !      growth state.
     &,NL(LAND_FIELD)             ! WORK Mean leaf nitrogen
!                                 !      concentration (kg N/kg C).
     &,NL_BAL(LAND_FIELD)         ! WORK Mean leaf nitrogen
!                                 !      concentration in balanced
!                                 !      growth state (kg N/kg C).
     &,N_LEAF(LAND_FIELD)         ! WORK Nitrogen contents of the leaf,
     &,N_ROOT(LAND_FIELD)         !      root,
     &,N_STEM(LAND_FIELD)         !      and stem (kg N/m2).
     &,QS(LAND_FIELD)             ! WORK Saturated specific humidity
!                                 !      (kg H2O/kg air).
     &,RA_RC(LAND_FIELD)          ! WORK Ratio of aerodynamic resistance
!                                 !      to canopy resistance.
     &,RDC(LAND_FIELD)            ! WORK Canopy dark respiration,
!                                 !      without soil water dependence
!                                 !      (mol CO2/m2/s).
     &,RESP_P_G(LAND_FIELD)       ! WORK Plant growth respiration rate
!                                 !      (kg C/m2/sec).
     &,RESP_P_M(LAND_FIELD)       ! WORK Plant maintenance respiration
!                                 !      rate (kg C/m2/sec).
     &,ROOT(LAND_FIELD)           ! WORK Root carbon (kg C/m2).

      INTEGER
     & I,J,K,L                    ! WORK Loop counters.

      REAL
     & O2                         ! Atmospheric concentration of
!                                 ! oxygen (kg O2/kg air).
      PARAMETER (O2 = 0.23)

      INTEGER
     & ITER                       ! Number of iterations to
!                                 ! determine the canopy climate.
      PARAMETER (ITER = 1)

!-----------------------------------------------------------------------
! Calculate the surface to level 1 humidity deficit and the surface
! density of the air
!-----------------------------------------------------------------------
      CALL QSAT(QS,TSTAR,PSTAR,LAND_FIELD)
      DO J=1,VEG_PTS
        L = VEG_INDEX(J)
        I = LAND_INDEX(L)
        DQ(L) = MAX(0.0,(QS(L) - Q1(I)))
      ENDDO

!-----------------------------------------------------------------------
! Calculate the PAR absorption factor
!-----------------------------------------------------------------------
      DO J=1,VEG_PTS
        L = VEG_INDEX(J)

        FPAR(L) = (1 - EXP(-KPAR(FT)*LAI(L))) / KPAR(FT)

      ENDDO


!-----------------------------------------------------------------------
! Iterate to ensure that the canopy humidity deficit is consistent with
! the H2O flux. Ignore the (small) difference between the canopy and
! reference level CO2 concentration. Intially set the canopy humidity
! deficit using the previous value of GC.
!-----------------------------------------------------------------------
      DO K=1,ITER

!-----------------------------------------------------------------------
! Diagnose the canopy level humidity deficit and CO2 concentration
!-----------------------------------------------------------------------
        DO J=1,VEG_PTS
          L = VEG_INDEX(J)
          RA_RC(L) = RA(L) * GC(L)
          DQC(L) = DQ(L) / (1 + RA_RC(L))
          CO2C(L) = CO2(J)
        ENDDO

!-----------------------------------------------------------------------
! Call CANOPY to calculate the canopy resistance and photosynthesis
!-----------------------------------------------------------------------
        CALL CANOPY (LAND_FIELD,LAND_INDEX,P_FIELD
     &,              VEG_PTS,VEG_INDEX,dayfrac
     &,              FT,DQC,IPAR,TSTAR,CO2C,O2,PSTAR
     &,              FPAR,FSMC
     &,              GC,ANETC,CI,RDC)

      ENDDO

      DO J=1,VEG_PTS
        L = VEG_INDEX(J)
!-----------------------------------------------------------------------
! Assume that root biomass is equal to balanced growth leaf biomass
!-----------------------------------------------------------------------
        LAI_BAL(L) = (A_WS(FT)*ETA_SL(FT)*HT(L)/A_WL(FT))
     &             **(1.0/(B_WL(FT)-1))
        ROOT(L) = SIGL(FT) * LAI_BAL(L)

!-----------------------------------------------------------------------
! Calculate the actual and balanced mean leaf nitrogen concentration
! assuming perfect light acclimation
!-----------------------------------------------------------------------
        NL(L) = (FPAR(L) / LAI(L)) * NL0(FT)
        NL_BAL(L) = (1 - EXP(-KPAR(FT)*LAI_BAL(L)))
     &            / (KPAR(FT)*LAI_BAL(L)) * NL0(FT)

!-----------------------------------------------------------------------
! Calculate the total nitrogen content of the leaf, root and stem
!-----------------------------------------------------------------------
        N_LEAF(L) = NL(L) * SIGL(FT) * LAI(L)
        N_ROOT(L) = NR_NL(FT) * NL_BAL(L) * ROOT(L)
        N_STEM(L) = NS_NL(FT) * NL_BAL(L) * ETA_SL(FT) * HT(L) * LAI(L)

!-----------------------------------------------------------------------
! Calculate the Gross Primary Productivity, the plant maintenance
! respiration rate, and the wood maintenance respiration rate
! in kg C/m2/sec
!-----------------------------------------------------------------------
        GPP(L) = 12.0E-3 * (ANETC(L) + RDC(L)*FSMC(L))
        RESP_P_M(L) = 12.0E-3 * RDC(L)
     &     * (N_LEAF(L)*FSMC(L) + N_STEM(L) + N_ROOT(L)) / N_LEAF(L)
        RESP_W(L) = 12.0E-3 * RDC(L) * N_STEM(L) / N_LEAF(L)

!-----------------------------------------------------------------------
! Calculate the total plant respiration and the Net Primary Productivity
!-----------------------------------------------------------------------
        RESP_P_G(L) = R_GROW(FT) * (GPP(L) - RESP_P_M(L))
        RESP_P(L) = RESP_P_M(L) + RESP_P_G(L)
        NPP(L) = GPP(L) - RESP_P(L)

      ENDDO

      RETURN
      END
