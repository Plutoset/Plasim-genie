
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
!!! Subroutine TRIFFID ------------------------------------------------
!!!
!!!                     Top-down
!!!                     Representation of
!!!                     Interactive
!!!                     Foliage and
!!!                     Flora
!!!                     Including
!!!                     Dynamics
!!!
!!! Purpose : Simulates changes in vegetation structure, areal
!!!           coverage and the carbon contents of vegetation and soil.
!!!           can be used to advance these variables dynamically
!!!           (GAMMA=1/TIMESTEP) or to iterate towards  equilibrium
!!!           (GAMMA --> 0.0, FORW=1.0).
!!!
!!!
!!!  Model            Modification history:
!!! version  Date
!!!  4.4     10/97     New Deck. Peter Cox
!!!  4.5   12/05/98    Operate only on points indexed with TRIF_INDEX.
!!!                    Richard Betts
!!!
!!!END ----------------------------------------------------------------
      SUBROUTINE TRIFFID (LAND_FIELD,TRIF_PTS,TRIF_INDEX,FORW,GAMMA
     &,                   FRAC_VS,FRAC_AGRIC,G_LEAF,NPP,RESP_S,RESP_W
     &,                   CS,FRAC,HT,LAI,C_VEG,CV,LIT_C,LIT_C_T)


      USE land_const
      IMPLICIT NONE

      INTEGER
     & LAND_FIELD                 ! IN Total number of land points.
     &,TRIF_PTS                   ! IN Number of points on which
!                                 !    TRIFFID may operate
     &,TRIF_INDEX(LAND_FIELD)     ! IN Indices of land points on
!                                 !    which TRIFFID may operate
     &,L,N,T                      ! WORK Loop counters

      REAL
     & FORW                       ! IN Forward timestep weighting.
     &,FRAC_VS(LAND_FIELD)        ! IN Total fraction of gridbox
!                                 !    covered by veg or soil.
     &,GAMMA                      ! IN Inverse timestep (/360days).
     &,FRAC_AGRIC(LAND_FIELD)     ! IN Fraction of agriculture.
     &,G_LEAF(LAND_FIELD,NPFT)    ! IN Turnover rate for leaf and
C                                 !    fine root biomass (/360days).
     &,NPP(LAND_FIELD,NPFT)       ! INOUT Net primary productivity
C                                 !       (kg C/m2/360days).
     &,RESP_S(LAND_FIELD)         ! INOUT Soil respiration
C                                 !       (kg C/m2/360days).
     &,RESP_W(LAND_FIELD,NPFT)    ! INOUT Wood maintenance respiration
C                                 !       (kg C/m2/360days).
     &,CS(LAND_FIELD)             ! INOUT Soil carbon (kg C/m2).
     &,FRAC(LAND_FIELD,NTYPE)     ! INOUT Fractional cover of each
C                                 !       Functional Type.
     &,HT(LAND_FIELD,NPFT)        ! INOUT Vegetation height (m).
     &,LAI(LAND_FIELD,NPFT)       ! INOUT Leaf area index.
     &,C_VEG(LAND_FIELD,NPFT)     ! OUT Total carbon content of
C                                 !     the vegetation (kg C/m2).
     &,CV(LAND_FIELD)             ! OUT Gridbox mean vegetation
C                                 !     carbon (kg C/m2).
     &,LIT_C(LAND_FIELD,NPFT)     ! OUT Carbon Litter (kg C/m2/360days).
     &,LIT_C_T(LAND_FIELD)        ! OUT Gridbox mean carbon litter
C                                 !     (kg C/m2/360days).

      REAL
     & DCVEG(LAND_FIELD,NPFT)     ! WORK Change in vegetation carbon
C                                 !      during the timestep
C                                 !      (kg C/m2/timestep).
     &,DFRAC(LAND_FIELD,NPFT)     ! WORK Change in areal fraction
C                                 !      during the timestep
C                                 !      (/timestep).
     &,FRAC_FLUX                  ! WORK PFT fraction to be used
C                                 !      in the calculation of
C                                 !      the gridbox mean fluxes.
     &,LAI_BAL(LAND_FIELD,NPFT)   ! WORK Leaf area index in balanced
C                                 !      growth state.
     &,LEAF(LAND_FIELD,NPFT)      ! WORK Leaf biomass (kg C/m2).
     &,PC_S(LAND_FIELD,NPFT)      ! WORK Net carbon flux available
C                                 !      for spreading
C                                 !      (kg C/m2/yr).
     &,PHEN(LAND_FIELD,NPFT)      ! WORK Phenological state.
     &,ROOT(LAND_FIELD,NPFT)      ! WORK Root biomass (kg C/m2).
     &,WOOD(LAND_FIELD,NPFT)      ! WORK Woody biomass (kg C/m2).

C----------------------------------------------------------------------
C Loop through Functional Types
C----------------------------------------------------------------------
      DO N=1,NPFT

C----------------------------------------------------------------------
C Loop through TRIFFID points
C----------------------------------------------------------------------
        DO T=1,TRIF_PTS
          L=TRIF_INDEX(T)

C----------------------------------------------------------------------
C Diagnose the balanced-growth leaf area index and the associated leaf,
C wood, root and total vegetation carbon
C----------------------------------------------------------------------
          LAI_BAL(L,N) = (A_WS(N)*ETA_SL(N)*HT(L,N)
     &              /A_WL(N))**(1.0/(B_WL(N)-1))
          LEAF(L,N) = SIGL(N)*LAI_BAL(L,N)
          ROOT(L,N) = LEAF(L,N)
          WOOD(L,N) = A_WL(N)*(LAI_BAL(L,N)**B_WL(N))
          C_VEG(L,N) = LEAF(L,N) + ROOT(L,N) + WOOD(L,N)
C----------------------------------------------------------------------
C Diagnose the phenological state
C----------------------------------------------------------------------
          PHEN(L,N) = LAI(L,N)/LAI_BAL(L,N)

        ENDDO

C----------------------------------------------------------------------
C Update vegetation carbon contents
C----------------------------------------------------------------------
        CALL VEGCARB (LAND_FIELD,TRIF_PTS,TRIF_INDEX,N,FORW
     &,               GAMMA,G_LEAF(1,N),NPP(1,N),RESP_W(1,N)
     &,               LEAF(1,N),ROOT(1,N),WOOD(1,N)
     &,               DCVEG(1,N),PC_S(1,N))

      ENDDO

C-----------------------------------------------------------------------
C Diagnose the new value of Canopy Height, Leaf Area Index and Total
C Vegetation Carbon
C-----------------------------------------------------------------------
      DO N=1,NPFT

        DO T=1,TRIF_PTS
          L=TRIF_INDEX(T)

          HT(L,N) = WOOD(L,N) / (A_WS(N) * ETA_SL(N))
     &            * (A_WL(N)/WOOD(L,N))**(1.0/B_WL(N))
          LAI_BAL(L,N) = LEAF(L,N) / SIGL(N)
          LAI(L,N) = PHEN(L,N) * LAI_BAL(L,N)
          C_VEG(L,N) = LEAF(L,N) + ROOT(L,N) + WOOD(L,N)

        ENDDO

      ENDDO

C----------------------------------------------------------------------
C Update the areal coverage of each functional type
C----------------------------------------------------------------------
      CALL LOTKA (LAND_FIELD,TRIF_PTS,TRIF_INDEX
     &,           C_VEG,FORW,FRAC_VS,FRAC_AGRIC,GAMMA,LAI_BAL,PC_S
     &,           FRAC,DFRAC)
C----------------------------------------------------------------------
C Diagnose the litterfall from the carbon balance of each vegetation
C type
C----------------------------------------------------------------------
      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)

        LIT_C_T(L) = 0.0

        DO N=1,NPFT
          FRAC_FLUX=FRAC(L,N)-(1.0-FORW)*DFRAC(L,N)
          LIT_C(L,N) = NPP(L,N)-GAMMA/FRAC_FLUX*(C_VEG(L,N)*FRAC(L,N)
     &               -(C_VEG(L,N)-DCVEG(L,N))*(FRAC(L,N)-DFRAC(L,N)))
          LIT_C_T(L) = LIT_C_T(L)+FRAC_FLUX*LIT_C(L,N)
        ENDDO
      ENDDO

C----------------------------------------------------------------------
C Call SOIL_C to update the soil carbon content
C----------------------------------------------------------------------
      CALL SOILCARB (LAND_FIELD,TRIF_PTS,TRIF_INDEX
     &,              FORW,GAMMA,LIT_C_T,RESP_S,CS)

C----------------------------------------------------------------------
C Diagnose the gridbox mean vegetation carbon
C----------------------------------------------------------------------
      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)
        CV(L) = 0.0
        DO N=1,NPFT
          CV(L) = CV(L)+FRAC(L,N)*C_VEG(L,N)
        ENDDO
      ENDDO

      RETURN
      END
