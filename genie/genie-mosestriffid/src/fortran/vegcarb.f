
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
!!! Subroutine VEGCARB ------------------------------------------------
!!!
!!! Purpose : Updates carbon contents of the vegetation.
!!!
!!!
!!!  Model            Modification history:
!!! version  Date
!!!  4.4     10/97     New Deck. Peter Cox
!!!  4.5   12/05/98    Operate only on points indexed with TRIF_INDEX.
!!!                    Richard Betts
!!!
!!!END ----------------------------------------------------------------
       SUBROUTINE VEGCARB (LAND_FIELD,TRIF_PTS,TRIF_INDEX
     &,                    N,FORW,GAMMA,G_LEAF,NPP,RESP_W
     &,                    LEAF,ROOT,WOOD,DCVEG,PC_S)


      USE land_const
      IMPLICIT NONE

      INTEGER
     & LAND_FIELD                 ! IN Total number of land points.
     &,TRIF_PTS                   ! IN Number of points on which
!                                 !    TRIFFID may operate
     &,TRIF_INDEX(LAND_FIELD)     ! IN Indices of land points on
!                                 !    which TRIFFID may operate
     &,N                          ! IN Plant functional type.
     &,L,T                        ! WORK Loop counters

      REAL
     & FORW                       ! IN Forward timestep weighting.
     &,GAMMA                      ! IN Inverse timestep (/360days).
     &,G_LEAF(LAND_FIELD)         ! IN Turnover rate for leaf and
!                                 !    fine root biomass (/360days).
     &,NPP(LAND_FIELD)            ! INOUT Net primary productivity
!                                 !       (kg C/m2/360days).
     &,RESP_W(LAND_FIELD)         ! INOUT Wood maintenance respiration
!                                 !       (kg C/m2/360days).
     &,LEAF(LAND_FIELD)           ! INOUT Leaf biomass (kg C/m2).
     &,ROOT(LAND_FIELD)           ! INOUT Root biomass (kg C/m2).
     &,WOOD(LAND_FIELD)           ! INOUT Woody biomass (kg C/m2).
     &,DCVEG(LAND_FIELD)          ! OUT Change in vegetation carbon
C                                 !     during the timestep
C                                 !     (kg C/m2/timestep).
     &,PC_S(LAND_FIELD)           ! OUT Net carbon flux available
!                                 !     for spreading (kg C/m2/360days).
     &,DFPAR_DLAI                 ! WORK Rate of change of FPAR
C                                 !      with leaf area index.
     &,DLAI                       ! WORK Increment to the leaf area
C                                 !      index.
     &,DLAMG_DLAI,DLIT_DLAI       ! WORK Required for calculation
C                                 !      of the equilibrium increments.
     &,DNPP_DLAI(LAND_FIELD)      ! WORK Rate of change of NPP
C                                 !      with leaf area index
!                                 !      (kg C/m2/360days/LAI).
     &,DPC_DLAI(LAND_FIELD)       ! WORK Rate of change of PC
C                                 !      with leaf area index
!                                 !      (kg C/m2/360days/LAI).
     &,DPCG_DLAI(LAND_FIELD)      ! WORK Rate of change of PC_G
C                                 !      with leaf area index
!                                 !      (kg C/m2/360days/LAI).
     &,DRESPW_DLAI                ! WORK Rate of change of RESP_W
C                                 !      with leaf area index
     &,FPAR                       ! WORK PAR interception factor.
     &,LAI(LAND_FIELD)            ! WORK Leaf area index.
     &,LAMBDA_G                   ! WORK Fraction of NPP available
C                                 !      for spreading.
     &,LIT_C_L(LAND_FIELD)        ! WORK Local rate of Carbon Litter
!                                 !      production (kg C/m2/360days).
     &,PC(LAND_FIELD)             ! WORK Net carbon flux available
!                                 !      to vegetation (kg C/m2/360days)
     &,PC_G(LAND_FIELD)           ! WORK Net carbon flux available
!                                 !      for growth (kg C/m2/360days).

      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)

        LAI(L) = LEAF(L)/SIGL(N)
C----------------------------------------------------------------------
C Calculate the local production rate for carbon litter
C----------------------------------------------------------------------
        LIT_C_L(L) = G_LEAF(L)*LEAF(L)+G_ROOT(N)*ROOT(L)
     &               + G_WOOD(N)*WOOD(L)

C----------------------------------------------------------------------
C Diagnose the net local carbon flux into the vegetation
C----------------------------------------------------------------------
        PC(L) = NPP(L) - LIT_C_L(L)

C----------------------------------------------------------------------
C Variables required for the implicit and equilibrium calculations
C----------------------------------------------------------------------
        DLIT_DLAI = (G_LEAF(L)*LEAF(L)+G_ROOT(N)*ROOT(L))/LAI(L)
     &            + B_WL(N)*G_WOOD(N)*WOOD(L)/LAI(L)

        FPAR = (1 - EXP(-KPAR(N)*LAI(L)))/KPAR(N)
        DFPAR_DLAI = EXP(-KPAR(N)*LAI(L))

        DNPP_DLAI(L) = NPP(L)*DFPAR_DLAI/FPAR
     &               + (1-R_GROW(N))*RESP_W(L)
     &               *(DFPAR_DLAI/FPAR-B_WL(N)/LAI(L))

        LAMBDA_G = 1 - (LAI(L) - LAI_MIN(N))
     &                /(LAI_MAX(N) - LAI_MIN(N))
        DLAMG_DLAI = -1.0/(LAI_MAX(N) - LAI_MIN(N))

        PC_G(L) = LAMBDA_G * NPP(L) - LIT_C_L(L)
        DPCG_DLAI(L) = LAMBDA_G*DNPP_DLAI(L)
     &               + DLAMG_DLAI*NPP(L)
     &               - DLIT_DLAI
        DPC_DLAI(L) = DNPP_DLAI(L) - DLIT_DLAI

      ENDDO

C----------------------------------------------------------------------
C Update vegetation carbon contents
C----------------------------------------------------------------------
      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)
        DCVEG(L) = LEAF(L)+ROOT(L)+WOOD(L)
      ENDDO

      CALL GROWTH (LAND_FIELD,TRIF_PTS,TRIF_INDEX
     &,            N,DPCG_DLAI,FORW,GAMMA,PC_G,LEAF,ROOT,WOOD)

      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)
        DCVEG(L) = LEAF(L)+ROOT(L)+WOOD(L)-DCVEG(L)
      ENDDO

C----------------------------------------------------------------------
C Diagnose the carbon available for spreading and apply implicit
C corrections to the driving fluxes.
C----------------------------------------------------------------------
      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)
        DLAI = LEAF(L)/SIGL(N) - LAI(L)
        PC_S(L) = PC(L) + FORW*DPC_DLAI(L)*DLAI - DCVEG(L)*GAMMA

        FPAR = (1 - EXP(-KPAR(N)*LAI(L)))/KPAR(N)
        DFPAR_DLAI = EXP(-KPAR(N)*LAI(L))
        DRESPW_DLAI = RESP_W(L)*B_WL(N)/LAI(L)

        NPP(L) = NPP(L) + FORW*DNPP_DLAI(L)*DLAI
        RESP_W(L) = RESP_W(L) + FORW*DRESPW_DLAI*DLAI
      ENDDO

      RETURN
      END
