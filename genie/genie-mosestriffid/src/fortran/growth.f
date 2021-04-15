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
!!! Subroutine GROWTH -------------------------------------------------
!!!
!!! Purpose : Increments leaf, root and wood carbon.
!!!
!!!
!!!  Model            Modification history:
!!! version  Date
!!!  4.4     10/97     New Deck. Peter Cox
!!!  4.5   12/05/98    Operate only on points indexed with TRIF_INDEX.
!!!                    Richard Betts
!!!
!!!END ----------------------------------------------------------------
      SUBROUTINE GROWTH (LAND_FIELD,TRIF_PTS,TRIF_INDEX
     &,                  N,DPCG_DLAI,FORW,GAMMA,PC_G
     &,                  LEAF,ROOT,WOOD)


      USE land_const
      IMPLICIT NONE

      INTEGER
     & LAND_FIELD                 ! IN Total number of land points.
     &,TRIF_PTS                   ! IN Number of points on which
!                                 !    TRIFFID may operate
     &,TRIF_INDEX(LAND_FIELD)     ! IN Indices of land points on
!                                 !    which TRIFFID may operate
     &,N                          ! IN Vegetation type.
     &,L,T                        ! WORK Loop counters

      REAL
     & DPCG_DLAI(LAND_FIELD)      ! IN Rate of change of PC_G with
C                                 !    leaf area index
C                                 !    (kg C/m2/360days/LAI).
     &,FORW                       ! IN Forward timestep weighting.
     &,GAMMA                      ! IN Inverse timestep (/360days).
     &,PC_G(LAND_FIELD)           ! IN Net carbon flux available
C                                 !    for growth (kg C/m2/360days).
     &,LEAF(LAND_FIELD)           ! INOUT Leaf biomass (kg C/m2).
     &,ROOT(LAND_FIELD)           ! INOUT Root biomass (kg C/m2).
     &,WOOD(LAND_FIELD)           ! INOUT Woody biomass (kg C/m2).

      REAL
     & DENOM                      ! WORK Denominator of update
C                                 !      equation.
     &,DLEAF,DROOT,DWOOD          ! WORK Increments to leaf, root
C                                 !      and woody biomass (kg C/m2).
     &,DL_DW                      ! WORK Rate of change of leaf
C                                 !      carbon with wood carbon.
     &,DLAI_DW                    ! WORK Rate of change of leaf area
C                                 !      index with wood carbon
C                                 !      (LAI m2/kg C).
     &,DR_DW                      ! WORK Rate of change of root
C                                 !      carbon with wood carbon.
     &,NUMER                      ! WORK Numerator of the update
C                                 !      equation.
     &,WOOD_MAX                   ! WORK Maximum wood carbon (kg C/m2).
     &,WOOD_MIN                   ! WORK Minimum wood carbon (kg C/m2).

      DO T=1,TRIF_PTS
        L=TRIF_INDEX(T)

C----------------------------------------------------------------------
C Calculate the increment to the wood carbon
C----------------------------------------------------------------------
        DL_DW = LEAF(L)/(B_WL(N)*WOOD(L))
        DR_DW = DL_DW
        DLAI_DW = DL_DW/SIGL(N)

        NUMER = PC_G(L)
        DENOM = (1+DL_DW+DR_DW)*GAMMA-FORW*DLAI_DW*DPCG_DLAI(L)
        DENOM = MAX(DENOM,DENOM_MIN)

        DWOOD = NUMER/DENOM

C----------------------------------------------------------------------
C Ensure that the local leaf area index does not drop below its
C minimum value or exceed its maximum value.
C----------------------------------------------------------------------
        WOOD_MIN = A_WL(N)*(LAI_MIN(N)**B_WL(N))
        WOOD_MAX = A_WL(N)*(LAI_MAX(N)**B_WL(N))

        DWOOD = MAX((WOOD_MIN-WOOD(L)),DWOOD)
        DWOOD = MIN((WOOD_MAX-WOOD(L)),DWOOD)

C----------------------------------------------------------------------
C Diagnose the increments to leaf and root carbon
C----------------------------------------------------------------------
        DLEAF = SIGL(N)*((WOOD(L)+DWOOD)/A_WL(N))**(1.0/B_WL(N))
     &         -LEAF(L)
        DROOT = DLEAF

C----------------------------------------------------------------------
C Update carbon contents
C----------------------------------------------------------------------
        LEAF(L) = LEAF(L)+DLEAF
        ROOT(L) = ROOT(L)+DROOT
        WOOD(L) = WOOD(L)+DWOOD

      ENDDO

      RETURN
      END
