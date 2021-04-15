!$Id: phenol.f 2508 2005-07-15 09:11:06Z cvs-gw $
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
!!! Subroutine PHENOL -------------------------------------------------
!!!
!!!
!!! Purpose :  Parametrizes leaf phenological changes and updates the
!!!            leaf area index and the leaf turnover rate.
!!!
!!!  Model            Modification history:
!!! version  Date
!!!  4.4     10/97     New Deck. Peter Cox
!!!
!!!END -----------------------------------------------------------------
      SUBROUTINE PHENOL (LAND_FIELD,VEG_PTS,VEG_INDEX,N,G_LEAF,HT
     &,                  DTIME_PHEN,G_LEAF_PHEN,LAI)

      USE land_const
      IMPLICIT NONE

      INTEGER
     & LAND_FIELD                 ! IN Total number of land points.
     &,VEG_PTS                    ! IN Number of vegetated points.
     &,VEG_INDEX(LAND_FIELD)      ! IN Index of vegetated points
C                                 !    on the land grid.
     &,N                          ! IN Plant functional type.

      REAL
     & G_LEAF(LAND_FIELD)         ! IN Rate of leaf turnover (/360days).
     &,HT(LAND_FIELD)             ! IN Canopy height (m).
     &,DTIME_PHEN                 ! IN Timestep (years).
     &,G_LEAF_PHEN(LAND_FIELD)    ! OUT Rate of leaf turnover
C                                 !     including leaf phenology
!                                 !     (/360days).
     &,LAI(LAND_FIELD)            ! INOUT Leaf area index.
     &,DPHEN                      ! WORK Increment to phenological
C                                 !      state.
     &,LAI_BAL(LAND_FIELD)        ! WORK Balanced growth LAI.
     &,PHEN(LAND_FIELD)           ! WORK Phenological state.

      INTEGER
     & J,L                        ! Loop counters

!      REAL RPI !Reciprocal of Pi
      REAL P0  !Constant in phenology rate equation, affects steepness of dp/dt step
      PARAMETER(P0=50.0)

C-----------------------------------------------------------------------
C Diagnose the phenological state
C-----------------------------------------------------------------------
      DO J=1,VEG_PTS
        L = VEG_INDEX(J)
        LAI_BAL(L) = (A_WS(N)*ETA_SL(N)*HT(L)
     &               /A_WL(N))**(1.0/(B_WL(N)-1))
        PHEN(L) = LAI(L)/LAI_BAL(L)
      ENDDO

C-----------------------------------------------------------------------
C Update the phenological state and output the leaf turnover rate in
C terms of the balanced growth LAI
C-----------------------------------------------------------------------
!      RPI = 1.0/(4.0*ATAN(1.0))

      DO J=1,VEG_PTS
        L = VEG_INDEX(J)

!bowie Smoother function added by PPH because step function leads to 
!bowie imperfect restarts.  This new function didn't solve the problem
!bowie but it may come in useful later.
!        DPHEN = DTIME_PHEN * G_GROW(N) * 2.0 * RPI
!     &            *ATAN(P0*(2.0*G_LEAF_0(N)-G_LEAF(L)))
!        IF(G_LEAF(L).GT.2*G_LEAF_0(N)) THEN
!          DPHEN = MAX(DPHEN,(0.01-PHEN(L)))
!          G_LEAF_PHEN(L) = -DPHEN/DTIME_PHEN
!        ELSE
!          DPHEN = DPHEN * (1.0-PHEN(L))
!          DPHEN = MIN(DPHEN,(1.0-PHEN(L)))
!          G_LEAF_PHEN(L) = PHEN(L)*G_LEAF(L)
!        ENDIF

        IF (G_LEAF(L).GT.2*G_LEAF_0(N)) THEN
          DPHEN = -DTIME_PHEN*G_GROW(N)
          DPHEN = MAX(DPHEN,(0.01-PHEN(L)))
          G_LEAF_PHEN(L) = -DPHEN/DTIME_PHEN
        ELSE
          DPHEN = DTIME_PHEN*G_GROW(N)*(1.0-PHEN(L))
          DPHEN = MIN(DPHEN,(1.0-PHEN(L)))
          G_LEAF_PHEN(L) = PHEN(L)*G_LEAF(L)
        ENDIF

C-----------------------------------------------------------------------
C Update the leaf area index
C-----------------------------------------------------------------------
        PHEN(L) = PHEN(L) + DPHEN
        LAI(L) = PHEN(L)*LAI_BAL(L)

      ENDDO

      RETURN

      END
