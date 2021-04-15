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
!***********************************************************************
! Calculates the leaf turnover rate as a function of temperature and
! soil water availability
!***********************************************************************
      SUBROUTINE LEAF_LIT (LAND_FIELD,VEG_PTS,VEG_INDEX,N,FSMC,TSTAR
     &,                    G_LEAF)

      USE land_const
      IMPLICIT NONE

      INTEGER
     & LAND_FIELD                 ! IN Total number of land points.
     &,VEG_PTS                    ! IN Number of vegetated points.
     &,VEG_INDEX(LAND_FIELD)      ! IN Index of vegetated points
!                                 !    on the land grid.
     &,N                          ! IN Plant functional type.

      REAL
     & FSMC(LAND_FIELD)           ! IN Soil moisture availability
!                                 !    factor.
     &,TSTAR(LAND_FIELD)          ! IN Surface temperature (K).
     &,G_LEAF(LAND_FIELD)         ! OUT Rate of leaf turnover
!                                 !     (/360days).
     &,FM,FT                      ! WORK Soil moisture and leaf
!                                        temperature amplifiers of
!                                        leaf turnover.

      INTEGER
     & J,L                        ! Loop counters

!-----------------------------------------------------------------------
! Calculate the leaf turnover rate
!-----------------------------------------------------------------------
      DO J=1,VEG_PTS
        L = VEG_INDEX(J)

        FT = 1.0
        FM = 1.0
        IF (TSTAR(L) .LT. TLEAF_OF(N)) THEN
          FT = 1.0 + DGL_DT(N)*(TLEAF_OF(N)-TSTAR(L))
        ELSEIF (FSMC(L) .LT. FSMC_OF(N)) THEN
          FM = 1.0 + DGL_DM(N)*(FSMC_OF(N)-FSMC(L))
        ENDIF

        G_LEAF(L) = G_LEAF_0(N)*FT*FM

      ENDDO

      RETURN

      END
