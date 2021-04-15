
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
! Routine to calculate the land surface parameters of a given PFT from
! its areal fraction and structural properties.
C
! Written by Peter Cox (June 1997)
C**********************************************************************
      SUBROUTINE PFT_SPARM  (LAND_FIELD,N,TILE_INDEX,TILE_PTS
     &,                      ALBSOIL,HT,LAI
     &,                      ALBSNC_T,ALBSNF_T,CATCH_T
     &,                      Z0_T)

      USE land_const
      IMPLICIT NONE

      INTEGER
     & LAND_FIELD                 ! IN Number of land points.
     &,N                          ! IN Plant functional type.
     &,TILE_PTS                   ! IN Number of land points which
!                                 !    include the surface type.
     &,TILE_INDEX(LAND_FIELD)     ! IN Indices of land points which
!                                 !    include the surface type.
     &,J,L                        ! WORK Loop counters.

      REAL
     & ALBSOIL(LAND_FIELD)        ! IN Soil albedo.
     &,HT(LAND_FIELD)             ! IN Vegetation height (m).
     &,LAI(LAND_FIELD)            ! IN Leaf area index.
     &,ALBSNC_T(LAND_FIELD)       ! OUT Snow-covered albedo.
     &,ALBSNF_T(LAND_FIELD)       ! OUT Snow-free albedo.
     &,CATCH_T(LAND_FIELD)        ! OUT Canopy capacity (kg/m2).
     &,Z0_T(LAND_FIELD)           ! OUT Roughness length (m).
     &,FLIT                       ! WORK Weighting factor for albedo.


      DO J=1,TILE_PTS
        L = TILE_INDEX(J)
        FLIT = 1.0 - EXP(-KEXT(N) * LAI(L))
        ALBSNC_T(L) = ALBSNC_MIN(N) * (1 - FLIT)
     &              + ALBSNC_MAX(N) * FLIT
        ALBSNF_T(L) = ALBSOIL(L) * (1 - FLIT) + ALBSNF_MAX(N) * FLIT
        Z0_T(L) = DZ0V_DH(N) * HT(L)
        CATCH_T(L) = CATCH0(N) + DCATCH_DLAI(N) * LAI(L)
      ENDDO


      RETURN
      END
