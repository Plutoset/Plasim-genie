!$Id: canopy.f 2817 2005-12-22 14:10:10Z cvs-pph $
!***********************************************************************
! Calculates the canopy resistance, net photosynthesis and transpiration
! by scaling-up the leaf level response using the "Big-Leaf" approach
! of Sellers et al. (1994)
!
! Written by Peter Cox (May 1995)
!***********************************************************************
      SUBROUTINE CANOPY (LAND_FIELD,LAND_INDEX,P_FIELD
     &,                  VEG_PTS,VEG_INDEX,dayfrac
     &,                  FT,DQC,IPAR,TSTAR,CO2C,O2,PSTAR
     &,                  FPAR,FSMC
     &,                  GC,ANETC,CI,RDC)

      USE phys_const, only : con_r
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
     & CO2C(LAND_FIELD)           ! IN Canopy level CO2 concentration
!                                 !    (kg CO2/kg air).
     &,dayfrac(land_field)        ! IN Fraction of 24 hours that sun is above horizon (-)
     &,DQC(LAND_FIELD)            ! IN Canopy level specific humidity
!                                 !    deficit (kg H2O/kg air).
     &,O2                         ! IN Atmospheric O2 concentration
!                                 !    (kg O2/kg air).
     &,PSTAR(LAND_FIELD)          ! IN Surface pressure (Pa).
     &,IPAR(P_FIELD)              ! IN Incident PAR (W/m2).
     &,TSTAR(LAND_FIELD)          ! IN Surface temperature (K).
     &,FPAR(LAND_FIELD)           ! IN PAR absorption factor.
     &,FSMC(LAND_FIELD)           ! IN Soil water factor.


      REAL
     & ANETC(LAND_FIELD)          ! OUT Net canopy photosynthesis
!                                 !     (mol CO2/m2/s).
     &,CI(LAND_FIELD)             ! OUT Internal CO2 concentration
!                                 !     (mol CO2/m3).
     &,GC(LAND_FIELD)             ! OUT Canopy conductance for H2O
!                                 !     (m/s).
     &,RDC(LAND_FIELD)            ! OUT Canopy dark respiration
!                                 !     (mol CO2/m2/s).
     &,ANETL(LAND_FIELD)          ! WORK Net leaf photosynthesis
!                                 !      (mol CO2/m2/s/LAI).
     &,APAR(LAND_FIELD)           ! WORK PAR absorbed by the top leaf
!                                 !      (W/m2).
     &,CA(LAND_FIELD)             ! WORK Canopy level CO2 pressure
!                                 !      (Pa).
     &,GL(LAND_FIELD)             ! WORK Leaf conductance for H2O
!                                 !      (m/s).
     &,OA(LAND_FIELD)             ! WORK Atmospheric O2 pressure
!                                 !      (Pa).
     &,RD(LAND_FIELD)             ! WORK Dark respiration of top leaf
!                                 !      (mol CO2/m2/s).

      INTEGER
     & I,J,L                      ! WORK Loop counters.

!-----------------------------------------------------------------------
! Calculate the atmospheric pressures of CO2 and O2
!-----------------------------------------------------------------------
      DO J=1,VEG_PTS
        L = VEG_INDEX(J)
        I = LAND_INDEX(L)

        CA(L) = CO2C(L) / EPCO2 * PSTAR(L)
        OA(L) = O2 / EPO2 * PSTAR(L)

!-----------------------------------------------------------------------
! Calculate the PAR absorbed by the top leaf
!-----------------------------------------------------------------------
        APAR(L) = (1 - OMEGA(FT)) * IPAR(I)

      ENDDO

!-----------------------------------------------------------------------
! Call the leaf level model for the top leaf of the C3 and C4 plants
!-----------------------------------------------------------------------

      IF ( C3(FT) .EQ. 1 ) THEN

        CALL LEAF_C3 (LAND_FIELD,VEG_PTS,VEG_INDEX,FT,dayfrac
     &,               DQC,APAR,TSTAR,CA,OA,FSMC
     &,               GL,ANETL,CI,RD)

      ELSE

        CALL LEAF_C4 (LAND_FIELD,VEG_PTS,VEG_INDEX,FT,dayfrac
     &,               DQC,APAR,TSTAR,CA,PSTAR,FSMC
     &,               GL,ANETL,CI,RD)

      ENDIF

!-----------------------------------------------------------------------
! Scale-up to the canopy level
!-----------------------------------------------------------------------
      DO J=1,VEG_PTS
        L = VEG_INDEX(J)

        ANETC(L) = ANETL(L) * FPAR(L)
        GC(L) = FPAR(L) * GL(L)
        RDC(L) = RD(L) * FPAR(L)

      ENDDO

      RETURN

      END
