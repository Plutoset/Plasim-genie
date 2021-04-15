        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:21 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INITIALISE_SLABOCEAN__genmod
          INTERFACE 
            SUBROUTINE INITIALISE_SLABOCEAN(TSTAR,ALBEDO,SEAICE_FRAC,   &
     &TEMPTOP,ILAND,TEST_ENERGY,TEST_WATER)
              REAL(KIND=8) :: TSTAR(64,32)
              REAL(KIND=8) :: ALBEDO(64,32)
              REAL(KIND=8) :: SEAICE_FRAC(64,32)
              REAL(KIND=8) :: TEMPTOP(64,32)
              INTEGER(KIND=4) :: ILAND(64,32)
              REAL(KIND=8) :: TEST_ENERGY
              REAL(KIND=8) :: TEST_WATER
            END SUBROUTINE INITIALISE_SLABOCEAN
          END INTERFACE 
        END MODULE INITIALISE_SLABOCEAN__genmod
