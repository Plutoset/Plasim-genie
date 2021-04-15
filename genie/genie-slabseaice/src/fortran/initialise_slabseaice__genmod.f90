        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:22 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INITIALISE_SLABSEAICE__genmod
          INTERFACE 
            SUBROUTINE INITIALISE_SLABSEAICE(TSTAR,ALBEDO,SEAICE_FRAC,  &
     &CONDUCTFLUX,ILAND,TEST_ENERGY,TEST_WATER,KSIC_LOOP)
              REAL(KIND=8) :: TSTAR(64,32)
              REAL(KIND=8) :: ALBEDO(64,32)
              REAL(KIND=8) :: SEAICE_FRAC(64,32)
              REAL(KIND=8) :: CONDUCTFLUX(64,32)
              INTEGER(KIND=4) :: ILAND(64,32)
              REAL(KIND=8) :: TEST_ENERGY
              REAL(KIND=8) :: TEST_WATER
              INTEGER(KIND=4) :: KSIC_LOOP
            END SUBROUTINE INITIALISE_SLABSEAICE
          END INTERFACE 
        END MODULE INITIALISE_SLABSEAICE__genmod
