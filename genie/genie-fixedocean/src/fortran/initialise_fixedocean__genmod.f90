        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:22 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INITIALISE_FIXEDOCEAN__genmod
          INTERFACE 
            SUBROUTINE INITIALISE_FIXEDOCEAN(TSTAR,ALBEDO,SEAICE_FRAC,  &
     &ILAND)
              REAL(KIND=8) :: TSTAR(64,32)
              REAL(KIND=8) :: ALBEDO(64,32)
              REAL(KIND=8) :: SEAICE_FRAC(64,32)
              INTEGER(KIND=4) :: ILAND(64,32)
            END SUBROUTINE INITIALISE_FIXEDOCEAN
          END INTERFACE 
        END MODULE INITIALISE_FIXEDOCEAN__genmod
