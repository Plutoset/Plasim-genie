        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:22 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FIXEDOCEAN__genmod
          INTERFACE 
            SUBROUTINE FIXEDOCEAN(ISTEP,TSTAR,SEAICE_FRAC,              &
     &ENERGYCARRY_OCN_ICE,DTCARRY_OCN_ICE,ALBEDO,ILAND)
              INTEGER(KIND=4) :: ISTEP
              REAL(KIND=8) :: TSTAR(64,32)
              REAL(KIND=8) :: SEAICE_FRAC(64,32)
              REAL(KIND=8) :: ENERGYCARRY_OCN_ICE(64,32)
              REAL(KIND=8) :: DTCARRY_OCN_ICE(64,32)
              REAL(KIND=8) :: ALBEDO(64,32)
              INTEGER(KIND=4) :: ILAND(64,32)
            END SUBROUTINE FIXEDOCEAN
          END INTERFACE 
        END MODULE FIXEDOCEAN__genmod
