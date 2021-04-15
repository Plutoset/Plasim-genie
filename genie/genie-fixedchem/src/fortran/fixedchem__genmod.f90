        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:59 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FIXEDCHEM__genmod
          INTERFACE 
            SUBROUTINE FIXEDCHEM(ISTEP,CO2,N2O,CH4,ICONV)
              INTEGER(KIND=4) :: ISTEP
              REAL(KIND=8) :: CO2(64,32)
              REAL(KIND=8) :: N2O(64,32)
              REAL(KIND=8) :: CH4(64,32)
              INTEGER(KIND=4) :: ICONV
            END SUBROUTINE FIXEDCHEM
          END INTERFACE 
        END MODULE FIXEDCHEM__genmod
