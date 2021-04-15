        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:58 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FIXEDICESHEET__genmod
          INTERFACE 
            SUBROUTINE FIXEDICESHEET(ISTEP,ILAND_ATM,OROG_ATM,ALBEDO_ATM&
     &,ICEFRAC_ATM,ICONV)
              INTEGER(KIND=4) :: ISTEP
              INTEGER(KIND=4) :: ILAND_ATM(64,32)
              REAL(KIND=8) :: OROG_ATM(64,32)
              REAL(KIND=8) :: ALBEDO_ATM(64,32)
              REAL(KIND=8) :: ICEFRAC_ATM(64,32)
              INTEGER(KIND=4) :: ICONV
            END SUBROUTINE FIXEDICESHEET
          END INTERFACE 
        END MODULE FIXEDICESHEET__genmod
