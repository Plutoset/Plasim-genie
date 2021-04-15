        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:18 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE AWI_GENIE__genmod
          INTERFACE 
            SUBROUTINE AWI_GENIE(IDL,IL,ALON,JL,ALAT,A,MASK,IDLO,ILO,   &
     &ALONO,JLO,ALATO,AO,MASKO,IER)
              INTEGER(KIND=4) :: JLO
              INTEGER(KIND=4) :: ILO
              INTEGER(KIND=4) :: IDLO
              INTEGER(KIND=4) :: JL
              INTEGER(KIND=4) :: IL
              INTEGER(KIND=4) :: IDL
              REAL(KIND=8) :: ALON(IL+1)
              REAL(KIND=8) :: ALAT(JL+1)
              REAL(KIND=8) :: A(IDL,JL)
              LOGICAL(KIND=4) :: MASK(IDL,JL)
              REAL(KIND=8) :: ALONO(ILO+1)
              REAL(KIND=8) :: ALATO(JLO+1)
              REAL(KIND=8) :: AO(IDLO,JLO)
              LOGICAL(KIND=4) :: MASKO(IDLO,JLO)
              INTEGER(KIND=4) :: IER
            END SUBROUTINE AWI_GENIE
          END INTERFACE 
        END MODULE AWI_GENIE__genmod
