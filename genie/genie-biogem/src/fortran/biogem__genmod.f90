        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:05 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE BIOGEM__genmod
          INTERFACE 
            SUBROUTINE BIOGEM(DUM_DTS,DUM_GENIE_CLOCK,DUM_TS,DUM_TS1,   &
     &DUM_SFCATM1,DUM_SFXATM1,DUM_SFCOCN1,DUM_SFXOCN1,DUM_SFCSED1,      &
     &DUM_SFXSED1,DUM_SFXSUMROK1)
              REAL(KIND=8), INTENT(IN) :: DUM_DTS
              INTEGER(KIND=8), INTENT(IN) :: DUM_GENIE_CLOCK
              REAL(KIND=8), INTENT(INOUT) :: DUM_TS(2,64,32,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_TS1(2,64,32,32)
              REAL(KIND=8), INTENT(IN) :: DUM_SFCATM1(19,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXATM1(19,64,32)
              REAL(KIND=8), INTENT(OUT) :: DUM_SFCOCN1(56,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXOCN1(56,64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_SFCSED1(56,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXSED1(56,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXSUMROK1(56,64,32)
            END SUBROUTINE BIOGEM
          END INTERFACE 
        END MODULE BIOGEM__genmod
