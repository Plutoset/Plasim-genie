        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:05 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DIAG_BIOGEM_TIMESLICE__genmod
          INTERFACE 
            SUBROUTINE DIAG_BIOGEM_TIMESLICE(DUM_DTS,DUM_GENIE_CLOCK,   &
     &DUM_SFCATM1,DUM_SFXATM1,DUM_SFXOCN1,DUM_SFCSED1,DUM_SFXSED1)
              REAL(KIND=8), INTENT(IN) :: DUM_DTS
              INTEGER(KIND=8), INTENT(IN) :: DUM_GENIE_CLOCK
              REAL(KIND=8), INTENT(IN) :: DUM_SFCATM1(19,64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_SFXATM1(19,64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_SFXOCN1(56,64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_SFCSED1(56,64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_SFXSED1(56,64,32)
            END SUBROUTINE DIAG_BIOGEM_TIMESLICE
          END INTERFACE 
        END MODULE DIAG_BIOGEM_TIMESLICE__genmod
