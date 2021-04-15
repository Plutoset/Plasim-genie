        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:12 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INITIALISE_SEDGEM__genmod
          INTERFACE 
            SUBROUTINE INITIALISE_SEDGEM(DUM_GENIE_TIMESTEP,            &
     &DUM_SFXSUMSED,DUM_SFCSUMOCN,DUM_SFCSED,DUM_SFXOCN)
              REAL(KIND=8), INTENT(IN) :: DUM_GENIE_TIMESTEP
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXSUMSED(56,36,36)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFCSUMOCN(56,36,36)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFCSED(56,36,36)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXOCN(56,36,36)
            END SUBROUTINE INITIALISE_SEDGEM
          END INTERFACE 
        END MODULE INITIALISE_SEDGEM__genmod
