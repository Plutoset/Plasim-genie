        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:59 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ATCHEM__genmod
          INTERFACE 
            SUBROUTINE ATCHEM(DUM_DTS,DUM_SFXSUMATM,DUM_SFCATM)
              REAL(KIND=8), INTENT(IN) :: DUM_DTS
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXSUMATM(19,64,32)
              REAL(KIND=8), INTENT(OUT) :: DUM_SFCATM(19,64,32)
            END SUBROUTINE ATCHEM
          END INTERFACE 
        END MODULE ATCHEM__genmod
