        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:05 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE BIOGEM_CLIMATE__genmod
          INTERFACE 
            SUBROUTINE BIOGEM_CLIMATE(DUM_HGHT_SIC,DUM_FRAC_SIC,DUM_COST&
     &,DUM_SOLFOR,DUM_FXSW,DUM_U,DUM_TAU,DUM_MLD,DUM_SOLCONST)
              REAL(KIND=8), INTENT(IN) :: DUM_HGHT_SIC(64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_FRAC_SIC(64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_COST(64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_SOLFOR(32)
              REAL(KIND=8), INTENT(IN) :: DUM_FXSW(64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_U(3,64,32,32)
              REAL(KIND=8), INTENT(IN) :: DUM_TAU(2,64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_MLD(64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SOLCONST
            END SUBROUTINE BIOGEM_CLIMATE
          END INTERFACE 
        END MODULE BIOGEM_CLIMATE__genmod
