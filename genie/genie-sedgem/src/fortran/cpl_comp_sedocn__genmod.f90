        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:07 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CPL_COMP_SEDOCN__genmod
          INTERFACE 
            SUBROUTINE CPL_COMP_SEDOCN(DUM_N_SED,DUM_N_I_OCN,DUM_N_J_OCN&
     &,DUM_N_I_SED,DUM_N_J_SED,DUM_SFCSED1,DUM_SFCSED)
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_SED
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_SED
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_OCN
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_OCN
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_SED
              REAL(KIND=8), INTENT(OUT) :: DUM_SFCSED1(DUM_N_SED,       &
     &DUM_N_I_OCN,DUM_N_J_OCN)
              REAL(KIND=8), INTENT(IN) :: DUM_SFCSED(DUM_N_SED,         &
     &DUM_N_I_SED,DUM_N_J_SED)
            END SUBROUTINE CPL_COMP_SEDOCN
          END INTERFACE 
        END MODULE CPL_COMP_SEDOCN__genmod
