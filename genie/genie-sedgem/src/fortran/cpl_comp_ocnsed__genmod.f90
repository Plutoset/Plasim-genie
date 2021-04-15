        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:07 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CPL_COMP_OCNSED__genmod
          INTERFACE 
            SUBROUTINE CPL_COMP_OCNSED(DUM_OCNSTEP,DUM_MBIOGEM,         &
     &DUM_MSEDGEM,DUM_N_OCN,DUM_N_I_OCN,DUM_N_J_OCN,DUM_N_I_SED,        &
     &DUM_N_J_SED,DUM_SFCOCN1,DUM_SFCSUMOCN)
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_SED
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_SED
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_OCN
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_OCN
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_OCN
              INTEGER(KIND=4), INTENT(IN) :: DUM_OCNSTEP
              INTEGER(KIND=4), INTENT(IN) :: DUM_MBIOGEM
              INTEGER(KIND=4), INTENT(IN) :: DUM_MSEDGEM
              REAL(KIND=8), INTENT(IN) :: DUM_SFCOCN1(DUM_N_OCN,        &
     &DUM_N_I_OCN,DUM_N_J_OCN)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFCSUMOCN(DUM_N_OCN,   &
     &DUM_N_I_SED,DUM_N_J_SED)
            END SUBROUTINE CPL_COMP_OCNSED
          END INTERFACE 
        END MODULE CPL_COMP_OCNSED__genmod
