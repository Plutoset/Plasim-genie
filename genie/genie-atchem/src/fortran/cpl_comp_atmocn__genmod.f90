        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:59 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CPL_COMP_ATMOCN__genmod
          INTERFACE 
            SUBROUTINE CPL_COMP_ATMOCN(DUM_N_ATM,DUM_N_I_ATM,DUM_N_J_ATM&
     &,DUM_N_I_OCN,DUM_N_J_OCN,DUM_SFCATM,DUM_SFCATM1)
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_OCN
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_OCN
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_ATM
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_ATM
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_ATM
              REAL(KIND=8), INTENT(IN) :: DUM_SFCATM(DUM_N_ATM,         &
     &DUM_N_I_ATM,DUM_N_J_ATM)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFCATM1(DUM_N_ATM,     &
     &DUM_N_I_OCN,DUM_N_J_OCN)
            END SUBROUTINE CPL_COMP_ATMOCN
          END INTERFACE 
        END MODULE CPL_COMP_ATMOCN__genmod
