        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:59 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CPL_COMP_ATMLND__genmod
          INTERFACE 
            SUBROUTINE CPL_COMP_ATMLND(DUM_N_ATM,DUM_N_I_ATM,DUM_N_J_ATM&
     &,DUM_N_I_LND,DUM_N_J_LND,DUM_SFCATM,DUM_SFCATM_LND)
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_LND
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_LND
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_ATM
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_ATM
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_ATM
              REAL(KIND=8), INTENT(IN) :: DUM_SFCATM(DUM_N_ATM,         &
     &DUM_N_I_ATM,DUM_N_J_ATM)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFCATM_LND(DUM_N_ATM,  &
     &DUM_N_I_LND,DUM_N_J_LND)
            END SUBROUTINE CPL_COMP_ATMLND
          END INTERFACE 
        END MODULE CPL_COMP_ATMLND__genmod
