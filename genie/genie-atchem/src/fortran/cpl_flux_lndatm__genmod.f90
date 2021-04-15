        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:59 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CPL_FLUX_LNDATM__genmod
          INTERFACE 
            SUBROUTINE CPL_FLUX_LNDATM(DUM_DTS,DUM_N_ATM,DUM_N_I_ATM,   &
     &DUM_N_J_ATM,DUM_N_I_LND,DUM_N_J_LND,DUM_SFXATM_LND,DUM_SFXSUMATM)
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_LND
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_LND
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_J_ATM
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_I_ATM
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_ATM
              REAL(KIND=8), INTENT(IN) :: DUM_DTS
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXATM_LND(DUM_N_ATM,  &
     &DUM_N_I_LND,DUM_N_J_LND)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXSUMATM(DUM_N_ATM,   &
     &DUM_N_I_ATM,DUM_N_J_ATM)
            END SUBROUTINE CPL_FLUX_LNDATM
          END INTERFACE 
        END MODULE CPL_FLUX_LNDATM__genmod
