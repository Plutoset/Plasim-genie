        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:05 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INITIALISE_BIOGEM__genmod
          INTERFACE 
            SUBROUTINE INITIALISE_BIOGEM(DUM_SALN0,DUM_RHOAIR,DUM_CD,   &
     &DUM_DS,DUM_DPHI,DUM_USC,DUM_DSC,DUM_FSC,DUM_RH0SC,DUM_RHOSC,      &
     &DUM_CPSC,DUM_SOLCONST,DUM_SCF,DUM_IPS,DUM_IPF,DUM_IAS,DUM_IAF,    &
     &DUM_JSF,DUM_K1,DUM_DZ,DUM_DZA,DUM_C,DUM_CV,DUM_S,DUM_SV,DUM_TS,   &
     &DUM_TS1,DUM_SFCATM1,DUM_SFXATM1,DUM_SFCOCN1,DUM_SFXOCN1,          &
     &DUM_SFCSED1,DUM_SFXSED1,REINIT)
              REAL(KIND=8), INTENT(IN) :: DUM_SALN0
              REAL(KIND=8), INTENT(IN) :: DUM_RHOAIR
              REAL(KIND=8), INTENT(IN) :: DUM_CD
              REAL(KIND=8), INTENT(IN) :: DUM_DS
              REAL(KIND=8), INTENT(IN) :: DUM_DPHI
              REAL(KIND=8), INTENT(IN) :: DUM_USC
              REAL(KIND=8), INTENT(IN) :: DUM_DSC
              REAL(KIND=8), INTENT(IN) :: DUM_FSC
              REAL(KIND=8), INTENT(IN) :: DUM_RH0SC
              REAL(KIND=8), INTENT(IN) :: DUM_RHOSC
              REAL(KIND=8), INTENT(IN) :: DUM_CPSC
              REAL(KIND=8), INTENT(IN) :: DUM_SOLCONST
              REAL(KIND=8), INTENT(IN) :: DUM_SCF
              INTEGER(KIND=4), INTENT(IN) :: DUM_IPS(32)
              INTEGER(KIND=4), INTENT(IN) :: DUM_IPF(32)
              INTEGER(KIND=4), INTENT(IN) :: DUM_IAS(32)
              INTEGER(KIND=4), INTENT(IN) :: DUM_IAF(32)
              INTEGER(KIND=4), INTENT(IN) :: DUM_JSF
              INTEGER(KIND=4), INTENT(IN) :: DUM_K1(64,32)
              REAL(KIND=8), INTENT(IN) :: DUM_DZ(32)
              REAL(KIND=8), INTENT(IN) :: DUM_DZA(32)
              REAL(KIND=8), INTENT(IN) :: DUM_C(0:32)
              REAL(KIND=8), INTENT(IN) :: DUM_CV(0:32)
              REAL(KIND=8), INTENT(IN) :: DUM_S(0:32)
              REAL(KIND=8), INTENT(IN) :: DUM_SV(0:32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_TS(2,64,32,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_TS1(2,64,32,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFCATM1(19,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXATM1(19,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFCOCN1(56,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXOCN1(56,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFCSED1(56,64,32)
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXSED1(56,64,32)
              LOGICAL(KIND=4), INTENT(IN) :: REINIT
            END SUBROUTINE INITIALISE_BIOGEM
          END INTERFACE 
        END MODULE INITIALISE_BIOGEM__genmod
