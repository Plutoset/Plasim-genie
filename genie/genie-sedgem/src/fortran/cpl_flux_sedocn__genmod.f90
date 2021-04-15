        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:07 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CPL_FLUX_SEDOCN__genmod
          INTERFACE 
            SUBROUTINE CPL_FLUX_SEDOCN(DUM_N_MAXOCN,DUM_N_MAXI,         &
     &DUM_N_MAXJ,DUM_NS_MAXI,DUM_NS_MAXJ,DUM_SFXOCN1,DUM_SFXOCN)
              INTEGER(KIND=4), INTENT(IN) :: DUM_NS_MAXJ
              INTEGER(KIND=4), INTENT(IN) :: DUM_NS_MAXI
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_MAXJ
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_MAXI
              INTEGER(KIND=4), INTENT(IN) :: DUM_N_MAXOCN
              REAL(KIND=8), INTENT(INOUT) :: DUM_SFXOCN1(DUM_N_MAXOCN,  &
     &DUM_N_MAXI,DUM_N_MAXJ)
              REAL(KIND=8), INTENT(IN) :: DUM_SFXOCN(DUM_N_MAXOCN,      &
     &DUM_NS_MAXI,DUM_NS_MAXJ)
            END SUBROUTINE CPL_FLUX_SEDOCN
          END INTERFACE 
        END MODULE CPL_FLUX_SEDOCN__genmod
