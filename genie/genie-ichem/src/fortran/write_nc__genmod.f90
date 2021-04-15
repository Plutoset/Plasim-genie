        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:14 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WRITE_NC__genmod
          INTERFACE 
            SUBROUTINE WRITE_NC(LON,LAT,PLEV,YEARN,DOYN,ANNMEAN,TOTMEAN,&
     &OUTDIR)
              REAL(KIND=4) :: LON(64)
              REAL(KIND=4) :: LAT(32)
              REAL(KIND=4) :: PLEV(7)
              REAL(KIND=4) :: YEARN
              REAL(KIND=4) :: DOYN
              REAL(KIND=4) :: ANNMEAN(64,32,7)
              REAL(KIND=4) :: TOTMEAN(64,32,7)
              CHARACTER(LEN=200) :: OUTDIR
            END SUBROUTINE WRITE_NC
          END INTERFACE 
        END MODULE WRITE_NC__genmod
