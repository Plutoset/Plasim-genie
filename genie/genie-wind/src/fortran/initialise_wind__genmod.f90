        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:16 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE INITIALISE_WIND__genmod
          INTERFACE 
            SUBROUTINE INITIALISE_WIND(ALON1,ABOXEDGE1_LON,ALAT1,       &
     &ABOXEDGE1_LAT,ALON2,ABOXEDGE2_LON,ALAT2,ABOXEDGE2_LAT,ALON3,      &
     &ABOXEDGE3_LON,ALAT3,ABOXEDGE3_LAT,WINDSPEEDX2,WINDSPEEDY3,STRESSX2&
     &,STRESSY2,STRESSX3,STRESSY3)
              REAL(KIND=8), INTENT(IN) :: ALON1(64)
              REAL(KIND=8), INTENT(IN) :: ABOXEDGE1_LON(65)
              REAL(KIND=8), INTENT(IN) :: ALAT1(32)
              REAL(KIND=8), INTENT(IN) :: ABOXEDGE1_LAT(33)
              REAL(KIND=8), INTENT(IN) :: ALON2(64)
              REAL(KIND=8), INTENT(IN) :: ABOXEDGE2_LON(65)
              REAL(KIND=8), INTENT(IN) :: ALAT2(32)
              REAL(KIND=8), INTENT(IN) :: ABOXEDGE2_LAT(33)
              REAL(KIND=8), INTENT(IN) :: ALON3(64)
              REAL(KIND=8), INTENT(IN) :: ABOXEDGE3_LON(65)
              REAL(KIND=8), INTENT(IN) :: ALAT3(32)
              REAL(KIND=8), INTENT(IN) :: ABOXEDGE3_LAT(33)
              REAL(KIND=8), INTENT(OUT) :: WINDSPEEDX2(64,32)
              REAL(KIND=8), INTENT(OUT) :: WINDSPEEDY3(64,32)
              REAL(KIND=8), INTENT(OUT) :: STRESSX2(64,32)
              REAL(KIND=8), INTENT(OUT) :: STRESSY2(64,32)
              REAL(KIND=8), INTENT(OUT) :: STRESSX3(64,32)
              REAL(KIND=8), INTENT(OUT) :: STRESSY3(64,32)
            END SUBROUTINE INITIALISE_WIND
          END INTERFACE 
        END MODULE INITIALISE_WIND__genmod
