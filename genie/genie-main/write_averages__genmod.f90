        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:15:17 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WRITE_AVERAGES__genmod
          INTERFACE 
            SUBROUTINE WRITE_AVERAGES(ISTEP,ALON1_ATM,ALAT1_ATM,        &
     &ALON1_OCN,ALAT1_OCN,ALON1_SIC,ALAT1_SIC,NETSOLAR_ATM,NETSOLAR_OCN,&
     &NETSOLAR_SIC,NETLONG_ATM,NETLONG_OCN,NETLONG_SIC,SENSIBLE_ATM,    &
     &SENSIBLE_OCN,SENSIBLE_SIC,LATENT_ATM,LATENT_OCN,LATENT_SIC,       &
     &STRESSX_ATM,STRESSX_OCN,STRESSX_SIC,STRESSY_ATM,STRESSY_OCN,      &
     &STRESSY_SIC,CONDUCTFLUX_ATM,CONDUCTFLUX_OCN,CONDUCTFLUX_SIC,      &
     &EVAP_ATM,EVAP_OCN,EVAP_SIC,PRECIP_ATM,PRECIP_OCN,PRECIP_SIC,      &
     &RUNOFF_ATM,RUNOFF_OCN,RUNOFF_SIC,WATERFLUX_ATM,WATERFLUX_OCN,     &
     &WATERFLUX_SIC,SEAICEFRAC_ATM,SEAICEFRAC_OCN,SEAICEFRAC_SIC,       &
     &TSTAR_ATM,TSTAR_OCN,TSTAR_SIC,ALBEDO_ATM,ALBEDO_OCN,ALBEDO_SIC)
              INTEGER(KIND=4) :: ISTEP
              REAL(KIND=8) :: ALON1_ATM(64)
              REAL(KIND=8) :: ALAT1_ATM(32)
              REAL(KIND=8) :: ALON1_OCN(64)
              REAL(KIND=8) :: ALAT1_OCN(32)
              REAL(KIND=8) :: ALON1_SIC(64)
              REAL(KIND=8) :: ALAT1_SIC(32)
              REAL(KIND=8) :: NETSOLAR_ATM(64,32)
              REAL(KIND=8) :: NETSOLAR_OCN(64,32)
              REAL(KIND=8) :: NETSOLAR_SIC(64,32)
              REAL(KIND=8) :: NETLONG_ATM(64,32)
              REAL(KIND=8) :: NETLONG_OCN(64,32)
              REAL(KIND=8) :: NETLONG_SIC(64,32)
              REAL(KIND=8) :: SENSIBLE_ATM(64,32)
              REAL(KIND=8) :: SENSIBLE_OCN(64,32)
              REAL(KIND=8) :: SENSIBLE_SIC(64,32)
              REAL(KIND=8) :: LATENT_ATM(64,32)
              REAL(KIND=8) :: LATENT_OCN(64,32)
              REAL(KIND=8) :: LATENT_SIC(64,32)
              REAL(KIND=8) :: STRESSX_ATM(64,32)
              REAL(KIND=8) :: STRESSX_OCN(64,32)
              REAL(KIND=8) :: STRESSX_SIC(64,32)
              REAL(KIND=8) :: STRESSY_ATM(64,32)
              REAL(KIND=8) :: STRESSY_OCN(64,32)
              REAL(KIND=8) :: STRESSY_SIC(64,32)
              REAL(KIND=8) :: CONDUCTFLUX_ATM(64,32)
              REAL(KIND=8) :: CONDUCTFLUX_OCN(64,32)
              REAL(KIND=8) :: CONDUCTFLUX_SIC(64,32)
              REAL(KIND=8) :: EVAP_ATM(64,32)
              REAL(KIND=8) :: EVAP_OCN(64,32)
              REAL(KIND=8) :: EVAP_SIC(64,32)
              REAL(KIND=8) :: PRECIP_ATM(64,32)
              REAL(KIND=8) :: PRECIP_OCN(64,32)
              REAL(KIND=8) :: PRECIP_SIC(64,32)
              REAL(KIND=8) :: RUNOFF_ATM(64,32)
              REAL(KIND=8) :: RUNOFF_OCN(64,32)
              REAL(KIND=8) :: RUNOFF_SIC(64,32)
              REAL(KIND=8) :: WATERFLUX_ATM(64,32)
              REAL(KIND=8) :: WATERFLUX_OCN(64,32)
              REAL(KIND=8) :: WATERFLUX_SIC(64,32)
              REAL(KIND=8) :: SEAICEFRAC_ATM(64,32)
              REAL(KIND=8) :: SEAICEFRAC_OCN(64,32)
              REAL(KIND=8) :: SEAICEFRAC_SIC(64,32)
              REAL(KIND=8) :: TSTAR_ATM(64,32)
              REAL(KIND=8) :: TSTAR_OCN(64,32)
              REAL(KIND=8) :: TSTAR_SIC(64,32)
              REAL(KIND=8) :: ALBEDO_ATM(64,32)
              REAL(KIND=8) :: ALBEDO_OCN(64,32)
              REAL(KIND=8) :: ALBEDO_SIC(64,32)
            END SUBROUTINE WRITE_AVERAGES
          END INTERFACE 
        END MODULE WRITE_AVERAGES__genmod
