        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:21 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SLABOCEAN__genmod
          INTERFACE 
            SUBROUTINE SLABOCEAN(ISTEP,TSTAR_ATM,LATENT_ATM,SENSIBLE_ATM&
     &,NETSOLAR_ATM,NETLONG_ATM,PREC_ATM,EVAP_ATM,RUNOFF_ATM,           &
     &SEAICE_FRAC_ATM,TEMPTOP_ATM,CONDUCTFLUX_ATM,ALBEDO_ATM,ILAND,     &
     &TEST_ENERGY_OCEAN,TEST_WATER_OCEAN)
              INTEGER(KIND=4) :: ISTEP
              REAL(KIND=8) :: TSTAR_ATM(64,32)
              REAL(KIND=8) :: LATENT_ATM(64,32)
              REAL(KIND=8) :: SENSIBLE_ATM(64,32)
              REAL(KIND=8) :: NETSOLAR_ATM(64,32)
              REAL(KIND=8) :: NETLONG_ATM(64,32)
              REAL(KIND=8) :: PREC_ATM(64,32)
              REAL(KIND=8) :: EVAP_ATM(64,32)
              REAL(KIND=8) :: RUNOFF_ATM(64,32)
              REAL(KIND=8) :: SEAICE_FRAC_ATM(64,32)
              REAL(KIND=8) :: TEMPTOP_ATM(64,32)
              REAL(KIND=8) :: CONDUCTFLUX_ATM(64,32)
              REAL(KIND=8) :: ALBEDO_ATM(64,32)
              INTEGER(KIND=4) :: ILAND(64,32)
              REAL(KIND=8) :: TEST_ENERGY_OCEAN
              REAL(KIND=8) :: TEST_WATER_OCEAN
            END SUBROUTINE SLABOCEAN
          END INTERFACE 
        END MODULE SLABOCEAN__genmod
