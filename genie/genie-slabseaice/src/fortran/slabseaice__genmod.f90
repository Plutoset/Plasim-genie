        !COMPILER-GENERATED INTERFACE MODULE: Sun Apr 11 06:14:22 2021
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SLABSEAICE__genmod
          INTERFACE 
            SUBROUTINE SLABSEAICE(ISTEP,TSTAR_ATM,LATENT_ATM,           &
     &SENSIBLE_ATM,NETSOLAR_ATM,NETLONG_ATM,LATENT_INST,SENSIBLE_INST,  &
     &NETSOLAR_INST,NETLONG_INST,SEAICE_FRAC_ATM,TEMPTOP_ATM,           &
     &CONDUCTFLUX_ATM,ALBEDO_ATM,ILAND,TEST_ENERGY_SEAICE,              &
     &TEST_WATER_SEAICE,KSIC_LOOP)
              INTEGER(KIND=4) :: ISTEP
              REAL(KIND=8) :: TSTAR_ATM(64,32)
              REAL(KIND=8) :: LATENT_ATM(64,32)
              REAL(KIND=8) :: SENSIBLE_ATM(64,32)
              REAL(KIND=8) :: NETSOLAR_ATM(64,32)
              REAL(KIND=8) :: NETLONG_ATM(64,32)
              REAL(KIND=8) :: LATENT_INST(64,32)
              REAL(KIND=8) :: SENSIBLE_INST(64,32)
              REAL(KIND=8) :: NETSOLAR_INST(64,32)
              REAL(KIND=8) :: NETLONG_INST(64,32)
              REAL(KIND=8) :: SEAICE_FRAC_ATM(64,32)
              REAL(KIND=8) :: TEMPTOP_ATM(64,32)
              REAL(KIND=8) :: CONDUCTFLUX_ATM(64,32)
              REAL(KIND=8) :: ALBEDO_ATM(64,32)
              INTEGER(KIND=4) :: ILAND(64,32)
              REAL(KIND=8) :: TEST_ENERGY_SEAICE
              REAL(KIND=8) :: TEST_WATER_SEAICE
              INTEGER(KIND=4) :: KSIC_LOOP
            END SUBROUTINE SLABSEAICE
          END INTERFACE 
        END MODULE SLABSEAICE__genmod
