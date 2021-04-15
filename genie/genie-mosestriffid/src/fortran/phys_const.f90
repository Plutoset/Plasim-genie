!###############################################################
! This f90 module contains physical constants used by multiple
! GENIE components.
!
! Every value MUST be declared as a parameter so no-one can 
! accidently modify any values at run time.
! Every value MUST have a short description and the SI units.
!###############################################################
MODULE phys_const

  !###### Earth orbital "constants" ######
  REAL,PARAMETER :: con_orb_obl           = 0.41              ! Earth orbit obliquity (radians)
  REAL,PARAMETER :: con_orb_ecc           = 0.0167            ! Earth orbit eccentricity (-)
  REAL,PARAMETER :: con_orb_r0            = 1.496E11          ! Mean Earth-Sun distance (m) (149597870691. m = 1 Astronomical Unit to Astrophysicists!)
  REAL,PARAMETER :: con_orb_p2ve          = 1.352631          ! Angle of perihelion relative to vernal equinox (radians)
  REAL,PARAMETER :: con_s0                = 1367.0            ! Solar constant at mean Earth-Sun distance (W/m2)

  REAL,PARAMETER :: con_zeroc             = 273.15            ! Zero deg celcius in Kelvin (K)
  REAL,PARAMETER :: con_latfus            = 0.334E6           ! Latent heat of fusion at 0degC (J/kg)
  REAL,PARAMETER :: con_lateva            = 2.501E6           ! Latent heat of condensation at 0degC (J/kg)
  REAL,PARAMETER :: con_g                 = 9.8               ! Gravitational acceleration at Earth's surface (m/s2) 
  REAL,PARAMETER :: con_karman            = 0.4               ! von Karman's "constant" (-)
  REAL,PARAMETER :: con_gascon            = 287.05            ! Gas constant for dry air (J/kg/K)
  REAL,PARAMETER :: con_r                 = 8.3144            ! Molar gas constant for dry air (J/mol/K)
  REAL,PARAMETER :: con_cp                = 1005.0            ! Specific heat of dry air at constant pressure (W/kg/)
  REAL,PARAMETER :: con_kappa             = con_gascon/con_cp ! ()
  REAL,PARAMETER :: con_rhowat            = 1000.0            ! Density of pure water at STP (kg/m3)
  REAL,PARAMETER :: con_sboltz            = 5.67E-8           ! Stefan-Boltzman constant (W/m2/K4)
  REAL,PARAMETER :: con_epsil             = 0.62198           ! Ratio of molecular weights of water and dry air (-)

  REAL,PARAMETER :: con_molm_CO2          = 44.0E-3           ! Molar mass of CO2 (kg)
  REAL,PARAMETER :: con_molm_C            = 12.0E-3           ! Molar mass of C (kg)
  REAL,PARAMETER :: con_molm_dair         = 29.0E-3           ! Molar mass of dry air (kg)

  REAL,PARAMETER :: con_pi                = 3.141592654       ! Pi (radians)
  REAL,PARAMETER :: con_rpi               = 1.0/con_pi        ! Reciprocal of pi (/radians)
  REAL,PARAMETER :: con_rpi180            = 180.0*con_rpi     ! Conversion factor radians=>degrees (degrees/radians)
  REAL,PARAMETER :: con_pi180             = con_pi/180.0      ! Conversion factor degrees=>radians (radians/degrees)
  REAL,PARAMETER :: con_radea             = 6.370E6           ! Mean radius of the Earth (m)
  REAL,PARAMETER :: con_aearth            = 4.0*con_pi*con_radea*con_radea ! Total surface area of spherical Earth (m2)
  REAL,PARAMETER :: con_secday            = 86400.0           ! Seconds in 24 hour day (s)
  REAL,PARAMETER :: con_secdaysr          = 86164.8           ! Seconds in sidereal day (s)

CONTAINS

  SUBROUTINE make_pi
    IMPLICIT NONE
!    con_pi                = 4.0*ATAN(1.0)
!    con_rpi               = 1.0/con_pi
!    con_rpi180            = 180.0*con_rpi
!    con_pi180             = con_pi/180.0
    RETURN
  END SUBROUTINE make_pi

END MODULE phys_const
