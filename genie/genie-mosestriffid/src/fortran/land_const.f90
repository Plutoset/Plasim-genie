!#######################################################################################
!#######################################################################################
! Module containing constants used by GENIE-land
! 19/11/03 PPH
!#######################################################################################
!#######################################################################################
MODULE land_const

  IMPLICIT NONE

  !***** NB Don't like setting these again here but can't use *****
  !*****    INCLUDE statement with f90 code                   *****
  INTEGER :: nlon           ! Number of atmosphere points on longitude axis
  INTEGER :: nlat           ! Number of atmosphere points on latitude axis
  INTEGER :: land_pts       ! Total number of land points on atmos grid

  ! ***** NB Is there a way of inquiring about the machine's REAL*4 kind when
  ! *****    compiling with -r8 (or equivalent)?
  INTEGER,PARAMETER :: kr_4 = 4  !Machine specific KIND of single precision reals
  INTEGER,PARAMETER :: kr_8 = 8  !Machine specific KIND of double precision reals
#ifdef real8
  INTEGER,PARAMETER :: kr_g = kr_8
#else
  INTEGER,PARAMETER :: kr_g = kr_4
#endif

#ifdef ncreal8
  INTEGER,PARAMETER :: kr_nc = kr_8
#else
  INTEGER,PARAMETER :: kr_nc = kr_4
#endif

  !###################################
  ! Time constants
  !###################################
  REAL,PARAMETER :: daylen_land    = 86400.0     !Daylength (s)
!bowie  REAL,PARAMETER :: yearlen_land   = daylen_land*dayinyear_land  !Year length (s)
  REAL :: yearlen_land                           !Year length (s)
  REAL :: dayinyear_land                         ! No. of days in a year (days)
  INTEGER :: nstepyear                           ! No. of land steps in a year (steps)
  INTEGER :: idiag_land, irest_land, iacc_land   ! Freq of diags, restarts and accounting (koverall steps)
  INTEGER :: itrif_land, iphen_land              ! Freq of phenology and TRIFFID updates (koverall steps)
  INTEGER :: steps_since_ice                     ! Number of land steps since last ice sheet update

  !###################################
  ! Formerly land.nsty.cmn
  !###################################
  INTEGER,PARAMETER :: nnvg     = 2            ! Number of non-vegetation surface types
  INTEGER,PARAMETER :: npft     = 5            ! Number of plant functional types.   
  INTEGER,PARAMETER :: ntype    = nnvg + npft  ! Number of surface types. 
  INTEGER,PARAMETER :: soil     = 6            ! Index of the surface type 'Soil'
  !                                 ! PFT    1 - Broadleaf Tree              
  !                                 ! PFT    2 - Needleleaf Tree             
  !                                 ! PFT    3 - C3 Grass                    
  !                                 ! PFT    4 - C4 Grass                    
  !                                 ! PFT    5 - Shrub                     
  !                                 ! NVG    6 - Soil
  !                                 ! NVG    7 - Landice
  !###################################
  ! Formerly land.desc.cmn
  !###################################  
  INTEGER,PARAMETER :: ITER_EQ    = 1      !NB JULES value = 10
  REAL,PARAMETER    :: DENOM_MIN  = 1.0E-6 !NB JULES value = 1.0E-6
  REAL,PARAMETER    :: GAMMA_EQ   = 1.0E-4 !NB JULES value = 1.0E-1
  !###################################
  ! Formerly land.carb.cmn
  !###################################
  REAL,PARAMETER    :: M_AIR       = 28.966
  REAL,PARAMETER    :: EPCO2       = 1.5194
  REAL,PARAMETER    :: EPSILON     = 0.62198
  REAL,PARAMETER    :: EPO2        = 1.106
  REAL,PARAMETER    :: M_CO2       = M_AIR*EPCO2
  REAL,PARAMETER    :: O2CONV_A2O  = M_AIR * 1E6 / M_CO2
  REAL,PARAMETER    :: CO2CONV_O2A = M_CO2 * 1e-3 / (360.0 * 24.0 * 3600.0)
  !###################################
  ! Formerly land.nveg.cmn
  !###################################
  REAL,DIMENSION(nnvg)              :: ALBSNC_NVG = (/ 0.80,  0.80   /)  !Tile snow-covered albedo (-)
  REAL,DIMENSION(nnvg)              :: ALBSNF_NVG = (/ 0.35,  0.75   /)  !Tile snow-free albedo (-)
  REAL,PARAMETER,DIMENSION(nnvg)    :: GS_NVG     = (/ 1.0E-6,1.0E6  /)  !Initial surface conductance (m s-1)
  REAL,PARAMETER,DIMENSION(nnvg)    :: Z0_NVG     = (/ 3.0e-2,1.0E-4 /)  !Roughness length (m)
  REAL,DIMENSION(nnvg)              :: HCAP_NVG   = (/ 3.3E5, 6.3E5  /)  !Sub-surface heat capacity (J/K/m3) [TUNABLE]
  REAL,DIMENSION(nnvg)              :: HCON_NVG   = (/ 1.00,  0.27   /)  !Surface thermal conductivity (W/m/K) [TUNABLE]

  !###################################
  ! Formerly land.seed.cmn
  !###################################
  REAL,PARAMETER    :: FRAC_MIN    = 1.0E-6
  REAL,PARAMETER    :: FRAC_SEED   = 0.01
  !###################################
  ! Formerly land.trif.cmn
  !###################################
  REAL,PARAMETER,DIMENSION(npft) :: C3        = (/      1,     1,     1,     0,     1 /)               
  REAL,PARAMETER,DIMENSION(npft) :: CROP      = (/      0,     0,     1,     1,     0 /)               
  REAL,PARAMETER,DIMENSION(npft) :: ALPHA     = (/   0.08,  0.08,  0.08, 0.040,  0.08 /)               
  REAL,PARAMETER,DIMENSION(npft) :: A_WL      = (/   0.65,  0.65, 0.005, 0.005,  0.10 /)               
  REAL,PARAMETER,DIMENSION(npft) :: A_WS      = (/  10.00, 10.00,  1.00,  1.00, 10.00 /)               
  REAL,PARAMETER,DIMENSION(npft) :: B_WL      = (/  1.667, 1.667, 1.667, 1.667, 1.667 /)               
  REAL,PARAMETER,DIMENSION(npft) :: DGL_DM    = (/    0.0,   0.0,   0.0,   0.0,   0.0 /)               
  REAL,PARAMETER,DIMENSION(npft) :: DGL_DT    = (/    9.0,   9.0,   0.0,   0.0,   9.0 /)               
  REAL,PARAMETER,DIMENSION(npft) :: DQCRIT    = (/  0.090, 0.060, 0.100, 0.075, 0.100 /)               
  REAL,PARAMETER,DIMENSION(npft) :: ETA_SL    = (/   0.01,  0.01,  0.01,  0.01,  0.01 /)               
  REAL,PARAMETER,DIMENSION(npft) :: F0        = (/  0.875, 0.875, 0.900, 0.800, 0.900 /)               
  REAL,PARAMETER,DIMENSION(npft) :: FSMC_OF   = (/   0.00,  0.00,  0.00,  0.00,  0.00 /)               
  REAL,PARAMETER,DIMENSION(npft) :: GLMIN     = (/ 1.0E-6,1.0E-6,1.0E-6,1.0E-6,1.0E-6 /)               
  REAL,PARAMETER,DIMENSION(npft) :: G_AREA    = (/  0.004, 0.004,  0.10,  0.10,  0.05 /)               
  REAL,PARAMETER,DIMENSION(npft) :: G_GROW    = (/  20.00, 20.00, 20.00, 20.00, 20.00 /)               
  REAL,PARAMETER,DIMENSION(npft) :: G_LEAF_0  = (/   0.25,  0.25,  0.25,  0.25,  0.25 /)               
  REAL,PARAMETER,DIMENSION(npft) :: G_ROOT    = (/   0.25,  0.25,  0.25,  0.25,  0.25 /)               
  REAL,PARAMETER,DIMENSION(npft) :: G_WOOD    = (/   0.01,  0.01,  0.20,  0.20,  0.05 /)               
  REAL,PARAMETER,DIMENSION(npft) :: KPAR      = (/   0.50,  0.50,  0.50,  0.50,  0.50 /)               
  REAL,PARAMETER,DIMENSION(npft) :: LAI_MAX   = (/   9.00,  9.00,  4.00,  4.00,  4.00 /)               
  REAL,PARAMETER,DIMENSION(npft) :: LAI_MIN   = (/   3.00,  3.00,  1.00,  1.00,  1.00 /)               
  REAL,PARAMETER,DIMENSION(npft) :: NL0       = (/  0.040, 0.030, 0.060, 0.030, 0.030 /)               
  REAL,PARAMETER,DIMENSION(npft) :: NR_NL     = (/   2.00,  2.00,  2.00,  2.00,  2.00 /)               
  REAL,PARAMETER,DIMENSION(npft) :: NS_NL     = (/   0.10,  0.10,  1.00,  1.00,  0.10 /)               
  REAL,PARAMETER,DIMENSION(npft) :: OMEGA     = (/   0.15,  0.15,  0.15,  0.17,  0.15 /)               
  REAL,PARAMETER,DIMENSION(npft) :: R_GROW    = (/   0.25,  0.25,  0.25,  0.25,  0.25 /)               
  REAL,PARAMETER,DIMENSION(npft) :: SIGL      = (/ 0.0375,0.1000,0.0250,0.0500,0.0500 /)               
  REAL,PARAMETER,DIMENSION(npft) :: TLEAF_OF  = (/ 273.15,243.15,258.15,258.15,243.15 /)               
  REAL,PARAMETER,DIMENSION(npft) :: TLOW      = (/    0.0,  -5.0,   0.0,  13.0,   0.0 /)               
  REAL,PARAMETER,DIMENSION(npft) :: TUPP      = (/   36.0,  31.0,  36.0,  45.0,  36.0 /)
  !###################################
  ! Formerly in pft_sparm
  !###################################
  REAL,DIMENSION(npft)           :: ALBSNC_MAX  = (/ 0.15, 0.15, 0.60, 0.60, 0.40 /) ! Snow-covered albedo for large LAI.
  REAL,DIMENSION(npft)           :: ALBSNC_MIN  = (/ 0.30, 0.30, 0.80, 0.80, 0.80 /) ! Snow-covered albedo for zero LAI.
  REAL,DIMENSION(npft)           :: ALBSNF_MAX  = (/ 0.10, 0.10, 0.20, 0.20, 0.20 /) ! Snow-free albedo for large LAI.
  REAL,PARAMETER,DIMENSION(npft) :: DZ0V_DH     = (/ 0.05, 0.05, 0.10, 0.10, 0.10 /) ! Rate of change of vegetation roughness length with height.
  REAL,PARAMETER,DIMENSION(npft) :: CATCH0      = (/ 0.50, 0.50, 0.50, 0.50, 0.50 /) ! Minimum canopy capacity (kg/m2).
  REAL,PARAMETER,DIMENSION(npft) :: DCATCH_DLAI = (/ 0.05, 0.05, 0.05, 0.05, 0.05 /) ! Rate of change of canopy capacity with LAI.
  REAL,PARAMETER,DIMENSION(npft) :: INFIL_F     = (/ 4.00, 4.00, 2.00, 2.00, 2.00 /) ! Infiltration enhancement factor.
  REAL,PARAMETER,DIMENSION(npft) :: KEXT        = (/ 0.50, 0.50, 0.50, 0.50, 0.50 /) ! Light extinction coefficient.
  REAL,PARAMETER,DIMENSION(npft) :: ROOTD_FT    = (/ 3.00, 1.00, 0.50, 0.50, 0.50 /) ! Rootdepth (m).

  !###################################
  ! Soil parameters
  !###################################
  REAL,PARAMETER :: dz_soil   = 2.0      ! Soil layer thickness (m)
  REAL,PARAMETER :: drain_max = 0.4E-5   ! Fixed bucket drainage rate at saturation (kg/m2/s)
  REAL           :: msat      = 1.0E3    ! Saturated soil moisture content (kg/m2) [TUNABLE]
  REAL,PARAMETER :: v_crit    = 0.9      ! Fractional soil moisture content at critical point (-)
  REAL,PARAMETER :: v_wilt    = 0.1      ! Fractional soil moisture content at wilting point (-)
  REAL           :: kaps      = 0.5E-8   ! Specific soil respiration rate at 25 deg and optimum soil moisture (/s). [TUNABLE]
  REAL           :: q10       = 2.0      ! Q10 factor for soil respiration. [TUNABLE]

  !###################################
  ! Misc.
  !###################################
  REAL,PARAMETER :: rl_blend = 0.05      !Reciprocal of the blending height for roughness length (/m)
  REAL,PARAMETER :: hleaf = 5.7E4        !Leaf specific heat capacity (J/K/kgC)
  REAL,PARAMETER :: hwood = 1.1E4        !Wood specific heat capacity (J/K/kgC)
  REAL           :: hcap_veg = 3.3E6     !Canopy heat capacity (J/K/m2) [TUNABLE]
  REAL           :: snow_thresh = 271.15 !Threshold of soil temperature below which precipitation
                                         ! falls as snow (K) [TUNABLE]
  REAL           :: lying_snow_max = 300.0 ! Maximum allowed snow water equivalent (kg/m2)

  !###################################
  ! Surface flux parameters
  !###################################
  REAL           :: ene_tune = 1.0       ! Tunable multiplicative parameter for sensible heat, 
                                         !   evaporation, and latent heat
  REAL           :: str_tune = 1.0       ! Tunable multiplicative parameter for wind stress
  REAL           :: alb_tune = 0.5       ! Tunable multiplicative parameter for surface albedo

  !###################################
  ! File/directory names 
  !###################################
  CHARACTER(len=200), PARAMETER :: out_dir = './mosestriffid/'  ! expecting a dir of this name to write to

END MODULE land_const
