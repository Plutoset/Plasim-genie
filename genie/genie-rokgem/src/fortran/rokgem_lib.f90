!
! File: rokgem_lib.f90
! 
! Description: this is the library module contains all the variable allocations for RokGeM
!
! Uses:
!
! - <genie_control>
! - <gem_cmn>
! - <gem_util>
! - <gem_carbchem>

MODULE rokgem_lib


  use genie_control
  use gem_cmn
  use gem_util
  use gem_carbchem
  IMPLICIT NONE
  SAVE

  ! ****************************************************************************************************************************** !
  ! *** NAMELIST DEFINITIONS ***************************************************************************************************** !
  ! ****************************************************************************************************************************** !


  ! ### EDIT ADD AND/OR EXTEND NAME-LIST PARAMETER AND CONTROL OPTIONS ########################################################### !
  ! ------------------- RUN CONTROL ---------------------------------------------------------------------------------------------- !
  logical::ctrl_continuing                                                               ! continuing run?
  NAMELIST /ini_rokgem_nml/ctrl_continuing
  REAL::start_year                                                                       ! Simulation start year [real]
  NAMELIST /ini_rokgem_nml/start_year
  ! ------------------- I/O DEFINITIONS ------------------------------------------------------------------------------------------ !
  CHARACTER(len=127)::par_indir_name,par_outdir_name,par_rstdir_name                     ! 
  NAMELIST /ini_rokgem_nml/par_indir_name,par_outdir_name,par_rstdir_name
  CHARACTER(len=63)::par_infile_name,par_outfile_name                                    ! 
  NAMELIST /ini_rokgem_nml/par_infile_name,par_outfile_name
  logical::opt_screen_output                                                             ! output to screen
  NAMELIST /ini_rokgem_nml/opt_screen_output
  CHARACTER(len=63)::par_output_years_file_0d                                            ! file containing years for 0D output to be generated (to screen and file)
  CHARACTER(len=63)::par_output_years_file_2d                                            ! file containing years for 2D output to be generated (to file)  
  NAMELIST /ini_rokgem_nml/par_output_years_file_0d,par_output_years_file_2d
  logical::opt_2d_ascii_output                                                           ! output 2d fields to .dat files
  logical::opt_2d_netcdf_output                                                          ! output 2d fields to netcdf
  logical::opt_append_data                                                               ! append data to output files on restart
  NAMELIST /ini_rokgem_nml/opt_2d_ascii_output,opt_2d_netcdf_output,opt_append_data
  ! --- RIVER ROUTING PARAMETERS ------------------------------------------------------------------------------------------------- !
  INTEGER::routing_scheme                                                                ! routing scheme to use: 1 = 'roof' routing using k1 file; 
  NAMELIST /ini_rokgem_nml/routing_scheme                                                ! 2 = intermediate using detailed map, but roof for stuff that ends up on genie land grid (about half)
                                                                                         ! 3 = detailed scheme, scaling up coastal ocean flux to match total number of genie land cells
  CHARACTER(len=63)::topo                                                                ! file containing basic (roof) river routing to read (k1 file)
  NAMELIST /ini_rokgem_nml/topo
  CHARACTER(len=63)::routing                                                             ! prefix of file containing detailed river routing to read (based on detailed topographic data) - suffix is grid dimensions (n_i_n_j.dat)
  NAMELIST /ini_rokgem_nml/routing
  INTEGER::max_drain_cells                                                               ! maximum number of ocean cells a single land cell routes to
  NAMELIST /ini_rokgem_nml/max_drain_cells
  !--- WEATHERING PARAMETERS ----------------------------------------------------------------------------------------------------- !
  LOGICAL:: opt_short_circuit_atm                                                        !short circuit atmosphere by not taking CO2 directly, instead have less DIC put into ocean.
  NAMELIST /ini_rokgem_nml/opt_short_circuit_atm
  CHARACTER(len=63)::par_weathopt                                                        ! weathering scheme ID string ('Global_avg','GKWM',or 'GEM_CO2')
  NAMELIST /ini_rokgem_nml/par_weathopt
  LOGICAL:: opt_weather_T_Ca                                                             ! CaCO3 weathering-temperature feedback
  LOGICAL:: opt_weather_T_Si                                                             ! CaSiO3 weathering-temperature feedback
  LOGICAL:: opt_weather_R_explicit                                                       ! if true then R/R_0 is used rather than the 1 + 0.045(T-T_0) parameterisation from GEOCARB.
  LOGICAL:: opt_weather_R_Ca                                                             ! CaCO3 weathering-runoff feedback
  LOGICAL:: opt_weather_R_Si                                                             ! CaSiO3 weathering-runoff feedback
  NAMELIST /ini_rokgem_nml/opt_weather_R_explicit,opt_weather_R_Ca,opt_weather_R_Si
  LOGICAL:: opt_weather_P_explicit                                                       ! if true then P/P_0 is used rather than the [2RCO2/(1+RCO2)]^0.4 parameterisation from GEOCARB
  LOGICAL:: opt_weather_P_Ca                                                             ! CaCO3 weathering-productivity feedback
  LOGICAL:: opt_weather_P_Si                                                             ! CaSiO3 weathering-productivity feedback
  NAMELIST /ini_rokgem_nml/opt_weather_T_Ca,opt_weather_T_Si,opt_weather_P_explicit,opt_weather_P_Ca,opt_weather_P_Si
  CHARACTER(len=63)::par_prodopt                                                         ! prodictivity to use ("GPP" or "NPP")
  NAMELIST /ini_rokgem_nml/par_prodopt
  REAL:: par_weather_T0                                                                  ! weathering reference mean global land surface temperature (C)
  REAL:: par_weather_R0                                                                  ! weathering reference mean global runoff (mm/s)  
  REAL:: par_weather_P0                                                                  ! weathering reference mean global land productivity (kgC m-2 yr-1)
  REAL:: par_weather_CO20                                                                ! weathering reference mean global land atmospheric CO2 level (ppm)  
  NAMELIST /ini_rokgem_nml/par_weather_T0,par_weather_R0,par_weather_P0,par_weather_CO20 
  REAL:: par_outgas_CO2                                                                  ! CO2 outgassing rate (mol C yr-1)
  REAL:: par_outgas_CO2_13C                                                              ! mean volcanic/metamorphic d13C (o/oo)
  NAMELIST /ini_rokgem_nml/par_outgas_CO2,par_outgas_CO2_13C          
  ! ------------------- GLOBAL AVERAGE WEATHERING PARAMETERS --------------------------------------------------------------------- !
  REAL:: par_weather_CaSiO3                                                              ! global silicate weathering rate (mol ALK yr-1)
  REAL:: par_weather_CaCO3                                                               ! global carbonate weathering rate (mol ALK yr-1)
  REAL:: par_weather_CaCO3_13C                                                           ! mean carbonate d13C (o/oo)
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3,par_weather_CaCO3,par_weather_CaCO3_13C
  real:: par_weather_CaSiO3_fracLi                                                       ! global silicate Li abundance
  real:: par_weather_CaSiO3_Li_d7Li                                                      ! global silicate d7Li (o/oo)
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_fracLi,par_weather_CaSiO3_Li_d7Li
  real::par_weather_CaSiO3_Li_7Li_epsilon                                                ! clay fractionation at normalized weathering (o/oo)
  real::par_weather_CaSiO3_Li_7Li_epsilon_max                                            ! maximum clay fractionation (o/oo)  
  NAMELIST /ini_rokgem_nml/par_weather_CaSiO3_Li_7Li_epsilon,par_weather_CaSiO3_Li_7Li_epsilon_max
  ! ------------------- 2D WEATHERING PARAMETERS --------------------------------------------------------------------------------- !
  CHARACTER(len=63)::par_lith_data 
  NAMELIST /ini_rokgem_nml/par_lith_data                                                 !name of lithological data set - corresponding to directory genie-rokgem/data/input/lithologies_rg_lith_data_036_036
  LOGICAL:: truncate_to_land                                                             ! truncate lithological maps to genie land-mask - if option is set to false than flux from land in genie ocean, goes direct to ocean
  NAMELIST /ini_rokgem_nml/truncate_to_land
  LOGICAL:: calibrate_weath                                                              ! calibrate 2D weathering - if .true. use values below
  NAMELIST /ini_rokgem_nml/calibrate_weath  
  REAL:: calibrate_weather_GKWM_CaCO3                                                    ! calibration values for 2D CaCO3 weathering - to avoid drift, set equal to (half of CaCO3 sediment burrial flux)/(original uncorrected flux) 
  REAL:: calibrate_weather_GEM_CO2_CaCO3                                                  ! (e.g. 1.5754 for Gi, 1.0505 for Am)
  NAMELIST /ini_rokgem_nml/calibrate_weather_GKWM_CaCO3,calibrate_weather_GEM_CO2_CaCO3
  REAL:: calibrate_weather_GKWM_CaSiO3                                                   ! calibration values for 2D CaSiO3 weathering - to avoid drift, set equal to (half of CaCO3 sediment burrial flux)/(original uncorrected flux) 
  REAL:: calibrate_weather_GEM_CO2_CaSiO3                                                 ! (e.g. 0.8510 for Gi, 0.7917 for Am) or leave as 1.0 for uncalibrated.
  NAMELIST /ini_rokgem_nml/calibrate_weather_GKWM_CaSiO3,calibrate_weather_GEM_CO2_CaSiO3
  LOGICAL:: calibrate_runoff                                                             ! calibrate runoff in 2D weathering functions to values in papers
  NAMELIST /ini_rokgem_nml/calibrate_runoff
  ! ############################################################################################################################## !


  ! *************************************
  ! *** MODEL CONFIGURATION CONSTANTS ***
  ! *************************************

  ! *** array dimensions ***
  ! grid dimensions
  INTEGER,PARAMETER::n_i                                  = ilon1_rok ! 
  INTEGER,PARAMETER::n_j                                  = ilat1_rok ! 
  ! grid properties array dimensions 
  INTEGER,PARAMETER::n_phys_rok                           = 08    ! number of grid properties descriptors
  INTEGER,PARAMETER::n_phys_ocnrok                        = 06    ! number of grid properties descriptors

  ! *** PRIMARY rokgem ARRAYS ***
  real,dimension(n_phys_rok,n_i,n_j)::phys_rok                                         ! 'physical' array info - see below (lats, lons, areas of grid cells)

  ! *** array index values ***
  ! weathering 'physics' properties array indices
  INTEGER,PARAMETER::ipr_lat                              = 01    ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipr_latn                             = 02    ! latitude (degrees) [northerly point]
  INTEGER,PARAMETER::ipr_lon                              = 03    ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipr_lone                             = 04    ! longitude (degrees) [easterly point]
  INTEGER,PARAMETER::ipr_dlat                             = 05    ! latitude (degrees) [width]
  INTEGER,PARAMETER::ipr_dlon                             = 06    ! longitude (degrees) [width]
  INTEGER,PARAMETER::ipr_A                                = 07    ! area (m2)
  INTEGER,PARAMETER::ipr_rA                               = 08    ! reciprocal area (to speed up numerics)

  ! *** array index names ***
  ! atmosphere interface 'physics'
  CHARACTER(len=16),DIMENSION(n_phys_rok),PARAMETER::string_phys_rok = (/ &
       & 'lat             ', &
       & 'latn            ', &
       & 'lon             ', &
       & 'lone            ', &
       & 'dlat            ', &
       & 'dlon            ', &
       & 'A               ', &
       & 'rA              '/)

  ! ocean/atm interface stuff: grids, depth and location of oceans, and tracers
  INTEGER,PARAMETER                              ::n_io = ilon1_rok
  INTEGER,PARAMETER                              ::n_jo = ilat1_rok
  INTEGER,PARAMETER                              ::n_ko = inl1_ocn                                 ! no. of depth levels in ocean
  REAL,DIMENSION(n_phys_ocnrok,n_io,n_jo)        ::phys_ocnrok                                     ! 'physical' array info for ocean-atmosphere - see above (lats, lons, areas of grid cells)
  INTEGER,DIMENSION(ilon1_ocn,ilat1_ocn)         ::goldstein_k1                                    ! taken from goldstein (put this in somewhere: goldstein_k1(:,:) = go_k1(:,:))  
 
 ! ocean-atmosphere interface 'physics' properties array indices
  INTEGER,PARAMETER::ipoa_lat                    = 01                                              ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipoa_lon                    = 02                                              ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipoa_dlat                   = 03                                              ! latitude (degrees) [width]
  INTEGER,PARAMETER::ipoa_dlon                   = 04                                              ! longitude (degrees) [width]
  INTEGER,PARAMETER::ipoa_A                      = 05                                              ! area (m2)
  INTEGER,PARAMETER::ipoa_rA                     = 06                                              ! reciprocal area (to speed up numerics)

  ! *** miscellaneous ***
  ! should really get from somewhere in global GENIE
  REAL                                           :: gridcell_area                                  ! in km^2:
  INTEGER                                        :: nlandcells
  INTEGER                                        :: ncells_antarctica                              ! number of grid cells taken up by Antarctica
  INTEGER                                        :: nrows_antarctica                               ! number of lattitude rows taken up by Antarctica

  ! Output
  INTEGER                                        :: tstep_count
  REAL                                           :: tsteps_per_year
  INTEGER,PARAMETER                              :: n_output_years_max = 10000
  REAL, DIMENSION(:), ALLOCATABLE                :: output_years_0d
  REAL, DIMENSION(:), ALLOCATABLE                :: output_years_2d
  INTEGER , DIMENSION(:), ALLOCATABLE            :: output_tsteps_0d
  INTEGER , DIMENSION(:), ALLOCATABLE            :: output_tsteps_2d
  INTEGER                                        :: output_counter_0d
  INTEGER                                        :: output_counter_2d
  REAL                                           :: year
  INTEGER                                        :: year_int, year_remainder
  CHARACTER(LEN=11)                              :: year_text
  INTEGER,PARAMETER                              :: nglobtracers = 5                               ! number of output 'tracers'
  INTEGER                                        :: n_outputs
  REAL, DIMENSION(:), ALLOCATABLE                :: outputs
  CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE   :: time_series_names
  CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE   :: output_descriptions 
  CHARACTER(len=7)                               :: string_ncrunid_rg                              ! netCDF runID
  CHARACTER(len=254)                             :: string_ncout2d_rg                              ! netCDF output filename
  INTEGER                                        :: ncout2d_ntrec_rg                               ! count for netcdf datasets
  INTEGER                                        :: ncout2d_iou_rg                                 ! io for netcdf datasets

  ! *** global average weathering variables ***
  REAL                                           :: weather_fCaCO3
  REAL                                           :: weather_fCaSiO3
          
  ! *** landmask and runoff routing arrays ***
  INTEGER                                        :: landmask(n_i,n_j)
  REAL                                           :: runoff_drainage(n_i+2,n_j+2)                   !'+2' comes from fact that *.k1 file is 38x38
  INTEGER                                        :: runoff_detail_i   
  INTEGER                                        :: runoff_detail_j
  REAL, DIMENSION(:,:), ALLOCATABLE              :: runoff_detail                                  ! contains n_i*n_j rows, each with a sucession of (lat, long, fraction) data 
                                                                                                   ! for each ocean cell corresponding to the land cell in question (each row represents the land
                                                                                                   ! cell given by lat= floor(rownumber/n_i) lon=mod(rownumber,n_i).
  INTEGER                                        :: runoff_drainto(n_i,n_j,2)                      !'+2' comes from fact that *.k1 file is 38x38
  REAL                                           :: runoff_coast(n_i,n_j)
  REAL                                           :: runoff_calib                                   ! calibration faction for routing schemes 2 and 3

  ! 2D basic weathering variables
  INTEGER                                        :: par_nliths                                     ! number of rock types (no. of files listed in x_lithologies.txt) - 6 for Gibbs, 7 for Amiotte            
  REAL, DIMENSION(:,:), ALLOCATABLE              :: weath_consts                               
  CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE   :: lithology_names
  REAL, DIMENSION(:,:,:), ALLOCATABLE            :: lithology
  REAL, DIMENSION(:,:,:), ALLOCATABLE            :: bicarb_flux
  REAL                                           :: total_bicarb_flux(n_i,n_j)                     ! NOTE: when referring to bicarb, what is actually being calculated is flux of Ca2+!
  REAL                                           :: total_bicarb_flux_Ca(n_i,n_j)
  REAL                                           :: total_bicarb_flux_Si(n_i,n_j)  
  REAL                                           :: weather_fCaCO3_2D(n_i,n_j)                     ! weathering fluxes after T & P feedbacks
  REAL                                           :: weather_fCaSiO3_2D(n_i,n_j)
  
  ! conversion factors to speed up numerics
  REAL                                           :: conv_GKWM
  REAL                                           :: conv_GKWM_runoff 
  REAL                                           :: conv_GEM_CO2

  contains
    
! Subroutine: define_river_array
!
! dynamic memory allocation
!
! Uses:
!
!  - <genie_util.f90>
!
! Calls:
!
! - <check_iostat>

subroutine define_river_array()
      
      USE genie_util, ONLY : check_iostat
      
      implicit none
      
      ! locals
      integer :: alloc_stat
      
      ALLOCATE(runoff_detail(runoff_detail_i,runoff_detail_j),stat=alloc_stat)                       
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      
    end subroutine define_river_array



    subroutine define_2D_arrays()
      
      USE genie_util, ONLY : check_iostat
      
      implicit none
      
      ! locals
      integer :: alloc_stat
      
      ALLOCATE(weath_consts(par_nliths,4),stat=alloc_stat)                                                ! coeffs in weathering equations
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      ALLOCATE(lithology_names(par_nliths),stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      ALLOCATE(lithology(par_nliths,n_i,n_j),stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      ALLOCATE(bicarb_flux(par_nliths,n_i,n_j),stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      
    end subroutine define_2D_arrays


! Subroutine: define_river_array
!
! dynamic memory cleanup
!
! Uses:
!
!  - <genie_util.f90>
!
! Calls:
!
! - <check_iostat>
    
    subroutine deallocate_arrays()
      
      USE genie_util, ONLY : check_iostat
      
      implicit none
      
      ! locals
      integer :: alloc_stat
            
      if (ALLOCATED(runoff_detail)) DEALLOCATE(runoff_detail,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      if (ALLOCATED(weath_consts)) DEALLOCATE(weath_consts,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      if (ALLOCATED(lithology_names)) DEALLOCATE(lithology_names,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      if (ALLOCATED(lithology)) DEALLOCATE(lithology,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      if (ALLOCATED(bicarb_flux)) DEALLOCATE(bicarb_flux,stat=alloc_stat)
      call check_iostat(alloc_stat,__LINE__,__FILE__)
      
    end subroutine deallocate_arrays

END MODULE rokgem_lib

