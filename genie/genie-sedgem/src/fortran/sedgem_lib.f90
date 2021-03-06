! ******************************************************************************************************************************** !
! sedgem_lib.f90
! 
! LIBRARY MODULE
! ******************************************************************************************************************************** !


MODULE sedgem_lib
  

  use genie_control
  USE gem_cmn
  USE gem_util
  use gem_carbchem
  IMPLICIT NONE
  SAVE


  ! ****************************************************************************************************************************** !
  ! *** NAMELIST DEFINITIONS ***************************************************************************************************** !
  ! ****************************************************************************************************************************** !


  ! #### EDIT ADD AND/OR EXTEND NAME-LIST PARAMETER AND CONTROL OPTIONS ########################################################## !
  ! ------------------- RUN CONTROL ---------------------------------------------------------------------------------------------- !
  LOGICAL::ctrl_continuing                                       ! continuing run?
  NAMELIST /ini_sedgem_nml/ctrl_continuing  
  REAL::start_year                                               ! Simulation start year [REAL]
  REAL::par_misc_t_runtime                                       ! run length (years)
  NAMELIST /ini_sedgem_nml/start_year,par_misc_t_runtime
  ! ------------------- PHYSICAL CONFIGURATION ----------------------------------------------------------------------------------- !
  REAL::par_sed_top_th                                           ! top ('well-mixed') sediment layer thickness (cm)
  REAL::par_sed_poros_det                                        ! detrital porosity (cm3(pore water) / cm3(sed))
  REAL::par_sed_poros_CaCO3                                      ! carbonate porosity in top layer (cm3(pore water) / cm3(sed))
  NAMELIST /ini_sedgem_nml/par_sed_top_th,par_sed_poros_det,par_sed_poros_CaCO3
  REAL::par_sed_Dmax_neritic                                     ! maximum depth of neritic (shallow water) sediments
  NAMELIST /ini_sedgem_nml/par_sed_Dmax_neritic
  ! ------------------- DIAGENESIS SCHEME: SELECTION ----------------------------------------------------------------------------- !
  character(len=63)::par_sed_diagen_CaCO3opt                     ! CaCO3 diagenesis scheme
  character(len=63)::par_sed_diagen_opalopt                      ! opal diagenesis scheme
  character(len=63)::par_sed_diagen_Corgopt                      ! Corg diagenesis scheme
  NAMELIST /ini_sedgem_nml/par_sed_diagen_CaCO3opt,par_sed_diagen_opalopt,par_sed_diagen_Corgopt
  ! ------------------- DIAGENESIS SCHEME: CONTROL ------------------------------------------------------------------------------- !
  LOGICAL::ctrl_sed_bioturb                                      ! Bioturbate sediment stack?
  logical::ctrl_sed_bioturb_Archer                               ! Use Archer et al. [2002] bioturbation scheme?
  NAMELIST /ini_sedgem_nml/ctrl_sed_bioturb,ctrl_sed_bioturb_Archer
  integer::par_n_sed_mix                                         ! maximum layer depth for bioturbation (# cm layers below surface)
  NAMELIST /ini_sedgem_nml/par_n_sed_mix
  REAL::par_sed_mix_k_sur_max                                    ! maximum surface bioturbation mixing rate (cm2 yr-1)
  REAL::par_sed_mix_k_sur_min                                    ! minimum surface bioturbation mixing rate (cm2 yr-1)
  NAMELIST /ini_sedgem_nml/par_sed_mix_k_sur_max,par_sed_mix_k_sur_min
  REAL::par_sed_fdet                                             ! prescribed (additional) flux of detrital material to the seds
  REAL::par_sed_diagen_fPOCfrac                                  ! fraction of POC rain available for driving CaCO3 dissolution
  NAMELIST /ini_sedgem_nml/par_sed_fdet,par_sed_diagen_fPOCfrac
  ! ------------------- DIAGENESIS SCHEME: ARCHER 1991 --------------------------------------------------------------------------- !
  REAL::par_sed_archer1991_dissc                                 ! dissolution rate constant, units of 1/s
  REAL::par_sed_archer1991_dissn                                 ! dissolution rate order
  REAL::par_sed_archer1991_rc                                    ! organic degradation rate constant, 1/s
  NAMELIST /ini_sedgem_nml/par_sed_archer1991_dissc,par_sed_archer1991_dissn,par_sed_archer1991_rc
  integer::par_sed_archer1991_iterationmax                       ! loop limit in 'o2org' subroutine
  NAMELIST /ini_sedgem_nml/par_sed_archer1991_iterationmax
  ! ------------------- CaCO3 PRECIPITATION -------------------------------------------------------------------------------------- !
  REAL::par_sed_CaCO3precip_sf                                   ! CaCO3 precipitation scale factor (abiotic)
  REAL::par_sed_CaCO3precip_exp                                  ! CaCO3 precipitation rate law lower (abiotic)
  NAMELIST /ini_sedgem_nml/par_sed_CaCO3precip_sf,par_sed_CaCO3precip_exp
  REAL::par_sed_reef_CaCO3precip_sf                              ! CaCO3 precipitation scale factor (corals)
  REAL::par_sed_reef_CaCO3precip_exp                             ! CaCO3 precipitation rate law power (corals)
  NAMELIST /ini_sedgem_nml/par_sed_reef_CaCO3precip_sf,par_sed_reef_CaCO3precip_exp
  logical::par_sed_reef_calcite                                  ! CaCO3 precipitation as calcite (otherwise aragonite)? 
  NAMELIST /ini_sedgem_nml/par_sed_reef_calcite                            
  REAL::par_sed_CaCO3_abioticohm_min                             ! Min threshold for abiotic CaCO3 precipitation
  REAL::par_sed_CaCO3_coralohm_max                               ! Max threshold for coral CaCO3 precipitation
  NAMELIST /ini_sedgem_nml/par_sed_CaCO3_abioticohm_min,par_sed_CaCO3_coralohm_max
  real::par_sed_poros_CaCO3_reef                                 ! reef CaCO3 porosity 
  NAMELIST /ini_sedgem_nml/par_sed_poros_CaCO3_reef                            
  ! ------------------- HYDROTHERMAL, OCEAN CRUSTAL WEATHERING, CLAY FORMATION --------------------------------------------------- !
  real::par_sed_hydroip_fLi                                      ! hydrothermal Li flux (mol yr-1) 
  real::par_sed_hydroip_fLi_d7Li                                 ! hydrothermal Li flux d7Li (o/oo)                      
  NAMELIST /ini_sedgem_nml/par_sed_hydroip_fLi,par_sed_hydroip_fLi_d7Li
  real::par_sed_lowTalt_fLi_alpha                                ! Li low temperature alteration sink (mol yr-1) (Li/Ca normalized)
  real::par_sed_lowTalt_7Li_epsilon                              ! Li low temperature alteration sink 7Li epsilon (o/oo)                              
  NAMELIST /ini_sedgem_nml/par_sed_lowTalt_fLi_alpha,par_sed_lowTalt_7Li_epsilon
  real::par_sed_clay_fLi_alpha                                   ! Li clay formation sink (mol yr-1) (Li/Ca norm)
  real::par_sed_clay_7Li_epsilon                                 ! Li clay formation sink 7Li epsilon (o/oo)              
  NAMELIST /ini_sedgem_nml/par_sed_clay_fLi_alpha,par_sed_clay_7Li_epsilon
  ! ------------------- MISC CONTROLS -------------------------------------------------------------------------------------------- !
  logical::ctrl_sed_forcedohmega_ca                              ! Ca-only adjustment for forced ocean saturation?
  NAMELIST /ini_sedgem_nml/ctrl_sed_forcedohmega_ca
  real::par_sed_ohmegamin                                        ! forced minimum saturation (calcite ohmega) anywhere
  real::par_sed_ohmegamin_flux                                   ! imposed sed->ocn flux (mol Ca cm-2 per time-step) for saturation
  NAMELIST /ini_sedgem_nml/par_sed_ohmegamin,par_sed_ohmegamin_flux
  ! ------------------- I/O: DIRECTORY DEFINITIONS ------------------------------------------------------------------------------- !
  CHARACTER(len=255)::par_indir_name                             ! 
  CHARACTER(len=255)::par_outdir_name                            ! 
  CHARACTER(len=255)::par_rstdir_name                            ! 
  NAMELIST /ini_sedgem_nml/par_indir_name,par_outdir_name,par_rstdir_name
  CHARACTER(len=127)::par_infile_name,par_outfile_name           ! 
  NAMELIST /ini_sedgem_nml/par_infile_name,par_outfile_name
  CHARACTER(len=127)::par_sed_topo_D_name                        ! Sediment water depth grid name
  CHARACTER(len=127)::par_sed_reef_mask_name                     ! Shallow water sediment (coral reef) mask name
  CHARACTER(len=127)::par_sedcore_save_mask_name                 ! Sediment core save mask name
  CHARACTER(len=127)::par_sed_mix_k_name                         ! Biodiffusion profile name
  NAMELIST /ini_sedgem_nml/par_sed_topo_D_name,par_sed_reef_mask_name,par_sedcore_save_mask_name,par_sed_mix_k_name  
  CHARACTER(len=127)::par_output_years_file_0d                   ! file containing years for 0D output to be generated (to summary file)
  CHARACTER(len=127)::par_output_years_file_2d                   ! file containing years for 2D output to be generated (to netcdf/ascii)
  NAMELIST /ini_sedgem_nml/par_output_years_file_0d,par_output_years_file_2d
  ! ------------------- I/O: MISC ------------------------------------------------------------------------------------------------ !
  logical::ctrl_append_data                                      ! append data to output files on restart
  logical::ctrl_timeseries_output                                ! save timeseries output
  NAMELIST /ini_sedgem_nml/ctrl_append_data,ctrl_timeseries_output
  logical::ctrl_data_save_ascii                                  ! Save ascii output?
  logical::ctrl_data_save_wtfrac                                 ! Report sediment data as a mass fraction?
  NAMELIST /ini_sedgem_nml/ctrl_data_save_ascii,ctrl_data_save_wtfrac
  logical::ctrl_misc_debug1                                      ! Debug level #1?
  logical::ctrl_misc_debug2                                      ! Debug level #2?
  logical::ctrl_misc_debug3                                      ! Debug level #3?
  logical::ctrl_misc_debug4                                      ! Debug level #4?
  NAMELIST /ini_sedgem_nml/ctrl_misc_debug1,ctrl_misc_debug2,ctrl_misc_debug3,ctrl_misc_debug4
  logical::ctrl_misc_report_err                                  ! report errors?
  NAMELIST /ini_sedgem_nml/ctrl_misc_report_err
  integer::par_misc_debug_i                                      ! i sediment coordinate for debug reporting
  integer::par_misc_debug_j                                      ! j sediment coordinate for debug reporting
  NAMELIST /ini_sedgem_nml/par_misc_debug_i,par_misc_debug_j
  ! ############################################################################################################################## !


  ! ****************************************************************************************************************************** !
  ! MODEL CONFIGURATION CONSTANTS - ARRAY DIMENSIONS
  ! ****************************************************************************************************************************** !


  ! grid dimensions
  INTEGER,PARAMETER::n_i = ilon1_sed                           ! max i dimension copied from genie_control
  INTEGER,PARAMETER::n_j = ilat1_sed                           ! max j dimension copied from genie_control
  ! misc arrays dimensions 
  INTEGER,PARAMETER::n_sed_tot      = 500                      ! # sedimentary stack sub-layers [500/025]
  INTEGER,PARAMETER::n_sed_tot_init = 050                      ! initial number of sedimentary stack sub-layers filled [050 / 005]
  INTEGER,PARAMETER::n_sed_tot_drop = 010                      ! # sedimentary stack sub-layers to drop off bottom [010 / 005]
  ! grid properties array dimensions 
  INTEGER,PARAMETER::n_phys_sed     = 14                       ! # grid properties descriptors
  ! options array dimensions
  INTEGER,PARAMETER::n_opt_sed      = 26                       ! 


  ! *** array index values ***
  ! sediment grid properties array indices
  INTEGER,PARAMETER::ips_lat                              = 01 ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ips_lon                              = 02 ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ips_dlat                             = 03 ! latitude (degrees) [width]
  INTEGER,PARAMETER::ips_dlon                             = 04 ! longitude (degrees) [width]
  INTEGER,PARAMETER::ips_latn                             = 05 ! latitude (degrees) [north edge]
  INTEGER,PARAMETER::ips_lone                             = 06 ! longitude (degrees) [east edge]
  INTEGER,PARAMETER::ips_D                                = 07 ! depth (m)
  INTEGER,PARAMETER::ips_A                                = 08 ! area (m2)
  INTEGER,PARAMETER::ips_rA                               = 09 ! reciprocal area (to speed up numerics)
  INTEGER,PARAMETER::ips_mask_sed                         = 10 ! sediment grid point mask (sediment = 1.0)
  INTEGER,PARAMETER::ips_mask_sed_reef                    = 11 ! reef grid point mask (reef = 1.0)
  INTEGER,PARAMETER::ips_mask_sed_muds                    = 12 ! shallow sediment grid point mask (muds = 1.0)
  INTEGER,PARAMETER::ips_poros                            = 13 ! sediment surface porosity
  INTEGER,PARAMETER::ips_mix_k0                           = 14 ! maximum (surface) sediment bioturbation mixing rate (cm2 yr-1)
  ! options - sediements
  INTEGER,PARAMETER::iopt_sed_save_diag_final             = 20 ! save final sediment data?
  INTEGER,PARAMETER::iopt_sed_save_diag                   = 21 ! save sediment diagnostics time-slice data?
  INTEGER,PARAMETER::iopt_sed_diagen_AltoasymSi           = 23 ! asymptotic [Si] dependence on %refrac/%opal? 
  INTEGER,PARAMETER::iopt_sed_diagen_AltoKSi              = 24 ! KSi dependence on %refrac/%opal?

  ! *** look-up table constants ***
  ! CaCO3 (calcite)
  ! NOTE: following Ridgwell [2001]
  INTEGER,PARAMETER::lookup_i_D_min      = 0                   ! 
  INTEGER,PARAMETER::lookup_i_D_max      = 10                  ! 
  INTEGER,PARAMETER::lookup_i_dCO3_min   = -100                ! 
  INTEGER,PARAMETER::lookup_i_dCO3_max   = 100                 ! 
  INTEGER,PARAMETER::lookup_i_concO2_min = 4                   ! (equivalent to 200 umol kg-1)
  INTEGER,PARAMETER::lookup_i_concO2_max = 4                   ! (equivalent to 200 umol kg-1)
  INTEGER,PARAMETER::lookup_i_frac_min   = 1                   ! 
  INTEGER,PARAMETER::lookup_i_frac_max   = 10                  ! 
  INTEGER,PARAMETER::lookup_i_fCorg_min  = 0                   ! 
  INTEGER,PARAMETER::lookup_i_fCorg_max  = 50                  ! 
  REAL,PARAMETER::lookup_D_max      = 10000.0                  ! 
  REAL,PARAMETER::lookup_dCO3_max   = 100.0 * 1.0E-06          ! 
  REAL,PARAMETER::lookup_concO2_max = 200.0 * 1.0E-06          ! D(concO2) = 50 umol kg-1
  REAL,PARAMETER::lookup_frac_max   = 1.0                      ! 
  REAL,PARAMETER::lookup_fCorg_max  = 50.0 * 1.0E-06           ! 
  ! opal
  ! NOTE: following Ridgwell [2001]
  INTEGER,PARAMETER::lookup_i_opalpc_min       = 1             ! (2%)
  INTEGER,PARAMETER::lookup_i_opalpc_max       = 50            ! (100%)
  INTEGER,PARAMETER::lookup_i_concSi_min       = 0             ! (0 umol kg-1)
  INTEGER,PARAMETER::lookup_i_concSi_max       = 25            ! (250 umol kg-1)
  INTEGER,PARAMETER::lookup_i_T_min            = 270           ! (270 K)
  INTEGER,PARAMETER::lookup_i_T_max            = 280           ! (280 K)
  INTEGER,PARAMETER::lookup_i_KSi0_min         = 1             ! (0.010 yr-1 == 0.0 s-1)
  INTEGER,PARAMETER::lookup_i_KSi0_max         = 100           ! (1.000 yr-1 == 3.16E-08)
  INTEGER,PARAMETER::lookup_i_opaltorefrac_min = 0             ! (0.0 [refrac%/opal%])            
  INTEGER,PARAMETER::lookup_i_opaltorefrac_max = 10            ! (10.0 [refrac%/opal%]) 
  REAL,PARAMETER::lookup_opalpc_max       = 1.0                ! D(opalpc) = 2%
  REAL,PARAMETER::lookup_concSi_max       = 250.0 * 1.0E-06    ! D(concSi) = 10 umol kg-1
  REAL,PARAMETER::lookup_T_max            = 280.0              ! D(T)      = 1 K
  REAL,PARAMETER::lookup_KSi0_max         = 1.000 / conv_yr_s  ! D(KSi0)   = 0.010 yr-1
  REAL,PARAMETER::lookup_opaltorefrac_max = 10.0               ! D(opaltorefrac) = 1.0

  ! *** array index names ***
  ! sediment 'physics' (grid)
  CHARACTER(len=16),DIMENSION(n_phys_sed),PARAMETER::string_phys_sed = (/ &
       & 'lat             ', &
       & 'lon             ', &
       & 'dlat            ', &
       & 'dlon            ', &
       & 'latn            ', &
       & 'lone            ', &
       & 'D               ', &
       & 'A               ', &
       & 'rA              ', &
       & 'mask_sed        ', &
       & 'mask_sed_reef   ', &
       & 'mask_sed_muds   ', &
       & 'poros           ', &
       & 'misc_k0         ' /)


  ! ********************************************************************************************************************************
  ! GLOBAL VARIABLE AND RUN-TIME SET PARAMETER ARRAYS
  ! ********************************************************************************************************************************


  ! *** GRid parameters ***
  ! I/O - strings
  CHARACTER(len=63)::string_runid
  CHARACTER(len=63)::string_restartid                          ! 
  CHARACTER(len=7) ::string_ncrunid                            ! runid for netcdf output
  CHARACTER(len=127)::string_ncout2d                           ! name for netcdf output file
  CHARACTER(len=127)::string_nctsglob                          ! name for netcdf output file
  CHARACTER(len=127)::string_nccore                            ! name for netcdf output file
  CHARACTER(len=127)::string_nctop                             ! name for netcdf output file
  ! I/O - netCDF parameters
  integer::ntrec_sout                                          ! count for netcdf datasets
  integer::ntrec_siou                                          ! io for netcdf datasets
  ! flag for Archer sediment iteration (singular matrix) failure error
  logical::error_Archer = .FALSE.

  ! *** Array definitions ***
  ! bioturbation mixing rate array
  REAL,ALLOCATABLE,DIMENSION(:)::par_sed_mix_k                 ! bioturbation mixing rate profile array
  ! look-up tables
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:)   :: lookup_sed_dis_cal  ! CaCO3 diagensis look-up table [Ridgwell, 2001]
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:,:) :: lookup_sed_dis_opal ! opal diagenesis look-up table [Ridgwell, 2001]
  ! allocatable 2-D sediment arrays
  real,ALLOCATABLE,DIMENSION(:,:,:)::phys_sed                  ! sediment 'physics' (mainly grid details)
  LOGICAL,ALLOCATABLE,DIMENSION(:,:)::sed_mask                 ! sediment mask (.TRUE. == sediment grid point exists)
  LOGICAL,ALLOCATABLE,DIMENSION(:,:)::sed_mask_reef            ! shallow water sediment mask - coral reefs
  LOGICAL,ALLOCATABLE,DIMENSION(:,:)::sed_mask_muds            ! shallow water sediment mask - muds
  REAL,ALLOCATABLE,DIMENSION(:,:,:,:)::sed                     ! the sediment layer stack
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_top                   ! top sedimentary layer
  REAL,ALLOCATABLE,DIMENSION(:,:)::sed_top_h                   ! top height of sedimentary column (cm)
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_fsed                  ! rain flux to sediments (mol cm-2 yr-1)
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_fdis                  ! sediment dissolution flux - solids tracers (mol cm-2 yr-1)
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sedocn_fnet               ! net sediment->ocean flux - ocean tracers (mol cm-2 yr-1)
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_carb                  ! carbonate chemistry overlying sediment surface 
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_carbconst             ! carbonate chemistry constants
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_carbalk               ! carbonate chemistry alkalinity
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_carbisor              ! carbonate (carbon) isotopic properties array
  LOGICAL,ALLOCATABLE,DIMENSION(:,:)::sed_save_mask            ! sediment data save mask (.TRUE. == save sediment grid point)
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_fsed_OLD              ! 
  REAL,ALLOCATABLE,DIMENSION(:,:,:)::sed_fdis_OLD              ! 
  ! sediments - conversion
  real,DIMENSION(n_sed)::conv_sed_cm3_mol                    ! convert solids volume to number of moles
  real,DIMENSION(n_sed)::conv_sed_mol_cm3                    ! convert number of moles to solids volume
  real,DIMENSION(n_sed)::conv_sed_cm3_g                      ! convert solids volume to mass
  real,DIMENSION(n_sed)::conv_sed_g_cm3                      ! convert mass to solids volume
  real,DIMENSION(n_sed)::conv_sed_mask                       ! mask for which sediment tracers contribute to total solids volume
  ! misc
  LOGICAL,DIMENSION(n_opt_sed) :: opt_sed                      ! options arrays

  ! *** MISC sediment parameters ***
  ! sediment mixing and layer configuration
!!$  INTEGER::n_sed_mix                     ! depth of bioturbated layer below top ('well-mixed') layer (integer number of cm)
  REAL::par_sed_interf_th                ! sediment interface dissolution layer thickness (cm)
  REAL::par_sed_dporos_top               ! compaction factor (surface sediment layer porosity compared to underlying sediment stack)
  REAL::par_sed_mix_zmix                 ! depth scale for bioturbation (cm) [Archer et al., 2002]
  REAL::par_sed_mix_c0_O2                ! half saturation constant of O2 for bioturbation (mol kg-1) [Archer et al., 2002]
  ! CaCO3 and Corg diagenesis
  REAL::par_caldis_k                     ! carbonate dissolution "rate constant"
  REAL::par_caldis_exp                   ! carbonate dissolution exponent
  REAL::par_sed_presfrac_FeO             ! fraction of sedimentating scavenged Fe and POM Fe flux preserved in the sediments
  REAL::par_sed_FeO_fdis                 ! forced dussolution flux of Fe from the sediments
  REAL::par_sed_diagen_fPOCmax           ! max POC rain flux in carbonate dissolution (umol cm-2 yr-1)
  ! opal diagenesis
  REAL::par_sed_opal_KSi0                ! base opal dissolution rate constant (intercept at zero opal rain rate)
  REAL::par_sed_opal_Sitoopalmax         ! asymptotic [Si] %refrac/%opal ratio max limite sediments  
  !GHC 20/05/09 time-series saving parameters
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


CONTAINS


  ! ****************************************************************************************************************************** !
  ! CALCULATE TOTAL SEDIMENT VOLUME
  ! NOTE: this function calculate the total volume of sediment tracers (i.e., not taking into account sediment porosity)
  !       -> a mask array <conv_sed_mask> is applied so that only those sediment tracers that actually have a solid volume 
  !          contribute to the total silids volume
  !         (isotopic properties, and carbonate 'age', for instance, do not have any real volume and are not counted)
  FUNCTION fun_calc_sed_vol(dum_sed)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_sed_vol
    ! dummy arguments
    REAL,INTENT(in),DIMENSION(n_sed)::dum_sed
    ! return value
    fun_calc_sed_vol = sum(conv_sed_mask(:)*dum_sed(:))
  END FUNCTION fun_calc_sed_vol
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE TOTAL SEDIMENT MASS
  ! NOTE: similar to 'fun_calc_sed_vol' above, but converted to total sediment tracer mass, rather than left as volume
  FUNCTION fun_calc_sed_mass(dum_sed)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_sed_mass
    ! dummy arguments
    REAL,INTENT(in),DIMENSION(n_sed)::dum_sed
    ! return value
    fun_calc_sed_mass = sum(conv_sed_mask(:)*conv_sed_cm3_g(:)*dum_sed(:))
  END FUNCTION fun_calc_sed_mass
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SEDIMENT POROSITY (SUB-SURFACE)
  ! NOTE: as per Zeebe and Zachos [2007]
  FUNCTION fun_calc_sed_poros(dum_frac_CaCO3)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_sed_poros
    ! dummy arguments
    REAL,INTENT(in)::dum_frac_CaCO3
    ! local variables
    real::loc_F
    ! calculate local constants
    loc_F = (par_sed_poros_CaCO3 - par_sed_poros_det)/(1.0 - par_sed_poros_CaCO3)
    ! return value
    fun_calc_sed_poros = (par_sed_poros_det + dum_frac_CaCO3*loc_F)/(1.0 + dum_frac_CaCO3*loc_F)
  END FUNCTION fun_calc_sed_poros
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SEDIMENT POROSITY (SURFACE SEDIMENT LAYER)
  ! NOTE: assumes that the solids volume in the surface sediment is par_sed_dporos_top times that of the sediment stack
  FUNCTION fun_calc_sed_poros_top(dum_frac_CaCO3_top)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_sed_poros_top
    ! dummy arguments
    REAL,INTENT(in)::dum_frac_CaCO3_top
    ! return value
    fun_calc_sed_poros_top = 1.0 - par_sed_dporos_top*(1.0 - fun_calc_sed_poros(dum_frac_CaCO3_top))
  END FUNCTION fun_calc_sed_poros_top
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SEDIMENT POROSITY (NEAR-SURFACE)
  ! NOTE: Archer [1996]
  FUNCTION fun_calc_sed_poros_nsur(dum_frac_CaCO3,dum_sed_th)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_sed_poros_nsur
    ! dummy arguments
    REAL,INTENT(in)::dum_frac_CaCO3 ! 
    REAL,INTENT(in)::dum_sed_th     ! sediment thickness (cm)
    ! local variables
    real::loc_sed_poros
    real::loc_sed_poros_alpha
    ! calculate local constants
    loc_sed_poros = fun_calc_sed_poros(dum_frac_CaCO3)
    loc_sed_poros_alpha = fun_calc_sed_poros_alpha(dum_frac_CaCO3)
    ! return value
    fun_calc_sed_poros_nsur = loc_sed_poros - &
         & (loc_sed_poros_alpha*(1.0 - loc_sed_poros)/dum_sed_th)*(exp(-dum_sed_th/loc_sed_poros_alpha) - 1.0)
  END FUNCTION fun_calc_sed_poros_nsur
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SEDIMENT POROSITY DEPTH SCALE (ALPHA)
  ! NOTE: Archer [1996]
  FUNCTION fun_calc_sed_poros_alpha(dum_frac_CaCO3)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_sed_poros_alpha
    ! dummy arguments
    REAL,INTENT(in)::dum_frac_CaCO3
    ! return value
    fun_calc_sed_poros_alpha = 0.25*dum_frac_CaCO3 + 3.0*(1.0 - dum_frac_CaCO3)
  END FUNCTION fun_calc_sed_poros_alpha
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE NEAR-SURFACE-TO-SUB-SURFACE COMPACTION RATIO
  FUNCTION fun_calc_r_sed_por(dum_frac_CaCO3,dum_sed_th)
    IMPLICIT NONE
    ! result variable
    REAL::fun_calc_r_sed_por
    ! dummy arguments
    REAL,INTENT(in)::dum_frac_CaCO3 ! 
    REAL,INTENT(in)::dum_sed_th     ! sediment thickness (cm)
    ! return value
    fun_calc_r_sed_por = (1.0 - fun_calc_sed_poros(dum_frac_CaCO3))/(1.0 - fun_calc_sed_poros_nsur(dum_frac_CaCO3,dum_sed_th))
  END FUNCTION fun_calc_r_sed_por
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CALCULATE SEDIMENT CORE-TOP DATA
  function fun_sed_coretop()
    IMPLICIT NONE
    ! result variable
    REAL,DIMENSION(n_sed,n_i,n_j)::fun_sed_coretop
    ! local variables
    INTEGER::i,j,l,is
    REAL::loc_sed_tot_wt
    REAL::loc_sed_tot_vol
    REAL,DIMENSION(n_sed,n_i,n_j)::loc_sed
    real::loc_tot,loc_frac,loc_standard

    ! *** calculate core-top composition ***

    ! initialize local array
    loc_sed(:,:,:) = 0.0
    ! loop through all sediment grid points and convert data
    ! NOTE: normailze solid sediment components to a mass fraction and multiply by 100.0
    ! NOTE: convert isotopic composition to delta notation in units of (o/oo)
    ! NOTE: screen-out non-wet (i,j) grid points
    DO i=1,n_i
       DO j=1,n_j
          IF (sed_mask(i,j)) THEN
             loc_sed_tot_wt  = fun_calc_sed_mass(sed_top(:,i,j))
             loc_sed_tot_vol = fun_calc_sed_vol(sed_top(:,i,j))
             DO l=1,n_l_sed
                is = conv_iselected_is(l)
                SELECT CASE (sed_type(is))
                case (par_sed_type_bio,par_sed_type_abio)
                   ! solid components
                   IF (ctrl_data_save_wtfrac) THEN
                      loc_sed(is,i,j) = 100.0*conv_sed_cm3_g(is)*sed_top(is,i,j)/loc_sed_tot_wt
                   ELSE
                      loc_sed(is,i,j) = sed_top(is,i,j)/loc_sed_tot_vol
                   ENDIF
                case (par_sed_type_POM)
                   ! particulate organic matter components
                   ! NOTE: mass (or volume) fraction has little meaning for the P,N,Fe,O2 components of POM,
                   !       so just calculate the ratio of these components with POC
                   if (loc_sed(is_POC,i,j) > const_real_nullsmall) loc_sed(is,i,j) = sed_top(is,i,j)/sed_top(is_POC,i,j)
                case (par_sed_type_age)
                   ! age
                   ! NOTE: extract core-top age
                   IF (sed_top(is_CaCO3,i,j) > const_real_nullsmall) THEN
                      loc_sed(is_CaCO3_age,i,j) = sed_top(is_CaCO3_age,i,j)/sed_top(is_CaCO3,i,j)
                   ELSE
                      loc_sed(is_CaCO3_age,i,j) = 0.0
                   ENDIF
                case (11:20)
                   ! isotopes
                   loc_tot  = sed_top(sed_dep(is),i,j)
                   loc_frac = sed_top(is,i,j)
                   loc_standard = const_standards(sed_type(is))
                   loc_sed(is,i,j) = fun_calc_isotope_delta(loc_tot,loc_frac,loc_standard,.FALSE.)
                case default
                   ! everything else
                   ! NOTE: assume no normalization
                   loc_sed(is,i,j) = sed_top(is,i,j)
                end select
             END DO
          ENDIF
       END DO
    END DO
    ! set coretop sediment composition
    fun_sed_coretop(:,:,:) = loc_sed(:,:,:)

  END function fun_sed_coretop
  ! ****************************************************************************************************************************** !


END MODULE sedgem_lib

