! ******************************************************************************************************************************** !
! biogem_lib.f90
! BioGeM
! LIBRARY MODULE
! ******************************************************************************************************************************** !


MODULE biogem_lib
  
  
  use genie_control
  USE gem_util
  use gem_carbchem
  IMPLICIT NONE
  SAVE


  ! ****************************************************************************************************************************** !
  ! *** NAMELIST DEFINITIONS ***************************************************************************************************** !
  ! ****************************************************************************************************************************** !


  ! ### EDIT ADD AND/OR EXTEND NAME-LIST PARAMETER AND CONTROL OPTIONS ########################################################### !
  ! ------------------- TRACER INITIALIZATION ------------------------------------------------------------------------------------ !
  REAL,DIMENSION(n_ocn)::ocn_init                                ! ocean tracer array initial values
  NAMELIST /ini_biogem_nml/ocn_init
  ! ------------------- RUN CONTROL ---------------------------------------------------------------------------------------------- !
  logical::ctrl_continuing                                       ! continuing run?
  NAMELIST /ini_biogem_nml/ctrl_continuing
  REAL::par_misc_t_start                                         ! start time (years)
  REAL::par_misc_t_runtime                                       ! run time (years)
  NAMELIST /ini_biogem_nml/par_misc_t_start,par_misc_t_runtime
  LOGICAL::ctrl_misc_t_BP                                        ! years before present?
  NAMELIST /ini_biogem_nml/ctrl_misc_t_BP
  ! ------------------- MISC CONTROL --------------------------------------------------------------------------------------------- !
  logical::ctrl_misc_Snorm                                       ! 
  logical::ctrl_misc_noSnorm                                     ! 
  logical::ctrl_misc_nobioupdate                                 ! 
  NAMELIST /ini_biogem_nml/ctrl_misc_Snorm,ctrl_misc_noSnorm,ctrl_misc_nobioupdate
  REAL::par_misc_brinerejection_frac                             ! sea-ice brine rejection fraction
  integer::par_misc_brinerejection_jmax                          ! max j for sea-ice brine rejection
  logical::ctrl_misc_brinerejection_bgc                          ! include biogeochem in Sea-ice brine rejection?
  NAMELIST /ini_biogem_nml/par_misc_brinerejection_frac,par_misc_brinerejection_jmax,ctrl_misc_brinerejection_bgc
  ! ------------------- BOUNDARY CONDITIONS -------------------------------------------------------------------------------------- !
  logical::ctrl_force_sed_closedsystem                           ! Set dissolution flux = rain flux to close system?
  NAMELIST /ini_biogem_nml/ctrl_force_sed_closedsystem
  logical::ctrl_force_GOLDSTEInTS                                ! Allow temperature / salinity forcing of climate?
  logical::ctrl_force_seaice                                     ! Replace internal fractional sea-ice cover field?
  logical::ctrl_force_windspeed                                  ! Replace internal wind-speed field?
  NAMELIST /ini_biogem_nml/ctrl_force_GOLDSTEInTS,ctrl_force_seaice,ctrl_force_windspeed
  logical::ctrl_force_CaCO3toPOCrainratio                        ! Replace internal CaCO3:POC export rain ratio?
  NAMELIST /ini_biogem_nml/ctrl_force_CaCO3toPOCrainratio
  logical::ctrl_force_POCdtoPOCrainratio                         ! Replace internal POCd:POC export rain ratio?
  NAMELIST /ini_biogem_nml/ctrl_force_POCdtoPOCrainratio
  logical::ctrl_force_Cd_alpha                                   ! Replace internal [Cd/P]POM/[Cd/P]SW alpha?
  NAMELIST /ini_biogem_nml/ctrl_force_Cd_alpha
  logical::ctrl_force_scav_fpart_POC                             ! Replace internal POC flux for 230Th and 231Pa isotope scavenging
  logical::ctrl_force_scav_fpart_CaCO3                           ! Replace internal CaCO3 flux for 230Th & 231Pa isotope scavenging
  logical::ctrl_force_scav_fpart_opal                            ! Replace internal opal flux for 230Th and 231Pa isotope scavenging
  logical::ctrl_force_scav_fpart_det                             ! Replace internal det flux for 230Th and 231Pa isotope scavenging
  NAMELIST /ini_biogem_nml/ctrl_force_scav_fpart_POC,ctrl_force_scav_fpart_CaCO3
  NAMELIST /ini_biogem_nml/ctrl_force_scav_fpart_opal,ctrl_force_scav_fpart_det
  REAL::par_gastransfer_a                                        ! Value of Wanninkhof [1992] gas transfer coeff (a)
  NAMELIST /ini_biogem_nml/par_gastransfer_a
  CHARACTER(len=127)::par_seaice_file                            ! 
  CHARACTER(len=127)::par_windspeed_file                         !
  NAMELIST /ini_biogem_nml/par_seaice_file,par_windspeed_file
  CHARACTER(len=127)::par_CaCO3toPOCrainratio_file               !
  CHARACTER(len=127)::par_POCdtoPOCrainratio_file                !
  CHARACTER(len=127)::par_Cd_alpha_file                          !
  NAMELIST /ini_biogem_nml/par_CaCO3toPOCrainratio_file,par_POCdtoPOCrainratio_file,par_Cd_alpha_file
  CHARACTER(len=127)::par_scav_fpart_POC_file                    !
  CHARACTER(len=127)::par_scav_fpart_CaCO3_file                  !
  CHARACTER(len=127)::par_scav_fpart_opal_file                   !
  CHARACTER(len=127)::par_scav_fpart_det_file                    !
  NAMELIST /ini_biogem_nml/par_scav_fpart_POC_file,par_scav_fpart_CaCO3_file,par_scav_fpart_opal_file,par_scav_fpart_det_file
  logical::ctrl_force_solconst                                   ! Replace solar constant?
  NAMELIST /ini_biogem_nml/ctrl_force_solconst
  logical::ctrl_force_oldformat                                  ! Use old tracer forcing file format? 
  NAMELIST /ini_biogem_nml/ctrl_force_oldformat
  ! ------------------- BIOLOGICAL NEW PRODUCTION -------------------------------------------------------------------------------- !
  CHARACTER(len=63)::par_bio_prodopt                             ! biological scheme ID string (e.g., 1N1T_PO4MM, 1N1T_PO4MM_Cd)
  NAMELIST /ini_biogem_nml/par_bio_prodopt  
  real::par_bio_k0_PO4                                           ! base [PO4] uptake rate (mol kg-1 yr-1)
  real::par_bio_k0_NO3                                           ! base [NO3] uptake rate (mol kg-1 yr-1)
  NAMELIST /ini_biogem_nml/par_bio_k0_PO4,par_bio_k0_NO3
  real::par_bio_c0_PO4                                           ! [PO4] M-M half-sat value (mol kg-1)
  real::par_bio_c0_NO3                                           ! [NO3] M-M half-sat value (mol kg-1)
  real::par_bio_c0_N                                             ! [NO3]+[NH4] M-M half-sat value (mol kg-1)
  real::par_bio_c0_Fe                                            ! [Fe] M-M half-sat value (mol kg-1)
  real::par_bio_c0_Fe_sp                                         ! [Fe] M-M half-sat value (mol kg-1) for siliceous phytoplankton
  real::par_bio_c0_Fe_nsp                                        ! [Fe] M-M half-sat value (mol kg-1) for non-siliceous phytoplank.
  real::par_bio_c0_SiO2                                          ! [H4SiO4] M-M half-sat value (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_c0_PO4,par_bio_c0_NO3,par_bio_c0_N
  NAMELIST /ini_biogem_nml/par_bio_c0_Fe,par_bio_c0_Fe_sp,par_bio_c0_Fe_nsp,par_bio_c0_SiO2
  real::par_bio_zc                                               ! biological production zone depth (m) (OCMIP-2)
  real::par_bio_tau                                              ! biological production time-scale (days) (OCMIP-2)
  real::par_bio_relprod_sp                                       ! uptake rate modification factor for siliceous phytoplankton
  real::par_bio_I_eL                                             ! light e-folding depth (m) (OCMIP-2)
  real::par_bio_kT0                                              ! coefficient for temperature-dependent uptake rate modifier
  real::par_bio_kT_eT                                            ! e-folding temp (K) for temp-dependent uptake rate modifier
  NAMELIST /ini_biogem_nml/par_bio_zc,par_bio_tau,par_bio_relprod_sp,par_bio_I_eL,par_bio_kT0,par_bio_kT_eT
  ! ------------------- ORGANIC MATTER EXPORT RATIOS ----------------------------------------------------------------------------- !
  real::par_bio_red_POP_PON                                      ! N/P organic matter Redfield ratio
  real::par_bio_red_POP_POC                                      ! C/P organic matter Redfield ratio 
  real::par_bio_red_POP_PO2                                      ! O2/P organic matter pseudo-Redfield ratio
  real::par_bio_red_PON_ALK                                      ! ALK/N alkalinty correction factor
  NAMELIST /ini_biogem_nml/par_bio_red_POP_PON,par_bio_red_POP_POC,par_bio_red_POP_PO2,par_bio_red_PON_ALK
  real::par_bio_red_DOMfrac                                      ! production fraction of dissolved organic matter
  NAMELIST /ini_biogem_nml/par_bio_red_DOMfrac
  ! ------------------- INORGANIC MATTER EXPORT RATIOS --------------------------------------------------------------------------- !
  real::par_bio_red_POC_CaCO3_pP                                 ! exponent for modifier of CaCO3:POC export ratio
  real::par_bio_red_POC_CaCO3                                    ! base CaCO3:POC export ratio
  NAMELIST /ini_biogem_nml/par_bio_red_POC_CaCO3,par_bio_red_POC_CaCO3_pP
  real::par_bio_red_POC_opal
  NAMELIST /ini_biogem_nml/par_bio_red_POC_opal
  ! ------------------- REMINERALIZATION ----------------------------------------------------------------------------------------- !
  real::par_bio_remin_DOMlifetime                                ! DOC lifetime (yrs)
  NAMELIST /ini_biogem_nml/par_bio_remin_DOMlifetime
  real::par_bio_remin_CH4rate                                    ! specific CH4 oxidation rate (d-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_CH4rate
  LOGICAL::ctrl_bio_remin_POC_fixed                              ! fixed-profile POM remineralization
  LOGICAL::ctrl_bio_remin_CaCO3_fixed                            ! fixed-profile CaCO3 remineralization
  LOGICAL::ctrl_bio_remin_opal_fixed                             ! fixed-profile opal remineralization
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_POC_fixed,ctrl_bio_remin_CaCO3_fixed,ctrl_bio_remin_opal_fixed
  LOGICAL::ctrl_bio_remin_POC_ballast                            ! ballasting parameterization?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_POC_ballast
  real::par_bio_remin_POC_frac2                                  ! initial fractional abundance of POC component (#2)
  real::par_bio_remin_CaCO3_frac2                                ! initial fractional abundance of CaCO3 component (#2)
  real::par_bio_remin_opal_frac2                                 ! initial fractional abundance of opal component (#2)
  NAMELIST /ini_biogem_nml/par_bio_remin_POC_frac2,par_bio_remin_CaCO3_frac2,par_bio_remin_opal_frac2
  real::par_bio_remin_POC_eL1                                    ! remineralization length #1 for POC
  real::par_bio_remin_POC_eL2                                    ! remineralization length #2 for POC
  real::par_bio_remin_CaCO3_eL1                                  ! remineralization length #1 for CaCO3
  real::par_bio_remin_CaCO3_eL2                                  ! remineralization length #2 for CaCO3
  real::par_bio_remin_opal_eL1                                   ! remineralization length #1 for opal
  real::par_bio_remin_opal_eL2                                   ! remineralization length #2 for opal
  NAMELIST /ini_biogem_nml/par_bio_remin_POC_eL1,par_bio_remin_POC_eL2
  NAMELIST /ini_biogem_nml/par_bio_remin_CaCO3_eL1,par_bio_remin_CaCO3_eL2
  NAMELIST /ini_biogem_nml/par_bio_remin_opal_eL1,par_bio_remin_opal_eL2
  real::par_bio_remin_sinkingrate                                ! prescribed particle sinking rate (m d-1) 
  NAMELIST /ini_biogem_nml/par_bio_remin_sinkingrate
  real::par_bio_remin_ballast_kc                                 ! organic matter carrying capacity of CaCO3
  real::par_bio_remin_ballast_ko                                 ! organic matter carrying capacity of opal
  real::par_bio_remin_ballast_kl                                 ! organic matter carrying capacity of detital (lithogenic)
  NAMELIST /ini_biogem_nml/par_bio_remin_ballast_kc,par_bio_remin_ballast_ko,par_bio_remin_ballast_kl
  LOGICAL::ctrl_bio_remin_ONtoNH4                                ! Aerobic remineralization of ON -> NH4 (not NO3)?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_ONtoNH4
  REAL::par_bio_remin_denitrO2thresh                             ! Denitrification [O2] threshold (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_remin_denitrO2thresh
  LOGICAL::ctrl_bio_remin_reminfix                               ! Catch mis-behaving rapidly-oxidizing species going < 0.0?
  NAMELIST /ini_biogem_nml/ctrl_bio_remin_reminfix
  ! ------------------- ISOTOPIC FRACTIONATION ----------------------------------------------------------------------------------- !
  real::par_d13C_DIC_Corg_ef                                     ! frac for intercellular C fix
  real::par_d13C_DIC_Corg_ef_sp                                  ! frac for intercellular C fix of siliceous phytoplankton
  real::par_d13C_DIC_Corg_ef_nsp                                 ! frac for intercellular C fix of non-siliceous phytoplankton
  NAMELIST /ini_biogem_nml/par_d13C_DIC_Corg_ef,par_d13C_DIC_Corg_ef_sp,par_d13C_DIC_Corg_ef_nsp
  real::par_d30Si_opal_epsilon                                   ! fractionation of 30Si during opal formation by diatoms
  NAMELIST /ini_biogem_nml/par_d30Si_opal_epsilon
  real::par_d114Cd_POCd_epsilon                                  ! *** d114Cd = 1.0006 ***
  namelist /ini_biogem_nml/par_d114Cd_POCd_epsilon
  real::par_d7Li_LiCO3_epsilon                                   ! 7/6Li fractionation between Li and LiCO3
  namelist /ini_biogem_nml/par_d7Li_LiCO3_epsilon
  ! ------------------- IRON CYCLING --------------------------------------------------------------------------------------------- !
  real::par_det_Fe_sol                                           ! fractional solubility of Fe in dust
  real::par_det_Fe_sol_exp                                       ! exponent for aeolian Fe solubility
  NAMELIST /ini_biogem_nml/par_det_Fe_sol,par_det_Fe_sol_exp
  LOGICAL::ctrl_bio_red_fixedFetoC                               ! fixed cellular Fe:C ratio?
  NAMELIST /ini_biogem_nml/ctrl_bio_red_fixedFetoC
  real::par_bio_red_POFe_POC                                     ! Fe:C 'Redfield' ratio
  NAMELIST /ini_biogem_nml/par_bio_red_POFe_POC
  LOGICAL::ctrl_bio_Fe_fixedKscav                                ! Fixed scavening rate (if not: Parekh scheme)?
  NAMELIST /ini_biogem_nml/ctrl_bio_Fe_fixedKscav
  real::par_scav_Fe_Ks                                           ! Fixed Fe scavenging rate (d-1) 
  NAMELIST /ini_biogem_nml/par_scav_Fe_Ks
  real::par_scav_Fe_sf_POC                                       ! Parekh Fe scavenging rate scale factor - POC
  real::par_scav_Fe_sf_CaCO3                                     ! Parekh Fe scavenging rate scale factor - CaCO3
  real::par_scav_Fe_sf_opal                                      ! Parekh Fe scavenging rate scale factor - opal
  real::par_scav_Fe_sf_det                                       ! Parekh Fe scavenging rate scale factor - det
  NAMELIST /ini_biogem_nml/par_scav_Fe_sf_POC,par_scav_Fe_sf_CaCO3,par_scav_Fe_sf_opal,par_scav_Fe_sf_det
  real::par_scav_fremin                                          ! Fraction of scavenged Fe that can be remineralized
  logical::ctrl_bio_NO_fsedFe                                    ! Prevent return of Fe from the sediments?
  NAMELIST /ini_biogem_nml/par_scav_fremin,ctrl_bio_NO_fsedFe
  real::par_K_FeL_pP
  NAMELIST /ini_biogem_nml/par_K_FeL_pP                          ! log10 of Fe ligand stability constant K'(FeL)
  real::par_bio_FetoC_pP                                         ! [FeT] dependent Fe:C ratio [Ridgwell, 2001] -- power
  real::par_bio_FetoC_K                                          ! [FeT] dependent Fe:C ratio [Ridgwell, 2001] -- scaling
  real::par_bio_FetoC_C                                          ! [FeT] dependent Fe:C ratio [Ridgwell, 2001] -- constant
  NAMELIST /ini_biogem_nml/par_bio_FetoC_pP,par_bio_FetoC_K,par_bio_FetoC_C
  ! ------------------- SILICIC ACIC CYCLING ------------------------------------------------------------------------------------- !
  ! ------------------- NITROGEN CYCLING ----------------------------------------------------------------------------------------- !
  real::par_bio_mu1                                              ! mu-1 maximum rate of export production (yr-1) 
  real::par_bio_mu2                                              ! mu-2 maximum rate of export production from N2-fixation (yr-1)
  real::par_bio_N2fixthresh                                      ! threshold NO3+NH4 to encourage N2 fixation (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_mu1,par_bio_mu2,par_bio_N2fixthresh
  real::par_bio_Nstar_offset                                     ! N* offset (mol kg-1)
  NAMELIST /ini_biogem_nml/par_bio_Nstar_offset
  ! ------------------- TRACE METAL CYCLING -------------------------------------------------------------------------------------- !
  real::par_bio_red_POC_POCd                                     ! Default cellular C:Cd (Cd/C) ratio
  real::par_bio_red_POC_POCd_alpha                               ! [Cd/P]POM/[Cd/P]SW partition coefficient (alpha)
  NAMELIST /ini_biogem_nml/par_bio_red_POC_POCd,par_bio_red_POC_POCd_alpha
  LOGICAL::ctrl_bio_red_CdtoC_Felim                              ! Fe-limitation dependent Cd:C 'Redfield' uptake ratio?
  NAMELIST /ini_biogem_nml/ctrl_bio_red_CdtoC_Felim
  real::par_bio_red_CdtoC_Felim_min                              ! minimum (Fe replete) Cd:C 'Redfield' uptake ratio
  real::par_bio_red_CdtoC_Felim_max                              ! maximum (Fe limited) Cd:C 'Redfield' uptake ratio
  NAMELIST /ini_biogem_nml/par_bio_red_CdtoC_Felim_min,par_bio_red_CdtoC_Felim_max
  real::par_bio_red_CaCO3_LiCO3                                     ! Default CaCO3 Cd/Ca ratio
  real::par_bio_red_CaCO3_LiCO3_alpha                               ! partition coefficient (alpha)
  NAMELIST /ini_biogem_nml/par_bio_red_CaCO3_LiCO3,par_bio_red_CaCO3_LiCO3_alpha
  ! ------------------- 230Th AND 231Pa CYCLING ---------------------------------------------------------------------------------- !
  CHARACTER(len=63)::par_scav_230Th_scavopt                     ! scavenging scheme ID string (e.g., 'equilibrium') for 230Th
  CHARACTER(len=63)::par_scav_231Pa_scavopt                     ! scavenging scheme ID string (e.g., 'equilibrium') for 231Pa
  NAMELIST /ini_biogem_nml/par_scav_230Th_scavopt,par_scav_231Pa_scavopt
  real::par_scav_230Th_KPOC                                     ! eqm scavenging coefficient for POC associated 230Th
  real::par_scav_230Th_KCaCO3                                   ! eqm scavenging coefficient for calciate associated 230Th
  real::par_scav_230Th_Kopal                                    ! eqm scavenging coefficient for opal associated 230Th
  real::par_scav_230Th_Kdet                                     ! eqm scavenging coefficient for detrital matter associated 230Th
  NAMELIST /ini_biogem_nml/par_scav_230Th_KPOC,par_scav_230Th_KCaCO3,par_scav_230Th_Kopal,par_scav_230Th_Kdet
  real::par_scav_231Pa_KPOC                                     ! eqm scavenging coefficient for POC associated 231Pa
  real::par_scav_231Pa_KCaCO3                                   ! eqm scavenging coefficient for calciate associated 231Pa
  real::par_scav_231Pa_Kopal                                    ! eqm scavenging coefficient for opal associated 231Pa
  real::par_scav_231Pa_Kdet                                     ! eqm scavenging coefficient for detrital matter associated 231Pa
  NAMELIST /ini_biogem_nml/par_scav_231Pa_KPOC,par_scav_231Pa_KCaCO3,par_scav_231Pa_Kopal,par_scav_231Pa_Kdet
  real::par_scav_230Th_indepsinkingvel
  real::par_scav_231Pa_indepsinkingvel
  NAMELIST /ini_biogem_nml/par_scav_230Th_indepsinkingvel,par_scav_231Pa_indepsinkingvel
  ! ------------------- ABIOTIC PRECIPITATION ------------------------------------------------------------------------------------ !
  real::par_bio_CaCO3precip_sf                                   ! Scale factor for CaCO3 precipitation
  real::par_bio_CaCO3precip_exp                                  ! Rate law power for CaCO3 precipitation
  NAMELIST /ini_biogem_nml/par_bio_CaCO3precip_sf,par_bio_CaCO3precip_exp
  LOGICAL::ctrl_bio_CaCO3precip                                  ! Allow abiotic CaCO3 precipitation?
  LOGICAL::ctrl_bio_CaCO3precip_sur                              ! Restrict precipitation to surface layer?
  NAMELIST /ini_biogem_nml/ctrl_bio_CaCO3precip,ctrl_bio_CaCO3precip_sur
  ! ------------------- I/O DIRECTORY DEFINITIONS -------------------------------------------------------------------------------- !
  CHARACTER(len=255)::par_indir_name                             ! 
  CHARACTER(len=255)::par_outdir_name                            ! 
  CHARACTER(len=255)::par_rstdir_name                            ! 
  CHARACTER(len=255)::par_fordir_name                            ! 
  NAMELIST /ini_biogem_nml/par_indir_name,par_outdir_name,par_rstdir_name,par_fordir_name
  CHARACTER(len=127)::par_infile_name,par_outfile_name           ! 
  NAMELIST /ini_biogem_nml/par_infile_name,par_outfile_name
  ! ------------------- DATA SAVING: TIME-SLICES --------------------------------------------------------------------------------- !
  LOGICAL::ctrl_data_save_slice_ocnatm                           ! time-slice data save: Atmospheric (interface) composition (2D)?
  LOGICAL::ctrl_data_save_slice_ocn                              ! time-slice data save: Ocean composition (3D)?         
  LOGICAL::ctrl_data_save_slice_ocnsed                           ! time-slice data save: Sediment (interface) composition (2D)? 
  LOGICAL::ctrl_data_save_slice_fairsea                          ! time-slice data save: Air-sea gas exchange (2D)?
  LOGICAL::ctrl_data_save_slice_focnatm                          ! time-slice data save: Ocean-atmosphere flux (2D)?
  LOGICAL::ctrl_data_save_slice_focnsed                          ! time-slice data save: Ocean-sediment flux (2D)?
  LOGICAL::ctrl_data_save_slice_fsedocn                          ! time-slice data save: Sediment-ocean flux (2D)? 
  LOGICAL::ctrl_data_save_slice_bio                              ! time-slice data save: Biological fluxes (3D)? 
  LOGICAL::ctrl_data_save_slice_carb                             ! time-slice data save: Aqueous carbonate system properties (3D)?
  LOGICAL::ctrl_data_save_slice_carbconst                        ! time-slice data save: Aqueous carbonate system constants (3D)?
  LOGICAL::ctrl_data_save_slice_phys_atm                         ! time-slice data save: Atmospheric physical properties (2D)?
  LOGICAL::ctrl_data_save_slice_phys_ocn                         ! time-slice data save: Ocean physical properties (3D)? 
  LOGICAL::ctrl_data_save_slice_misc                             ! time-slice data save: Miscellaneous properties (-)?
  LOGICAL::ctrl_data_save_slice_diag                             ! time-slice data save: biogeochemical diagnostics?
  NAMELIST /ini_biogem_nml/ &
       & ctrl_data_save_slice_ocnatm,ctrl_data_save_slice_ocn,ctrl_data_save_slice_ocnsed, &
       & ctrl_data_save_slice_fairsea, &
       & ctrl_data_save_slice_focnatm,ctrl_data_save_slice_focnsed,ctrl_data_save_slice_fsedocn, &
       & ctrl_data_save_slice_bio,ctrl_data_save_slice_carb,ctrl_data_save_slice_carbconst, &
       & ctrl_data_save_slice_phys_atm,ctrl_data_save_slice_phys_ocn,ctrl_data_save_slice_misc,ctrl_data_save_slice_diag
  real::par_data_save_slice_dt                                   ! Integration interval (yr) 
  NAMELIST /ini_biogem_nml/par_data_save_slice_dt
  CHARACTER(len=127)::par_infile_slice_name                      ! 
  NAMELIST /ini_biogem_nml/par_infile_slice_name
  integer::par_data_save_slice_n                                 ! number of timesteps in sub-inteval (e.g., monthly) saving 
  NAMELIST /ini_biogem_nml/par_data_save_slice_n
  ! ------------------- DATA SAVING: TIME-SERIES --------------------------------------------------------------------------------- !
  LOGICAL::ctrl_data_save_sig_ocnatm                             ! time-series data save: Atmospheric (interface) composition?
  LOGICAL::ctrl_data_save_sig_ocn                                ! time-series data save: Oceanic composition?  
  LOGICAL::ctrl_data_save_sig_fexport                            ! time-series data save: Export flux?      
  LOGICAL::ctrl_data_save_sig_fairsea                            ! time-series data save: Air-sea gas exchange?      
  LOGICAL::ctrl_data_save_sig_ocnsed                             ! time-series data save: Sediment (interface) composition?
  LOGICAL::ctrl_data_save_sig_focnatm                            ! time-series data save: Ocean->atmosphere flux?
  LOGICAL::ctrl_data_save_sig_focnsed                            ! time-series data save: Ocean->sediment flux?
  LOGICAL::ctrl_data_save_sig_fsedocn                            ! time-series data save: Sediment->ocean flux?
  LOGICAL::ctrl_data_save_sig_ocn_sur                            ! time-series data save: Ocean surface tracers?
  LOGICAL::ctrl_data_save_sig_carb_sur                           ! time-series data save: Ocean surface carbonate chemistry?
  LOGICAL::ctrl_data_save_sig_misc                               ! time-series data save: Miscellaneous properties?
  LOGICAL::ctrl_data_save_sig_diag                               ! time-series data save: biogeochemical diagnostics?
  NAMELIST /ini_biogem_nml/ &
       & ctrl_data_save_sig_ocnatm,ctrl_data_save_sig_ocn,ctrl_data_save_sig_fexport,ctrl_data_save_sig_ocnsed, &
       & ctrl_data_save_sig_fairsea, &
       & ctrl_data_save_sig_focnatm,ctrl_data_save_sig_focnsed,ctrl_data_save_sig_fsedocn, &
       & ctrl_data_save_sig_ocn_sur,ctrl_data_save_sig_carb_sur,ctrl_data_save_sig_misc,ctrl_data_save_sig_diag
  real::par_data_save_sig_dt                                     ! Integration interval (yr)  
  NAMELIST /ini_biogem_nml/par_data_save_sig_dt
  CHARACTER(len=127)::par_infile_sig_name                        ! 
  NAMELIST /ini_biogem_nml/par_infile_sig_name
  ! ------------------- DATA SAVING: MISC ---------------------------------------------------------------------------------------- !
  LOGICAL::ctrl_data_save_derived                                ! save 'derived' data (e.g., salinity-normalized ocean tracers)?
  LOGICAL::ctrl_data_save_GLOBAL                                 ! save global diagnostics (at time-slice intervals)?
  NAMELIST /ini_biogem_nml/ctrl_data_save_derived,ctrl_data_save_GLOBAL
  LOGICAL::ctrl_data_save_slice_ascii                            ! Save time-slice data in ASCII format?
  LOGICAL::ctrl_data_save_sig_ascii                              ! Save time-series data in ASCII format?
  NAMELIST /ini_biogem_nml/ctrl_data_save_slice_ascii,ctrl_data_save_sig_ascii
  logical::opt_append_data                                       ! append data to output files on restart
  NAMELIST /ini_biogem_nml/opt_append_data
  real::par_data_save_ben_Dmin                                   ! minimum depth for 'benthic' average
  NAMELIST /ini_biogem_nml/par_data_save_ben_Dmin
  ! ------------------- TRACER AUDITING AND DEBUGGING OPTIONS -------------------------------------------------------------------- !
  LOGICAL::ctrl_audit                                            ! audit tracer inventory?
  LOGICAL::ctrl_audit_fatal                                      ! halt on audit fail?
  NAMELIST /ini_biogem_nml/ctrl_audit,ctrl_audit_fatal
  real::par_misc_audit_relerr                                    ! threshold of relative inventory change to trigger audit error
  NAMELIST /ini_biogem_nml/par_misc_audit_relerr
  LOGICAL::ctrl_debug_reportwarnings                             ! report all run-time warnings?
  NAMELIST /ini_biogem_nml/ctrl_debug_reportwarnings
  LOGICAL::ctrl_debug_lvl1                                       ! report 'level #1' debug?
  LOGICAL::ctrl_debug_lvl2                                       ! report 'level #2' debug?
  NAMELIST /ini_biogem_nml/ctrl_debug_lvl1,ctrl_debug_lvl2
  ! ------------------- TRACER FORCING ------------------------------------------------------------------------------------------- !
  REAL,DIMENSION(n_atm)::par_atm_force_scale_time                ! scale tracer forcing time points
  REAL,DIMENSION(n_atm)::par_atm_force_scale_val                 ! scale tracer forcing value
  NAMELIST /ini_biogem_nml/par_atm_force_scale_time,par_atm_force_scale_val
  REAL,DIMENSION(n_ocn)::par_ocn_force_scale_time                ! scale tracer forcing time points
  REAL,DIMENSION(n_ocn)::par_ocn_force_scale_val                 ! scale tracer forcing value
  NAMELIST /ini_biogem_nml/par_ocn_force_scale_time,par_ocn_force_scale_val
  integer::par_force_point_i                                     ! 'i' coordinate of point forcing 
  integer::par_force_point_j                                     ! 'j' coordinate of point forcing 
  integer::par_force_point_k                                     ! 'k' coordinate of point forcing 
  NAMELIST /ini_biogem_nml/par_force_point_i,par_force_point_j,par_force_point_k
  ! ############################################################################################################################## !


  ! ****************************************************************************************************************************** !
  ! MODEL CONFIGURATION CONSTANTS - ARRAY DIMENSIONS
  ! ****************************************************************************************************************************** !


  ! *** array dimensions ***
  ! grid dimensions
  INTEGER,PARAMETER::n_i = ilon1_ocn                             ! 
  INTEGER,PARAMETER::n_j = ilat1_ocn                             ! 
  INTEGER,PARAMETER::n_k = inl1_ocn                              !
  ! misc arrays dimensions 
  INTEGER,PARAMETER::n_phys_ocn                           = 21   ! number of ocean box physical descriptors
  INTEGER,PARAMETER::n_phys_ocnatm                        = 20   ! number of ocean-atmosphere interface physical descriptors
  INTEGER,PARAMETER::n_data_max                           = 10001 ! (maximum) number of (time series) data points
  ! options arrays dimensions
  INTEGER,PARAMETER::n_opt_misc                           = 14   ! miscellaneous
  INTEGER,PARAMETER::n_opt_atm                            = 01   ! atmosphere
  INTEGER,PARAMETER::n_opt_bio                            = 06   ! biogeochemical cycling
  INTEGER,PARAMETER::n_opt_force                          = 08   ! forcings
  INTEGER,PARAMETER::n_opt_data                           = 30   ! data (I/O)
  INTEGER,PARAMETER::n_opt_select                         = 05   ! (tracer) selections
  INTEGER,PARAMETER::n_diag_bio                           = 09   ! 
  INTEGER,PARAMETER::n_diag_geochem                       = 07   !
  INTEGER,PARAMETER::n_diag_misc_2D                       = 02   !  


  ! ****************************************************************************************************************************** !
  ! MODEL CONFIGURATION CONSTANTS - ARRAY INDICES NAMES
  ! ****************************************************************************************************************************** !


  ! *** array index values ***
  ! ocean 'physics' properties array indices
  INTEGER,PARAMETER::ipo_lat                              = 01   ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipo_lon                              = 02   ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipo_dlat                             = 03   ! latitude (degrees) [width]
  INTEGER,PARAMETER::ipo_dlon                             = 04   ! longitude (degrees) [width]
  INTEGER,PARAMETER::ipo_latn                             = 05   ! latitude (degrees) [north edge]
  INTEGER,PARAMETER::ipo_lone                             = 06   ! longitude (degrees) [east edge]
  INTEGER,PARAMETER::ipo_Dmid                             = 07   ! depth (m) [mid-point]
  INTEGER,PARAMETER::ipo_dD                               = 08   ! depth (m) [thickness]
  INTEGER,PARAMETER::ipo_Dbot                             = 09   ! depth (m) [bottom]
  INTEGER,PARAMETER::ipo_Dtop                             = 10   ! depth (m) [top]
  INTEGER,PARAMETER::ipo_A                                = 11   ! area (m2)
  INTEGER,PARAMETER::ipo_rA                               = 12   ! reciprocal area (to speed up numerics)
  INTEGER,PARAMETER::ipo_V                                = 13   ! ocean box volume (m3)
  INTEGER,PARAMETER::ipo_rV                               = 14   ! reciprocal volume (to speed up numerics)
  INTEGER,PARAMETER::ipo_M                                = 15   ! ocean box water mass (kg)
  INTEGER,PARAMETER::ipo_rM                               = 16   ! reciprocal mass (to speed up numerics)
  INTEGER,PARAMETER::ipo_mask_ocn                         = 17   ! wet grid point mask (wet = 1.0)
  INTEGER,PARAMETER::ipo_rho                              = 18   ! density
  INTEGER,PARAMETER::ipo_gu                               = 19   ! ocean velocity - u component
  INTEGER,PARAMETER::ipo_gv                               = 20   ! ocean velocity - v component
  INTEGER,PARAMETER::ipo_gw                               = 21   ! ocean velocity - w component
  ! ocean-atmosphere interface 'physics' properties array indices
  INTEGER,PARAMETER::ipoa_lat                             = 01   ! latitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipoa_lon                             = 02   ! longitude (degrees) [mid-point]
  INTEGER,PARAMETER::ipoa_dlat                            = 03   ! latitude (degrees) [width]
  INTEGER,PARAMETER::ipoa_dlon                            = 04   ! longitude (degrees) [width]
  INTEGER,PARAMETER::ipoa_A                               = 05   ! area (m2)
  INTEGER,PARAMETER::ipoa_rA                              = 06   ! reciprocal area (to speed up numerics)
  INTEGER,PARAMETER::ipoa_mask_ocn                        = 07   ! wet grid point mask (wet = 1.0)
  INTEGER,PARAMETER::ipoa_u                               = 08   ! surface wind speed (m s-1)
  INTEGER,PARAMETER::ipoa_seaice                          = 09   ! fractional seaice cover
  INTEGER,PARAMETER::ipoa_seaice_th                       = 10   ! fractional seaice cover
  INTEGER,PARAMETER::ipoa_solfor                          = 11   ! solar forcing (W m-2)
  INTEGER,PARAMETER::ipoa_fxsw                            = 12   ! sw incident at surface (W m-2)
  INTEGER,PARAMETER::ipoa_tau_u                           = 13   ! wind stress - u component
  INTEGER,PARAMETER::ipoa_tau_v                           = 14   ! wind stress - v component
  INTEGER,PARAMETER::ipoa_cost                            = 15   ! cost
  INTEGER,PARAMETER::ipoa_KCO2                            = 16   ! CO2 air-sea gas exchange coefficient (mol m-2 yr-1 uatm-1)
  INTEGER,PARAMETER::ipoa_totFe                           = 17   ! total aeolian Fe input (mol yr-1)
  INTEGER,PARAMETER::ipoa_solFe                           = 18   ! aeolian Fe solubility (fraction)
  INTEGER,PARAMETER::ipoa_mld                             = 19   ! MDL (m below surface)
  INTEGER,PARAMETER::ipoa_seaice_dV                       = 20   ! change in sea-ice volumn
  ! options - misc
!!$  INTEGER,PARAMETER::iopt_misc_t_timescale_BP             = 01   ! simulation time scale as years Before Present?
!!$  INTEGER,PARAMETER::iopt_misc_audit                      = 02   ! carry out tracer audit?
!!$  INTEGER,PARAMETER::iopt_misc_audit_fatal                = 03   ! audit fatal error?
!!$  INTEGER,PARAMETER::iopt_misc_t_timescale_BioGeM         = 04   ! Over-ride goldstein time control?
!!$  INTEGER,PARAMETER::iopt_misc_BioGeM_restart             = 05   ! BioGeM restart? 
!!$  INTEGER,PARAMETER::iopt_misc_sed_closedsystem           = 06   ! set dissolution flux = settling flux to 'close' system
  integer,parameter::iopt_misc_O2_equil                   = 07   ! force O2 equilibrium of ocean with atmosphere
!!$  integer,parameter::iopt_misc_debug1                     = 08   ! debug level #1
!!$  integer,parameter::iopt_misc_debug2                     = 09   ! debug level #2
  integer,parameter::iopt_misc_debugij                    = 10   ! debug - explicit reporting of (i,j) location in main loop
  integer,parameter::iopt_misc_debugwarn                  = 11   ! debug - report all warnings
!!$  integer,parameter::iopt_misc_Snorm                      = 12   ! [SALINITY NORMALIZED SCHEME]
!!$  integer,parameter::iopt_misc_noSnorm                    = 13   ! [NON-SALINITY NORMALIZED SCHEME]
!!$  integer,parameter::iopt_misc_nobio                      = 14   ! [NO BIOLOGICAL UPDATING OF TRACER ARRAYS]
  ! options - 'biology'
!!$  integer,parameter::iopt_bio_remin_POC_fixed             = 01   ! fixed POC remineralization profiles? 
!!$  integer,parameter::iopt_bio_remin_CaCO3_fixed           = 02   ! fixed CaCO3 remineralization profiles? 
!!$  integer,parameter::iopt_bio_remin_opal_fixed            = 03   ! fixed opal remineralization profiles? 
!!$  integer,parameter::iopt_bio_Fe_fixedFetoC               = 05   ! fixed cellular Fe:C ratio?
!!$  integer,parameter::iopt_bio_Fe_fixedKscav               = 06   ! fixed (Dutkiewicz et al. [2005]) scavenging rate const?
  ! options - force 
  INTEGER,PARAMETER::iopt_force_GOLDSTEIn_CO2             = 01   ! use BioGeM CO2 to force to C-GOLDSTEIn energy balance calc?
!!$  INTEGER,PARAMETER::iopt_force_GOLDSTEInTS               = 02   ! allow direct forcing of cgoldstein T,S (array <ts>)?
!!$  INTEGER,PARAMETER::iopt_force_seaice                    = 03   ! over-write cgoldstein fractional sea-ice?
!!$  INTEGER,PARAMETER::iopt_force_windspeed                 = 04   ! over-write cgoldstein reconstructed wind-speed?
!!$  INTEGER,PARAMETER::iopt_force_CaCO3toPOCrainratio       = 05   ! over-write biogem CaCO3:POC export rain ratio?
  INTEGER,PARAMETER::iopt_force_freshwater                = 06   ! modify surface tracer concentrations by fresh water dilution?
!!$  integer,parameter::iopt_force_Cd_alpha                  = 07   ! Apply prescribed partition coefficient field?
!!$  INTEGER,PARAMETER::iopt_force_POCdtoPOCrainratio        = 08   ! over-write biogem CaCd:POC export rain ratio?
  ! options - save
!!$  INTEGER,PARAMETER::iopt_data_save_slice_ocnatm          = 01   ! save atmospheric (interface) composition time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_ocn             = 02   ! save ocean composition time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_ocnsed          = 03   ! save sediment (interface) composition time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_focnatm         = 04   ! save ocn-atm interface flux time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_focnsed         = 05   ! save ocn-sed interface flux time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_fsedocn         = 06   ! save sed-ocn interface flux time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_bio             = 07   ! save biological fluxes time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_carb            = 08   ! save aqueous carbonate chem time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_carbconst       = 09   ! save aqueous carbonate const time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_phys_atm        = 10   ! save atmospheric 'physical' properties time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_phys_ocn        = 11   ! save oceanic 'physical' properties time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_slice_misc            = 12   ! save miscellaneous properties (e.g. overturning) times-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_ocnatm            = 13   ! run-time data save of atmospheric tracers?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_ocn               = 14   ! run-time data save of ocean tracers?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_fexport           = 15   ! run-time data save of export flux?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_ocnsed            = 16   ! run-time data save of core-top sediment composition?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_focnatm           = 17   ! run-time data save of ocean-atmosphere flux?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_focnsed           = 18   ! run-time data save of ocean-sediment flux?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_fsedocn           = 19   ! run-time data save of sediment-ocean flux?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_ocn_sur             = 20   ! run-time data save of ocean surface tracers?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_carb_sur            = 21   ! run-time data save of ocean surface carbonate chem?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_misc              = 22   ! run-time data save of miscelaneous properties?
  INTEGER,PARAMETER::iopt_data_save_timeslice_fnint       = 23   ! construct time slice filename with integer year only?
  INTEGER,PARAMETER::iopt_data_save_config                = 24   ! save copies of biogem config files?
!!$  INTEGER,PARAMETER::iopt_data_save_derived               = 25   ! save derived data (e.g., salinity-normalized tracers)? 
!!$  INTEGER,PARAMETER::iopt_data_save_ascii_slice           = 26   ! save time-slice data in ascii format? 
!!$  INTEGER,PARAMETER::iopt_data_save_ascii_series          = 27   ! save time-series data in ascii format? 
!!$  INTEGER,PARAMETER::iopt_data_save_GLOBAL                = 28   ! save global diagnostics (at time-slice intervals) (ASCII-only)
!!$  INTEGER,PARAMETER::iopt_data_save_slice_diag            = 29   ! save diagnostics time-slice?
!!$  INTEGER,PARAMETER::iopt_data_save_sig_diag              = 30   ! save diagnostics time-series?
  ! tracer selection combination options
  INTEGER,PARAMETER::iopt_select_carbchem                 = 01   ! 
  INTEGER,PARAMETER::iopt_select_ocnatm_CO2               = 02   ! 
  INTEGER,PARAMETER::iopt_select_ocnatm_O2                = 03   ! 
  INTEGER,PARAMETER::iopt_select_ocnatm_N2                = 04   ! 
  INTEGER,PARAMETER::iopt_select_ocnatm_HC                = 05   !
  ! diagnostics - biology
  INTEGER,PARAMETER::idiag_bio_dPO4                      = 01    ! 
  INTEGER,PARAMETER::idiag_bio_dPO4_1                    = 02    ! 
  INTEGER,PARAMETER::idiag_bio_dPO4_2                    = 03    ! 
  INTEGER,PARAMETER::idiag_bio_N2fixation                = 04    ! 
  INTEGER,PARAMETER::idiag_bio_NH4assim                  = 05    ! 
  INTEGER,PARAMETER::idiag_bio_kT                        = 06    ! 
  INTEGER,PARAMETER::idiag_bio_kI                        = 07    ! 
  INTEGER,PARAMETER::idiag_bio_kN                        = 08    ! 
  INTEGER,PARAMETER::idiag_bio_DOMfrac                   = 09    ! 
  ! diagnostics - geochemistry 
  INTEGER,PARAMETER::idiag_geochem_ammox_dNO3            = 01    ! 
  INTEGER,PARAMETER::idiag_geochem_ammox_dNH4            = 02    !
  INTEGER,PARAMETER::idiag_geochem_Nredct_dN2            = 03
  INTEGER,PARAMETER::idiag_geochem_Nredct_dNH4           = 04
  INTEGER,PARAMETER::idiag_geochem_Sredct_dH2S           = 05
  INTEGER,PARAMETER::idiag_geochem_Sredct_dNH4           = 06
  INTEGER,PARAMETER::idiag_geochem_dCH4                  = 07
  ! diagnostics - misc - 2D
  INTEGER,PARAMETER::idiag_misc_2D_FpCO2                 = 01
  INTEGER,PARAMETER::idiag_misc_2D_FpCO2_13C             = 02

  ! *** array index names ***
  ! ocean 'physics'    
  CHARACTER(len=16),DIMENSION(n_phys_ocn),PARAMETER::string_phys_ocn = (/ &
       & 'lat             ', &
       & 'lon             ', &
       & 'dlat            ', &
       & 'dlon            ', &
       & 'latn            ', &
       & 'lone            ', &
       & 'Dmid            ', &
       & 'dD              ', &
       & 'Dbot            ', &
       & 'Dtop            ', &
       & 'A               ', &
       & 'rA              ', &
       & 'V               ', &
       & 'rV              ', &
       & 'M               ', &
       & 'rM              ', &
       & 'mask_ocn        ', &
       & 'rho             ', &
       & 'u               ', &
       & 'v               ', &
       & 'w               ' /)
  ! ocean-atmosphere interface 'physics'
  CHARACTER(len=16),DIMENSION(n_phys_ocnatm),PARAMETER::string_phys_ocnatm = (/ &
       & 'lat             ', &
       & 'lon             ', &
       & 'dlat            ', &
       & 'dlon            ', &
       & 'A               ', &
       & 'rA              ', &
       & 'mask_ocn        ', &
       & 'u               ', &
       & 'seaice          ', &
       & 'seaice_th       ', &
       & 'solfor          ', &
       & 'fxws            ', &
       & 'tau_u           ', &
       & 'tau_v           ', &
       & 'cost            ', &
       & 'KCO2            ', &
       & 'totFe           ', &
       & 'solFe           ', &
       & 'MLD             ', &
       & 'seaice_dV       ' /)
  ! diagnostics - biology
  CHARACTER(len=14),DIMENSION(n_diag_bio),PARAMETER::string_diag_bio = (/ &
       & 'dPO4          ', &
       & 'dPO4_1        ', &
       & 'dPO4_2        ', &
       & 'N2fixation    ', &
       & 'NH4assim      ', &
       & 'k_temp        ', &
       & 'k_light       ', &
       & 'k_nutrients   ', &
       & 'DOMfrac       ' /)
  ! diagnostics - geochemistry
  CHARACTER(len=14),DIMENSION(n_diag_geochem),PARAMETER::string_diag_geochem = (/ &
       & 'NH4_oxid_dNO3 ', &
       & 'NH4_oxid_dNH4 ', &
       & 'NO3_redct_dN2 ', &
       & 'NO3_redct_dNH4', &
       & 'SO4_redct_dH2S', &
       & 'SO4_redct_dNH4', &
       & 'dCH4          ' /)
  ! diagnostics - misc - 2D
  CHARACTER(len=14),DIMENSION(n_diag_misc_2D),PARAMETER::string_diag_misc_2D = (/ &
       & 'FpCO2         ', &
       & 'FpCO2_13C     ' /)

  ! *** miscellaneous ***
  ! Rau et al. [1996,1997] parameter values
  REAL,PARAMETER::const_d13C_DIC_Corg_ed    = 0.7                ! epsilon(d) 13C fractionation factor
  REAL,PARAMETER::const_d13C_DIC_Corg_Q2_x2 = +2.829E-10         ! 2nd order polymonial c(i) approximation: x2
  REAL,PARAMETER::const_d13C_DIC_Corg_Q2_x  = -1.788E-07         ! 2nd order polymonial c(i) approximation: x
  REAL,PARAMETER::const_d13C_DIC_Corg_Q2_c  = +3.170E-05         ! 2nd order polymonial c(i) approximation: c
  ! changes in T or S required to trigger re-calculation of carbonate dissociation constants and Schmidt number
  REAL,parameter::par_carb_dT = 0.1                              ! UNITS: (K)
  REAL,parameter::par_carb_dS = 0.1                              ! UNITS: (o/oo)
  ! parameter determining the maximum flux between surface ocean and atmosphere,
  ! relative to the disequilibrium between ocean and atmosphere
  ! (i)   a value of 1.0 will allow a surface ocean cell to no more than equilibriate during a time-step
  ! (ii)  a value of 2.0 will allow the air-sea difference in partial pressure to be reversed
  ! (iii) a very large value will place no restrictions on air-sea gas exchange
  real,parameter::par_airsea_r_dflux_deqm_max = 1.00


  ! ****************************************************************************************************************************** !
  ! GLOBAL VARIABLE AND RUN-TIME SET PARAMETER ARRAYS
  ! ****************************************************************************************************************************** !

  ! *** Miscellanenous ***
  integer::par_misc_debug_i                                      ! 'i' index value for spatially-explicit debugging
  integer::par_misc_debug_j                                      ! 'j' index value for spatially-explicit debugging
!!$  REAL::par_misc_audit_relerr                                    ! threshold for tracer audit action
  ! strings
  CHARACTER(len=6) ::string_runid                                ! 
  CHARACTER(len=31)::string_restartid                            ! 
  CHARACTER(len=7) ::string_ncrunid                              ! 
  CHARACTER(len=254) ::string_nctsi                              ! 
  CHARACTER(len=254) ::string_nctsint                            ! 
  CHARACTER(len=254) ::string_nctsglob                           ! 
  CHARACTER(len=254) ::string_ncout2d                            ! 
  CHARACTER(len=254) ::string_ncout3d                            ! 
  integer::ncout2d_ntrec                                         ! count for netcdf datasets
  integer::ncout3d_ntrec                                         ! count for netcdf datasets
  integer::ncout2d_iou                                           ! io for netcdf datasets
  integer::ncout3d_iou                                           ! io for netcdf datasets
  ! Schmidt Number coefficients
  real,dimension(4,n_atm)::par_Sc_coef                           ! 
  !  Bunsen Solubility Coefficient coefficients
  real,dimension(6,n_atm)::par_bunsen_coef                       ! 

  ! *** Miscellanenous run-time control options ***
  LOGICAL,DIMENSION(n_opt_misc)::opt_misc                        ! 

  ! *** time control ***
  REAL::par_misc_t_end                                           !
  real::par_misc_t_err                                           ! 
  LOGICAL::par_misc_t_go = .FALSE.                               ! 
  LOGICAL::par_misc_t_echo_header = .TRUE.                       ! 

  ! *** GOLDSTEIN interface with BioGeM ***   
  real,dimension(n_ocn)::tstoocn_offset                          ! tracer units offset (GOLDSTEIN <-> BIOGEM conversion)
  ! ocean
  ! NOTE: ocean tracers (dissolved and particulate) are stored as concentrations (mol kg-1)
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::ocn                         ! ocean tracer array 
  ! atmosphere
  logical,DIMENSION(n_atm)::ocnatm_airsea_eqm                    ! 
  real,DIMENSION(n_atm,n_i,n_j)::ocnatm_airsea_pv                ! 
  real,DIMENSION(n_atm,n_i,n_j)::ocnatm_airsea_solconst          ! 
  ! 'biology'
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::bio_part                    ! ocean tracer particle field (NOTE: <n_sed> tracers)
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::bio_remin                   ! ocean tracer particle remin. field (NOTE: <n_ocn> tracers)
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::bio_settle                  ! ocean tracer particle settling field (NOTE: <n_sed> tracers)
  REAL,DIMENSION(n_sed,n_sed,n_i,n_j)::bio_part_red              ! 'Redfield' ratios
  ! 'physics' 
  REAL,DIMENSION(n_phys_ocn,n_i,n_j,n_k)::phys_ocn               ! 
  REAL,DIMENSION(n_phys_ocnatm,n_i,n_j)::phys_ocnatm             ! 
  ! aqueous carbonate system
  REAL,DIMENSION(n_carb,n_i,n_j,n_k)::carb                       ! 
  REAL,DIMENSION(n_carbconst,n_i,n_j,n_k)::carbconst             ! 
  REAL,DIMENSION(n_carbalk,n_i,n_j,n_k)::carbalk                 ! 
  REAL,DIMENSION(n_carbisor,n_i,n_j,n_k)::carbisor               ! carbonate (carbon) isotopic properties array
  REAL,DIMENSION(3,n_i,n_j,n_k)::carb_TSn                        ! 
  ! diagnostics
  REAL,DIMENSION(n_diag_bio,n_i,n_j)::diag_bio                   ! biology diagnostics
  REAL,DIMENSION(n_diag_geochem,n_i,n_j,n_k)::diag_geochem       ! geochemistry diagnostics
  REAL,DIMENSION(n_ocn,n_i,n_j)::diag_weather                    ! weathering diagnostics
  REAL,DIMENSION(n_atm,n_i,n_j)::diag_airsea                     ! air-sea gas exchange diagnostics
  REAL,DIMENSION(n_diag_misc_2D,n_i,n_j)::diag_misc_2D           ! 
  ! integrated run-time storage arrays
  REAL::int_ocn_tot_M_sig                                        ! 
  REAL::int_ocn_tot_M_sur_sig                                    ! 
  REAL::int_ocn_tot_V_sig                                        ! 
  REAL,DIMENSION(n_ocn)::int_ocn_sig                             ! 
  REAL,DIMENSION(n_atm)::int_ocnatm_sig                          ! 
  REAL,DIMENSION(n_sed)::int_fexport_sig                         ! 
  REAL,DIMENSION(n_atm)::int_focnatm_sig                         ! 
  REAL,DIMENSION(n_sed)::int_focnsed_sig                         ! 
  REAL,DIMENSION(n_ocn)::int_fsedocn_sig                         ! 
  REAL,DIMENSION(n_ocn)::int_ocn_sur_sig                         ! 
  REAL,DIMENSION(n_ocn)::int_ocn_ben_sig                         ! 
  REAL,DIMENSION(n_carb)::int_carb_sur_sig                       ! 
  REAL,DIMENSION(n_carb)::int_carb_ben_sig                       ! 
  REAL::int_misc_seaice_sig                                      ! 
  real::int_misc_seaice_sig_th,int_misc_seaice_sig_vol           ! 
  real::int_misc_opsi_min_sig,int_misc_opsi_max_sig              ! 
  real::int_misc_opsia_min_sig,int_misc_opsia_max_sig            ! 
  real::int_misc_SLT_sig                                         ! 
  real::int_misc_det_Fe_tot_sig,int_misc_det_Fe_dis_sig          ! 
  REAL,DIMENSION(n_sed)::int_ocnsed_sig                          ! 
  REAL,DIMENSION(n_diag_bio)::int_diag_bio_sig                   ! biology diagnostics
  REAL,DIMENSION(n_diag_geochem)::int_diag_geochem_sig           ! geochemistry diagnostics
  REAL,DIMENSION(n_ocn)::int_diag_weather_sig                    ! weathering diagnostics
  REAL,DIMENSION(n_atm)::int_diag_airsea_sig                     ! air-sea gas exchange diagnostics
  REAL,DIMENSION(n_diag_misc_2D)::int_diag_misc_2D_sig           ! 
  ! ### ADD ADDITIONAL TIME-SERIES ARRAY DEFINITIONS HERE ######################################################################## !
  ! 
  ! ############################################################################################################################## !
  ! integrated time slice storage arrays - ocean
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::int_ocn_timeslice           ! 
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::int_bio_part_timeslice      !
  REAL,DIMENSION(n_sed,n_i,n_j,n_k)::int_bio_settle_timeslice    !
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::int_bio_remin_timeslice     !
  REAL,DIMENSION(n_phys_ocn,n_i,n_j,n_k)::int_phys_ocn_timeslice !
  REAL,DIMENSION(n_phys_ocnatm,n_i,n_j)::int_phys_ocnatm_timeslice   !
  REAL,DIMENSION(n_carb,n_i,n_j,n_k)::int_carb_timeslice         !
  REAL,DIMENSION(n_carbconst,n_i,n_j,n_k)::int_carbconst_timeslice   !
  REAL,DIMENSION(n_carbisor,n_i,n_j,n_k)::int_carbisor_timeslice !
  !  integrated time slice storage arrays - ocean-atmosphere interface
  REAL,DIMENSION(n_atm,n_i,n_j)::int_sfcatm1_timeslice           ! 
  REAL,DIMENSION(n_atm,n_i,n_j)::int_focnatm_timeslice           ! 
  !  integrated time slice storage arrays - ocean-sediment interface
  REAL,DIMENSION(n_sed,n_i,n_j)::int_sfcsed1_timeslice           ! 
  REAL,DIMENSION(n_sed,n_i,n_j)::int_focnsed_timeslice           ! 
  REAL,DIMENSION(n_ocn,n_i,n_j)::int_fsedocn_timeslice           ! 
  !  integrated time slice storage arrays - GOLDSTEIn
  REAL,DIMENSION(0:n_j,0:n_k)::int_opsi_timeslice                ! 
  REAL,DIMENSION(0:n_j,0:n_k)::int_opsia_timeslice               ! 
  REAL,DIMENSION(0:n_j,0:n_k)::int_opsip_timeslice               ! 
  REAL,DIMENSION(0:n_j,0:n_k)::int_zpsi_timeslice                ! 
  REAL,DIMENSION(3,n_i,n_j,n_k)::int_u_timeslice                 ! 
  REAL,DIMENSION(n_diag_bio,n_i,n_j)::int_diag_bio_timeslice             ! biology diagnostics
  REAL,DIMENSION(n_diag_geochem,n_i,n_j,n_k)::int_diag_geochem_timeslice ! geochemistry diagnostics
  REAL,DIMENSION(n_ocn,n_i,n_j)::int_diag_weather_timeslice      ! weathering diagnostics
  REAL,DIMENSION(n_atm,n_i,n_j)::int_diag_airsea_timeslice       ! air-sea gas exchange diagnostics
  ! ### ADD ADDITIONAL TIME-SLICE ARRAY DEFINITIONS HERE ######################################################################### !
  ! 
  ! ############################################################################################################################## !
  ! audit arrays
  REAL,DIMENSION(n_ocn)::audit_ocn_init                           !
  REAL,DIMENSION(n_ocn)::audit_ocn_old                            !
  REAL,DIMENSION(n_ocn)::audit_ocn_new                            !
  REAL,DIMENSION(n_ocn)::audit_ocn_delta                          !
  ! options arrays
  LOGICAL,DIMENSION(n_opt_atm)::opt_atm                           !
  LOGICAL,DIMENSION(n_opt_bio)::opt_bio                           !
  LOGICAL,DIMENSION(n_opt_force)::opt_force = .FALSE.             !
  LOGICAL,DIMENSION(n_opt_data)::opt_data                         !
  LOGICAL,DIMENSION(n_opt_select)::opt_select                     !
  ! integrated time series arrays
  REAL,DIMENSION(n_data_max)::par_data_save_sig                   !
  REAL,DIMENSION(n_data_max)::par_data_save_timeslice             !
  ! forcing - restoring
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::force_restore_ocn            ! 
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::force_restore_ocn_I          ! 
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::force_restore_ocn_II         ! 
  REAL,DIMENSION(n_ocn,2,n_data_max)::force_restore_ocn_sig       ! 
  REAL,DIMENSION(n_ocn)::force_restore_ocn_sig_x                  ! 
  REAL,DIMENSION(n_ocn)::force_restore_ocn_tconst                 ! 
  INTEGER,DIMENSION(n_ocn,2)::force_restore_ocn_sig_i             ! 
  LOGICAL,DIMENSION(n_ocn)::force_restore_ocn_select              ! 
  LOGICAL,DIMENSION(n_ocn)::force_restore_ocn_sur                 ! 
  INTEGER,DIMENSION(n_ocn,n_i,n_j)::force_restore_ocn_k1          ! 
  REAL,DIMENSION(n_atm,n_i,n_j)::force_restore_atm                ! 
  REAL,DIMENSION(n_atm,n_i,n_j)::force_restore_atm_I              ! 
  REAL,DIMENSION(n_atm,n_i,n_j)::force_restore_atm_II             ! 
  REAL,DIMENSION(n_atm,2,n_data_max)::force_restore_atm_sig       ! 
  REAL,DIMENSION(n_atm)::force_restore_atm_sig_x                  ! 
  REAL,DIMENSION(n_atm)::force_restore_atm_tconst                 ! 
  INTEGER,DIMENSION(n_atm,2)::force_restore_atm_sig_i             ! 
  LOGICAL,DIMENSION(n_atm)::force_restore_atm_select              ! 
!!$  REAL,DIMENSION(n_sed,n_i,n_j)::force_restore_sed
!!$  REAL,DIMENSION(n_sed,n_i,n_j)::force_restore_sed_I
!!$  REAL,DIMENSION(n_sed,n_i,n_j)::force_restore_sed_II
!!$  REAL,DIMENSION(n_sed,2,n_data_max)::force_restore_sed_sig
!!$  REAL,DIMENSION(n_sed)::force_restore_sed_sig_x
!!$  REAL,DIMENSION(n_sed)::force_restore_sed_tconst
!!$  INTEGER,DIMENSION(n_sed,2)::force_restore_sed_sig_i
!!$  LOGICAL,DIMENSION(n_sed)::force_restore_sed_select
  ! forcing - flux
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::force_flux_ocn              ! 
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::force_flux_ocn_I            ! 
  REAL,DIMENSION(n_ocn,n_i,n_j,n_k)::force_flux_ocn_II           ! 
  REAL,DIMENSION(n_ocn,2,n_data_max)::force_flux_ocn_sig         ! 
  REAL,DIMENSION(n_ocn)::force_flux_ocn_sig_x                    ! 
  INTEGER,DIMENSION(n_ocn,2)::force_flux_ocn_sig_i               ! 
  LOGICAL,DIMENSION(n_ocn)::force_flux_ocn_select                ! 
  LOGICAL,DIMENSION(n_ocn)::force_flux_ocn_scale                 ! 
  INTEGER,DIMENSION(n_ocn,n_i,n_j)::force_flux_ocn_k1            ! 
  REAL,DIMENSION(n_atm,n_i,n_j)::force_flux_atm                  ! 
  REAL,DIMENSION(n_atm,n_i,n_j)::force_flux_atm_I                ! 
  REAL,DIMENSION(n_atm,n_i,n_j)::force_flux_atm_II               ! 
  REAL,DIMENSION(n_atm,2,n_data_max)::force_flux_atm_sig         ! 
  REAL,DIMENSION(n_atm)::force_flux_atm_sig_x                    ! 
  INTEGER,DIMENSION(n_atm,2)::force_flux_atm_sig_i               ! 
  LOGICAL,DIMENSION(n_atm)::force_flux_atm_select                ! 
  LOGICAL,DIMENSION(n_atm)::force_flux_atm_scale                 ! 
  REAL,DIMENSION(n_sed,n_i,n_j)::force_flux_sed                  ! 
  REAL,DIMENSION(n_sed,n_i,n_j)::force_flux_sed_I                ! 
  REAL,DIMENSION(n_sed,n_i,n_j)::force_flux_sed_II               ! 
  REAL,DIMENSION(n_sed,2,n_data_max)::force_flux_sed_sig         ! 
  REAL,DIMENSION(n_sed)::force_flux_sed_sig_x                    ! 
  INTEGER,DIMENSION(n_sed,2)::force_flux_sed_sig_i               ! 
  LOGICAL,DIMENSION(n_sed)::force_flux_sed_select                ! 
  LOGICAL,DIMENSION(n_sed)::force_flux_sed_scale                 ! 
  ! forcing - misc 
  REAL,DIMENSION(2,n_data_max)::force_solconst_sig               ! 
  real,DIMENSION(n_ocn)::force_restore_docn_nuts                 !   
  integer,DIMENSION(n_atm)::force_atm_uniform                    !   
  integer,DIMENSION(n_ocn)::force_ocn_uniform                    !  
  integer,DIMENSION(n_sed)::force_sed_uniform                    !      
  integer,DIMENSION(n_atm)::force_atm_point_i                    !   
  integer,DIMENSION(n_ocn)::force_ocn_point_i                    !  
  integer,DIMENSION(n_sed)::force_sed_point_i                    !      
  integer,DIMENSION(n_atm)::force_atm_point_j                    !   
  integer,DIMENSION(n_ocn)::force_ocn_point_j                    !  
  integer,DIMENSION(n_sed)::force_sed_point_j                    !   
  integer,DIMENSION(n_ocn)::force_ocn_point_k                    !            
  ! ### ADD ADDITIONAL FORCINGS ARRAY DEFINITIONS HERE ########################################################################### !
  ! 
  ! ############################################################################################################################## !
  ! misc
  REAL,DIMENSION(n_i,n_j)::par_phys_seaice                       ! 
  REAL,DIMENSION(n_i,n_j)::par_phys_windspeed                    ! 
  REAL,DIMENSION(n_i,n_j)::par_bio_CaCO3toPOCrainratio           ! 
  REAL,DIMENSION(n_i,n_j)::par_bio_Cd_alpha                      ! 
  REAL,DIMENSION(n_i,n_j)::par_bio_POCdtoPOCrainratio            ! 
  REAL,DIMENSION(n_i,n_j,n_k)::par_scav_fpart_POC                !
  REAL,DIMENSION(n_i,n_j,n_k)::par_scav_fpart_CaCO3              !
  REAL,DIMENSION(n_i,n_j,n_k)::par_scav_fpart_opal               !
  REAL,DIMENSION(n_i,n_j,n_k)::par_scav_fpart_det                !

  ! ****************************************************************************************************************************** !
  ! *** GLOBAL VARIABLES AND RUN-TIME SET PARAMETERS ***************************************************************************** !
  ! ****************************************************************************************************************************** !


  ! *** copies of GOLDSTEIn variables ***
  ! dimensional scale values for the ocean
  REAL::goldstein_usc                                            ! 
  REAL::goldstein_dsc                                            ! 
  REAL::goldstein_fsc                                            ! 
  REAL::goldstein_rh0sc                                          ! 
  REAL::goldstein_rhosc                                          ! 
  REAL::goldstein_cpsc                                           ! 
  ! miscellaneous constants
  REAL::goldstein_saln0                                          ! EMBM reference salinity
  REAL::goldstein_rhoair                                         ! air density
  REAL::goldstein_cd                                             ! drag coefficient for wind stress calc
  REAL::goldstein_ds                                             ! grid spacing; sin(lat)
  REAL::goldstein_dphi                                           ! grid spacing; long
  real::goldstein_scf                                            ! 
  real::phys_solar_constant = 0.0                                ! 
  ! depth and location of oceans
  INTEGER,DIMENSION(n_i,n_j)::goldstein_k1                       ! 
  INTEGER::goldstein_jsf                                         ! 
  INTEGER,DIMENSION(n_j)::goldstein_ips                          ! 
  INTEGER,DIMENSION(n_j)::goldstein_ipf                          ! 
  INTEGER,DIMENSION(n_j)::goldstein_ias                          ! 
  INTEGER,DIMENSION(n_j)::goldstein_iaf                          ! 
  ! miscellaneous
  REAL,DIMENSION(n_k)::goldstein_dz                              ! 
  REAL,DIMENSION(n_k)::goldstein_dza                             ! 
  REAL,DIMENSION(0:n_j)::goldstein_c                             ! 
  REAL,DIMENSION(0:n_j)::goldstein_cv                            ! 
  REAL,DIMENSION(0:n_j)::goldstein_s                             ! 
  REAL,DIMENSION(0:n_j)::goldstein_sv                            !
 
  ! *** I/O ***
  ! string formation associated variables
  INTEGER::n_char_years                                          !
  INTEGER::n_char_years_fractional                               !
  ! integrated values storage arrays
  REAL::int_t_sig                                                ! integrated time for run-time (signal) save (years)
  REAL::int_t_timeslice                                          ! integrated time for time-slice save (years)
  REAL::int_t_timeslice_TOT = 0.0                                ! integrated time for time-slice save (TOTAL) (years)
  integer::int_t_sig_count                                       !
  integer::int_t_timeslice_count                                 !
  ! time series arrays - data save
!!$  REAL::par_data_save_sig_dt                                     ! 
!!$  REAL::par_data_save_timeslice_dt                               ! 
  INTEGER::par_data_save_sig_i                                   ! 
  INTEGER::par_data_save_timeslice_i                             ! 

  ! *** MISC ***
  real::par_bio_c0_I                                             ! 
  real::par_bio_c0_Cd                                            ! 
  real::par_det_Fe_frac                                          ! mass abundance of Fe in dust
!!$  real::par_det_Fe_sol                                           ! aeolian Fe solubility
  real::par_K_FeL                                                ! 
  real::par_scav_Fe_exp                                          ! see: Parekh et al. [2005] 
  real::par_scav_Fe_k0                                           ! Parekh et al. [2006] initial scavenging rate
  real::par_part_red_FeTmin                                      ! 
  real::par_part_red_FetoCmax                                    ! 
  real::par_bio_red_O2_H2SO4                                     ! pseudo 'Redfield ratio' to convert O2 deficit to sulphate O2
  real::par_bio_red_O2_NO3                                       ! pseudo 'Redfield ratio' to convert O2 deficit to nitrate O2
  real::par_bio_remin_opal_K                                     ! opal particulate base dissolution rate (yr-1)


CONTAINS


  ! ****************************************************************************************************************************** !
  ! I/O ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD TIME-SERIES DATA (2 VARIABLES)
  SUBROUTINE sub_load_data_t2(dum_filename,dum_data_scale,dum_data,dum_n_elements)
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename
    REAL,INTENT(in),DIMENSION(2)::dum_data_scale
    REAL,INTENT(inout),DIMENSION(2,n_data_max)::dum_data
    INTEGER,INTENT(inout)::dum_n_elements
    ! local variables
    INTEGER::n
    INTEGER::loc_n_elements,loc_n_start
    REAL,DIMENSION(2,n_data_max)::loc_data
    ! initialize local variables
    loc_data(:,:) = 0.0
    ! check file format
    CALL sub_check_fileformat(TRIM(dum_filename),loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=TRIM(dum_filename),action='read')
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! read in forcing function data
    DO n = 1,loc_n_elements
       READ(unit=in,fmt=*) loc_data(1,n),loc_data(2,n)
    END DO
    CLOSE(in)
    ! re-scale data
    loc_data(1,:) = dum_data_scale(1)*loc_data(1,:)
    loc_data(2,:) = dum_data_scale(2)*loc_data(2,:)
    ! 
    IF (loc_n_elements > n_data_max) THEN
       CALL sub_report_error( &
            & 'biogem_lib','load_data_t2','loc_n_elements > n_data_max', &
            & 'STOPPING', &
            & (/REAL(loc_n_elements),REAL(n_data_max)/),.TRUE. &
            & )
    ELSE if (loc_n_elements > 0) THEN
       IF (ctrl_misc_t_BP .AND. (loc_data(1,loc_n_elements) >= loc_data(1,1))) THEN
          dum_data(1,:) = loc_data(1,:) - par_misc_t_end
          dum_data(2,:) = loc_data(2,:)
       END IF
       IF (ctrl_misc_t_BP .AND. (loc_data(1,loc_n_elements) < loc_data(1,1))) THEN
          DO n = 1,loc_n_elements
             dum_data(1,n) = loc_data(1,loc_n_elements - n + 1) - par_misc_t_end
             dum_data(2,n) = loc_data(2,loc_n_elements - n + 1)
          END DO
       END IF
       IF (.NOT.(ctrl_misc_t_BP) .AND. (loc_data(1,loc_n_elements) <= loc_data(1,1))) THEN
          dum_data(1,:) = par_misc_t_end - loc_data(1,:)
          dum_data(2,:) = loc_data(2,:)
       END IF
       IF (.NOT.(ctrl_misc_t_BP) .AND. (loc_data(1,loc_n_elements) > loc_data(1,1))) THEN
          DO n = 1,loc_n_elements
             dum_data(1,n) = par_misc_t_end - loc_data(1,loc_n_elements - n + 1)
             dum_data(2,n) = loc_data(2,loc_n_elements - n + 1)
          END DO
       END IF
       dum_n_elements = loc_n_elements
    else
       dum_n_elements = 0
    END IF
  END SUBROUTINE sub_load_data_t2
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD TIME-SERIES DATA (1 VARIABLE)
  SUBROUTINE sub_load_data_t1(dum_filename,dum_data_scale,dum_data,dum_n_elements)
    ! dummy variables
    CHARACTER(len=*),INTENT(in)::dum_filename
    REAL,INTENT(in)::dum_data_scale
    REAL,INTENT(inout),DIMENSION(n_data_max)::dum_data
    INTEGER,INTENT(inout)::dum_n_elements
    ! local variables
    INTEGER::n
    INTEGER::loc_n_elements,loc_n_start
    REAL,DIMENSION(n_data_max)::loc_data
    ! initialize local variables
    loc_data(:) = 0.0
    ! check file format
    CALL sub_check_fileformat(TRIM(dum_filename),loc_n_elements,loc_n_start)
    ! open file pipe
    OPEN(unit=in,file=TRIM(dum_filename),action='read')
    ! goto start-of-file tag
    loc_data = 0.0
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)')
    END DO
    ! read in forcing function data
    DO n = 1,loc_n_elements
       READ(unit=in,fmt=*) loc_data(n)
    END DO
    CLOSE(in)
    ! re-scale data
    loc_data(:) = dum_data_scale*loc_data(:)
    ! 
    IF (loc_n_elements > n_data_max) THEN
       CALL sub_report_error( &
            & 'biogem_lib','load_data_t1','loc_n_elements > n_data_max', &
            & 'STOPPING', &
            & (/REAL(loc_n_elements),REAL(n_data_max)/),.TRUE. &
            & )
    ELSE if (loc_n_elements > 0) THEN
       IF (ctrl_misc_t_BP .AND. (loc_data(loc_n_elements) >= loc_data(1))) THEN
          dum_data(1:loc_n_elements) = loc_data(1:loc_n_elements) - par_misc_t_end
       END IF
       IF (ctrl_misc_t_BP .AND. (loc_data(loc_n_elements) < loc_data(1))) THEN
          DO n = 1,loc_n_elements
             dum_data(n) = loc_data(loc_n_elements - n + 1) - par_misc_t_end
          END DO
       END IF
       IF (.NOT.(ctrl_misc_t_BP) .AND. (loc_data(loc_n_elements) <= loc_data(1))) THEN
          dum_data(1:loc_n_elements) = par_misc_t_end - loc_data(1:loc_n_elements)
       END IF
       IF (.NOT.(ctrl_misc_t_BP) .AND. (loc_data(loc_n_elements) > loc_data(1))) THEN
          DO n = 1,loc_n_elements
             dum_data(n) = par_misc_t_end - loc_data(loc_n_elements - n + 1)
          END DO
       END IF
       dum_n_elements = loc_n_elements
    else
       dum_n_elements = 0
    END IF
  END SUBROUTINE sub_load_data_t1
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE THE TIME SCALE SAVE STRING
  SUBROUTINE sub_init_char()
    ! local variables
    INTEGER::n,loc_digit
    REAL::loc_t
    ! find the length (in number of digits) of the longest run-time date
    ! NOTE: add par_misc_t_err to the real number before integer conversion to ensure that the integer part is correctly extracted
    n_char_years = 0
    loc_t = MAX(par_misc_t_start,par_misc_t_end) + par_misc_t_err
    DO n=99,1,-1
       loc_digit = INT(loc_t*10.0**(-(n-1)) + par_misc_t_err)
       IF (loc_digit > 0) THEN 
          n_char_years = n
          EXIT
       END IF
    END DO
    ! set number of decimal places (in years) that save time is stored to
    n_char_years_fractional = 3
  END SUBROUTINE sub_init_char
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FORM THE TIME-SLICE FILENAME
  FUNCTION fun_data_timeslice_filename(dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext)
    ! result variable
    CHARACTER(len=255)::fun_data_timeslice_filename
    ! dummy arguments
    CHARACTER(len=*),INTENT(in)::dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_t_int,loc_t_int_fractional
    REAL::loc_t,loc_t_fractional
    CHARACTER(len=n_char_years)::loc_char_years
    CHARACTER(len=n_char_years_fractional)::loc_char_years_fractional
    ! form filename
    IF (ctrl_misc_t_BP) THEN
       loc_t = par_data_save_timeslice(par_data_save_timeslice_i) + par_misc_t_end
    ELSE
       loc_t = par_misc_t_end - par_data_save_timeslice(par_data_save_timeslice_i)
    END IF
    !
    loc_t_int = INT(loc_t)
    loc_char_years = fun_conv_num_char_n(n_char_years,loc_t_int)
    IF (opt_data(iopt_data_save_timeslice_fnint)) THEN
       loc_filename = &
            & TRIM(dum_string_dir)// &
            & TRIM(dum_string_runid)//'_'//loc_char_years//'_'//TRIM(dum_string_name)// &
            & TRIM(dum_string_ext)
    ELSE
       IF (loc_t > 0.0) THEN
          loc_t_fractional = loc_t - real(loc_t_int)
       ELSE
          loc_t_fractional = 0.0
       END IF
       loc_t_int_fractional = INT(loc_t_fractional*10**n_char_years_fractional)
       loc_char_years_fractional = fun_conv_num_char_n(n_char_years_fractional,loc_t_int_fractional)
       loc_filename = &
            & TRIM(dum_string_dir)// &
            & TRIM(dum_string_runid)//'_'//loc_char_years//'_'//loc_char_years_fractional//'_'//TRIM(dum_string_name)// &
            & TRIM(dum_string_ext)
    END IF
    ! return function value
    fun_data_timeslice_filename = loc_filename
  END FUNCTION fun_data_timeslice_filename
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FORM THE TIME-SERIES FILENAME
  FUNCTION fun_data_timeseries_filename(dum_t,dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext)
    ! result variable
    CHARACTER(len=255)::fun_data_timeseries_filename
    ! dummy arguments
    real,intent(in)::dum_t
    CHARACTER(len=*),INTENT(in)::dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_t_int,loc_t_int_fractional
    REAL::loc_t,loc_t_fractional
    CHARACTER(len=n_char_years)::loc_char_years
    CHARACTER(len=n_char_years_fractional)::loc_char_years_fractional
    ! form filename
    IF (ctrl_misc_t_BP) THEN
       loc_t = dum_t + par_misc_t_end
    ELSE
       loc_t = par_misc_t_end - dum_t
    END IF
    !
    loc_t_int = INT(loc_t)
    loc_char_years = fun_conv_num_char_n(n_char_years,loc_t_int)
    IF (loc_t > 0.0) THEN
       loc_t_fractional = loc_t - real(loc_t_int)
    ELSE
       loc_t_fractional = 0.0
    END IF
    loc_t_int_fractional = INT(loc_t_fractional*10**n_char_years_fractional)
    loc_char_years_fractional = fun_conv_num_char_n(n_char_years_fractional,loc_t_int_fractional)
    loc_filename = &
         & TRIM(dum_string_dir)// &
         & TRIM(dum_string_runid)//'_'//TRIM(dum_string_name)// &
         & TRIM(dum_string_ext)
    ! return function value
    fun_data_timeseries_filename = loc_filename
  END FUNCTION fun_data_timeseries_filename
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! FORM THE TIME-SERIES FILENAME
  FUNCTION fun_data_timesnap_filename(dum_t,dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext)
    ! result variable
    CHARACTER(len=255)::fun_data_timesnap_filename
    ! dummy arguments
    real,intent(in)::dum_t
    CHARACTER(len=*),INTENT(in)::dum_string_dir,dum_string_runid,dum_string_name,dum_string_ext
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_t_int,loc_t_int_fractional
    REAL::loc_t,loc_t_fractional
    CHARACTER(len=n_char_years)::loc_char_years
    CHARACTER(len=n_char_years_fractional)::loc_char_years_fractional
    ! form filename
    IF (ctrl_misc_t_BP) THEN
       loc_t = dum_t + par_misc_t_end
    ELSE
       loc_t = par_misc_t_end - dum_t
    END IF
    !
    loc_t_int = INT(loc_t)
    loc_char_years = fun_conv_num_char_n(n_char_years,loc_t_int)
    IF (opt_data(iopt_data_save_timeslice_fnint)) THEN
       loc_filename = &
            & TRIM(dum_string_dir)// &
            & TRIM(dum_string_runid)//'_'//loc_char_years//'_'//TRIM(dum_string_name)// &
            & TRIM(dum_string_ext)
    ELSE
       IF (loc_t > 0.0) THEN
          loc_t_fractional = loc_t - real(loc_t_int)
       ELSE
          loc_t_fractional = 0.0
       END IF
       loc_t_int_fractional = INT(loc_t_fractional*10**n_char_years_fractional)
       loc_char_years_fractional = fun_conv_num_char_n(n_char_years_fractional,loc_t_int_fractional)
       loc_filename = &
            & TRIM(dum_string_dir)// &
            & TRIM(dum_string_runid)//'_'//loc_char_years//'_'//loc_char_years_fractional//'_'//TRIM(dum_string_name)// &
            & TRIM(dum_string_ext)
    END IF
    ! return function value
    fun_data_timesnap_filename = loc_filename
  END FUNCTION fun_data_timesnap_filename
  ! ****************************************************************************************************************************** !


END MODULE biogem_lib
