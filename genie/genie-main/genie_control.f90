!--------------------------------------------------------
!> Variables which control the overall GENIE configuration.
!!
!> This Fortran90 module contains variables which pertain
!> to the overall GENIE configuration.  For example, the
!> various grid sizes and logical flags specifying which
!> components are used.
!--------------------------------------------------------

MODULE genie_control

! ======================================================================
! Grid dimension variables for tracer (1), u (2) and v (3) points
! Atmosphere
      integer :: ilon1_atm,ilat1_atm
      integer :: ilon2_atm,ilat2_atm
      integer :: ilon3_atm,ilat3_atm
      integer :: inl1_atm,inl2_atm
! Ocean
      integer :: ilon1_ocn,ilat1_ocn
      integer :: ilon2_ocn,ilat2_ocn
      integer :: ilon3_ocn,ilat3_ocn
      integer :: inl1_ocn,inl2_ocn
! SG intrac_ocn = Number of ocean tracers
      integer :: intrac_ocn
! Sea-ice
      integer :: ilon1_sic,ilat1_sic
      integer :: ilon2_sic,ilat2_sic
      integer :: ilon3_sic,ilat3_sic
! Land 
      integer :: ilon1_lnd,ilat1_lnd
      integer :: ilon2_lnd,ilat2_lnd
      integer :: ilon3_lnd,ilat3_lnd
! Land ice sheets
      integer :: ilon1_lic,ilat1_lic
      integer :: ilon2_lic,ilat2_lic
      integer :: ilon3_lic,ilat3_lic
! sediments
      integer :: ilon1_sed,ilat1_sed
! weathering
      integer :: ilon1_rok,ilat1_rok
!     
! ======================================================================
! Assigning grid sizes (add your own [commented] entries below)
! For IGCM

#ifndef GENIENL
#define GENIENL 7 
#endif

#ifndef GENIENX
#define GENIENX 64
#endif

#ifndef GENIENY
#define GENIENY 32
#endif

      parameter(ilon1_atm=GENIENX,ilat1_atm=GENIENY)
      parameter(ilon2_atm=GENIENX,ilat2_atm=GENIENY)
      parameter(ilon3_atm=GENIENX,ilat3_atm=GENIENY)
      parameter(inl1_atm=GENIENL)

! For GOLDSTEIN ocean
#ifndef GOLDSTEINNLONS
#define GOLDSTEINNLONS 36
#endif
#ifndef GOLDSTEINNLATS
#define GOLDSTEINNLATS 36
#endif
#ifndef GOLDSTEINNLEVS
#define GOLDSTEINNLEVS 8
#endif
#ifndef GOLDSTEINNTRACS
#define GOLDSTEINNTRACS 2
#endif
#ifndef GOLDSTEINMAXISLES
#define GOLDSTEINMAXISLES 5
#endif

      parameter(ilon1_ocn=GOLDSTEINNLONS,ilat1_ocn=GOLDSTEINNLATS)
      parameter(ilon2_ocn=GOLDSTEINNLONS,ilat2_ocn=GOLDSTEINNLATS)
      parameter(ilon3_ocn=GOLDSTEINNLONS,ilat3_ocn=GOLDSTEINNLATS)
      parameter(inl1_ocn=GOLDSTEINNLEVS,inl2_ocn=inl1_ocn+1)
      parameter(intrac_ocn=GOLDSTEINNTRACS)

! For c-GOLDSTEIN sea-ice
      parameter(ilon1_sic=GOLDSTEINNLONS,ilat1_sic=GOLDSTEINNLATS)
      parameter(ilon2_sic=GOLDSTEINNLONS,ilat2_sic=GOLDSTEINNLATS)
      parameter(ilon3_sic=GOLDSTEINNLONS,ilat3_sic=GOLDSTEINNLATS)

! For ice sheets in EMBM and ENTS
      parameter(ilon1_lic=GOLDSTEINNLONS,ilat1_lic=GOLDSTEINNLATS)
      parameter(ilon2_lic=GOLDSTEINNLONS,ilat2_lic=GOLDSTEINNLATS)
      parameter(ilon3_lic=GOLDSTEINNLONS,ilat3_lic=GOLDSTEINNLATS)

! For ENTS
      parameter(ilon1_lnd=GOLDSTEINNLONS,ilat1_lnd=GOLDSTEINNLATS)
      parameter(ilon2_lnd=GOLDSTEINNLONS,ilat2_lnd=GOLDSTEINNLATS)
      parameter(ilon3_lnd=GOLDSTEINNLONS,ilat3_lnd=GOLDSTEINNLATS)

! For sediments
#ifndef SEDGEMNLONS
#define SEDGEMNLONS 36
#endif
#ifndef SEDGEMNLATS
#define SEDGEMNLATS 36
#endif

      parameter(ilon1_sed=SEDGEMNLONS,ilat1_sed=SEDGEMNLATS)

! For weathering
#ifndef ROKGEMNLONS
#define ROKGEMNLONS 36
#endif
#ifndef ROKGEMNLATS
#define ROKGEMNLATS 36
#endif

      parameter(ilon1_rok=ROKGEMNLONS,ilat1_rok=ROKGEMNLATS)

!
! ======================================================================
! Miscellaneous control variables

      ! used in for netcdf output in various places
      integer, parameter :: BUFSIZ = 1024  !< to hold character strings
      integer, parameter :: nfiles=4
      integer, parameter :: nmaxdims=4
      integer, parameter :: nall=100

      ! biogeochemistry time stepping ratios
      integer :: conv_kocn_katchem  !< atchem loop modifier (relative to ocean loop)
      integer :: conv_kocn_ksedgem  !< sedgem loop modifier (relative to ocean loop)
      integer :: conv_kocn_kbiogem  !< biogem loop modifier (relative to ocean loop)
      integer :: conv_kocn_krokgem  !< rokgem loop modifier (relative to ocean loop)
      integer :: conv_kocn_kgemlite !< gemlite loop modifier (relative to ocean loop)

      ! total defined tracer numbers
      ! WARNING: parameter information duplicated in gem_cmn.f90
      integer, parameter :: intrac_atm_max = 19
      integer, parameter :: intrac_ocn_max = 56
      integer, parameter :: intrac_sed_max = 56

      ! others
      integer :: koverall_total
      integer :: katm_loop
      integer :: ksic_loop
      integer :: kocn_loop
      integer :: klnd_loop
      integer :: kicesheet_loop
      integer :: kicesheet_start
      integer :: kicesheet_multiply
      integer :: kchem_loop
      integer :: katmos_int
      logical :: lgraphics
      character(len=BUFSIZ) :: coastline_file
      character(len=BUFSIZ) :: glimmer_file
      character(len=BUFSIZ) :: fixedatmos_file
      logical :: flag_ebatmos  !< .true. indicates that EMBM is included in model 'recipe'
      logical :: flag_plasimatmos  !< .true. indicates that PLASIM is included in model 'recipe'
      logical :: flag_igcmatmos  !< .true. indicates that IGCM is included in model 'recipe'
      logical :: flag_fixedocean  !< .true. indicates that prescribed ocean is included in model 'recipe'
      logical :: flag_slabocean  !< .true. indicates that slab ocean is included in model 'recipe'
      logical :: flag_goldsteinocean  !< .true. indicates that GOLDSTEIN ocean is included in model 'recipe'
      logical :: flag_fixedseaice
      logical :: flag_slabseaice
      logical :: flag_goldsteinseaice
      logical :: flag_icesheet
      logical :: flag_fixedatmos
      logical :: flag_fakeatmos
      logical :: flag_land
      logical :: flag_ents
      logical :: flag_fixedland
      logical :: flag_fixedicesheet
      logical :: flag_fixedchem
      logical :: flag_atchem
      logical :: flag_biogem
      logical :: flag_sedgem
      logical :: flag_rokgem
      logical :: flag_checkfluxes_sic
      logical :: flag_checkfluxes_ocn
      logical :: flag_checkfluxes_surf
      logical :: flag_checkfluxes_atlantic
      logical :: flag_ichem
      logical :: flag_glim_pforce = .false.
      ! Set true to force glimmer with precip only, not P-E
      logical :: flag_wind

      logical :: write_flag_atm,write_flag_ocn,write_flag_sic
      character(len=BUFSIZ) :: outputdir_name
      character(len=BUFSIZ) :: fname_restart_main
      character(len=BUFSIZ) :: fname_fluxrestart

      integer :: dt_write
      
      ! Days per year.
      real, parameter :: global_daysperyear = 365.25

END MODULE genie_control
