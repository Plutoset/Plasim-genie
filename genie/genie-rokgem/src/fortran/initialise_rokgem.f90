
! File: initialise_rokgem.f90
!
! Contains initialisation subroutine for RokGeM, as called in <initialise_genie.F> through <genie_ini_wrappers.f90>
!
! Subroutine: initialise_rokgem
!
! Initialisation subroutine for RokGeM, called in <initialise_genie.F> through <genie_ini_wrappers.f90>
!
! Uses:
!
!  - <rokgem_lib.f90>
!  - <rokgem_data.f90>
!  - <rokgem_box.f90>
!  - <rokgem_data_netCDF.f90>
!
! Calls:
!
! - <sub_load_goin_rokgem>
! - <sub_init_phys_rok>
! - <sub_init_netcdf_rg>
! - <sub_load_rokgem_restart>
! - <sub_data_output_years>
! - <sub_ini_output>
! - <sub_load_data_ij>
! - <sub_land>
! - <sub_antarctica>
! - <define_river_array>
! - <sub_drainage>
! - <define_2D_arrays>
! - <sub_load_weath>
!
! Input/Output:
!
! dum_genie_timestep - number of seconds in a genie timestep
! dum_sfxrok - rocks-surface ocean tracer composition; rok grid
! dum_sfxsumrok1 - rocks-surface ocean fluxes; integrated, ocn grid

subroutine initialise_rokgem( &
     & dum_genie_timestep,    &
     & dum_sfxrok,              &
     & dum_sfxsumrok1           )
  
  use rokgem_lib
  use rokgem_data
  use rokgem_box
  USE rokgem_data_netCDF

! dummy arguments
  REAL,intent(inout)::dum_genie_timestep
  REAL,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxrok     ! rocks-surface tracer composition; rok grid
  REAL,dimension(n_ocn,n_i,n_j),intent(inout)::dum_sfxsumrok1 ! rocks-surface fluxes; integrated, ocn grid

  ! local variables
  integer::loc_iou

  print*,' '
  print*,'======================================================='
  print*,' Initialising rokgem module'
  print*,'======================================================='  

  ! *** load goin information ***
  CALL sub_load_goin_rokgem()

  ! *** initialize rokgem ***
  CALL sub_init_phys_rok()

  ! *** load restart information ***
!  IF (ctrl_continuing) then
!     call sub_load_rokgem_restart()
!  end if

 ! *** setup for netcdf output  ***
  print*, 'initialize netCDF'
  string_ncout2d_rg  = TRIM(par_outdir_name)//'fields_rokgem_2d.nc' !note: this needs to be less than 100 characters
  print*, 'netcdf ouput file: ',TRIM(string_ncout2d_rg)
  ! initialise 2d netcdf files
  IF (ctrl_continuing.AND.opt_append_data.AND.(par_outdir_name.eq.par_rstdir_name)) THEN
     call sub_load_rokgem_restart()
  ELSE   
     ncout2d_ntrec_rg = 0
     call sub_init_netcdf_rg(trim(string_ncout2d_rg),loc_iou)
  ENDIF
  print*, 'netcdf record number: ',ncout2d_ntrec_rg
  print*,'par_outdir_name = par_rstdir_name:',par_outdir_name.eq.par_rstdir_name
  !ncout2d_iou = loc_iou

  ! *** initialize external interface arrays ***
  dum_sfxsumrok1(:,:,:) = 0.0
  dum_sfxrok(:,:,:)     = 0.0

  ! *** initialize timestep counter ***
  tstep_count = 0
  tsteps_per_year = conv_yr_s/(dum_genie_timestep*kocn_loop*conv_kocn_krokgem)
  PRINT*,'timesteps per year                                  :',tsteps_per_year
  ! *** load in years for output generation and initialise output ***
  CALL sub_data_output_years()
  year = min(output_years_0d(output_counter_0d),output_years_2d(output_counter_2d))
  CALL sub_ini_output()
  print*,'======================================================='


! ======= RIVER ROUTING =================================================================!

        print*,'--- RIVER ROUTING ---'

! Read basic land run-off routing file into array runoff_drainage (k1 file)

        CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(topo),n_i+2,n_j+2,runoff_drainage)

! Get landmask and number of landcells

        print*,'getting land mask from k1 file'
        CALL sub_land(runoff_drainage,landmask)
        print*,'number of land cells = ',nlandcells

! work out number of cells and rows in antarctica
          
          CALL sub_antarctica(landmask,ncells_antarctica,nrows_antarctica)

! Read detailed land run-off routing file (routing_new.dat) into array runoff_detail
          runoff_detail_i = 3*max_drain_cells
          runoff_detail_j = n_i*n_j
          CALL define_river_array()
          CALL sub_load_data_ij(TRIM(par_indir_name)//TRIM(routing)//'_'// &
                               & fun_conv_num_char_n(3,n_i)//'_'//fun_conv_num_char_n(3,n_j)//'.dat', &
                               & runoff_detail_i,runoff_detail_j,runoff_detail)

! Work out where on the coast to dump riverine solutes from each grid point 

          CALL sub_drainage(runoff_drainage,runoff_drainto,runoff_detail,runoff_coast)

! Note: sub_coastal_output is used to do the actual routing in the main loop


! ======= 2D WEATHERING =================================================================!

  ! *** load in data for 2D weathering scheme if selected
  IF (par_weathopt.ne.'Global_avg') THEN
     print*,'--- 2D WEATHERING ---'
     gridcell_area = phys_rok(ipr_A,1,1)/1.0E6         ! factor of 1E6 to convert from m^2 to km^2
     print*,'gridcell area = ',gridcell_area
     conv_GKWM = 0.5*gridcell_area                     ! Formula is for Flux of bicarbonate produced, and we want riverine flux of Ca2+ 
                                                       ! (in line with global average formulation inputs), these are in ratio of 2:1 so factor of 0.5.
                                                       ! And fluxes are calculated per km^2 so multiply by gridcell_area
     conv_GKWM_runoff = 0.1 * conv_yr_s                ! Have separate constant for runoff as it is raised to a power in the formula;
                                                       ! runoff units are mm/s in EMBM but cm/yr in Gibbs' formula.         
                                                       ! normalise to annual average runoff used by Gibbs (41.8 cm/yr = 1.32E-05 mm/s)
                                                       !  - number is divided by annual average runoff during calculation in sub_GKWM

     conv_GEM_CO2 = 1.0E3 * conv_yr_s * gridcell_area  ! Runoff units are mm/s in EMBM but l*km^-2*s-1 in the GEM-CO2 formula. (factor 1E6).
                                                       ! Fluxes are calculated per km^2 so multiply by gridcell_area.
                                                       ! Formula is for Flux of CO2 consumed, and we want riverine flux of Ca2+ 
                                                       ! (in line with global average formulation inputs), these are in ratio of 1:1 so OK.
                                                       ! Factor of 10^-3 * conv_yr_s as formula calculates quantites in 10^-3 mol/s and we want mol/yr.
     IF (calibrate_runoff) THEN
          conv_GKWM_runoff = conv_GKWM_runoff*(1.32E-05/par_weather_R0)
          conv_GEM_CO2 = conv_GEM_CO2*(1.1859E-05/par_weather_R0)
     ENDIF

     print*,'conv_GKWM_runoff = ',conv_GKWM_runoff
     print*,'conv_GEM_CO2 = ',conv_GEM_CO2

     SELECT case (par_weathopt)
         case ('GKWM')
         par_nliths = 6
         case ('GEM_CO2')
         par_nliths = 7
     end SELECT
     print*,'number of rock types (no. of files listed in x_lithologies.txt) = ',par_nliths
     CALL define_2D_arrays()
     CALL sub_load_weath(lithology_names,lithology)
  ENDIF


  print*,'======================================================='
  print*,' Initialisation of rokgem module complete'
  print*,'======================================================='
  print*,' '

end subroutine initialise_rokgem
! ******************************************************************************************************************************** !
