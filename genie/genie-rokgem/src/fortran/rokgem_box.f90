!
! File: rokgem_box.f90
! 
! Description: this module contains all the main subroutines for running rokgem
! Note: variables listed as Output in the subroutines are actually inout because they are defined in <rokgem.lib>
!
! Uses:
!
! - <rokgem_lib.f90>
! - <rokgem_data.f90>

MODULE rokgem_box

  USE rokgem_lib
  USE rokgem_data
  USE rokgem_data_netCDF
  IMPLICIT NONE
  SAVE


CONTAINS


!========================================================================================!
! ========================== RIVER ROUTING ==============================================!
!========================================================================================!

! ======= GET LAND MASK AND NUMBER OF LAND CELLS  =======================================!

! moved to rokgem_data.f90 for netcdf purpuses (so as not to create a loop of modules calling modules)

! ======= GET NUMBER OF CELLS IN ANTARCTIA ==============================================!

! Subroutine: sub_antarctica
!
! Subroutine to work out the number of grid cells and rows taken up by Antarctica, for the purposes of subtracting 
! Antarctica from output when river-routing (see <sub_drainage>) scheme 3 is used as there is no detailed river-routing information for Antarctica.
!
! Input:
!
! dum_landmask(n_i,n_j) - this is an array with 1s for land and 0s for ocean
!
! Output:
!
! dum_ncells_antarctica - the number of grid cells taken up by Antarctica
! dum_nrows_antarctica - the number of grid rows taken up by Antarctica

       SUBROUTINE sub_antarctica(dum_landmask,dum_ncells_antarctica,dum_nrows_antarctica)

       INTEGER, INTENT(in)             :: dum_landmask(n_i,n_j)  
       INTEGER, INTENT(inout)          :: dum_ncells_antarctica
       INTEGER, INTENT(inout)          :: dum_nrows_antarctica             

       INTEGER                         :: i, j, n

       dum_ncells_antarctica = 0
       dum_nrows_antarctica = 0
       n = 0

       ! start at bottom and work way up until getting a clear lattitude (j) row without any land
       DO j=1,n_j
          dum_nrows_antarctica = j-2
          IF (n.EQ.36) EXIT
          n = 0
          DO i=1,n_i
             IF (dum_landmask(i,j).GT.0) THEN
                 dum_ncells_antarctica = dum_ncells_antarctica + 1
             ELSE
                 n = n + 1
             ENDIF
          END DO
       END DO

       PRINT*,'number of land cells in antarctica = ',dum_ncells_antarctica
       PRINT*,'number of lattitude rows taken up by antarctica = ',dum_nrows_antarctica

       END SUBROUTINE sub_antarctica

!======= SUBROUTINE TO WORK OUT DRAINAGE ================================================!

! Subroutine: sub_drainage
!
! Subroutine for working out the river routing from land cells to the ocean.
! Called during initialisation (see <intialise_rokgem.f90>)
!
! Calls:
!
! - <sub_save_data_ij>
!
! Input:
!
! dum_drainage - this is the *.k1 file
!
! Output:
!
! dum_dum_drainto_1 - array with coastal (lat,lon)s for routing fluxes (simple)
! dum_drainto_2 - detailed river routing file (lat,lon,amount)s - several for each cell
! dum_coast - detailed river routing file (lat,lon,amount)s - several for each cell
       
       SUBROUTINE sub_drainage(dum_drainage,dum_drainto_1,dum_drainto_2,dum_coast)
       
       IMPLICIT NONE
       INTEGER                         :: i, j, k, l, m, n, row, col, lon, lat
       REAL, INTENT(in)                :: dum_drainage(n_i+2,n_j+2)                        ! this is the *.k1 file
       INTEGER, INTENT(inout)          :: dum_drainto_1(n_i,n_j,2)                         ! array with coastal (lat,lon)s for routing fluxes (simple)
       REAL, INTENT(inout)             :: dum_drainto_2(runoff_detail_i,runoff_detail_j)   ! detailed river routing file (lat,lon,amount)s - several for each cell 
       REAL, INTENT(inout)             :: dum_coast(n_i,n_j)

! *******
! ROUTING
! *******

! BASIC DRAINAGE (scheme 1) ---------------------------------------------------------------------
! Work out where on the coast to dump riverine solutes from each grid point 
! (create array 'runoff_drainto' of lon, lat (i,j) values for each grid cell) 
! using the fact that 91=East, 92=South, 93=West & 94=North in the *.k1 files.
! Note that the input array 'dum_drainage' is 38x38 for the 36x36 genie grid.
       
       IF ( routing_scheme.gt.0 ) THEN

       PRINT*,'Calculating costal drainage cells'
       DO i = 1, n_i
          DO j = 1, n_j
              l = i + 1
             m = j + 1
             DO n = 1,n_i*n_j
                IF ( dum_drainage(l,m).lt.91. ) THEN
                   IF (n.eq.1) THEN
                         l = 1
                         m = 1
                       ENDIF
                        EXIT
                ELSEIF ( dum_drainage(l,m).eq.91. ) THEN
                     IF (l.eq.n_i+1) THEN
                     l = 2
                     ELSE
                         l = l + 1
                     ENDIF
                   CYCLE
                ELSEIF ( dum_drainage(l,m).eq.92. ) THEN
                         m = m - 1
                   CYCLE
                ELSEIF ( dum_drainage(l,m).eq.93. ) THEN
                     IF (l.eq.2) THEN
                     l = n_i+1
                     ELSE
                         l = l - 1
                     ENDIF
                   CYCLE
                ELSEIF ( dum_drainage(l,m).eq.94. ) THEN
                         m = m + 1
                   CYCLE
                ENDIF
             END DO
             dum_drainto_1(i,j,1) = l - 1
             dum_drainto_1(i,j,2) = m - 1     
         END DO
       END DO
       
! Save output to files 'runoff_drainto_long.dat' and 'runoff_drainto_latt.dat' which give 
! the lattitude and longitude respectively of the costal grid point that run-off reaches from
! each inland grid point (ocean is denoted by 0s)
       PRINT*,'saving lons. and lats. of costal cells to & runoff_drainto_long.dat and runoff_drainto_latt.dat files'
       CALL sub_save_data_ij(TRIM(par_outdir_name)//'runoff_drainto_long.dat',n_i,n_j,REAL(dum_drainto_1(:,:,1)))       ! from gem_util
       CALL sub_save_data_ij(TRIM(par_outdir_name)//'runoff_drainto_latt.dat',n_i,n_j,REAL(dum_drainto_1(:,:,2)))       ! from gem_util
       
       ENDIF

! INTERMEDIATE (level2) and DETAILED (level 3) ---------------------------------------------------
! use the array runoff_detail read in from file: contains n_i*n_j rows, each with a sucession of 
! (lat, long, fraction) data for each ocean cell corresponding to the land cell in question 
! (each row represents the landcell given by lat= floor(rownumber/n_j) lon=mod(rownumber,n_i)).
       IF ( routing_scheme.gt.1 ) THEN
       
! Adjust read in data so that 36 = highest northern latt. and 1 = lowest southern latt. rather than other way round
       DO i=1,n_i
          DO j=1,n_j
             row = (j-1)*n_j+i
             DO k=1,max_drain_cells
               col = 3*(k-1)
               lat = dum_drainto_2(col+1,row)
               IF (lat.GT.0.1) THEN
                 dum_drainto_2(col+1,row) = n_j + 1 - lat
               ENDIF
             END DO
          END DO
       END DO

! INTERMEDIATE (level 2)--------------------------------------------------------------------------
! With drainage that ends up on genie's land rather than in the ocean (about half!), use the basic 'roof' 
! routing provided by the k1 file to change the lats and lons contained in the detailed routing file appropriately.
       IF ( routing_scheme.eq.2 ) THEN
       DO i=1,n_i
          DO j=1,n_j
             row = (j-1)*n_j+i
             DO k=1,max_drain_cells
             col = 3*(k-1)
             lon = INT(dum_drainto_2(col+2,row))
             lat = INT(dum_drainto_2(col+1,row))
               IF (lat.GT.0.1) THEN
                 IF (landmask(lon,lat).EQ.1) THEN
                 dum_drainto_2(col+1,row) = dum_drainto_1(lon,lat,2)
                 dum_drainto_2(col+2,row) = dum_drainto_1(lon,lat,1)      
                 ENDIF
               ENDIF
             END DO
          END DO
       END DO
       ENDIF
 
       ENDIF   

! ****************
! COASTAL ENDPOINT
! ****************

! Produce map (runoff_coast) showing how many land grid cells' run-off is dumped into each
! costal grid cell

       PRINT*,'Producing map of costal drainage'
       
! initialise dum_coast, by setting all entries to 0.
       dum_coast(:,:) = 0.0
       
! BASIC (level 1)----------------------------------------------------------------------------------
! add 1 to each costal cell for every land cell that drains there
       IF ( routing_scheme.eq.1 ) THEN
        DO i = 1, n_i
          DO j = 1, n_j
            lon = dum_drainto_1(i,j,1)
            lat = dum_drainto_1(i,j,2)
            IF ((lon.NE.0).AND.(lat.NE.0)) THEN
               dum_coast(lon,lat) = dum_coast(lon,lat) + 1.0
            ENDIF
          END DO
        END DO
       ENDIF

! INTERMEDIATE (level2) and DETAILED (level 3)-----------------------------------------------------
! Ignore ocean cells denoted by 0 at start of line in detailed routing file; 
! also check that the cell is covered by the genie landmask (this is now done in Mathematica).
! Add up fractions and dump them into the coastal output array.
       IF ( routing_scheme.gt.1 ) THEN
        DO i=1,n_i
          DO j=1,n_j
             row = (j-1)*n_j+i
             DO k=1,max_drain_cells
             col = 3*(k-1)
             lon = INT(dum_drainto_2(col+2,row))
             lat = INT(dum_drainto_2(col+1,row))
                IF ((lon.NE.0).AND.(lat.NE.0)) THEN
                 dum_coast(lon,lat) = dum_coast(lon,lat) + dum_drainto_2(col+3,row)
               ENDIF
            END DO
          END DO
        END DO
       PRINT*,'sum(dum_coast(:,:)):',sum(dum_coast(:,:))
       ENDIF

! DETAILED (level 3)
! Take account of (approx. half) of flux that ends on land cells, by just taking ocean cells
! note: this means that some land cells won't be contributing much (if any) flux - including all of antarctica
       IF ( routing_scheme.eq.3 ) THEN
       dum_coast(:,:) = -1 * dum_coast(:,:) * (landmask(:,:)-1)
       PRINT*,'sum(dum_coast(:,:)):',sum(dum_coast(:,:))
       ENDIF

! INTERMEDIATE (level2) and DETAILED (level 3)-----------------------------------------------------
! scale the total up to the overall total (= no. of land cells)
! note: for scheme 3 subtract ncells_antarctica from nlandcells because of no Antarctica
! note: runoff_calib is overwritten in sub_coastal_output
       IF ( routing_scheme.gt.1 ) THEN
         runoff_calib = (nlandcells)/sum(dum_coast(:,:))
         IF ( routing_scheme.eq.3 ) THEN
         runoff_calib = (nlandcells-ncells_antarctica)/sum(dum_coast(:,:))
         ENDIF
         dum_coast(:,:) = dum_coast(:,:)*runoff_calib
         PRINT*,'calibration factor = ',runoff_calib
         PRINT*,'after calibration - sum(dum_coast(:,:):',sum(dum_coast(:,:))
       ENDIF

! save output to 'runoff_coast_/routing_scheme/.dat'
       PRINT*,'saving map of coastal drainage to runoff_coast_'//fun_conv_num_char_n(1,routing_scheme)//'.dat'
       CALL sub_save_data_ij(TRIM(par_outdir_name)//'runoff_coast_'//      &
                          &  fun_conv_num_char_n(1,routing_scheme)//'.dat',n_i,n_j,dum_coast(:,:))       ! from gem_util
             
       END SUBROUTINE sub_drainage

       
! ======= ROUTE WEATHERING FLUX TO COAST ================================================!

! Subroutine: sub_coastal_output
!
! Subroutine for routing flux from the land surface to the coastal ocean.
! Called during time-stepping.
!
! Input:
!
! dum_input_array - the array with fluxes
! dum_drainto_1 - array with coastal (lat,lon)s for routing fluxes (simple)
! dum_dum_drainto_2 - detailed river routing array with (lat,lon,amount)s - several for each cell
!
! Output:
!
! dum_output_coast - array with fluxes in coastal ocean

       SUBROUTINE sub_coastal_output(dum_input_array,dum_drainto_1,dum_drainto_2,dum_output_coast)

       ! Dummy variables
       REAL, INTENT(in)                :: dum_input_array(n_i,n_j)                         !array with fluxes
       INTEGER, INTENT(in)             :: dum_drainto_1(n_i,n_j,2)                         !array with coastal (lat,lon)s for routing fluxes (simple)
       REAL, INTENT(in)                :: dum_drainto_2(runoff_detail_i,runoff_detail_j)   !detailed river routing file (lat,lon,amount)s - several for each cell 
       REAL, INTENT(inout)             :: dum_output_coast(n_i,n_j)                        !array with fluxes in coastal ocean

       ! Local counting variables
       INTEGER                         :: i, j, k, row, col, lon, lat
       REAL                            :: tot

       dum_output_coast(:,:) = 0.0
       tot = 0.0

! dump weathering flux to relevant point on coast

! BASIC (level 1)----------------------------------------------------------------------------------
! add 1 to each costal cell for every land cell that drains there
       IF ( routing_scheme.eq.1 ) THEN
        DO i = 1, n_i
          DO j = 1, n_j
            lon = dum_drainto_1(i,j,1)
            lat = dum_drainto_1(i,j,2)
! ignore ocean cells denoted by 0...
            IF ((lon.NE.0).AND.(lat.NE.0)) THEN
               dum_output_coast(lon,lat) = dum_output_coast(lon,lat) + dum_input_array(i,j)
            ENDIF
! ...unless the lithology map has land in the genie ocean - then dump weathering flux 
! straight into that ocean cell. Currently not used as land output is truncated to the genie landmask.
! if land output isn't truncated, get approx 1.5 times more flux.
            IF ((dum_input_array(i,j).NE.0).AND.(lon.EQ.0).AND.(lat.EQ.0)) THEN
                  dum_output_coast(i,j) = dum_output_coast(i,j) + dum_input_array(i,j)  
            ENDIF
          END DO
        END DO
       ENDIF

! INTERMEDIATE (level2) and DETAILED (level 3)-----------------------------------------------------
! Ignore ocean cells denoted by 0 at start of line in detailed routing file; 
! also check that the cell is covered by the genie landmask (this is now done in Mathematica).
! Add up fractions (multiplied by the flux in the input array) and dump them into the coastal output array.
       IF ( routing_scheme.gt.1 ) THEN
        DO i=1,n_i
          DO j=1,n_j
             row = (j-1)*n_j+i
             DO k=1,max_drain_cells
                col = 3*(k-1)
                lon = INT(dum_drainto_2(col+2,row))
                lat = INT(dum_drainto_2(col+1,row))
! ignore ocean cells denoted by 0...
                IF ((lon.NE.0).AND.(lat.NE.0)) THEN
                   dum_output_coast(lon,lat) = dum_output_coast(lon,lat) + dum_input_array(n_i+1-i,j)*dum_drainto_2(col+3,row)
                ENDIF
! ...unless the lithology map has land in the genie ocean - then dump weathering flux 
! straight into that ocean cell. Currently not used as land output is truncated to the genie landmask.
! if land output isn't truncated, get approx 1.5 times more flux.
                IF ((k.EQ.1).AND.(dum_input_array(i,j).NE.0).AND.(landmask(i,j).EQ.0)) THEN
                   dum_output_coast(i,j) = dum_output_coast(i,j) + dum_input_array(i,j)
                ENDIF
             END DO
          END DO
        END DO
       ENDIF

! DETAILED (level 3)
! Take account of (approx. half) of flux that ends on land cells, by just taking ocean cells
! note: this means that some land cells won't be contributing much (if any) flux - including all of antarctica
       IF ( routing_scheme.eq.3 ) THEN
         dum_output_coast(:,:) = -1 * dum_output_coast(:,:) * (landmask(:,:)-1)
       ENDIF

! INTERMEDIATE (level2) and DETAILED (level 3)-----------------------------------------------------
! scale the ocean total up to the land total, taking into account there isn't a 1:1 mapping of land to ocean for these schemes
! note: for scheme 3 subtract ncells_antarctica from nlandcells because of no Antarctica
       IF ( routing_scheme.gt.1 ) THEN
         tot = sum(dum_output_coast(:,:))
         IF ( tot.lt.1.0E-19 ) THEN
            runoff_calib = 0.0
         ELSE
            IF ( routing_scheme.eq.2 ) THEN
               runoff_calib = sum(dum_input_array(:,:))/sum(dum_output_coast(:,:))
            ENDIF
            IF ( routing_scheme.eq.3 ) THEN
               runoff_calib = sum(dum_input_array(:,nrows_antarctica+1:n_j))/sum(dum_output_coast(:,:))
            ENDIF
         ENDIF
         dum_output_coast(:,:) = dum_output_coast(:,:)*runoff_calib
       ENDIF

       END SUBROUTINE sub_coastal_output


!========================================================================================!
!================================ WEATHERING  ===========================================!
!========================================================================================!

!======= GLOBAL AVERAGE WEATHERING (TAKEN FROM CBMS...BIOGEM) ===========================!

! Subroutine: sub_glob_avg_weath
!
! Subroutine to calculate global average weathering.
!
! Calls:
!
! - <sub_init_phys_ocnrok>
! - <sub_coastal_output>
! - <sub_save_data_ij>
!
! Input:
!
! dum_sfcatm1 - atmosphere composition interface array (to get temperature and pCO2 from)
! dum_runoff(n_i,n_j) - run-off array (taken from EMBM)
! dum_photo(n_i,n_j) - photosynthesis array
! dum_respveg(n_i,n_j) - vegetation respiration array
!
! Output:
!
! dum_sfxrok - ocean flux interface array (same no of tracers as used in biogem ocean)


       SUBROUTINE sub_glob_avg_weath(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok,dum_sfxatm1)

       ! code originally from biogem.main.f90 (cbms_goldstein.v8.0.1) by AJR:

       ! dummy variables
       REAL,INTENT(in)                 :: dum_sfcatm1(n_atm,n_io,n_jo)                             ! atmosphere composition interface array
       REAL,INTENT(in)                 :: dum_runoff(n_i,n_j)                                      ! run-off array (taken from EMBM)
       REAL,INTENT(in)                 :: dum_photo(n_i,n_j)                                       ! photosythesis from land veg module (ENTS)
       REAL,INTENT(in)                 :: dum_respveg(n_i,n_j)                                     ! vegetation respiration from land veg module (ENTS)
       REAL,INTENT(inout)              :: dum_sfxrok(n_ocn,n_i,n_j)                                ! ocean flux interface array (same no of tracers as used in biogem ocean)
       REAL,INTENT(inout)              :: dum_sfxatm1(n_atm,n_io,n_jo)                             ! atmosphere flux interface array

       ! local variables
       INTEGER                         :: i, j, k
       REAL                            :: loc_SLT
       REAL                            :: loc_SLT0
       REAL                            :: loc_maxSLT
       REAL                            :: loc_minSLT
       REAL                            :: loc_R
       REAL                            :: loc_R0
       REAL                            :: loc_maxR
       REAL                            :: loc_minR
       REAL                            :: loc_P
       REAL                            :: loc_P2(n_i,n_j)
       REAL                            :: loc_P0
       REAL                            :: loc_maxP
       REAL                            :: loc_minP
       REAL                            :: loc_CO2
       REAL                            :: loc_CO22(n_i,n_j)
       REAL                            :: loc_CO20
       REAL                            :: loc_maxCO2
       REAL                            :: loc_minCO2
       !REAL                            :: loc_A
       REAL                            :: loc_weather_ratio_CaSiO3
       REAL                            :: loc_weather_ratio_CaCO3
       REAL                            :: n, m   

       REAL                            :: loc_force_flux_weather_a(n_atm)                            ! total fluxes (atmosphere variables) 
       REAL                            :: loc_force_flux_weather_a_percell(n_ocn)                    ! flux per grid cell for even distribution (atmosphere variables)
       REAL                            :: loc_force_flux_weather_a_land(n_atm,n_i,n_j)                ! fluxes out of atmosphere (atmosphere variables)

       REAL                            :: loc_force_flux_weather_o(n_ocn)                            ! total fluxes (ocean variables)
       REAL                            :: loc_force_flux_weather_o_percell(n_ocn)                    ! flux per grid cell for even distribution (ocean variables)
       REAL                            :: loc_force_flux_weather_o_land(n_ocn,n_i,n_j)               ! fluxes shared over land (ocean variables)
       REAL                            :: loc_force_flux_weather_o_ocean(n_ocn,n_i,n_j)              ! fluxes into coastal positions in ocean (ocean variables) 

       real::loc_epsilon
       real::loc_alpha
       REAL                            :: loc_standard 
       real::loc_r_Li
       real::loc_R_7Li
           
       CHARACTER(LEN=7),DIMENSION(n_ocn)       :: globtracer_names


       ! initialise tracer names       
       globtracer_names(io_ALK)                  = 'ALK    '
       globtracer_names(io_DIC)                  = 'DIC    '
       globtracer_names(io_Ca)                   = 'Ca     '
       globtracer_names(io_DIC_13C)              = 'DIC_13C'
       globtracer_names(io_DIC_14C)              = 'DIC_14C'
       
       ! initialise arrays
       loc_force_flux_weather_a(:)                 = 0.0                            
       loc_force_flux_weather_a_percell(:)         = 0.0              
       loc_force_flux_weather_a_land(:,:,:)        = 0.0 
       loc_force_flux_weather_o(:)                 = 0.0                            
       loc_force_flux_weather_o_percell(:)         = 0.0              
       loc_force_flux_weather_o_land(:,:,:)        = 0.0       
       loc_force_flux_weather_o_ocean(:,:,:)       = 0.0

       ! set reference surface land (air) temperature, runoff and productivity and CO2 level
       loc_SLT0 = par_weather_T0
       loc_R0 = par_weather_R0
       loc_P0 = par_weather_P0
       loc_CO20 = par_weather_CO20

       ! Initialise ocean array for temperature
       CALL sub_init_phys_ocnrok()                      !in rokgetime_series_namesm_data

       
       ! calculate current mean surface land (air) temperature SLT (degrees C)
       
       IF ((n_i.EQ.n_io).AND.(n_j.EQ.n_jo)) THEN
       
       ! for equal area grid:
       loc_SLT = 0.0
       loc_maxSLT = 0.0
       loc_minSLT = 0.0
       DO i=1,n_i
          DO j=1,n_j
             m = landmask(i,j) * dum_sfcatm1(ia_T,i,j)
             loc_SLT = loc_SLT + m
             IF ((m.GT.loc_maxSLT).AND.(landmask(i,j).EQ.1)) THEN
                loc_maxSLT = m
             ENDIF
             IF ((m.LT.loc_minSLT).AND.(landmask(i,j).EQ.1)) THEN
                loc_minSLT = m
             ENDIF 
          END DO
       END DO
       loc_SLT = loc_SLT/nlandcells
       !code below is alternative to that above, but causes compilation issues!
       !loc_SLT = sum(landmask(:,:) * RESHAPE(dum_sfcatm1(ia_T,:,:),SHAPE=(/ n_i,n_j /)))/nlandcells
       
       ELSE
       ! from genie (accomodates non-equal-area grids):
!       goldstein_k1(:,:) = go_k1(:,:)
!       loc_A = 0.0
!       DO i=1,n_io
!          DO j=1,n_jo
!             IF (n_ko < goldstein_k1(i,j)) THEN
!                loc_SLT = loc_SLT + phys_ocnrok(ipoa_A,i,j)*dum_sfcatm1(ia_T,i,j)
!                loc_A = loc_A + phys_ocnrok(ipoa_A,i,j)
!             end IF
!          end DO
!       end DO
!       loc_SLT = loc_SLT/loc_A

       ENDIF
     

       ! calculate mean runoff (mm s-1)
       ! for equal area grid:
       loc_R = 0.0
       loc_maxR = 0.0
       loc_minR = 0.0
       DO i=1,n_i
          DO j=1,n_j
             m = landmask(i,j) * dum_runoff(i,j)
             loc_R = loc_R + m
             IF ((m.GT.loc_maxR).AND.(landmask(i,j).EQ.1)) THEN
                loc_maxR = m
             ENDIF
             IF ((m.LT.loc_minR).AND.(landmask(i,j).EQ.1)) THEN
                loc_minR = m
             ENDIF 
          END DO
       END DO
       loc_R = loc_R/nlandcells

       ! convert atm pCO2 to ppm
       DO i=1,n_i
          DO j=1,n_j
             loc_CO22(i,j) = 1.0E+06*dum_sfcatm1(ia_PCO2,i,j)
          END DO
       END DO
       ! calculate mean co2 (ppm)
       ! for equal area grid:
       loc_CO2 = 0.0
       loc_maxCO2 = 0.0
       loc_minCO2 = 0.0
       DO i=1,n_i
          DO j=1,n_j
             m = landmask(i,j) * loc_CO22(i,j)
             loc_CO2 = loc_CO2 + m
             IF ((m.GT.loc_maxCO2).AND.(landmask(i,j).EQ.1)) THEN
                loc_maxCO2 = m
             ENDIF
             IF ((m.LT.loc_minCO2).AND.(landmask(i,j).EQ.1)) THEN
                loc_minCO2 = m
             ENDIF 
          END DO
       END DO
       loc_CO2 = loc_CO2/nlandcells


       ! calculate mean surface productivity (kgC m-2 yr-1)
       
       SELECT case (par_prodopt)
         ! Global Primary Productivity
         case ('GPP')
         loc_P2(:,:) = dum_photo(:,:)
         ! Net Primary Productivity
         case ('NPP')
         loc_P2(:,:) = dum_photo(:,:) - dum_respveg(:,:)
       end SELECT
         
       ! for equal area grid:
       loc_P = 0.0
       loc_maxP = 0.0
       loc_minP = 0.0
       DO i=1,n_i
          DO j=1,n_j
             m = landmask(i,j) * loc_P2(i,j)
             loc_P = loc_P + m
             IF ((m.GT.loc_maxP).AND.(landmask(i,j).EQ.1)) THEN
                loc_maxP = m
             ENDIF
             IF ((m.LT.loc_minP).AND.(landmask(i,j).EQ.1)) THEN
                loc_minP = m
             ENDIF 
          END DO
       END DO
       loc_P = loc_P/nlandcells


      ! Do calculations

       loc_weather_ratio_CaCO3 = 1.0
       IF (opt_weather_T_Ca) THEN
           ! make sure that numbers stay positive
           n = 1.0 + 0.049*(loc_SLT - loc_SLT0) ! From BLAG (1983)
           IF (n.lt.0.0) THEN
              n = 0
           ENDIF
           loc_weather_ratio_CaCO3 = n
       ENDIF
       IF (opt_weather_R_Ca) THEN
          IF (opt_weather_R_explicit) THEN
             n = (loc_R/loc_R0)
          ELSE
             n = (1.0 + 0.045*(loc_SLT - loc_SLT0)) ! From GEOCARB
          ENDIF
          loc_weather_ratio_CaCO3 = n*loc_weather_ratio_CaCO3
       ENDIF
       IF (opt_weather_P_Ca) THEN
          IF (opt_weather_P_explicit) THEN
             n = (loc_P/loc_P0) ! From Lenton & Britton (2006)
          ELSE
             n = (2*(loc_CO2/loc_CO20)/(1+(loc_CO2/loc_CO20)))**0.4 ! From GEOCARB
          ENDIF
          loc_weather_ratio_CaCO3 = n*loc_weather_ratio_CaCO3
       ENDIF

       loc_weather_ratio_CaSiO3 = 1.0
       IF (opt_weather_T_Si) THEN
          n = exp(0.09*(loc_SLT - loc_SLT0)) ! From Brady (1991)
          loc_weather_ratio_CaSiO3 =  n
       ENDIF
       IF (opt_weather_R_Si) THEN
          IF (opt_weather_R_explicit) THEN
             n = (loc_R/loc_R0)**0.65
          ELSE
             n = (1.0 + 0.045*(loc_SLT - loc_SLT0))**0.65 ! From GEOCARB
          ENDIF
          loc_weather_ratio_CaSiO3 = n*loc_weather_ratio_CaSiO3
       ENDIF
       IF (opt_weather_P_Si) THEN
          IF (opt_weather_P_explicit) THEN
             n = (loc_P/loc_P0)  ! From Lenton & Britton (2006)
          ELSE
             n = (2*(loc_CO2/loc_CO20)/(1+(loc_CO2/loc_CO20)))**0.4 ! From GEOCARB
          ENDIF
          loc_weather_ratio_CaSiO3 = n*loc_weather_ratio_CaSiO3
       ENDIF

       weather_fCaCO3  = loc_weather_ratio_CaCO3*par_weather_CaCO3
       weather_fCaSiO3 = loc_weather_ratio_CaSiO3*par_weather_CaSiO3

       loc_force_flux_weather_o(io_Ca) = weather_fCaSiO3 + weather_fCaCO3
       loc_force_flux_weather_o(io_ALK) = 2.0*weather_fCaSiO3 + 2.0*weather_fCaCO3
       IF (opt_short_circuit_atm.eqv..true.) THEN
          loc_force_flux_weather_o(io_DIC) = par_outgas_CO2 + weather_fCaCO3
       ELSE
          loc_force_flux_weather_a(ia_PCO2) = -1.0*(2.0*weather_fCaSiO3 + weather_fCaCO3) !'-' because coming out of atmosphere
          loc_force_flux_weather_o(io_DIC) = par_outgas_CO2 + 2.0*weather_fCaSiO3 + 2.0*weather_fCaCO3
       ENDIF

       loc_standard = const_standards(ocn_type(io_DIC_13C))
       loc_force_flux_weather_o(io_DIC_13C) =  &
              & fun_calc_isotope_fraction(par_outgas_CO2_13C,loc_standard)*par_outgas_CO2 + &
              & fun_calc_isotope_fraction(par_weather_CaCO3_13C,loc_standard)*weather_fCaCO3
       loc_force_flux_weather_o(io_DIC_14C) = 0.0

       ! ######################################################################################################################### !
       ! LITHIUM CODE
       ! bulk silicate Li flux
       loc_force_flux_weather_o(io_Li) = par_weather_CaSiO3_fracLi*weather_fCaSiO3
       ! calculate clay fractionation
       IF (loc_weather_ratio_CaSiO3 > const_real_nullsmall) then
          loc_epsilon = min(par_weather_CaSiO3_Li_7Li_epsilon_max,par_weather_CaSiO3_Li_7Li_epsilon*(1.0/loc_weather_ratio_CaSiO3))
       else
          loc_epsilon = 0.0
       end IF
       ! calculate net Li isotopic weathering signature
       loc_standard = const_standards(ocn_type(io_Li_7Li))
       loc_r_Li = fun_calc_isotope_fraction(par_weather_CaSiO3_Li_d7Li,loc_standard)
       loc_alpha = 1.0 + loc_epsilon/1000.0
       loc_R_7Li = loc_r_Li/(1.0 - loc_r_Li)
       loc_force_flux_weather_o(io_Li_7Li) = (loc_alpha*loc_R_7Li/(1.0 + loc_alpha*loc_R_7Li))*loc_force_flux_weather_o(io_Li)
       ! bulk carbonate flux
       ! 
       ! *** DISCOUNT LI CONTENT OF CARBONATES ***
       ! 
       ! ######################################################################################################################### !

       ! Spread out atmosphere variables' fluxes onto land
       DO k=1,n_atm
          loc_force_flux_weather_a_percell(k) = loc_force_flux_weather_a(k)/nlandcells
          loc_force_flux_weather_a_land(k,:,:) = landmask(:,:) * loc_force_flux_weather_a_percell(k)
       END DO
       ! no need to route to the atmosphere - just take it straight from the cells above the land (assuming same grid)
       ! convert from Mol/yr to Mol/sec and put it into passing array (only take variable altered here - pCO2)
       dum_sfxatm1(ia_PCO2,:,:) =  loc_force_flux_weather_a_land(ia_PCO2,:,:)/(phys_rok(ipr_A,:,:)*conv_yr_s)

       ! Spread out ocean variables' fluxes onto land
       DO k=1,n_ocn
          loc_force_flux_weather_o_percell(k) = loc_force_flux_weather_o(k)/nlandcells
          loc_force_flux_weather_o_land(k,:,:) = landmask(:,:) * loc_force_flux_weather_o_percell(k)
       END DO
       ! route it into the coastal ocean cells (to pass to biogem in coupled model) and save the output to file
       DO k=1,n_ocn
!!$          IF((k.EQ.io_ALK).OR.(k.EQ.io_DIC).OR.(k.EQ.io_Ca).OR.(k.EQ.io_DIC_13C).OR.(k.EQ.io_DIC_14C)) THEN
          CALL sub_coastal_output(  loc_force_flux_weather_o_land(k,:,:), &
               runoff_drainto(:,:,:),runoff_detail(:,:), &
               loc_force_flux_weather_o_ocean(k,:,:))
!!$          ENDIF
       END DO
       ! convert from Mol/yr to Mol/sec and put it into passing array 
       dum_sfxrok(:,:,:) = loc_force_flux_weather_o_ocean(:,:,:)/conv_yr_s

       ! Output     
 
       IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN

          outputs = (/loc_SLT,loc_maxSLT,loc_minSLT,loc_R,loc_maxR,loc_minR,loc_P,&
               &loc_maxP,loc_minP,loc_CO2,loc_maxCO2,loc_minCO2,loc_weather_ratio_CaCO3,&
               &loc_weather_ratio_CaSiO3,weather_fCaCO3,weather_fCaSiO3,&
               &loc_force_flux_weather_a(ia_PCO2),loc_force_flux_weather_o(io_ALK),&
               &loc_force_flux_weather_o(io_DIC),loc_force_flux_weather_o(io_Ca),&
               &loc_force_flux_weather_o(io_DIC_13C),sum(loc_force_flux_weather_o_land(io_ALK,:,:)),&
               &sum(loc_force_flux_weather_o_land(io_DIC,:,:)),&
               &sum(loc_force_flux_weather_o_land(io_Ca,:,:)),&
               &sum(loc_force_flux_weather_o_land(io_DIC_13C,:,:)),&
               &sum(loc_force_flux_weather_o_ocean(io_ALK,:,:)),&
               &sum(loc_force_flux_weather_o_ocean(io_DIC,:,:)),&
               &sum(loc_force_flux_weather_o_ocean(io_Ca,:,:)),&
               &sum(loc_force_flux_weather_o_ocean(io_DIC_13C,:,:))/)
          
          call sub_output_0d(n_outputs,(/3,6,12,14,21,25/),outputs,output_descriptions,time_series_names)

       ENDIF
       
       IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN

          IF (opt_2d_netcdf_output) THEN
             call rokgem_netcdf(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,loc_P2,&
                               & loc_force_flux_weather_a_land,loc_force_flux_weather_o_land,loc_force_flux_weather_o_ocean)
          ENDIF

          IF (opt_2d_ascii_output) THEN
             DO k=1,n_ocn
                IF ((k.EQ.io_ALK).OR.(k.EQ.io_DIC).OR.(k.EQ.io_Ca).OR.(k.EQ.io_DIC_13C).OR.(k.EQ.io_DIC_14C)) THEN
                   CALL sub_save_data_ij(TRIM(par_outdir_name)//'globavg_land_'//TRIM(globtracer_names(k))// &
                        & '_year_'//TRIM(year_text)//'.dat', &
                                  & n_i,n_j,loc_force_flux_weather_o_land(k,:,:))                                   
                   CALL sub_save_data_ij(TRIM(par_outdir_name)//'globavg_ocean_'//TRIM(globtracer_names(k))// &
                        & '_year_'//TRIM(year_text)//'.dat', &
                                  & n_i,n_j,loc_force_flux_weather_o_ocean(k,:,:))                                   
                ENDIF
             END DO
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'globavg_atm_PCO2_year_'//TRIM(year_text)//'.dat', &
                                  & n_i,n_j,loc_force_flux_weather_a_land(ia_PCO2,:,:))                                   
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'temperature_year_'//TRIM(year_text)//'.dat',n_i,n_j, &
                  & dum_sfcatm1(ia_T,:,:))
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'runoff_year_'//TRIM(year_text)//'.dat',n_i,n_j,dum_runoff(:,:))   
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'productivity_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_P2(:,:))     
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'CO2_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_CO22(:,:)) 
          ENDIF

       ENDIF


       END SUBROUTINE sub_glob_avg_weath       


!======= 2D LITHOLOGY DEPENDENT WEATHERING ==============================================!

! Subroutine: sub_GKWM
!
! Subroutine for spatially-explicit weathering based on Gibbs et al. (1999)
!
! Calls:
!
! - <check_unit>
! - <check_iostat>
! - <sub_save_data_ij>
!
! Uses:
!
! <genie_util>, ONLY: check_unit, check_iostat
!
! Input:
!
! dum__runoff - run-off read in from exernal module (EMBM, or ENTS)
! dum_lithology - array containing proportions of each type of lithology in each land cell
!
! Output:
!
! dum_bicarb_flux - array containing fluxes of bicarbonate for each lithological type

       subroutine sub_GKWM       (                                  &
                                       & dum_runoff,                      &
                                       & dum_lithology,                   &
                                       & dum_bicarb_flux                  )
         
       USE genie_util, ONLY: check_unit, check_iostat

       
       IMPLICIT NONE
       
       ! dummy variables
       REAL, intent(in)                :: dum_runoff(n_i,n_j)
       REAL, intent(in)                :: dum_lithology(par_nliths,n_i,n_j)
       REAL, intent(inout)             :: dum_bicarb_flux(par_nliths,n_i,n_j)            ! F_HCO_3- is sum of this
       
       ! local variables
       REAL                            :: avg_runoff
       REAL                            :: r_avg_runoff
       REAL                            :: conv_factor(par_nliths)
       REAL                            :: rescale_runoff
       INTEGER                         :: i, j, k, ios

! Calculate average runoff and reciprocal
       avg_runoff=sum(landmask(:,:) * dum_runoff(:,:))/nlandcells
       r_avg_runoff = 1.0/avg_runoff

! Set calibration factor if option is set
       IF (opt_weather_R_Ca.or.opt_weather_R_Si) THEN
          rescale_runoff = avg_runoff
       ELSE
          rescale_runoff = par_weather_R0
       ENDIF

! Speed up numerics by combining conversion factor, calibration with average runoff, and k
       do k = 1, par_nliths
          conv_factor(k) = weath_consts(k,1) * conv_GKWM * ((conv_GKWM_runoff*rescale_runoff*r_avg_runoff) ** weath_consts(k,2))  ! see initialise_rokgem.f90 for info on conversion factors conv_*
       end do

! Calculate F_HCO_3- (bicarb_flux)       
       DO k = 1, par_nliths
          DO i = 1, n_i
                DO j = 1, n_j
               dum_bicarb_flux(k,i,j) = conv_factor(k) *                    &              
       &                                dum_lithology(k,i,j) *              &
       &                                (dum_runoff(i,j) ** weath_consts(k,2))
            END DO
         END DO
       END DO

! Save data to files bicarb_lith.dat where 'lith' is the lithology
       IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
       IF (opt_2d_ascii_output) THEN
       DO k = 1,par_nliths
!          PRINT*,'Saving map of bicarb flux for ',TRIM(lithology_names(k)),                            &
!                &' to bicarb_',lithology_names(k)(1:LEN(TRIM(lithology_names(k)))-4)//'.dat'
          call check_unit(17,__LINE__,__FILE__)
          OPEN(17,file=TRIM(par_outdir_name)//                                   &
                     & 'bicarb_'//lithology_names(k)(1:LEN(TRIM(lithology_names(k)))-4)//'_'//              &
                                    TRIM(year_text)//'.dat',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
               DO j=n_j,1,-1
             WRITE(17,*,iostat=ios)(dum_bicarb_flux(k,i,j),i=1,n_i)
             call check_iostat(ios,__LINE__,__FILE__)
          END DO
          CLOSE(17,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       END DO
       ENDIF
       ENDIF

       END SUBROUTINE sub_GKWM


!========================================================================================!
       
! Subroutine: sub_GEM_CO2 
!
! Subroutine for spatially-explicit weathering based on Amiotte-Suchet et al. (2003)
!
! Calls:
!
! - <check_unit>
! - <check_iostat>
! - <sub_save_data_ij>
!
! Uses:
!
! <genie_util>, ONLY: check_unit, check_iostat
!
! Input:
!
! dum__runoff - run-off read in from exernal module (EMBM, or ENTS)
! dum_lithology - array containing proportions of each type of lithology in each land cell
!
! Output:
!
! dum_bicarb_flux - array containing fluxes of bicarbonate for each lithological type

       SUBROUTINE sub_GEM_CO2     (                                  &
                                       & dum_runoff,                      &
                                       & dum_lithology,                   &
                                       & dum_bicarb_flux                  )

       USE genie_util, ONLY: check_unit, check_iostat

       IMPLICIT NONE
       
       ! dummy variables
       REAL,INTENT(in)                 :: dum_runoff(n_i,n_j)
       REAL, INTENT(in)                :: dum_lithology(par_nliths,n_i,n_j)
       REAL, INTENT(inout)             :: dum_bicarb_flux(par_nliths,n_i,n_j)            ! F_HCO_3- is sum of this

       ! local variables
       REAL                            :: avg_runoff
       REAL                            :: rescale_runoff
       REAL                            :: r_avg_runoff
       REAL                            :: conv_factor(par_nliths)
       INTEGER                         :: i, j, k, ios

! Calculate average runoff and reciprocal
       avg_runoff=sum(landmask(:,:) * dum_runoff(:,:))/nlandcells
       r_avg_runoff = 1.0/avg_runoff

! Set calibration factor if option is set
       IF (opt_weather_R_Ca.or.opt_weather_R_Si) THEN
          rescale_runoff = avg_runoff
       ELSE
          rescale_runoff = par_weather_R0
       ENDIF


! Speed up numerics by combining conversion factor, calibration with average runoff, and k
       do k = 1, par_nliths
          conv_factor(k) = weath_consts(k,1) * conv_GEM_CO2 * rescale_runoff * r_avg_runoff            ! see initialise_rokgem.f90 for info on conversion factors conv_GEM_CO2
       end do

! Calculate F_HCO_3- (bicarb_flux)       
       DO k = 1, par_nliths
          DO i = 1, n_i
                DO j = 1, n_j
               dum_bicarb_flux(k,i,j) = conv_factor(k) * dum_lithology(k,i,j) * dum_runoff(i,j)
            END DO
         END DO
      END DO

! Save data to files bicarb_lith.dat where 'lith' is the lithology
       IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
       IF (opt_2d_ascii_output) THEN
       DO k = 1,par_nliths
!          PRINT*,'Saving map of bicarb flux for ',TRIM(lithology_names(k)),                            &
!                &' to bicarb_',lithology_names(k)(1:LEN(TRIM(lithology_names(k)))-4)//'.dat'
          call check_unit(17,__LINE__,__FILE__)
          OPEN(17,file=TRIM(par_outdir_name)//                                   &
                     & 'bicarb_'//lithology_names(k)(1:LEN(TRIM(lithology_names(k)))-4)//'_year_'//          &
                                    TRIM(year_text)//'.dat',iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
               DO j=n_j,1,-1
             WRITE(17,*,iostat=ios)(dum_bicarb_flux(k,i,j),i=1,n_i)
             call check_iostat(ios,__LINE__,__FILE__)
          END DO
          CLOSE(17,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       END DO 
       ENDIF
       ENDIF

    

       END SUBROUTINE sub_GEM_CO2


! ======= SUM UP THE WEATHERING FLUX ====================================================!

! Subroutine: sum_bicarb_flux
!
! Subroutine to sum up bicarbonate fluxes.
!
! Calls:
!
! - <sub_save_data_ij>
!
! Input:
!
! dum_bicarb_flux - array containing fluxes of bicarbonate for each lithological type
!
! Output:
!
! dum_total_bicarb_flux - array containing total fluxes of bicarbonate from each grid cell

       SUBROUTINE sum_bicarb_flux(dum_bicarb_flux,dum_total_bicarb_flux)

       REAL, INTENT(in)                :: dum_bicarb_flux(par_nliths,n_i,n_j)            ! F_HCO_3- is sum of this
       REAL, INTENT(inout)             :: dum_total_bicarb_flux(n_i,n_j)                 ! F_HCO_3-

       INTEGER                         :: i, j, k

       dum_total_bicarb_flux(:,:) = 0.0

       DO k=1,par_nliths
          DO i=1,n_i
              DO j=1,n_j
                 dum_total_bicarb_flux(i,j) = dum_total_bicarb_flux(i,j) +               &
       &        dum_bicarb_flux(k,i,j)
              END DO
          END DO
       END DO

! Save data to file bicarb_total.dat
       IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
       IF (opt_2d_ascii_output) THEN
       CALL sub_save_data_ij(TRIM(par_outdir_name)//'bicarb_total_year_'//TRIM(year_text)//'.dat',n_i,n_j, &
            & dum_total_bicarb_flux(:,:))   
       ENDIF           
       ENDIF

       END SUBROUTINE sum_bicarb_flux


! ======= DIVIDE WEATHERING FLUX INTO CARBONATE AND SILICATE WEATHERING ==================!

! Subroutine: sum_bicarb_flux_CaSi
!
! Subroutine to divide up bicarbonate fluxes between those resulting from Carbonate rocks, and those from Silicate rocks.
!
! Calls:
!
! - <sub_save_data_ij>
!
! Input:
!
! dum__bicarb_flux - array containing fluxes of bicarbonate for each lithological type
!
! Output:
!
! dum_total_bicarb_flux_Ca - array containing total fluxes of bicarbonate resulting from Carbonate rocks for each grid cell
! dum_total_bicarb_flux_Si - array containing total fluxes of bicarbonate resulting from Silicate rocks for each grid cell

       SUBROUTINE sum_bicarb_flux_CaSi(dum_bicarb_flux,dum_total_bicarb_flux_Ca,dum_total_bicarb_flux_Si)

       REAL, INTENT(in)                :: dum_bicarb_flux(par_nliths,n_i,n_j)            ! F_HCO_3- is sum of this
       REAL, INTENT(inout)             :: dum_total_bicarb_flux_Ca(n_i,n_j)              ! F_HCO_3- for Ca rocks
       REAL, INTENT(inout)             :: dum_total_bicarb_flux_Si(n_i,n_j)              ! F_HCO_3- for Si rocks

       INTEGER                         :: i, j, k

       ! k = 1 is for carb rock type - corresponding to Carbonate rock weathering
       ! all other rock types are taken to be silicates (2 onwards); not sure if this is correct!
       ! Now have 0.125 of sand rock type as Carbonate weathering also - so have extra array terms for fCa and fSi in weath_consts array
       dum_total_bicarb_flux_Ca(:,:) = 0.0
       DO k=1,par_nliths
          DO i=1,n_i
              DO j=1,n_j
                 dum_total_bicarb_flux_Ca(i,j) = dum_total_bicarb_flux_Ca(i,j) +               &
       &        dum_bicarb_flux(k,i,j) * weath_consts(k,3)
              END DO
          END DO
       END DO

       dum_total_bicarb_flux_Si(:,:) = 0.0
       DO k=1,par_nliths
          DO i=1,n_i
              DO j=1,n_j
                 dum_total_bicarb_flux_Si(i,j) = dum_total_bicarb_flux_Si(i,j) +               &
       &        dum_bicarb_flux(k,i,j) * weath_consts(k,4)
              END DO
          END DO
       END DO

! Save data to file bicarb_total.dat
       IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN
       IF (opt_2d_ascii_output) THEN
       CALL sub_save_data_ij(TRIM(par_outdir_name)//'bicarb_total_Ca_year_'//TRIM(year_text)//'.dat',n_i,n_j, &
            & dum_total_bicarb_flux_Ca(:,:))
       CALL sub_save_data_ij(TRIM(par_outdir_name)//'bicarb_total_Si_year_'//TRIM(year_text)//'.dat',n_i,n_j, &
            & dum_total_bicarb_flux_Si(:,:))
       ENDIF      
       ENDIF       

       END SUBROUTINE sum_bicarb_flux_CaSi


!========================================================================================!

! Subroutine: sub_2D_weath
!
! Subroutine to calculate spatially-explicit weathering
!
! Calls:
!
! - <sub_init_phys_ocnrok>
! - <calc_P>
! - <sub_coastal_output>
! - <sub_save_data_ij>
!
! Input:
!
! dum_sfcatm1 - atmosphere composition interface array (to get temperature from)
! dum__runoff - run-off read in from exernal module (EMBM, or ENTS)
! dum_photo(n_i,n_j) - photosynthesis array
! dum_respveg(n_i,n_j) - vegetation respiration array
!
! Output:
!
! dum_sfxrok - ocean flux interface array (same no of tracers as used in biogem ocean)

       SUBROUTINE sub_2D_weath(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,dum_sfxrok)

       ! Based on SUBROUTINE sub_glob_avg_weath - see above

       ! dummy variables
       REAL,INTENT(in)                 :: dum_sfcatm1(n_atm,n_io,n_jo)                             ! atmosphere composition interface array
       REAL,INTENT(in)                 :: dum_runoff(n_i,n_j)
       REAL,INTENT(in)                 :: dum_photo(n_i,n_j)                                       ! photosythesis from land veg module (ENTS)
       REAL,INTENT(in)                 :: dum_respveg(n_i,n_j)                                     ! vegetation respiration from land veg module (ENTS)
       REAL,INTENT(inout)              :: dum_sfxrok(n_ocn,n_i,n_j)                                ! ocean flux interface array (same no of tracers as used in biogem ocean)
       
       ! local variables
       INTEGER                         :: i, j, k
       REAL                            :: loc_SLT(n_i,n_j)
       REAL                            :: loc_SLT0
       REAL                            :: loc_P(n_i,n_j)
       REAL                            :: loc_P0
       REAL                            :: loc_CO2(n_i,n_j)
       REAL                            :: loc_CO20
       REAL                            :: loc_weather_ratio_CaSiO3(n_i,n_j)
       REAL                            :: loc_weather_ratio_CaCO3(n_i,n_j)
       REAL                            :: n
       REAL                            :: loc_standard

       REAL                            :: loc_force_flux_weather_a_land(n_atm,n_i,n_j)             ! fluxes shared over land (atmosphere variables)
       REAL                            :: loc_force_flux_weather_o_land(n_ocn,n_i,n_j)               ! fluxes shared over land (ocean variables)
       REAL                            :: loc_force_flux_weather_o_ocean(n_ocn,n_i,n_j)              ! fluxes into coastal positions in ocean (ocean variables)      
              
       CHARACTER(LEN=7),DIMENSION(n_ocn)       :: globtracer_names


       ! initialise tracer names       
       globtracer_names(io_ALK)                  = 'ALK    '
       globtracer_names(io_DIC)                  = 'DIC    '
       globtracer_names(io_Ca)                   = 'Ca     '
       globtracer_names(io_DIC_13C)              = 'DIC_13C'
       globtracer_names(io_DIC_14C)              = 'DIC_14C'
       
       ! initialise arrays   
       loc_force_flux_weather_a_land(:,:,:)        = 0.0           
       loc_force_flux_weather_o_land(:,:,:)        = 0.0       
       loc_force_flux_weather_o_ocean(:,:,:)       = 0.0

       ! set reference surface land (air) temperature and productivity
       loc_SLT0 = par_weather_T0
       loc_P0 = par_weather_P0
       loc_CO20 = par_weather_CO20

       ! Initialise ocean array for temperature
       CALL sub_init_phys_ocnrok()                      !in rokgem_data

       ! extract temperature to local array to please intel compilers
       DO i=1,n_i
          DO j=1,n_j
             loc_SLT(i,j) = dum_sfcatm1(ia_T,i,j)
          END DO
       END DO

       ! calculate mean surface productivity (kgC m-2 yr-1)
       SELECT case (par_prodopt)
         ! Global Primary Productivity
         case ('GPP')
         loc_P(:,:) = dum_photo(:,:)
         ! Net Primary Productivity
         case ('NPP')
         loc_P(:,:) = dum_photo(:,:) - dum_respveg(:,:)
       end SELECT

       ! convert atm pCO2 to ppm
       DO i=1,n_i
          DO j=1,n_j
             loc_CO2(i,j) = 1.0E+06*dum_sfcatm1(ia_PCO2,i,j)
          END DO
       END DO

       ! Do calculations
      
       loc_weather_ratio_CaCO3(:,:) = 1.0
       IF (opt_weather_T_Ca) THEN
       DO i=1,n_i
          DO j=1,n_j
             ! make sure that numbers stay positive
             n = 1.0 + 0.049*(dum_sfcatm1(ia_T,i,j) - loc_SLT0)
             IF (n.lt.0.0) THEN
                n = 0
             ENDIF
             loc_weather_ratio_CaCO3(i,j) = n
          END DO
       END DO
       ENDIF
       IF (opt_weather_P_Ca) THEN
          IF (opt_weather_P_explicit) THEN
             loc_weather_ratio_CaCO3(:,:) = (loc_P(:,:)/loc_P0)*loc_weather_ratio_CaCO3(:,:)  ! From Lenton & Britton (2006)
          ELSE
             loc_weather_ratio_CaCO3(:,:) = loc_weather_ratio_CaCO3(:,:)* &
                                           & (2*(loc_CO2(:,:)/loc_CO20)/(1+(loc_CO2(:,:)/loc_CO20)))**0.4 ! From GEOCARB
          ENDIF
       ENDIF

       loc_weather_ratio_CaSiO3(:,:) = 1.0
       IF (opt_weather_T_Si) THEN
          DO i=1,n_i
             DO j=1,n_j
                loc_weather_ratio_CaSiO3(i,j) = exp(0.09*(dum_sfcatm1(ia_T,i,j) - loc_SLT0)) 
             END DO
          END DO
       ENDIF
       IF (opt_weather_P_Si) THEN
          IF (opt_weather_P_explicit) THEN
             loc_weather_ratio_CaSiO3(:,:) = (loc_P(:,:)/loc_P0)*loc_weather_ratio_CaSiO3(:,:)  ! From Lenton & Britton (2006)
          ELSE
             loc_weather_ratio_CaSiO3(:,:) = loc_weather_ratio_CaSiO3(:,:)* &
                                           & (2*(loc_CO2(:,:)/loc_CO20)/(1+(loc_CO2(:,:)/loc_CO20)))**0.4 ! From GEOCARB
          ENDIF
       ENDIF

       ! calibrations
       IF (calibrate_weath) THEN
         SELECT case (par_weathopt)
              case ('GKWM')
         weather_fCaCO3_2D(:,:)  = loc_weather_ratio_CaCO3(:,:)*total_bicarb_flux_Ca(:,:)*calibrate_weather_GKWM_CaCO3
         weather_fCaSiO3_2D(:,:) = loc_weather_ratio_CaSiO3(:,:)*total_bicarb_flux_Si(:,:)*calibrate_weather_GKWM_CaSiO3
            case ('GEM_CO2')
         weather_fCaCO3_2D(:,:)  = loc_weather_ratio_CaCO3(:,:)*total_bicarb_flux_Ca(:,:)*calibrate_weather_GEM_CO2_CaCO3
         weather_fCaSiO3_2D(:,:) = loc_weather_ratio_CaSiO3(:,:)*total_bicarb_flux_Si(:,:)*calibrate_weather_GEM_CO2_CaSiO3
         end SELECT
       ELSE
         weather_fCaCO3_2D(:,:)  = loc_weather_ratio_CaCO3(:,:)*total_bicarb_flux_Ca(:,:)
         weather_fCaSiO3_2D(:,:) = loc_weather_ratio_CaSiO3(:,:)*total_bicarb_flux_Si(:,:)
       ENDIF

       loc_force_flux_weather_o_land(io_Ca,:,:) = weather_fCaSiO3_2D(:,:) + weather_fCaCO3_2D(:,:)
       loc_force_flux_weather_o_land(io_ALK,:,:) = 2.0*weather_fCaSiO3_2D(:,:) + 2.0*weather_fCaCO3_2D(:,:)
       IF (opt_short_circuit_atm.eqv..true.) THEN
          loc_force_flux_weather_o_land(io_DIC,:,:) = landmask(:,:)*par_outgas_CO2/nlandcells + weather_fCaCO3_2D(:,:)
       ELSE
          loc_force_flux_weather_a_land(ia_PCO2,:,:) = -1.0*(2.0*weather_fCaSiO3_2D(:,:) + weather_fCaCO3_2D(:,:)) !'-' because coming out of atmosphere
          loc_force_flux_weather_o_land(io_DIC,:,:) = landmask(:,:)*par_outgas_CO2/nlandcells + 2.0*weather_fCaSiO3_2D(:,:) + &
               & 2.0*weather_fCaCO3_2D(:,:)
       ENDIF

       loc_standard = const_standards(ocn_type(io_DIC_13C))
       loc_force_flux_weather_o_land(io_DIC_13C,:,:) =  &
              & fun_calc_isotope_fraction(par_outgas_CO2_13C,loc_standard)*landmask(:,:)*par_outgas_CO2/nlandcells + &
              & fun_calc_isotope_fraction(par_weather_CaCO3_13C,loc_standard)*0.5*weather_fCaCO3_2D(:,:)
       loc_force_flux_weather_o_land(io_DIC_14C,:,:) = 0.0

       ! route fluxes into the coastal ocean cells (to pass to biogem in coupled model) and save the output to file
       DO k=1,n_ocn
          IF((k.EQ.io_ALK).OR.(k.EQ.io_DIC).OR.(k.EQ.io_Ca).OR.(k.EQ.io_DIC_13C).OR.(k.EQ.io_DIC_14C)) THEN
          CALL sub_coastal_output( loc_force_flux_weather_o_land(k,:,:),                                    &
                                     & runoff_drainto(:,:,:),runoff_detail(:,:),                            &
                                     & loc_force_flux_weather_o_ocean(k,:,:)                                )
          ENDIF
       END DO
       
       dum_sfxrok(:,:,:) = loc_force_flux_weather_o_ocean(:,:,:)
       
       ! convert from Mol/yr to Mol/sec for passing out
       dum_sfxrok(:,:,:) = dum_sfxrok(:,:,:)/conv_yr_s
       
      ! Output

      IF (tstep_count.eq.output_tsteps_0d(output_counter_0d)) THEN

         outputs = (/sum(landmask(:,:)*loc_SLT(:,:))/nlandcells,maxval(landmask(:,:)*loc_SLT(:,:)), &
              & minval(landmask(:,:)*loc_SLT(:,:)), &
                   & sum(landmask(:,:)*dum_runoff(:,:))/nlandcells,maxval(landmask(:,:)*dum_runoff(:,:)), &
                   & minval(landmask(:,:)*dum_runoff(:,:)), &
                   & sum(landmask(:,:)*loc_P(:,:))/nlandcells,maxval(landmask(:,:)*loc_P(:,:)),minval(landmask(:,:)*loc_P(:,:)), &
                   & sum(landmask(:,:)*loc_CO2(:,:))/nlandcells,maxval(landmask(:,:)*loc_CO2(:,:)), &
                   & minval(landmask(:,:)*loc_CO2(:,:)), &
                   & sum(loc_weather_ratio_CaCO3(:,:)*landmask(:,:))/nlandcells, &
                   & sum(loc_weather_ratio_CaSiO3(:,:)*landmask(:,:))/nlandcells, &
                   & sum(weather_fCaCO3_2D(:,:)),sum(weather_fCaSiO3_2D(:,:)), &
                   & sum(loc_force_flux_weather_a_land(ia_PCO2,:,:)), &
                   & sum(loc_force_flux_weather_o_land(io_ALK,:,:)),sum(loc_force_flux_weather_o_land(io_DIC,:,:)), &
                   & sum(loc_force_flux_weather_o_land(io_Ca,:,:)),sum(loc_force_flux_weather_o_land(io_DIC_13C,:,:)), &
                   & sum(loc_force_flux_weather_o_land(io_ALK,:,:)),sum(loc_force_flux_weather_o_land(io_DIC,:,:)), &
                   & sum(loc_force_flux_weather_o_land(io_Ca,:,:)),sum(loc_force_flux_weather_o_land(io_DIC_13C,:,:)), &
                   & sum(loc_force_flux_weather_o_ocean(io_ALK,:,:)),sum(loc_force_flux_weather_o_ocean(io_DIC,:,:)), &
                   & sum(loc_force_flux_weather_o_ocean(io_Ca,:,:)),sum(loc_force_flux_weather_o_ocean(io_DIC_13C,:,:))/)

          call sub_output_0d(n_outputs,(/3,6,12,14,21,25/),outputs,output_descriptions,time_series_names)
       
       ENDIF

       IF (tstep_count.eq.output_tsteps_2d(output_counter_2d)) THEN

          IF (opt_2d_netcdf_output) THEN
             call rokgem_netcdf(dum_sfcatm1,dum_runoff,dum_photo,dum_respveg,loc_P,&
                             & loc_force_flux_weather_a_land,loc_force_flux_weather_o_land,loc_force_flux_weather_o_ocean)
          ENDIF

          IF (opt_2d_ascii_output) THEN
             DO k=1,n_ocn
                IF ((k.EQ.io_ALK).OR.(k.EQ.io_DIC).OR.(k.EQ.io_Ca).OR.(k.EQ.io_DIC_13C).OR.(k.EQ.io_DIC_14C)) THEN
                   CALL sub_save_data_ij(TRIM(par_outdir_name)//'spatial_land_'//TRIM(globtracer_names(k))// &
                        & '_year_'//TRIM(year_text)//'.dat', &
                                  & n_i,n_j,loc_force_flux_weather_o_land(k,:,:))                                 
                   CALL sub_save_data_ij(TRIM(par_outdir_name)//'spatial_ocean_'//TRIM(globtracer_names(k))// &
                        & '_year_'//TRIM(year_text)//'.dat', &
                                  & n_i,n_j,loc_force_flux_weather_o_ocean(k,:,:))                                   
                ENDIF
             END DO
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'temperature_year_'//TRIM(year_text)//'.dat', &
                                  & n_i,n_j,dum_sfcatm1(ia_T,:,:))
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'runoff_year_'//TRIM(year_text)//'.dat',n_i,n_j,dum_runoff(:,:)) 
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'productivity_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_P(:,:))    
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'CO2_year_'//TRIM(year_text)//'.dat',n_i,n_j,loc_CO2(:,:))
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'loc_weather_ratio_CaSiO3_year_'//TRIM(year_text)//'.dat', &
                                  & n_i,n_j,loc_weather_ratio_CaSiO3(:,:))                                         
             CALL sub_save_data_ij(TRIM(par_outdir_name)//'loc_weather_ratio_CaCO3_year_'//TRIM(year_text)//'.dat', &
                                  & n_i,n_j,loc_weather_ratio_CaCO3(:,:))       
          ENDIF      
                            
       ENDIF


       END SUBROUTINE sub_2D_weath       

!========================================================================================!

END MODULE rokgem_box
