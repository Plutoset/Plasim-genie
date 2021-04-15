!$Id: land_netcdf.f90 2508 2005-07-15 09:11:06Z cvs-gw $
!#######################################################################################
!#######################################################################################
!
! ini_netcdf_land() - setup_nc_land()
!                   - create_fname_land()
!                   - ININC()
!                   - WRITEDIM()
! write_nc_land()   - WRITEVAR()
!                   - regrid_land2atm()
! end_nc_land()     - CLOSENC()
!#######################################################################################
!#######################################################################################
MODULE land_netcdf

  USE land_const, only : nlon,nlat,land_pts,ntype,npft,kr_nc
  IMPLICIT NONE

  INTEGER,PARAMETER :: nmaxdims = 4                      !Maximum number of dimensions a single variable can have
  INTEGER,PARAMETER :: nfiles   = 1                      !Number of output file types (e.g. monthly, annual, decadal)
  INTEGER,PARAMETER :: nall     = 32                     !Total number of variables
  INTEGER           :: istep0   = 1                      !Step of beginning of meaning period
  INTEGER :: ndim,nvar,ntime                             !Number of defined dimensions, variables, times
  INTEGER,DIMENSION(nall) :: natts,nattsvar,vdims,ndims  !
  INTEGER,DIMENSION(nmaxdims,nall) :: vadims             !Dimensions of a single variable
  INTEGER,DIMENSION(nfiles)        :: nco                !Netcdf file ID
  INTEGER,DIMENSION(nall,nfiles)   :: iddimo, idvaro     !IDs of dimension and variables

  CHARACTER(len=200),DIMENSION(nall,nfiles) :: dimname,varname
  CHARACTER(len=200),DIMENSION(2,nmaxdims,nall) :: attdimname,attvarname

CONTAINS

!#######################################################################################
! Routine works out new filename, opens file
!#######################################################################################
  SUBROUTINE ini_netcdf_land(istep,imode,dir_name)

    USE land_var, only : vlats,vlons
    IMPLICIT NONE

    INTEGER,INTENT(in) :: imode,istep
    INTEGER :: i,ifname1,idname,lnsig
    INTEGER,PARAMETER :: lmax = 10000
    CHARACTER(len=200) :: fname1
    CHARACTER(len=*) :: dir_name
    REAL(KIND=kr_nc) :: xcoord(nlon),ycoord(nlat),tlcoord(ntype),tcoord(lmax)

    ntime=1

    !Setup variable, dimension and attribute names
    CALL setup_nc_land

    idname=LNSIG(dir_name)
    CALL create_filename_land(istep,dir_name(1:idname),fname1)

    !Store length of filename
    ifname1=LNSIG(fname1)
    PRINT*,'LAND %% Writing ',fname1(1:ifname1)

    !Open and initialise single netCDf file
    CALL ininc(fname1(1:ifname1),nmaxdims,ndim,nvar,natts,nattsvar, &
               vdims,vadims,ndims,dimname(:,imode),varname(:,imode),attdimname,attvarname, &
               nco(imode),iddimo(:,imode),idvaro(:,imode))

    !Add data to dimension variables
    !LONGITUDE (tracer, u-point, v-point)
    DO i=1,nlon
      xcoord(i)=REAL(vlons(i),KIND=kr_nc)
    ENDDO
    CALL writedim(nco(imode),iddimo(1,imode),xcoord)

    !LATITUDE (tracer, u-point, v-point)
    DO i=1,nlat
      ycoord(i)=REAL(vlats(i),KIND=kr_nc)
    ENDDO
    CALL writedim(nco(imode),iddimo(2,imode),ycoord)

    !TILE
    DO i=1,ntype
      tlcoord(i)=REAL(i,KIND=kr_nc)
    ENDDO
    CALL writedim(nco(imode),iddimo(3,imode),tlcoord)

    !TIME
    DO i=1,1
      tcoord(i)=REAL(istep,KIND=kr_nc)
    ENDDO
    CALL writedim(nco(imode),iddimo(4,imode),tcoord)

  END SUBROUTINE ini_netcdf_land

!#######################################################################################
  SUBROUTINE setup_nc_land
    IMPLICIT NONE

    INTEGER :: loc_dim

    ndim = 0 
    nvar = 0
!---------------------
! Dimensions
!---------------------
    ndim=ndim+1
    dimname(ndim,1)='longitude'
    ndims(ndim)=nlon
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='longitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degree_east'
!---------------------
    ndim=ndim+1
    dimname(ndim,1)='latitude'
    ndims(ndim)=nlat
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='latitude'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='degree_north'
!---------------------
    ndim=ndim+1
    dimname(ndim,1)='tile'
    ndims(ndim)=ntype
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='tile'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='1'
!---------------------
    ndim=ndim+1
    dimname(ndim,1)='time'
    ndims(ndim)=ntime
    natts(ndim)=2
    attdimname(1,1,ndim)='long_name'
    attdimname(2,1,ndim)='time'
    attdimname(1,2,ndim)='units'
    attdimname(2,2,ndim)='years'
!---------------------
! Variables
!---------------------
    nvar=nvar+1
    varname(nvar,:)='force_swnet'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_net_downward_shortwave_flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='force_lwnet'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_net_downward_longwave_flux '
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='force_tair'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='air_temperature_in_bottom_atmosphere_level'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='K'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='force_qair'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='specific_humidity_in_bottom_atmosphere_level'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='1'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='force_wind'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='wind_speed_in_bottom_atmosphere_level'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='m s-1'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='force_precip'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='precipitation_flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='kg m-2 s-1'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='force_airp'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_air_pressure'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='Pa'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='surface_net_downward_shortwave_flux'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_net_downward_shortwave_flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='surface_net_downward_longwave_flux'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_net_downward_longwave_flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='netrad'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_net_radiation_flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='surface_upward_latent_heat_flux'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_upward_latent_heat_flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='surface_upward_sensible_heat_flux'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_upward_sensible_heat_flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='downward_heat_flux_in_soil'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='downward_heat_flux_in_soil'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='downward_canopy_heat_flux'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='downward_canopy_heat_flux'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='W m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='surface_temperature'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_temperature'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='K'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='soil_temperature'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='soil_temperature'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='K'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='soil_moisture_content'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='soil_moisture_content'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='kg m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='lwe_thickness_of_surface_snow_amount'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='lwe_thickness_of_surface_snow_amount'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='m'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='canopy_conductance'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='canopy_conductance'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='m s-1'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='vegetation_carbon_content'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='vegetation_carbon_content'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='kg m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='soil_carbon_content'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='soil_carbon_content'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='kg m-2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='gridbox_area'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='gridbox_area'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='m2'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='surface_albedo'
    vdims(nvar)=3
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='surface_albedo'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='1'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='tile_area_fraction'
    vdims(nvar)=4
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('tile',dimname,nall)
    vadims(4,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='tile_area_fraction'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='1'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='leaf_area_index'
    vdims(nvar)=4
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('tile',dimname,nall)
    vadims(4,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='leaf_area_index'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='1'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='canopy_height'
    vdims(nvar)=4
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('tile',dimname,nall)
    vadims(4,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='canopy_height'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='m'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='net_primary_productivity_of_carbon'
    vdims(nvar)=4
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('tile',dimname,nall)
    vadims(4,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='net_primary_productivity_of_carbon'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='kg m-2 s-1'
!---------------------
    nvar=nvar+1
    varname(nvar,:)='gross_primary_productivity_of_carbon'
    vdims(nvar)=4
    vadims(1,nvar)=loc_dim('longitude',dimname,nall)
    vadims(2,nvar)=loc_dim('latitude',dimname,nall)
    vadims(3,nvar)=loc_dim('tile',dimname,nall)
    vadims(4,nvar)=loc_dim('time',dimname,nall)
    nattsvar(nvar)=2
    attvarname(1,1,nvar)='long_name'
    attvarname(2,1,nvar)='gross_primary_productivity_of_carbon'
    attvarname(1,2,nvar)='units'
    attvarname(2,2,nvar)='kg m-2 s-1'

    RETURN
  END SUBROUTINE setup_nc_land

!#######################################################################################
! Filename format is 
!   land_mn_y[year]_b[begin-step-of-year]_e[end-step-of-year]
!########################################################################
  SUBROUTINE create_filename_land(istep,dir_name,fname1)

    USE land_const, only : daylen_land,dayinyear_land,nstepyear
    USE land_var, only   : dt_land
    IMPLICIT NONE

    INTEGER,INTENT(in) :: istep
    INTEGER :: iyear, i0, i1
    REAL :: vday
    CHARACTER(len=200),INTENT(out) :: fname1
    CHARACTER(len=*),INTENT(in)    :: dir_name
    CHARACTER(len=5)               :: cyear, cstep0, cstep1

    vday   = REAL(istep0)*dt_land/daylen_land  ! Decimal day since start of simulation
    iyear  = INT(vday/dayinyear_land)          ! Year of simulation
    i0     = istep0 - nstepyear * iyear        ! Start of meaning period step-of-year
    i1     = istep - nstepyear * iyear         ! End of meaning period step-of-year

    WRITE(cstep0,'(i5.5)')i0
    WRITE(cstep1,'(i5.5)')i1
    WRITE(cyear,'(i5.5)')iyear + 1

    fname1 = TRIM(dir_name)//'land_mn_y'//cyear//'_b'//cstep0//'_e'//cstep1//'.nc'

    istep0 = istep + 1

    RETURN
  END SUBROUTINE create_filename_land

!#######################################################################################
  SUBROUTINE write_nc_land(imode)
  
    USE land_diags

    IMPLICIT NONE

    INTEGER,INTENT(in) :: imode            !Output file type (only 1 available at present)
    INTEGER :: f                           !Loop counter
    REAL,DIMENSION(nlon,nlat,1)     :: work    !Local work array
    REAL,DIMENSION(nlon,nlat,ntype) :: work2   !Local work array

    !FORCING VARIABLES
    !solar rad down, longwave rad down, tair, qair, wind speed 
    !precip rate, air pressure
    DO f=1,nforce
      work=REGRID_LAND2ATM(diag_force(:,f),1)
      CALL writevar(nco(imode),idvaro(f,imode),REAL(work,KIND=kr_nc))
    ENDDO

    !OUTPUT VARIABLES
    DO f=1,ndiags
      work=REGRID_LAND2ATM(diag_var(:,f),1)
      CALL writevar(nco(imode),idvaro(f+nforce,imode),REAL(work,KIND=kr_nc))
    ENDDO

    DO f=1,ndiags_tile
      work2=REGRID_LAND2ATM(diag_var_tile(:,:,f),ntype)
      CALL writevar(nco(imode),idvaro(f+nforce+ndiags,imode),REAL(work2,KIND=kr_nc))
    ENDDO

    RETURN
  END SUBROUTINE write_nc_land

!#######################################################################################
  SUBROUTINE end_nc_land(imode)
    IMPLICIT NONE
    INTEGER,INTENT(in) :: imode

    CALL closenc(nco(imode))

    RETURN
  END SUBROUTINE end_nc_land
!#######################################################################################
! Function to regrid fields from land vectors to 2d atmosphere array
!#######################################################################################
  REAL FUNCTION regrid_land2atm(infield,nfields) RESULT(outarr)

    USE land_var, only : ij_land

    IMPLICIT NONE

!    REAL,DIMENSION(nlon,nlat,nfields) :: outarr
    INTEGER,INTENT(in) :: nfields
    DIMENSION :: outarr(nlon,nlat,nfields)
    REAL,INTENT(in),DIMENSION(land_pts,nfields) :: infield

    INTEGER :: i,j,l
    REAL,PARAMETER :: val_missing = -99999.0

    ! Set output field to missing data 
    outarr(:,:,:) = val_missing

    ! Replace output field dry points with values from input field
    DO l = 1,land_pts
      i = ij_land(l,1)
      j = ij_land(l,2)
      outarr(i,j,:) = infield(l,:)
    ENDDO

  END FUNCTION regrid_land2atm

END MODULE land_netcdf


! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='lying_snow'
!       vdims(nvar)=3
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Lying snow'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='mm'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='evaporation'
!       vdims(nvar)=3
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Evaporation rate'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='mm/s'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='sublimation'
!       vdims(nvar)=3
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Sublimation rate'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='mm/s'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='runoff'
!       vdims(nvar)=3
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Runoff rate'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='mm/s'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='snowmelt'
!       vdims(nvar)=3
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Snowmelt rate'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='mm/s'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='soil_carbon'
!       vdims(nvar)=3
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Soil carbon'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='kgC/m2'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='resp_s_dr'
!       vdims(nvar)=3
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Accumulated soil respiration'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='kgC/m2/year'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='albedo'
!       vdims(nvar)=3
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Surface albedo'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='no_units'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='gpp_dr'
!       vdims(nvar)=4
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('tile',dimname,nall)
!       vadims(4,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Accumulated GPP'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='kgC/m2/year'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='npp_dr'
!       vdims(nvar)=4
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('tile',dimname,nall)
!       vadims(4,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Accumulated NPP'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='kgC/m2/year'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='z0'
!       vdims(nvar)=4
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('tile',dimname,nall)
!       vadims(4,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Tile roughness length'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='m'
! !---------------------
!       nvar=nvar+1
!       varname(nvar,:)='tile_surf_temp'
!       vdims(nvar)=4
!       vadims(1,nvar)=loc_dim('longitude',dimname,nall)
!       vadims(2,nvar)=loc_dim('latitude',dimname,nall)
!       vadims(3,nvar)=loc_dim('tile',dimname,nall)
!       vadims(4,nvar)=loc_dim('time',dimname,nall)
!       nattsvar(nvar)=2
!       attvarname(1,1,nvar)='long_name'
!       attvarname(2,1,nvar)='Tile surface temperature'
!       attvarname(1,2,nvar)='units'
!       attvarname(2,2,nvar)='K'
