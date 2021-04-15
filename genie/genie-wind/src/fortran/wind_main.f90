
!
! Module wind_main
! ================
!
!   Subroutine initialise_wind
!   ==========================
!
!     - Import grid definition (typically provided by an atmospheric
!       component, e.g. from the EMBM)
!
!     - Export zonal wind components on u grid and meridional wind
!       component on v grid
!
!     - Export both zonal and meridional wind stress components on u
!       and v grids
!

MODULE wind_main

  USE genie_control

  implicit none

  CHARACTER(len=256) :: indir_path
  NAMELIST/WIND_CONTROL/indir_path
  CHARACTER(len=256) :: outdir_path
  NAMELIST/WIND_CONTROL/outdir_path
  CHARACTER(len=128) :: wind_speed_dataset_file
  NAMELIST/WIND_CONTROL/wind_speed_dataset_file
  CHARACTER(len=32) :: wind_speed_dataset_zonal_var
  NAMELIST/WIND_CONTROL/wind_speed_dataset_zonal_var
  CHARACTER(len=32) :: wind_speed_dataset_meridional_var
  NAMELIST/WIND_CONTROL/wind_speed_dataset_meridional_var
  REAL :: wind_speed_dataset_missing
  NAMELIST/WIND_CONTROL/wind_speed_dataset_missing
  REAL :: wind_speed_dataset_scaling
  NAMELIST/WIND_CONTROL/wind_speed_dataset_scaling
  CHARACTER(len=128) :: wind_stress_dataset_file
  NAMELIST/WIND_CONTROL/wind_stress_dataset_file
  CHARACTER(len=32) :: wind_stress_dataset_zonal_var
  NAMELIST/WIND_CONTROL/wind_stress_dataset_zonal_var
  CHARACTER(len=32) :: wind_stress_dataset_meridional_var
  NAMELIST/WIND_CONTROL/wind_stress_dataset_meridional_var
  REAL :: wind_stress_dataset_missing
  NAMELIST/WIND_CONTROL/wind_stress_dataset_missing
  REAL :: wind_stress_dataset_scaling
  NAMELIST/WIND_CONTROL/wind_stress_dataset_scaling
  LOGICAL :: output_wind_forcing
  NAMELIST/WIND_CONTROL/output_wind_forcing

CONTAINS
  
  SUBROUTINE load_namelist()

    USE genie_util, ONLY: message,die

    integer :: io_status                                                        !< I/O status
    integer,parameter :: io_unit=12                                             !< I/O unit
    character(len=BUFSIZ) :: output_string                                      !< Buffer for output string

    ! Read namelist from file data_wind
    open(unit=io_unit,file='data_wind',status='old',action='read',iostat=io_status)
    if (io_status.ne.0) then
       call die('Could not open namelist file for wind module',__LINE__,__FILE__)
       stop
    end if
    read(unit=io_unit,nml=WIND_CONTROL,iostat=io_status)
    if (io_status.ne.0) then
       call die('Could not read namelist file for wind module',__LINE__,__FILE__)
       stop
    endif
    close(unit=io_unit)
    ! Output namelist parameter-value pairs
    write(output_string,*) 'Path to input dataset directory: ',trim(indir_path)
    call message(trim(output_string),1)    
    call message("Wind speed dataset:",1)
    write(output_string,*) '   File name:                     ',trim(wind_speed_dataset_file)
    call message(trim(output_string),1)    
    write(output_string,*) '   Var. name of zonal component:  ',trim(wind_speed_dataset_zonal_var)
    call message(trim(output_string),1)    
    write(output_string,*) '   Var. name of merid. component: ',trim(wind_speed_dataset_meridional_var)
    call message(trim(output_string),1)    
    write(output_string,*) '   Missing value:                 ',wind_speed_dataset_missing
    call message(trim(output_string),1)    
    write(output_string,*) '   Scaling factor:                ',wind_speed_dataset_scaling
    call message(trim(output_string),1)    
    call message("Wind stress dataset:",1)
    write(output_string,*) '   File name:                     ',trim(wind_stress_dataset_file)
    call message(trim(output_string),1)    
    write(output_string,*) '   Var. name of zonal component:  ',trim(wind_stress_dataset_zonal_var)
    call message(trim(output_string),1)    
    write(output_string,*) '   Var. name of merid. component: ',trim(wind_stress_dataset_meridional_var)
    call message(trim(output_string),1)    
    write(output_string,*) '   Missing value:                 ',wind_stress_dataset_missing
    call message(trim(output_string),1)    
    write(output_string,*) '   Scaling factor:                ',wind_stress_dataset_scaling
    call message(trim(output_string),1)    
    write(output_string,*) '   Output wind forcing fields:    ',output_wind_forcing
    call message(trim(output_string),1)    

  END SUBROUTINE load_namelist

  SUBROUTINE interpolate_wind_speed( &
       & alon,                       & !< longitudinal axis
       & alat,                       & !< latitudinal axis
       & component,                  & !< component (1: zonal component, 2: meridional component)
       & field                       & !< interpolated field
       )

    use local_netcdf
    USE genie_util, ONLY: die

    REAL,INTENT(in),DIMENSION(:)    :: alon
    REAL,INTENT(in),DIMENSION(:)    :: alat
    INTEGER,INTENT(in)              :: component
    REAL,INTENT(out),DIMENSION(:,:) :: field

    CHARACTER(len=BUFSIZ)           :: filename
    REAL                            :: data_tmp
    INTEGER                         :: i,j
    INTEGER                         :: ncid,ncstatus,status

    TYPE(real2dVar),DIMENSION(1)    :: dataset_field
    TYPE(real1dVar),DIMENSION(2)    :: dataset_axis
    INTEGER                         :: nx_dataset,ny_dataset

    ! Read in dataset from NetCDF file
    filename=trim(indir_path(1:len(indir_path)))//'/'//trim(wind_speed_dataset_file(1:len(wind_speed_dataset_file)))
    call openNetCDFRead(trim(filename),ncid)
    if (component.eq.1) then
       dataset_field(1)%name=wind_speed_dataset_zonal_var
    else
       dataset_field(1)%name=wind_speed_dataset_meridional_var
    endif
    call lookupVars(ncid,dataset_field)
    ! Size of observational dataset
    nx_dataset=dataset_field(1)%dimLens(1)
    ny_dataset=dataset_field(1)%dimLens(2)
    allocate(dataset_field(1)%data(nx_dataset,ny_dataset),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    ncstatus=nf90_get_var(ncid,dataset_field(1)%id,dataset_field(1)%data(1:nx_dataset,1:ny_dataset))
    if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
    dataset_axis(1)%name=dataset_field(1)%dimnames(1)
    dataset_axis(2)%name=dataset_field(1)%dimnames(2)
    call lookupVars(ncid,dataset_axis)
    allocate(dataset_axis(1)%data(nx_dataset),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    allocate(dataset_axis(2)%data(ny_dataset),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    ncstatus=nf90_get_var(ncid,dataset_axis(1)%id,dataset_axis(1)%data(1:nx_dataset))
    if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
    do j=1,ny_dataset
       do i=1,nx_dataset
          if (abs((dataset_field(1)%data(i,j)-wind_speed_dataset_missing)/wind_speed_dataset_missing).lt.1e-5) then
             dataset_field(1)%data(i,j)=9.99999e19
          endif
          dataset_field(1)%data(i,j)=wind_speed_dataset_scaling*dataset_field(1)%data(i,j)
       enddo
    enddo
    ncstatus=nf90_get_var(ncid,dataset_axis(2)%id,dataset_axis(2)%data(1:ny_dataset))
    if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
    ! Flip latitudinal axis if required; test monotonicity of axes of dataset
    if (dataset_axis(2)%data(ny_dataset).lt.dataset_axis(2)%data(1)) then
       do j=1,int(ny_dataset/2+0.5)
          data_tmp=dataset_axis(2)%data(j)
          dataset_axis(2)%data(j)=dataset_axis(2)%data(ny_dataset+1-j)
          dataset_axis(2)%data(ny_dataset+1-j)=data_tmp
          do i=1,nx_dataset
             data_tmp=dataset_field(1)%data(i,j)
             dataset_field(1)%data(i,j)=dataset_field(1)%data(i,ny_dataset+1-j)
             dataset_field(1)%data(i,ny_dataset+1-j)=data_tmp
          enddo
       enddo
    endif
    do i=2,nx_dataset
       if (dataset_axis(1)%data(i).le.dataset_axis(1)%data(i-1)) then
          call die("Non-incremental longitudinal axis")
       endif
    enddo
    do j=2,ny_dataset
       if (dataset_axis(2)%data(j).le.dataset_axis(2)%data(j-1)) then
          call die("Non-incremental latitudinal axis")
       endif
    enddo

    ! interpolate
    call interpolate_field(       &
         & dataset_axis(1)%data,  &
         & dataset_axis(2)%data,  &
         & alon,                  &
         & alat,                  &
         & dataset_field(1)%data, &
         & field                  &
         & )

    ! clean up
    deallocate(dataset_axis(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate memory")
    deallocate(dataset_axis(2)%data,stat=status)
    if (status /= 0) call die("Could not deallocate memory")
    deallocate(dataset_field(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate memory")

    call closeNetCDF(ncid)

  END SUBROUTINE interpolate_wind_speed

  SUBROUTINE interpolate_wind_stress( &
       & alon,                       & !< longitudinal axis
       & alat,                       & !< latitudinal axis
       & component,                  & !< component (1: zonal component, 2: meridional component)
       & field                       & !< interpolated field
       )

    use local_netcdf
    USE genie_util, ONLY: die

    REAL,INTENT(in),DIMENSION(:)    :: alon
    REAL,INTENT(in),DIMENSION(:)    :: alat
    INTEGER,INTENT(in)              :: component
    REAL,INTENT(out),DIMENSION(:,:) :: field

    CHARACTER(len=BUFSIZ)           :: filename
    REAL                            :: data_tmp
    INTEGER                         :: i,j
    INTEGER                         :: ncid,ncstatus,status

    TYPE(real2dVar),DIMENSION(1)    :: dataset_field
    TYPE(real1dVar),DIMENSION(2)    :: dataset_axis
    INTEGER                         :: nx_dataset,ny_dataset

    ! Read in dataset from NetCDF file
    filename=trim(indir_path(1:len(indir_path)))//'/'//trim(wind_stress_dataset_file(1:len(wind_stress_dataset_file)))
    call openNetCDFRead(trim(filename),ncid)
    if (component.eq.1) then
       dataset_field(1)%name=wind_stress_dataset_zonal_var
    else
       dataset_field(1)%name=wind_stress_dataset_meridional_var
    endif
    call lookupVars(ncid,dataset_field)
    ! Size of observational dataset
    nx_dataset=dataset_field(1)%dimLens(1)
    ny_dataset=dataset_field(1)%dimLens(2)
    allocate(dataset_field(1)%data(nx_dataset,ny_dataset),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    ncstatus=nf90_get_var(ncid,dataset_field(1)%id,dataset_field(1)%data(1:nx_dataset,1:ny_dataset))
    if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
    dataset_axis(1)%name=dataset_field(1)%dimnames(1)
    dataset_axis(2)%name=dataset_field(1)%dimnames(2)
    call lookupVars(ncid,dataset_axis)
    allocate(dataset_axis(1)%data(nx_dataset),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    allocate(dataset_axis(2)%data(1:ny_dataset),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    ncstatus=nf90_get_var(ncid,dataset_axis(1)%id,dataset_axis(1)%data(1:nx_dataset))
    if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
    do j=1,ny_dataset
       do i=1,nx_dataset
          if (abs((dataset_field(1)%data(i,j)-wind_stress_dataset_missing)/wind_stress_dataset_missing).lt.1e-5) then
             dataset_field(1)%data(i,j)=9.99999e19
          endif
          dataset_field(1)%data(i,j)=wind_stress_dataset_scaling*dataset_field(1)%data(i,j)
       enddo
    enddo
    ncstatus=nf90_get_var(ncid,dataset_axis(2)%id,dataset_axis(2)%data(1:ny_dataset))
    if (ncstatus /= NF90_NOERR) call handle_nc_err(ncstatus)
    ! Flip latitudinal axis if required; test monotonicity of axes of dataset
    if (dataset_axis(2)%data(ny_dataset).lt.dataset_axis(2)%data(1)) then
       do j=1,int(ny_dataset/2+0.5)
          data_tmp=dataset_axis(2)%data(j)
          dataset_axis(2)%data(j)=dataset_axis(2)%data(ny_dataset+1-j)
          dataset_axis(2)%data(ny_dataset+1-j)=data_tmp
          do i=1,nx_dataset
             data_tmp=dataset_field(1)%data(i,j)
             dataset_field(1)%data(i,j)=dataset_field(1)%data(i,ny_dataset+1-j)
             dataset_field(1)%data(i,ny_dataset+1-j)=data_tmp
          enddo
       enddo
    endif
    do i=2,nx_dataset
       if (dataset_axis(1)%data(i).le.dataset_axis(1)%data(i-1)) then
          call die("Non-incremental longitudinal axis")
       endif
    enddo
    do j=2,ny_dataset
       if (dataset_axis(2)%data(j).le.dataset_axis(2)%data(j-1)) then
          call die("Non-incremental latitudinal axis")
       endif
    enddo

    ! interpolate
    call interpolate_field(       &
         & dataset_axis(1)%data,  &
         & dataset_axis(2)%data,  &
         & alon,                  &
         & alat,                  &
         & dataset_field(1)%data, &
         & field                  &
         & )

    ! clean up
    deallocate(dataset_axis(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate memory")
    deallocate(dataset_axis(2)%data,stat=status)
    if (status /= 0) call die("Could not deallocate memory")
    deallocate(dataset_field(1)%data,stat=status)
    if (status /= 0) call die("Could not deallocate memory")

    call closeNetCDF(ncid)

  END SUBROUTINE interpolate_wind_stress

  SUBROUTINE interpolate_field( &
       & in_lon_in,             & !< longitudinal axis of original field
       & in_lat,                & !< latitudinal axis of original field
       & out_lon_in,            & !< longitudinal axis of field to be interpolated
       & out_lat,               & !< latitudinal axis of field to be interpolated
       & in_field_in,           & !< original field
       & out_field              & !< interpolated field
       )

    USE genie_util, ONLY: die,message

    REAL,INTENT(in),DIMENSION(:)    :: in_lon_in
    REAL,INTENT(in),DIMENSION(:)    :: in_lat
    REAL,INTENT(in),DIMENSION(:)    :: out_lon_in
    REAL,INTENT(in),DIMENSION(:)    :: out_lat
    REAL,INTENT(in),DIMENSION(:,:)  :: in_field_in
    REAL,INTENT(out),DIMENSION(:,:) :: out_field

    REAL,ALLOCATABLE,DIMENSION(:)   :: in_lon
    REAL,ALLOCATABLE,DIMENSION(:,:) :: in_field

    INTEGER                         :: status
    REAL,ALLOCATABLE,DIMENSION(:)   :: out_lon
    INTEGER                         :: i,j,in_i,in_j
    INTEGER                         :: in_nx,in_ny,out_nx,out_ny
    REAL                            :: rlon,rlat,testmask,tmp

    REAL                            :: pi

    pi = atan(1.0)*4.0

!     prepare auxiliary arrays:
!     first index    function
!        1            sin()
!        2            cos()
    in_nx=size(in_lon_in)
    in_ny=size(in_lat)
    allocate(in_lon(0:in_nx),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    allocate(in_field(0:in_nx,in_ny),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    ! Note, the zeroth longitude index represents the same values as the
    ! last value (the actual coordinate is offset by 360 degrees) to
    ! facilitate dealing with periodicity of the longitude
    do i=1,in_nx
       in_lon(i)=in_lon_in(i)
    enddo
    in_lon(0)=in_lon_in(in_nx)-360.
    do j=1,in_ny
       do i=1,in_nx
          in_field(i,j)=in_field_in(i,j)
       enddo
       in_field(0,j)=in_field_in(in_nx,j)
    enddo
    out_nx=size(out_lon_in)
    out_ny=size(out_lat)
    allocate(out_lon(out_nx),stat=status)
    if (status /= 0) call die("Could not allocate memory")
    do i=1,out_nx
       out_lon(i)=out_lon_in(i)
    enddo
    do i=1,out_nx
       do while (out_lon(i).le.in_lon(0))
          out_lon(i)=out_lon(i)+360.
       enddo
       do while (out_lon(i).gt.in_lon(in_nx))
          out_lon(i)=out_lon(i)-360.
       enddo
    enddo
    !     bi-linear interpolation (except for values at North and
    !     South Pole, where the average of nearest row is taken),
    !     parts of this code is based on the interpolation routine
    !     'genie-cgoldstein/laz2siz.f' (modified from tri-linear to
    !     bi-linear interpolations), the "extrapolation" part has been
    !     replaced by a horizontal search for the nearest valid point
    !     on the sphere.
    do j=1,out_ny
       ! Special case for North Pole and South Pole, average neareast row of values
       if ((abs(out_lat(j)-90.0).lt.1.0e-5).and.(in_i.gt.1).and.(in_i.le.in_nx)) then
          tmp=0.0
          do i=1,in_nx
             if (in_field(i,in_ny).gt.1.0e10) then
                call die("Computation of value at North Pole failed due to missing value in dataset")
             endif
             tmp=tmp+in_field(i,in_ny) 
          enddo
          tmp=tmp/in_nx
          do i=1,out_nx
             out_field(i,j)=tmp
          enddo
          call message("Value at North Pole averaged from neareast row of dataset points",2)
       elseif ((abs(out_lat(j)+90.0).lt.1.0e-5).and.(in_i.gt.1).and.(in_i.le.in_nx)) then
          tmp=0.0
          do i=1,in_nx
             if (in_field(i,1).gt.1.0e10) then
                call die("Computation of value at South Pole failed due to missing value in dataset")
             endif
             tmp=tmp+in_field(i,1)
          enddo
          tmp=tmp/in_nx
          do i=1,out_nx
             out_field(i,j)=tmp
          enddo
          call message("Value at South Pole averaged from neareast row of dataset points",2)
       else
          do i=1,out_nx
             !     find location of model grid point on observation-based
             !     grid.
             in_i=0
             do while ((in_lon(in_i).lt.out_lon(i)).and.(in_i.le.in_nx))
                in_i=in_i+1
             enddo
             !     This could possibly be done more general without the restriction that
             !     any model point has to be inside the extremes of the latitude
             !     coordinates of the observation-based grid
             in_j=1
             do while ((in_lat(in_j).lt.out_lat(j)).and.(in_j.le.in_ny))
                in_j=in_j+1
             enddo
             if ((in_i.eq.0).or. (in_i.gt.in_nx).or.(in_j.eq.1).or.(in_j.gt.in_ny)) then
                call die("Coordinates outside of the boundaries set by observational dataset")
             endif
             rlon = (out_lon(i)-in_lon(in_i-1))/(in_lon(in_i)-in_lon(in_i-1))
             rlat = (out_lat(j)-in_lat(in_j-1))/(in_lat(in_j)-in_lat(in_j-1))
             testmask = max(in_field(in_i,in_j), &
                  & in_field(in_i-1,in_j),       &
                  & in_field(in_i,in_j-1),       &
                  & in_field(in_i-1,in_j-1))
             !     interpolate if no land at corners of rectangle encompassing the model grid
             !     location
             if (testmask.lt.1.e10) then
                out_field(i,j) = (1.0-rlon)*((1.0-rlat)*in_field(in_i-1,in_j-1)+rlat*in_field(in_i-1,in_j)) + &
                     & rlon*((1.0-rlat)*in_field(in_i,in_j-1)+rlat*in_field(in_i,in_j))
             else
                call die("Interpolation failed due to missing value in dataset")
             endif
          enddo
       endif
    enddo
    ! clean up
    deallocate(out_lon,stat=status)
    if (status /= 0) call die("Could not allocate memory")
    deallocate(in_lon,stat=status)
    if (status /= 0) call die("Could not allocate memory")
    deallocate(in_field,stat=status)
    if (status /= 0) call die("Could not allocate memory")

  END SUBROUTINE interpolate_field

  SUBROUTINE output_forcing_fields( &
     & alon1,                 & !< Longitudinal axis
     & aboxedge1_lon,         & !< Edges for longitudinal axis
     & alat1,                 & !< Latitudinal axis
     & aboxedge1_lat,         & !< Edges for latitudinal axis
     & alon2,                 & !< Longitudinal axis for u grid
     & aboxedge2_lon,         & !< Edges for longitudinal axis (u grid)
     & alat2,                 & !< Latitudinal axis for u grid
     & aboxedge2_lat,         & !< Edges for latitudinal axis (u grid)
     & alon3,                 & !< Longitudinal axis for v grid
     & aboxedge3_lon,         & !< Edges for longitudinal axis (v grid)
     & alat3,                 & !< Latitudinal axis for v grid
     & aboxedge3_lat,         & !< Edges for latitudinal axis (v grid)
     & windspeedx2,           & !< Zonal component of wind vectors on u grid
     & windspeedy3,           & !< Zonal component of wind vectors on u grid
     & stressx2,              & !< Zonal component of wind stress vectors on u grid
     & stressy2,              & !< Meridional component of wind stress vectors on u grid
     & stressx3,              & !< Zonal component of wind stress vectors on v grid
     & stressy3               & !< Meridional component of wind stress vectors on v grid
     & )

    use local_output

    REAL,INTENT(in),DIMENSION(ilon2_atm) :: alon1
    REAL,INTENT(in),DIMENSION(ilon2_atm+1) :: aboxedge1_lon
    REAL,INTENT(in),DIMENSION(ilat2_atm) :: alat1
    REAL,INTENT(in),DIMENSION(ilat2_atm+1) :: aboxedge1_lat
    REAL,INTENT(in),DIMENSION(ilon2_atm) :: alon2
    REAL,INTENT(in),DIMENSION(ilon2_atm+1) :: aboxedge2_lon
    REAL,INTENT(in),DIMENSION(ilat2_atm) :: alat2
    REAL,INTENT(in),DIMENSION(ilat2_atm+1) :: aboxedge2_lat
    REAL,INTENT(in),DIMENSION(ilon3_atm) :: alon3
    REAL,INTENT(in),DIMENSION(ilon3_atm+1) :: aboxedge3_lon
    REAL,INTENT(in),DIMENSION(ilat3_atm) :: alat3
    REAL,INTENT(in),DIMENSION(ilat3_atm+1) :: aboxedge3_lat
    REAL,INTENT(out),DIMENSION(ilon2_atm,ilat2_atm) :: windspeedx2
    REAL,INTENT(out),DIMENSION(ilon3_atm,ilat3_atm) :: windspeedy3
    REAL,INTENT(out),DIMENSION(ilon2_atm,ilat2_atm) :: stressx2
    REAL,INTENT(out),DIMENSION(ilon2_atm,ilat2_atm) :: stressy2
    REAL,INTENT(out),DIMENSION(ilon3_atm,ilat3_atm) :: stressx3
    REAL,INTENT(out),DIMENSION(ilon3_atm,ilat3_atm) :: stressy3
    INTEGER outdir_path_len

    outdir_path_len = len(trim(outdir_path))
    call resetOutput(outdir_path(1:outdir_path_len)//"/wind_forcing.nc")
    call defineDimension(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","lon", &
     & alon1,aboxedge1_lon(1:ilon1_atm),aboxedge1_lon(2:ilon1_atm+1),"longitude", &
     & "longitude (u grid)","degrees_east")
    call defineDimension(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","lat", &
     & alat1,aboxedge1_lat(1:ilat1_atm),aboxedge1_lat(2:ilat1_atm+1),"latitude", &
     & "latitude (u grid)","degrees_north")
    call defineDimension(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","lon_u", &
     & alon2,aboxedge2_lon(1:ilon2_atm),aboxedge2_lon(2:ilon2_atm+1),"longitude", &
     & "longitude (u grid)","degrees_east")
    call defineDimension(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","lat_u", &
     & alat2,aboxedge2_lat(1:ilat2_atm),aboxedge2_lat(2:ilat2_atm+1),"latitude", &
     & "latitude (u grid)","degrees_north")
    call defineDimension(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","lon_v", &
     & alon3,aboxedge3_lon(1:ilon3_atm),aboxedge3_lon(2:ilon3_atm+1),"longitude", &
     & "longitude (u grid)","degrees_east")
    call defineDimension(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","lat_v", &
     & alat3,aboxedge3_lat(1:ilat3_atm),aboxedge3_lat(2:ilat3_atm+1),"latitude", &
     & "latitude (u grid)","degrees_north")
    call writeVariable(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","uwspeed_u", &
     & windspeedx2,(/"lon_u","lat_u"/),"Zonal wind speed (u grid)","zonal wind speed", &
     & "m/s",-99.9999e19)
    call writeVariable(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","vwspeed_v", &
     & windspeedy3,(/"lon_v","lat_v"/),"Meridional wind speed (v grid)", &
     & "meridional wind speed","m/s",-99.9999e19)
    call writeVariable(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","uwstress_u", &
     & stressx2,(/"lon_u","lat_u"/),"Zonal wind stress (u grid)","zonal wind stress", &
     & "m/s",-99.9999e19)
    call writeVariable(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","vwstress_u", &
     & stressy2,(/"lon_u","lat_u"/),"Meridional wind stress (u grid)", &
     & "meridional wind stress","m/s",-99.9999e19)
    call writeVariable(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","uwstress_v", &
     & stressx3,(/"lon_v","lat_v"/),"Zonal wind stress (v grid)","zonal wind stress", &
     & "m/s",-99.9999e19)
    call writeVariable(outdir_path(1:outdir_path_len)//"/wind_forcing.nc","vwstress_v", &
     & stressy3,(/"lon_v","lat_v"/),"Meridional wind stress (v grid)", &
     & "meridional wind stress","m/s",-99.9999e19)

  END SUBROUTINE output_forcing_fields

END MODULE wind_main
