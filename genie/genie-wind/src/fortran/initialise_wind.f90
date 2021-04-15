SUBROUTINE initialise_wind(   &
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

  USE genie_control
  USE genie_util, ONLY: message
  USE wind_main

  implicit none

  REAL,INTENT(in),DIMENSION(ilon1_atm) :: alon1
  REAL,INTENT(in),DIMENSION(ilon1_atm+1) :: aboxedge1_lon
  REAL,INTENT(in),DIMENSION(ilat1_atm) :: alat1
  REAL,INTENT(in),DIMENSION(ilat1_atm+1) :: aboxedge1_lat
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

  ! ======================================================================
  ! Setting up wind module
  ! ======================================================================

  call message("=======================================================",1)
  call message(" Initialisation of wind module",1)
  call message("=======================================================",1)

  call message("",1)
  call message("Atmospheric grid definition obtained",1)

  call load_namelist()

  call message("",1)
  call message("Interpolate forcing fields",1)

  call interpolate_wind_stress( &
       & alon2,                 &
       & alat2,                 &
       & 1,                     &
       & stressx2               &
       & )
  call interpolate_wind_stress( &
       & alon2,                 &
       & alat2,                 &
       & 2,                     &
       & stressy2               &
       & )
  call interpolate_wind_stress( &
       & alon3,                 &
       & alat3,                 &
       & 1,                     &
       & stressx3               &
       & )
  call interpolate_wind_stress( &
       & alon3,                 &
       & alat3,                 &
       & 2,                     &
       & stressy3               &
       & )
  call interpolate_wind_speed(  &
       & alon2,                 &
       & alat2,                 &
       & 1,                     &
       & windspeedx2            &
       & )
  call interpolate_wind_speed(  &
       & alon3,                 &
       & alat3,                 &
       & 2,                     &
       & windspeedy3            &
       & )
  
  if (output_wind_forcing) then
     call output_forcing_fields(alon1,aboxedge1_lon, &
          & alat1, &
          & aboxedge1_lat, &
          & alon2, &
          & aboxedge2_lon, &
          & alat2, &
          & aboxedge2_lat, &
          & alon3, &
          & aboxedge3_lon, &
          & alat3, &
          & aboxedge3_lat, &
          & windspeedx2, &
          & windspeedy3, &
          & stressx2, &
          & stressy2, &
          & stressx3, &
          & stressy3 &
          & )
  endif

  call message("=======================================================",1)
  call message("Initialisation of wind module complete",1)
  call message("=======================================================",1)

END SUBROUTINE initialise_wind
