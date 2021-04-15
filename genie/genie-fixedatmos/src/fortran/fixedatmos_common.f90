
module fixedatmos_common

  !*FD Contains global variables and arrays for
  !*FD the fixed atmosphere code

#ifdef real8
  integer,parameter :: rk=8
#else
  integer,parameter :: rk=4
#endif

#ifdef ncreal8
  integer,parameter :: rk_libnc1=8
#else
  integer,parameter :: rk_libnc1=4
#endif

  integer :: nx !*FD The number of points in the global domain
                !*FD in the $x$-direction.
  integer :: ny !*FD The number of points in the global domain
                !*FD in the $y$-direction.
     
  real(rk) :: timestep=3600  !*FD Timestep is in seconds. Default is one hour.
  integer :: output_time=24  !*FD Frequency of screen output (timesteps). Default is
                             !*FD once every 24 time-steps.

  real(rk),parameter :: daysperyear=360.0

  real(rk_libnc1),dimension(:,:,:),allocatable :: tstar_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: precip_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: latent_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: sensible_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: netsolar_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: netlong_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: stressx_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: stressy_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: runoff_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: evap_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: conductflux_clim
  real(rk_libnc1),dimension(:,:,:),allocatable :: waterflux_clim
  real(rk_libnc1),dimension(:,:),  allocatable :: orog_clim

!Added for extra output variables
  real(rk_libnc1),dimension(:,:,:),allocatable :: tair2m_clim   !*FD Air temperature at 2m (K)
  real(rk_libnc1),dimension(:,:,:),allocatable :: qair2m_clim   !*FD Air specific humidity at 2m (g/kg)
  real(rk_libnc1),dimension(:,:,:),allocatable :: windu10m_clim !*FD Zonal wind speed at 10m (m/s)
  real(rk_libnc1),dimension(:,:,:),allocatable :: windv10m_clim !*FD Meridional wind speed at 10m (m/s)
  real(rk_libnc1),dimension(:,:,:),allocatable :: pstar_clim    !*FD Surface air pressure (hPa)


! Default flags to determine if we're using these fields
  logical :: tstar_flag    =.true.
  logical :: precip_flag   =.true.
  logical :: latent_flag   =.true.
  logical :: sensible_flag =.true.
  logical :: netsolar_flag =.true.
  logical :: netlong_flag  =.true.
  logical :: stressx_flag  =.true.
  logical :: stressy_flag  =.true.
  logical :: runoff_flag   =.true.
  logical :: evap_flag   =.true.
  logical :: conductflux_flag   =.true.
  logical :: waterflux_flag   =.true.
  logical :: landmask_flag =.true.
  logical :: orog_flag = .true.
  logical :: tair2m_flag   = .false.
  logical :: qair2m_flag   = .false.
  logical :: windu10m_flag = .false.
  logical :: windv10m_flag = .false.
  logical :: pstar_flag    = .false.

  real :: z1_fixedatmos

  integer  :: dt_restart_fixedatmos
     
  logical  :: first=.true.

  integer  :: ioffset

  integer  :: grid_type

  integer  :: iyear
  integer  :: imonth
  real(rk) :: day

  character(200) :: fname_climate, fname_restart_fixedatmos, outputdir_name
  character(200) :: fname_climate_xtra

  logical :: fix_energy_flag =.true.
  logical :: fix_water_flag =.true.
 
  real(rk) :: netsolar_fix,precip_fix

  !### Physical constants ###
  REAL,PARAMETER :: con_kelv = 273.15 ! Zero degC in Kelvin

contains
  
  !! simple error handler
  subroutine handle_err(iret)
    implicit none
    include 'netcdf.inc'
    integer iret
    
    if (iret.ne.0) then
       print*,' error in netcdf ',iret
       PRINT *, NF_STRERROR(iret)
       stop 1
    end if
    
    return
  end subroutine handle_err
  
end module fixedatmos_common
