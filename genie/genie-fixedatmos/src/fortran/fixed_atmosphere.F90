
module fixed_atmosphere

  !*FD Fixed atmosphere module for GENIE, to provide
  !*FD atmospheric variables from monthly mean climatology
  !*FD
  !*FD This version in Fortran 90 by Ian Rutt, after code
  !*FD from the fixed ocean routine adapted by Dan Lunt.

  ! To add a new variable:
  ! IN FIXEDATMOS.F90:
  ! (1) subroutine initialise_fixedatmos(
  ! (2) real(rk),dimension(:,:),intent(out) ::
  ! (3) allocate(
  ! (4) call get3d_data_nc(ncid,
  ! (5) call fixedatmos(
  ! (6) subroutine fixedatmos(
  ! (7) real(rk),dimension(:,:),intent(out) :: 
  ! (8) =(1.0-apfrac)*
  ! IN FIXEDATMOS_COMMON.F90:
  ! (9) real(4),dimension(:,:,:),allocatable :: 
  ! IN GENIE.F:
  ! (10) call initialise_fixedatmos(
  ! (11) call fixedatmos(

  implicit none

contains

  subroutine initialise_fixedatmos(nlfile,unit, &
             alon_atm,alat_atm, &
             alonedge_atm,alatedge_atm, &
             iland, &
             atmos_dt_tim, &
             tstar, &
             precip, & 
             latent, &
             sensible, &
             netsolar, &
             netlong, &
             stressx, &
             stressy, &
             runoff, &
             evap, &
             conductflux, &
             waterflux, &
             qair2m_atm, &
             windu10m_atm, &
             windv10m_atm, &
             pstar_atm, &
             atmos_lowestlh_atm, &
             orog, &
             interpmask,weighttot,grid_type_out)
  
    !*FD Initialises the fixed atmosphere.

    use fixedatmos_common
    use genie_util, ONLY: check_unit, check_iostat

#include "precision.inc"
    include 'netcdf.inc'

    character(*),           intent(in)  :: nlfile !*FD Namelist file to use for
                                                  !*FD the initialisation.
    integer,                intent(in)  :: unit   !*FD Logical fileunit to use.
    real(rk),dimension(:),intent(out) :: alon_atm 
    real(rk),dimension(:),intent(out) :: alat_atm 
    real(rk),dimension(:),intent(out) :: alonedge_atm 
    real(rk),dimension(:),intent(out) :: alatedge_atm
    real(rk),             intent(out) :: atmos_dt_tim !*FD Atmos timestep (s)
    real(rk),dimension(:,:),intent(inout) :: tstar  !*FD The starting temperatures.
    real(rk),dimension(:,:),intent(inout) :: precip !*FD The starting precipitation.
    real(rk),dimension(:,:),intent(inout) :: latent !*FD The starting latent heat.
    real(rk),dimension(:,:),intent(inout) :: sensible !*FD The starting sensible heat.
    real(rk),dimension(:,:),intent(inout) :: netsolar !*FD The starting net solar radiation.
    real(rk),dimension(:,:),intent(inout) :: netlong !*FD The starting net long-wave radiation.
    real(rk),dimension(:,:),intent(inout) :: stressx !*FD The starting u wind stress.
    real(rk),dimension(:,:),intent(inout) :: stressy !*FD The starting v wind stress.
    real(rk),dimension(:,:),intent(inout) :: runoff !*FD The starting runoff.
    real(rk),dimension(:,:),intent(inout) :: evap !*FD The starting runoff.
    real(rk),dimension(:,:),intent(inout) :: conductflux !*FD The startng conduction flux.
    real(rk),dimension(:,:),intent(inout) :: waterflux !*FD The starting water flux.
    real(rk),dimension(:,:),intent(inout) :: orog !*FD The global orography.
    integer,dimension(:,:),intent(inout)  :: iland  !*FD The land sea mask.
    real(rk),dimension(:,:),intent(inout) :: qair2m_atm   !*FD Air specific humidity (kg/kg)
    real(rk),dimension(:,:),intent(inout) :: windu10m_atm !*FD Zonal wind speed (m/s)
    real(rk),dimension(:,:),intent(inout) :: windv10m_atm !*FD Merid wind speed (m/s)
    real(rk),dimension(:,:),intent(inout) :: pstar_atm    !*FD Surface air pressure (Pa)
    real(rk),dimension(:,:),intent(inout) :: atmos_lowestlh_atm   !*FD Height of lowest atmos layer (m)
    real(rk),dimension(:,:),intent(in) :: interpmask !*FD The interpolation mask
    real(rk),intent(in) :: weighttot !*FD The total interpolated area
    integer grid_type_out !*FD The grid type, 1 = igcm, 2=goldstein

    ! Internally used variables

    integer :: ifail,ncid
    real(rk_libutil1),dimension(:),allocatable :: alon
    real(rk_libutil1),dimension(:),allocatable :: alat
    real(rk_libutil1),dimension(:),allocatable :: alonedge
    real(rk_libutil1),dimension(:),allocatable :: alatedge
    real(rk_libutil1),dimension(:,:),allocatable :: land

    integer iday

    integer i,j,ios
    real ax

    ! For embm grid stuff....
    real pi,th0,th1,s0,s1,jmax,ds
    real,dimension(:),allocatable :: s,sv


    ! need to put the stoponeror into the namelist....
    logical stoponerror
    parameter(stoponerror=.true.)

    ! Namelist for reading in parameters

    ! Conductflux and waterflux shpuld really come from fixedseaice...
    namelist/fixedatmos_control/fname_climate,fname_climate_xtra, &
         timestep, &
         output_time, &
         fname_restart_fixedatmos,  &
         outputdir_name, &
         dt_restart_fixedatmos, &
         grid_type, &
         fix_energy_flag, &
         fix_water_flag, &         
         netsolar_fix, &
         landmask_flag, &
         precip_fix, &
         tstar_flag, &
         precip_flag, &
         latent_flag, &
         sensible_flag, &
         netsolar_flag, &
         netlong_flag, &
         stressx_flag, &
         stressy_flag, &
         runoff_flag, &
         evap_flag, &
         orog_flag, &
         tair2m_flag, &
         qair2m_flag, &
         windu10m_flag, &
         windv10m_flag, &
         pstar_flag, &
         conductflux_flag, &
         waterflux_flag, &
         z1_fixedatmos

    print*,"Initialising fxatm"

    ! Check we've not already initialised

    if (.not.first) then
       print*,'Attempt to initialise already-initialised fixed atmosphere'
       stop
    endif

    ! Set up variables and allocate arrays appropriately

    nx=size(tstar,1) ; ny=size(tstar,2)

    allocate(land(nx,ny),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(tstar_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(precip_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(latent_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(sensible_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(netsolar_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(netlong_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(stressx_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(stressy_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(runoff_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(evap_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(conductflux_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(waterflux_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(orog_clim(nx,ny),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    allocate(tair2m_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(qair2m_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(windu10m_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(windv10m_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(pstar_clim(nx,ny,12),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    allocate(alon(nx),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(alat(ny),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(alonedge(nx+1),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(alatedge(ny+1),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    allocate(s(0:nx),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    allocate(sv(0:ny),stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)


    ! Set up blank filenames

    fname_climate='xxx'
    fname_climate_xtra='xxx'
    fname_restart_fixedatmos='xxx' 
    
    ! Open, read and close the namelist file
    call check_unit(unit,__LINE__,__FILE__)
    open(unit,file=nlfile,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    read(unit,fixedatmos_control,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    close(unit,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

    !Output atmos timestep for other GENIE components
    atmos_dt_tim = timestep

    ! Check to see if we have required info

    call fxatm_checkfile(fname_climate,'climate')
    call fxatm_checkfile(fname_climate_xtra,'xtraclimate')
    call fxatm_checkfile(fname_restart_fixedatmos,'restart')

    grid_type_out=grid_type

    ! **************************************
    ! Set up the latitudes and longitudes!!!

      if (grid_type.eq.1) then

    ! This is the igcm grid.....
    ! This is copied from genie-igcm3/initialise_atmos.F 
        ax=360.0/real(nx)
        do i=1,nx
          alon(i)=real((i-1.0)*ax,kind(alon))
          alonedge(i)=real((i-1.5)*ax,kind(alonedge))
        end do
        alonedge(nx+1)=real((nx-0.5)*ax,kind(alonedge))
        call gwtcnr(alat,ny/2)
        call gwtbox(alatedge,ny/2)

      endif

      if (grid_type.eq.2) then

    ! This is the embm grid.....
    ! This is copied from genie-embm/initialise_embm.F
      ax=360.0/real(nx)
      do i=1,nx
         alon(i)=real((i-0.5)*ax-260.0,kind(alon))
         alonedge(i)=real((i-1.0)*ax-260.0,kind(alonedge))
      end do
      alonedge(nx+1)=real(nx*ax-260.0,kind(alonedge))
      jmax=nx
      pi=4.0*atan(1.0)
      th0=-pi/2.0
      th1=pi/2 
      s0=sin(th0)
      s1=sin(th1)   
      ds=(s1-s0)/jmax
      do j=0,int(jmax)
        sv(j)=s0+j*ds
        s(j)=sv(j)-0.5*ds
      enddo
      do j=1,int(jmax)
         alat(j)=real(asin(s(j))*180.0/pi,kind(alat))
      end do
      do j=1,int(jmax+1)
         alatedge(j)=real(asin(sv(j-1))*180.0/pi,kind(alatedge))
      end do

      endif

    ! Let's write out the grid:

      print*,'Box center longitudes:'
      print*,alon
      print*
      print*,'Box center latitudes:'
      print*,alat
      print*
      print*,'Box edge longitudes:'
      print*,alonedge
      print*
      print*,'Box edge latitudes:'
      print*,alatedge
      print*

      alon_atm(:)=alon(:)
      alonedge_atm(:)=alonedge(:)
      alat_atm(:)=alat(:)
      alatedge_atm(:)=alatedge(:)

    ! **************************************


    ! Get starting data
    
    first=.false.
 
    print*,' Fixedatmos: Opening climatology file ',trim(fname_climate)
    call open_file_nc(trim(fname_climate),ncid)

    if (landmask_flag) then
      call get2d_data_nc(ncid,'lsm',nx,ny,land,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading lsm'
    endif

    if (tstar_flag) then
      call get3d_data_nc(ncid,'airs', nx,ny,12,tstar_clim, ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading airs'
    endif

    if (precip_flag) then
      call get3d_data_nc(ncid,'prate',nx,ny,12,precip_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading prate'
    endif

    if (latent_flag) then
      call get3d_data_nc(ncid,'latent',nx,ny,12,latent_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading latent'
    endif

    if (sensible_flag) then
      call get3d_data_nc(ncid,'sensible',nx,ny,12,sensible_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading sensible'
    endif

    if (netsolar_flag) then
      call get3d_data_nc(ncid,'netsolar',nx,ny,12,netsolar_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading netsolar'
    endif
 
    if (netlong_flag) then
      call get3d_data_nc(ncid,'netlong',nx,ny,12,netlong_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading netlong'
    endif

    if (stressx_flag) then
      call get3d_data_nc(ncid,'stressx',nx,ny,12,stressx_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading stressx'
    endif

    if (stressy_flag) then
      call get3d_data_nc(ncid,'stressy',nx,ny,12,stressy_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading stressy'
    endif

    if (runoff_flag) then
      call get3d_data_nc(ncid,'runoff',nx,ny,12,runoff_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading runoff'
    endif

    if (evap_flag) then
      call get3d_data_nc(ncid,'evap',nx,ny,12,evap_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading evap'
    endif

    if (conductflux_flag) then
      call get3d_data_nc(ncid,'conductflux',nx,ny,12,conductflux_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading conductflux'
    endif

    if (waterflux_flag) then
      call get3d_data_nc(ncid,'waterflux',nx,ny,12,waterflux_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading waterflux'
    endif

    if (orog_flag) then
      call get2d_data_nc(ncid,'orog',nx,ny,orog_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading orog'
    endif

    call close_file_nc(trim(fname_climate),ncid)

    !##################################################
    ! Read extra bits of atmos near-surface climate
    !##################################################
    print*,' Fixedatmos: Opening climatology file ',trim(fname_climate_xtra)
    call open_file_nc(trim(fname_climate_xtra),ncid)
    if (tair2m_flag) then
      call get3d_data_nc(ncid,'tair2m',nx,ny,12,tair2m_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading tair'
    endif
    if (qair2m_flag) then
      call get3d_data_nc(ncid,'qair2m',nx,ny,12,qair2m_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading qair'
    endif
    if (windu10m_flag) then
      call get3d_data_nc(ncid,'uwnd10m',nx,ny,12,windu10m_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading windu'
    endif
    if (windv10m_flag) then
      call get3d_data_nc(ncid,'vwnd10m',nx,ny,12,windv10m_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading windv'
    endif
    if (pstar_flag) then
      call get3d_data_nc(ncid,'pres',nx,ny,12,pstar_clim,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading pres'
    endif

    call close_file_nc(trim(fname_climate_xtra),ncid)

    ! READ IN THE RESTART FILE
    ! it's actually just the day number offset from 1st January

    print*,' fixedatmos: Opening restart file for read: ',trim(fname_restart_fixedatmos)
    call open_file_nc(trim(fname_restart_fixedatmos),ncid)
    call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
    call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
    call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
    call get1di_data_nc(ncid,'iday',1,iday,ifail)
    call close_file_nc(trim(fname_restart_fixedatmos),ncid)
    
    day=iday
    ioffset=mod(ioffset,360)

    if (landmask_flag) then
      iland(:,:)=int(land(:,:))
    endif

    call fixedatmos(0,tstar,precip,latent,sensible,netsolar,netlong, &
                      stressx,stressy,runoff,evap,conductflux,waterflux, &
                      qair2m_atm,windu10m_atm,windv10m_atm,pstar_atm, &
                      atmos_lowestlh_atm, &
                      interpmask,weighttot,orog)

    ! cleanup locally alloated memory
    if (allocated(land)) deallocate(land,stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (allocated(alon)) deallocate(alon,stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (allocated(alat)) deallocate(alat,stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (allocated(alonedge)) deallocate(alonedge,stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (allocated(alatedge)) deallocate(alatedge,stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (allocated(s)) deallocate(s,stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    if (allocated(sv)) deallocate(sv,stat=ios)
    call check_iostat(ios,__LINE__,__FILE__)

  end subroutine initialise_fixedatmos

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine fixedatmos(istep,tstar,precip,latent,sensible,netsolar,netlong, &
                      stressx,stressy,runoff,evap,conductflux,waterflux, &
                      qair2m_atm,windu10m_atm,windv10m_atm,pstar_atm, &
                      atmos_lowestlh_atm, &
                      interpmask,weighttot,orog)

    !*FD Do a fixed atmosphere time-step.

    use fixedatmos_common

#include "precision.inc"
    include 'netcdf.inc'

    integer,                intent(in)  :: istep  !*FD Timestep
    real(rk),dimension(:,:),intent(inout) :: tstar  !*FD Output temperature
    real(rk),dimension(:,:),intent(inout) :: precip !*FD Output precipitation
    real(rk),dimension(:,:),intent(inout) :: latent !*FD Output latent heat
    real(rk),dimension(:,:),intent(inout) :: sensible !*FD Output sensible heat.
    real(rk),dimension(:,:),intent(inout) :: netsolar !*FD Output net solar radiation.
    real(rk),dimension(:,:),intent(inout) :: netlong !*FD Output net long-wave radiation.
    real(rk),dimension(:,:),intent(inout) :: stressx !*FD Output u wind stress.
    real(rk),dimension(:,:),intent(inout) :: stressy !*FD Output v wind stress.
    real(rk),dimension(:,:),intent(inout) :: runoff !*FD Output runoff.
    real(rk),dimension(:,:),intent(inout) :: evap !*FD Output evaporation.
    real(rk),dimension(:,:),intent(inout) :: conductflux !*FD Output conduction flux.
    real(rk),dimension(:,:),intent(inout) :: waterflux !*FD Output water flux.
    real(rk),dimension(:,:),intent(inout) :: orog !*FD Global orography.

    real(rk),dimension(nx,ny)             :: tair2m_atm   !*FD Air temperature at 2m (K)
    real(rk),dimension(:,:),intent(inout) :: qair2m_atm   !*FD Air specific humidity at 2m (kg/kg)
    real(rk),dimension(:,:),intent(inout) :: windu10m_atm !*FD Zonal wind speed at 10m (m/s)
    real(rk),dimension(:,:),intent(inout) :: windv10m_atm !*FD Merid wind speed at 10m (m/s)
    real(rk),dimension(:,:),intent(inout) :: pstar_atm    !*FD Surface air pressure (Pa)
    real(rk),dimension(:,:),intent(inout) :: atmos_lowestlh_atm  !*FD Height of lowest atmos layer (m)

    real(rk),dimension(:,:), intent(in)  :: interpmask  !*FD Mask for interpolation
    real(rk),intent(in) :: weighttot !*FD The total interpolated area

    integer :: iday
    integer :: ncid
    integer :: mpth1,mpth2

!   THE CALNDR ROUTINE IS NOW IN THE LIBUTIL1 DIRECTORY.
    real(rk_libutil1) :: apfrac
    real(rk_libutil1) :: doy

    integer :: j,i

    character :: fname*200
    integer :: status,nrecsid,ioffsetid

    integer :: iyearid, imonthid, idayid   
    character(4) :: yearstring
    character(2) :: monthstring, daystring
    character(7) :: datestring

    iday=nint(day)
    doy=real(mod(real(ioffset+istep*timestep/(24*60*60.), &
         kind(daysperyear)),daysperyear),kind(day))
    if (mod(istep,output_time).eq.0) &
      print*,'Fixed atmos thinks DOY=',doy,' and istep is:',istep
    CALL CALNDR(DOY,Mpth1,ApFRAC)
    MPth2=mpth1+1
    if (mpth2.eq.13) mpth2=1

!     set TSTAR by interpolation from two monthly values
!     Back to grid of genie, and into celsius....
         
    if (tstar_flag)    tstar    = real((1.0-apfrac)*tstar_clim   (:,:,mpth1) + &
         apfrac*tstar_clim   (:,:,mpth2),kind(tstar))
    if (precip_flag)   precip   = real((1.0-apfrac)*precip_clim  (:,:,mpth1) + &
         apfrac*precip_clim  (:,:,mpth2),kind(precip))
    if (latent_flag)   latent   = real((1.0-apfrac)*latent_clim  (:,:,mpth1) + &
         apfrac*latent_clim  (:,:,mpth2),kind(latent))
    if (sensible_flag) sensible = real((1.0-apfrac)*sensible_clim(:,:,mpth1) + &
         apfrac*sensible_clim(:,:,mpth2),kind(sensible))
    if (netsolar_flag) netsolar = real((1.0-apfrac)*netsolar_clim(:,:,mpth1) + &
         apfrac*netsolar_clim(:,:,mpth2),kind(netsolar))
    if (netlong_flag)  netlong  = real((1.0-apfrac)*netlong_clim (:,:,mpth1) + &
         apfrac*netlong_clim (:,:,mpth2),kind(netlong))
    if (stressx_flag)  stressx  = real((1.0-apfrac)*stressx_clim (:,:,mpth1) + &
         apfrac*stressx_clim (:,:,mpth2),kind(stressx))
    if (stressx_flag)  stressy  = real((1.0-apfrac)*stressy_clim (:,:,mpth1) + &
         apfrac*stressy_clim (:,:,mpth2),kind(stressy))
    if (runoff_flag)   runoff   = real((1.0-apfrac)*runoff_clim  (:,:,mpth1) + &
         apfrac*runoff_clim  (:,:,mpth2),kind(runoff))
    if (evap_flag)     evap     = real((1.0-apfrac)*evap_clim    (:,:,mpth1) + &
         apfrac*evap_clim    (:,:,mpth2),kind(evap))
    if (conductflux_flag) conductflux = real((1.0-apfrac)*conductflux_clim(:,:,mpth1) + &
         apfrac*conductflux_clim(:,:,mpth2),kind(conductflux))
    if (waterflux_flag)   waterflux   = real((1.0-apfrac)*waterflux_clim  (:,:,mpth1) + &
         apfrac*waterflux_clim  (:,:,mpth2),kind(waterflux))
    if (orog_flag) orog=orog_clim

    if (tair2m_flag) tair2m_atm(:,:)     = real((1.0-apfrac)*tair2m_clim(:,:,mpth1)   + &
         apfrac*tair2m_clim(:,:,mpth2),kind(tair2m_atm))
    if (qair2m_flag) qair2m_atm(:,:)     = real(((1.0-apfrac)*qair2m_clim(:,:,mpth1)   + &
         apfrac*qair2m_clim(:,:,mpth2))*1.0E-3,kind(qair2m_atm))
    if (windu10m_flag) windu10m_atm(:,:) = real((1.0-apfrac)*windu10m_clim(:,:,mpth1) + &
         apfrac*windu10m_clim(:,:,mpth2),kind(windu10m_atm))
    if (windv10m_flag) windv10m_atm(:,:) = real((1.0-apfrac)*windv10m_clim(:,:,mpth1) + &
         apfrac*windv10m_clim(:,:,mpth2),kind(windv10m_atm))
    if (pstar_flag) pstar_atm(:,:)       = real(((1.0-apfrac)*pstar_clim(:,:,mpth1)    + &
         apfrac*pstar_clim(:,:,mpth2))*1.0E2,kind(pstar_atm))

!     Fix the netsolar and precip fields if required!!!!!
      if (fix_energy_flag) then
         do j=1,ny
          do i=1,nx
             netsolar(i,j)=netsolar(i,j)+netsolar_fix* &
             interpmask(i,j) / (weighttot) 
          enddo
        enddo
      endif

      if (fix_water_flag) then
        do j=1,ny
          do i=1,nx
             precip(i,j)=precip(i,j)+precip_fix* &
             interpmask(i,j) / (weighttot) 
          enddo
        enddo
      endif

! 2m air temperature overwrites lowest level air temperature only 
! if the latter is NOT selected in the config file.
      if (tair2m_flag.and.(.not.tstar_flag)) then
        do j=1,ny
          do i=1,nx
            tstar(i,j) = real(tair2m_atm(i,j) - con_kelv,kind(tstar))
          enddo
        enddo
      endif

! Fixed height of lowest (only) atmos layer
    atmos_lowestlh_atm(:,:) = real(z1_fixedatmos,kind(atmos_lowestlh_atm))

!     See if we need to write a restart....

    if (mod(istep,dt_restart_fixedatmos).eq.0) then

       write(datestring,'(i7.7)') istep
       write(yearstring,'(i4.4)') iyear
       write(monthstring,'(i2.2)') imonth
       write(daystring,'(i2.2)') iday

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------

       fname=trim(outputdir_name)// &
            '/fixedatmos_restart_'// &
            yearstring//'_'//monthstring//'_'//daystring//'.nc'
       print*,' Opening restart file for write: ',trim(fname)
       status=nf_create(trim(fname), nf_clobber, ncid)
       call handle_err(status)
       status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
       call handle_err(status)
       status=nf_def_var(ncid,'ioffset',nf_int,1,nrecsid,ioffsetid)
       call handle_err(status)
       status=nf_def_var(ncid,'iyear',nf_int,1,nrecsid,iyearid)
       call handle_err(status)
       status=nf_def_var(ncid,'imonth',nf_int,1,nrecsid,imonthid)
       call handle_err(status)
       status=nf_def_var(ncid,'iday',nf_int,1,nrecsid,idayid)
       call handle_err(status)
       status=nf_enddef(ncid)
       call handle_err(status)
       status=nf_put_var_int(ncid,ioffsetid,int(doy))
       call handle_err(status)
       status=nf_put_var_int(ncid,iyearid,iyear)
       call handle_err(status)
       status=nf_put_var_int(ncid,imonthid,imonth)
       call handle_err(status)
       status=nf_put_var_int(ncid,idayid,iday)
       call handle_err(status)
       status=nf_close(ncid)
       call handle_err(status)

    endif

    day=real(day+timestep/(24*60*60.),kind(day))
!     This bit so that we don't get too far out in our count....
!     Anchor to a day if we start drifting.
!     Means timestep can never be less than 1/1000 of a day!!!!
    if (abs(iday-day).le.1e-3) then
      day=iday
    endif
    if (day.ge.31) then
       day=day-30
       imonth=imonth+1
       if (imonth.eq.13) then
          imonth=1
          iyear=iyear+1
       endif
    endif
    
    iday=nint(day)

  end subroutine fixedatmos

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine fxatm_checkfile(fname,label)

    character(*) :: fname,label

    integer :: ifail
    logical lexist

    ifail=0
    if (trim(fname).eq.'xxx') then
       print*,' Missing filename for ',trim(label)
       ifail=1
    else
       inquire(file=fname,exist=lexist)
       if (.not.lexist) then
          print*,' Missing file ',trim(fname)
          ifail=1
       endif
    endif

    if (ifail.ne.0) then
       print*,' Correct error and try again '
       stop 1
    end if

  end subroutine fxatm_checkfile

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine end_fixedatmos()

      use fixedatmos_common
      use genie_util, ONLY: check_iostat

      implicit none

      ! locals
      integer :: ios

      ! clean up allocated memory
      if (allocated(tstar_clim)) deallocate(tstar_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(precip_clim)) deallocate(precip_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(latent_clim)) deallocate(latent_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(sensible_clim)) deallocate(sensible_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(netsolar_clim)) deallocate(netsolar_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(netlong_clim)) deallocate(netlong_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(stressx_clim)) deallocate(stressx_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(stressy_clim)) deallocate(stressy_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(runoff_clim)) deallocate(runoff_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(evap_clim)) deallocate(evap_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(conductflux_clim)) deallocate(conductflux_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(waterflux_clim)) deallocate(waterflux_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(orog_clim)) deallocate(orog_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(tair2m_clim)) deallocate(tair2m_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(qair2m_clim)) deallocate(qair2m_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(windu10m_clim)) deallocate(windu10m_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(windv10m_clim)) deallocate(windv10m_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)
      if (allocated(pstar_clim)) deallocate(pstar_clim,stat=ios)
      call check_iostat(ios,__LINE__,__FILE__)

  end subroutine end_fixedatmos

end module fixed_atmosphere

