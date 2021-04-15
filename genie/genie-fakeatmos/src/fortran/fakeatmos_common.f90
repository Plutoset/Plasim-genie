module fakeatmos_common

  ! File: fakeatmos_common.f90
  !  
  ! Description:
  !  
  ! Variables:
  !  nx - the number of points longitudinal direction (ocean and atmosphere)
  !  ny - the number of points latitudinal direction (ocean and atmosphere)
  !  relaxt - relaxation constant for temperature restoring
  !  relaxs - relaxation constant for salinity restoring
  !  trest_flag - true if temperature restoring is required 
  !  srest_flag - true if salinity restoring is required 
  !  fname_climate - variable contining boundary condition file name
  
  
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

  integer :: nx 
  integer :: ny 
       
  real :: relaxt     ! relaxation constant for temperature restoring
  real :: relaxs     ! relaxation constant for salinity restoring

  logical :: trest_flag   =.true.
  logical :: srest_flag   =.false.

  character(200) :: fname_climate


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
  
end module fakeatmos_common
