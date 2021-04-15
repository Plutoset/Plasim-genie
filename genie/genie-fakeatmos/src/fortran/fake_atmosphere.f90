module fake_atmosphere

  ! File: fake_atmosphere.f90
  !  
  ! Description:
  !  
  !  A module that provides flux, restoring of mixed boundary conditions for goldstein.   

  !  The main subroutines in this module are initialise_fakeatmos and fakeatmos
  !  A description is provided with each routine.
  !  
  !  For more information contact author: Agatha de Boer, agathamdb(at)yahoo.com
   
         
  implicit none

contains

  subroutine initialise_fakeatmos(stressxu, stressyu, stressxv, stressyv, &
             latent, sensible, netsolar, netlong, precip, evap, runoff, &
             conductflux, waterflux, trest, srest)

    !  Subroutine: initialise_fakeatmos    
    !
    !  Description:
    !   In initialise_fakeatmos the boundary variables for goldstein is initialised. 
    !   Note that only the wind stresses, conductflux and waterflux,
    !   trest and srest are used. The other variables are only necessary for 
    !   consitency with goldstein and other atmospheric routines. 
    !
    !  Input:
    !
    !  Input/Output:   
    !
    !  Output:
    !   stressxu - zonal winstress on u points
    !   stressyu - meridional windstress on u points
    !   stressxv - zonal winstress on v points
    !   stressyv - meridional windstress on v points
    !   latent - latent heat (dummy variable, not used) 
    !   sensible - sensible heat (dummy variable, not used)
    !   netsolar - net solar heat flux (dummy variable, not used)
    !   netlong - net longwater radiation (dummy variable, not used)
    !   precip - precipitation (dummy variable, not used)
    !   evap - evaporation (dummy variable, not used)
    !   runoff - runoff (dummy variable, not used)
    !   conductflux - usually conductive heat flux but used here to store total heat flux
    !   waterflux - total freshwater flux
    !   trest - temperature field to restore to (e.g., levitus)
    !   srest - salinity field to restore to (e.g., levitus)
  
    use fakeatmos_common

#include "precision.inc"
    include 'netcdf.inc'

    real(rk),dimension(:,:),intent(inout) :: precip  
    real(rk),dimension(:,:),intent(inout) :: latent 
    real(rk),dimension(:,:),intent(inout) :: sensible 
    real(rk),dimension(:,:),intent(inout) :: netsolar 
    real(rk),dimension(:,:),intent(inout) :: netlong 
    real(rk),dimension(:,:),intent(inout) :: stressxu 
    real(rk),dimension(:,:),intent(inout) :: stressyu 
    real(rk),dimension(:,:),intent(inout) :: stressxv 
    real(rk),dimension(:,:),intent(inout) :: stressyv 
    real(rk),dimension(:,:),intent(inout) :: runoff 
    real(rk),dimension(:,:),intent(inout) :: evap 
    real(rk),dimension(:,:),intent(inout) :: conductflux 
    real(rk),dimension(:,:),intent(inout) :: waterflux 

    real(rk),dimension(:,:),intent(inout) :: srest 
    real(rk),dimension(:,:),intent(inout) :: trest 


    ! Internally used variables

    integer :: ifail,ncid,i,j

!     file access checks
      integer :: ios

    ! need to put the stoponeror into the namelist....
    logical stoponerror
    parameter(stoponerror=.true.)

    ! Namelist for reading in parameters

      namelist/fakeatmos_nml/fname_climate, &
              trest_flag, &
              srest_flag, &
              relaxt,  &
              relaxs,  &
              nx, &
              ny

! Set up blank filenames
    fname_climate='xxx'

! read DATA (i.e. namelist) file
      open(unit=55,file='data_fakeatmos',status='old',iostat=ios)
      if (ios /= 0) then
         print*,'ERROR: could not open FAKEATMOS namelist file'
         print*,'iostat ', ios
         stop
      end if

! read in namelist
      read(UNIT=55,NML=fakeatmos_nml,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read FAKEATMOS namelist'
         print*,'iostat ', ios
         stop
      else
         close(56)
      end if

! initialize variables to zero
    latent(:,:) = 0
    sensible(:,:) = 0
    netsolar(:,:) = 0
    netlong(:,:) = 0
    precip(:,:) = 0 
    evap(:,:) = 0
    runoff(:,:) = 0
    conductflux(:,:) = 0
    waterflux(:,:) = 0
    trest(:,:) = 0
    srest(:,:) = 0

   
    print*,"Initialising fake atm (ocean Boundary Conditions)"

    ! Check to see if we have required info

    print*,'Boundary condition file is: ', fname_climate
    call fakeatm_checkfile(fname_climate,'BoundaryConditions')


! Get starting data
    
! Note that we read in stress on u and v points from same field
! If high precision in wind is required, this can easily be updated

    print*,' fakeatmos: Opening boundary condition file ',trim(fname_climate)
    call open_file_nc(trim(fname_climate),ncid)

      call get2d_data_nc(ncid,'stressx',nx,ny,stressxu,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading stressxu'

      call get2d_data_nc(ncid,'stressy',nx,ny,stressyu,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading stressyu'

      call get2d_data_nc(ncid,'stressx',nx,ny,stressxv,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading stressxv'

      call get2d_data_nc(ncid,'stressy',nx,ny,stressyv,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading stressyv'
 
      call get2d_data_nc(ncid,'temprestore',nx,ny,trest,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading temprestore'

      call get2d_data_nc(ncid,'heatflux',nx,ny,conductflux,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading heatflux'

      call get2d_data_nc(ncid,'saltrestore',nx,ny,srest,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading saltrestore'

      call get2d_data_nc(ncid,'freshwflux',nx,ny,waterflux,ifail)
      if ((ifail.ne.0).and.(stoponerror)) stop 'trouble reading freshwflux'

      call close_file_nc(trim(fname_climate),ncid)

  
! Convert waterflux from m/s to mm/s (because goldstein converts back)
! and the idea is not to change goldstein  
      do j=1,size(waterflux,2)
         do i=1,size(waterflux,1)
           waterflux(i,j) = waterflux(i,j) * 1000.0
         enddo
       enddo

  print*,'end of initialization'
  print*,'waterflux, heatflux, temprestore, stressxu'
  print*, waterflux(9,32), conductflux(9,32), trest(9,32), stressxu(9,32)
  print*,'relaxt and nx'
  print*, relaxt, nx

  end subroutine initialise_fakeatmos

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine fakeatmos(conductflux, waterflux, sst, sss, trest, srest)
    !  Subroutine: fakeatmos    
    !
    !  Description:
    !   fakeatmos calculates the heat and freshwater fluxes if necessary from the 
    !   model sst and sss fields and from the input data (to which it is restored)
    !
    !  Input:
    !   sst - surface temperature field from model 
    !   sss - surface salinity field  from model 
    !   trest - temperature field to restore to (e.g., levitus)
    !   srest - salinity field to restore to (e.g., levitus)
    !
    !  Input/Output:   
    !
    !  Output:
    !   conductflux - usually conductive heat flux but used here to store total heat flux
    !   waterflux - total freshwater flux
 
    use fakeatmos_common

#include "precision.inc"
    include 'netcdf.inc'

    real(rk),dimension(:,:),intent(inout) :: conductflux 
    real(rk),dimension(:,:),intent(inout) :: waterflux 
    real(rk),dimension(:,:),intent(inout) :: sst  
    real(rk),dimension(:,:),intent(inout) :: sss   
    real(rk),dimension(:,:),intent(inout) :: trest   
    real(rk),dimension(:,:),intent(inout) :: srest   


    integer :: j,i

! unused variables
!    character :: fname*200
!    integer :: status,nrecsid,ioffsetid

! restore to sst
   if (trest_flag) then
           do j=1,ny
              do i=1,nx
                 conductflux(i,j) = relaxt * (trest(i,j) - sst(i,j))
             enddo
           enddo   
   endif
    
! restore to sss  
! we need to add 34.9 to sss to change goldstein units to psu  
   if (srest_flag) then
           do j=1,ny
              do i=1,nx
      waterflux(i,j) = -1000.0*relaxs*(srest(i,j) - (sss(i,j)+34.9))
             enddo
           enddo

   endif

  print*,' During fake atmos'
  print*,'trest flag, srest flag, nx, relaxt'
  print*, trest_flag, srest_flag, nx, relaxt

    
   end subroutine fakeatmos

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  subroutine fakeatm_checkfile(fname,label)

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

  end subroutine fakeatm_checkfile

end module fake_atmosphere
