!********************************************************************
!*                                                                  *
!*           carbon isotope module for the atmosphere/land          *
!*                                                                  *
!********************************************************************
!
! This module is being developed by:
! Joy Singarayer
! School of Geographical Sciences
! Bristol University
! University Road
! Bristol BS8 1SS
! U.K.
! email: joy.singarayer@bris.ac.uk
!
!********************************************************************
module ichem_main

      USE ichem_var

contains

      subroutine initialise_ichem

! dummy routine as yet.
! will be used to initialise all arrays to zero and
! read any config data and read data files or restart files

      implicit none

      chemcount = 0
      idoy = 0     
      doy = 0.0

      end subroutine initialise_ichem

! ***************************************************************


      subroutine ichem(dt_atm,lco2_mm,l14co2_mm,massair, &
                        lon_atm,lat_atm,plev,ddt14co2_mm)

        use genie_util, ONLY: check_unit

      implicit none

#include "precision.inc"

!     This is the module that controls the c14 sources,
!       and sinks.  You must have genientrac=2 for it to work....


      integer l,ii,jj
      real(rk_in) :: lon_atm(mg)
      real(rk_in) :: lat_atm(jgg)
      real(rk_in) :: plev(nl)
      real(rk_in) :: dt_atm ! length of atm time step = 3600s
      real dt_yr

! input/output mass mixing ratio of CO2 and 14CO2
      real(rk_in) :: lco2_mm(mg,jgg)
      real(rk_in) :: l14co2_mm(mg,jgg,nl)
      real(rk_in) :: massair(mg,jgg,nl)
      real(rk_in) :: lmass14co2(mg,jgg,nl)
      real(rk_in) :: lmassco2(mg,jgg,nl)
      real(rk_in) :: l14co2_mm_old(mg,jgg,nl)
      real(rk_in) :: ddt14co2_mm(mg,jgg,nl)
      real(rk_in) :: lco2annmean(mg,jgg,nl)
 
      character outputdir_name*200

      integer ios


      namelist/ICHEM_CONTROL/outputdir_name

! time and counting steps
      dt_yr=dt_atm/360.0
      chemcount=chemcount + 1
      idoy = idoy + 1

! this should all go in initialise_ichem...*******
      if (chemcount.eq.1) then

! read DATA (i.e. namelist) file
         call check_unit(15,__LINE__,__FILE__)
         open(unit=15,file='data_ichem',status='old',iostat=ios)
         if (ios /= 0) then
            print*,'ERROR: could not open ichem namelist file'
            stop
         end if

! read in namelist
         read(UNIT=15,NML=ICHEM_CONTROL,IOSTAT=ios)
         if (ios /= 0) then
            print*,'ERROR: could not read ichem namelist'
            stop
         else
            close(15)
         end if
  
        do l=1,nl
          do jj=1,jgg
           do ii=1,mg
             l14c_annmean(ii,jj,l)=0.0
             lco2annmean(ii,jj,l)=0.0
             l14co2_mm_old(ii,jj,l)=0.0
             iyear=2000
           enddo
          enddo
        enddo

      end if


! transform from mass mixing ratio to number of atoms in each cell
      do l=1,nl
        do jj=1,jgg
          do ii=1,mg
              lmass14co2(ii,jj,l)=l14co2_mm(ii,jj,l)*massair(ii,jj,l)
              lmassco2(ii,jj,l)=lco2_mm(ii,jj)*massair(ii,jj,l)
               l14c_annmean(ii,jj,l) = l14c_annmean(ii,jj,l) +  &
                                      l14co2_mm(ii,jj,l)
               lco2annmean(ii,jj,l) = lco2annmean(ii,jj,l) + &
                                      lmassco2(ii,jj,l)  
          enddo
        enddo
      enddo


! decay tracers 
      do l=1,nl
        do jj=1,jgg
          do ii=1,mg
                lmass14co2(ii,jj,l)=lmass14co2(ii,jj,l)* &
                                EXP(-dt_yr/const_lamda_14C) 
          end do
        end do
      end do



! Only output yearly mean to start with.
      if (idoy.ge.360*24) then
         doy = idoy
         year = iyear
         call write_nc(lon_atm,lat_atm,plev,year, &
                        doy,l14c_annmean,lco2annmean,outputdir_name)
         idoy = 0
         iyear = iyear + 1
         do ii=1,mg
           do jj=1,jgg
             do l=1,nl
                  l14c_annmean(ii,jj,l)=0.0
             enddo
           enddo
         enddo
      end if

!     source -> will need to add in either read, or output from production.
!     production could be done outside igcm time step?? Need a logical for
!     if reading atmospheric or calculating production
      do l=1,1
        do jj=1,jgg
          do ii=1,mg
            lmass14co2(ii,jj,l)=lmass14co2(ii,jj,l)+1.0E16
          enddo
        enddo
      enddo


 
! need to convert from number of atoms into mass mixing ratio for genie_global
! 14co2 field - to pass back into diab.
! need to know what co2_atm units are for this.

      do l=1,nl
        do jj=1,jgg
          do ii=1,mg
               l14co2_mm(ii,jj,l)=lmass14co2(ii,jj,l)/massair(ii,jj,l)
         
          enddo
        enddo
      enddo             

! calculate tendencies for input back into igcm3_diab
! Should I be differencing mass mixing ratio from previous step, or
! mass from previous step?? probably mass, and then convert to mass mixing
! ratio??

      do l=1,nl
        do jj=1,jgg
           do ii=1,mg
            ddt14co2_mm(ii,jj,l)=(l14co2_mm(ii,jj,l) - &
                          l14co2_mm_old(ii,jj,l))/dt_atm
            l14co2_mm_old(ii,jj,l)=l14co2_mm(ii,jj,l)
           enddo
        enddo
      enddo


! convert to partial pressure, and then d14C and D14C and output to netcdf



     end subroutine ichem

end module ichem_main
