      subroutine fixedchem(istep
     :     ,co2,n2o,ch4,iconv)

      implicit none

#include "precision.inc"

      include 'netcdf.inc'

c     NEED TO PUT TIMESTEP INFO INTO JOB FILE!!     

#include "resolution_fixedchem.inc"

c     **WARNING: THIS LOOKS DANGEROUS IF WE CHANGE THE TIMESTEP.
      real timestep
c     10 days is the chemistry timestep.  Timestep is in seconds.
      parameter(timestep=10.*24.*60.*60.)

      real daysperyear
      parameter(daysperyear=360.0)

      real(rk_in) :: co2(mg,jgg)
      real(rk_in) :: n2o(mg,jgg)
      real(rk_in) :: ch4(mg,jgg)
      integer istep
      integer iconv

c     ****FOR THIS ROUTINE****

      integer ncid

      integer ifail

      character fname_restart_fixedchem*200
      character fname_gases_fixedchem*200

      character outputdir_name*200
      save outputdir_name

      integer dt_restart_fixedchem
      save dt_restart_fixedchem

      logical lexist
c     
      INTEGER IFIRST
      data ifirst/1/
      save ifirst

      integer ioffset
      save ioffset

      character fname*200
      integer status,nrecsid,ioffsetid

      integer ios

c     For this routine...
      real co2_fix,n2o_fix,ch4_fix
      save co2_fix,n2o_fix,ch4_fix
      logical fixed_gases
      save fixed_gases
      integer dt_vals
      save dt_vals
      real, allocatable, dimension(:) :: co2_vect
      real, allocatable, dimension(:) :: n2o_vect
      real, allocatable, dimension(:) :: ch4_vect
      save co2_vect,n2o_vect,ch4_vect

c     For date
      real(rk_libutil1) :: doy
      integer iyear
      integer imonth
      real day
      integer iday
      save iyear,imonth,day
      integer iyearid
      integer imonthid
      integer idayid   
      character yearstring*4
      character monthstring*2
      character daystring*2

c     Paramters...these values are from genie-igcm3/nikosrad.f
      real mw_air,mw_co2,mw_n2o,mw_ch4
      parameter(mw_air=28.964)
      parameter(mw_co2=44.011)
      parameter(mw_n2o=44.014)
      parameter(mw_ch4=16.043)

c     The number of entries in the gases file
      integer ntimes
      save ntimes
      integer daynum
      integer time1
      integer time2
      real timefrac
      integer iniday
      save iniday

      namelist/FIXEDCHEM_CONTROL/
     :        dt_restart_fixedchem,
     :        fname_restart_fixedchem,
     :        outputdir_name,
     :        co2_fix,
     :        n2o_fix,
     :        ch4_fix,
     :        fixed_gases,
     :        dt_vals,
     :        fname_gases_fixedchem,
     :        iniday

c     
      IF (IFIRST.EQ.1) THEN

         fname_restart_fixedchem='xxx'
         fname_gases_fixedchem='xxx'
          
c read DATA (i.e. namelist) file
         open(unit=14,file='data_fixedchem',status='old',iostat=ios)
         if (ios /= 0) then
            print*,'ERROR: could not open fixedchem namelist file'
            stop
         end if

c read in namelist
         read(UNIT=14,NML=FIXEDCHEM_CONTROL,IOSTAT=ios)
         if (ios /= 0) then
            print*,'ERROR: could not read fixedchem namelist'
            stop
         else
            close(14)
         end if
c 
c **********************************
c        READ IN THE RESTART FILE
c        it's actually just the day offset from 1st January

         ifail=0
         if (fname_restart_fixedchem.eq.'xxx') then
            print*,' Missing filename for fixedchem restart '
            ifail=1
         else
            inquire(file=trim(fname_restart_fixedchem),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :       trim(fname_restart_fixedchem)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop
         end if
         print*,' fixedchem: Opening restart file for read: ',
     &            trim(fname_restart_fixedchem)
         call open_file_nc(
     &       trim(fname_restart_fixedchem),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
         if (ifail.ne.0) then
           print*,'variable ioffset not found in fixedchem'
           stop
         end if   
         call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
         if (ifail.ne.0) then
           print*,'variable iyear not found in fixedchem'
           stop
         end if  
         call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
         if (ifail.ne.0) then
           print*,'variable imonth not found in fixedchem'
           stop
         end if  
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         if (ifail.ne.0) then
           print*,'variable iday not found in fixedchem'
           stop
         end if  
         call close_file_nc(
     &       trim(fname_restart_fixedchem),ncid)
c **********************************

c **********************************
c        READ IN THE GASES FILE
c        only if fixed_gases is .false.

       if (.not.fixed_gases) then
         ifail=0
         if (fname_gases_fixedchem.eq.'xxx') then
            print*,' Missing filename for fixedchem gases '
            ifail=1
         else
            inquire(file=trim(fname_gases_fixedchem),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :       trim(fname_gases_fixedchem)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop
         end if
         print*,' fixedchem: Opening gases file for read(1): ',
     &            trim(fname_gases_fixedchem)
         call open_file_nc(
     &       trim(fname_gases_fixedchem),ncid)
         call get1di_data_nc(ncid,'ntimes',1,ntimes,ifail)
         if (ifail.ne.0) then
           print*,'variable ntimes not found in fixedchem'
           stop
         end if
         call close_file_nc(
     &       trim(fname_gases_fixedchem),ncid)

         print*,'ntimes in fixedchem = ',ntimes   
          allocate(co2_vect(ntimes))
          allocate(n2o_vect(ntimes))
          allocate(ch4_vect(ntimes))

         print*,' fixedchem: Opening gases file for read(2): ',
     &            trim(fname_gases_fixedchem)
         call open_file_nc(
     &       trim(fname_gases_fixedchem),ncid)
         call get1d_data_nc(ncid,'co2',ntimes,co2_vect,ifail)
         if (ifail.ne.0) then
           print*,'variable co2 not found in fixedchem'
           stop
         end if   
         call get1d_data_nc(ncid,'n2o',ntimes,n2o_vect,ifail)
         if (ifail.ne.0) then
           print*,'variable n2o not found in fixedchem'
           stop
         end if  
         call get1d_data_nc(ncid,'ch4',ntimes,ch4_vect,ifail)
         if (ifail.ne.0) then
           print*,'variable ch4 not found in fixedchem'
           stop
         end if  
         call close_file_nc(
     &       trim(fname_gases_fixedchem),ncid)

       endif    ! end for fixed_gases
     
c **********************************

         day=iday
         ioffset=mod(ioffset,360)

         ifirst=0

      end if



      iday=nint(day)
      doy=mod(ioffset+istep*timestep/(24*60*60.),daysperyear)
      if (mod(istep,120).eq.0.or.istep.le.6) 
     &  print*,'Fixed chemistry thinks DOY=',doy


c     
c     Processing data.....

      if (fixed_gases) then
        co2(:,:)=co2_fix
        n2o(:,:)=n2o_fix
        ch4(:,:)=ch4_fix
      else

c     Will ths work with restarts??!! 
      daynum=iday+((imonth-1)*30)+(360*(iyear-2000))-1-iniday
      time1=int(daynum/real(dt_vals) + 1)
      time2=time1+1
      timefrac=mod(daynum,dt_vals)/real(dt_vals)


c     ONLY DO CO2 FOR NOW AS METHANE, N2O ARE TURNED OFF BY DEFAULT.
      if (time1.le.0) then
      print*,'fixedchem: warning, too early for gas record'
      print*,'co2(1)=',co2_vect(1)
      co2(:,:)=co2_vect(1)
      endif

      if ( (time2.le.ntimes).and.(time1.gt.0) ) then
      print*,'fixedchem: interpolating times...'
      print*,'timefrac=',timefrac
      print*,'co2(time1)=',co2_vect(time1)
      print*,'co2(time2)=',co2_vect(time2)
      co2(:,:)=co2_vect(time1)*(1-timefrac) + 
     :         co2_vect(time2)*(timefrac)
      endif

      if (time2.gt.ntimes) then
      print*,'fixedchem: warning, too late for gas record'
      print*,'co2(ntimes)=',co2_vect(ntimes)
      co2(:,:)=co2_vect(ntimes)
      endif

      print*,'co2=',co2(1,1)

      endif 

c     convert from ppm to kg/kg
      co2(:,:)=co2(:,:) * (mw_co2/mw_air)
      n2o(:,:)=n2o(:,:) * (mw_n2o/mw_air)
      ch4(:,:)=ch4(:,:) * (mw_ch4/mw_air)

c
c
c     See if we need to write a restart....

      if (mod(istep,dt_restart_fixedchem).eq.0) then

      write(yearstring,'(i4.4)') iyear
      write(monthstring,'(i2.2)') imonth
      write(daystring,'(i2.2)') iday

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      fname=trim(outputdir_name)//
     :        '/fixedchem_restart_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'.nc'
      print*,' Opening restart file for write: ',trim(fname)
      status=nf_create(trim(fname), nf_clobber, ncid)
      call check_err(status)
      status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
      call check_err(status)
      status=nf_def_var(ncid,'ioffset',nf_int,1,nrecsid,ioffsetid)
      call check_err(status)
      status=nf_def_var(ncid,'iyear',nf_int,1,nrecsid,iyearid)
      call check_err(status)
      status=nf_def_var(ncid,'imonth',nf_int,1,nrecsid,imonthid)
      call check_err(status)
      status=nf_def_var(ncid,'iday',nf_int,1,nrecsid,idayid)
      call check_err(status)
      status=nf_enddef(ncid)
      call check_err(status)
      status=nf_put_var_int(ncid,ioffsetid,int(doy))
      call check_err(status)
      status=nf_put_var_int(ncid,iyearid,iyear)
      call check_err(status)
      status=nf_put_var_int(ncid,imonthid,imonth)
      call check_err(status)
      status=nf_put_var_int(ncid,idayid,iday)
      call check_err(status)
      status=nf_close(ncid)
      call check_err(status)
      endif

      day=day+timestep/(24*60*60.)
c     This bit so that we don't get too far out in our count....
c     Anchor to a day if we start drifting.
c     Means timestep can never be less than 1/1000 of a day!!!!
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

c     Let's just set this to 1 anyway, to force the igcm3_diab to 
c       update gases every 10 days....
      iconv=1

      return
      end
