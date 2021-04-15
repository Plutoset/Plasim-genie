      subroutine fixedseaice(istep,
     :     tstar,
     :     seaice_frac,
     :     energycarry_ice_ocn,
     :     energycarry_ocn_ice,dtcarry_ocn_ice,
     :     albedo,iland)

      implicit none

#include "precision.inc"

      include 'fixedseaice_com.cmn'
      include 'netcdf.inc'

c     NEED TO PUT TIMESTEP INFO INTO JOB FILE!!     

      integer mg,jg
c     mg is longitudes, jg is latitudes per hemisphere
      parameter(mg=64,jg=16)

      real timestep
c     Six hours is the seaice timestep.  Timestep is in seconds.
      parameter(timestep=6.*60.*60.)

      real daysperyear
      parameter(daysperyear=360.0)

      integer nhem
c     nhem is number of hemispheres
      parameter(nhem=2)

      integer jgg
c     jgg is total latitudes
      parameter(jgg=nhem*jg)

      real abszero
      parameter(abszero=273.16)

      real t2m
c     SST (in Celsius) at which ice forms
      parameter(t2m=-2.0)

      character datestring*7

c     ****PASSED TO THIS ROUTINE
      integer istep
      real(rk_in) :: tstar(mg,jgg)
      real(rk_in) :: seaice_frac(mg,jgg)
      real(rk_in) :: dtcarry_ocn_ice(mg,jgg)
      real(rk_in) :: energycarry_ocn_ice(mg,jgg)
      real(rk_in) :: energycarry_ice_ocn(mg,jgg)
      real(rk_in) :: albedo(mg,jgg)
      integer iland(mg,jgg)

c     ****FOR THIS ROUTINE****

      integer ncid
      integer lnsig1
      integer mpth1,mpth2

      real(rk_libutil1) :: apfrac
      real(rk_libutil1) :: doy

      integer j,i

      REAL tstar3(mg,jgg)

      integer ifail

      real sst_clim(mg,jgg,12)
      save sst_clim

      integer ifname_sstetc
      character fname_sstetc*200

      integer ifname_restart
      character fname_restart_fixedseaice*200

      integer ilen
      character outputdir_name*200

      integer ios

      integer dt_restart_fixedseaice
      save dt_restart_fixedseaice

      logical lexist
c     
      INTEGER IFIRST
      data ifirst/1/
      save ifirst

      integer ioffset
      save ioffset

      character fname*200
      integer status,nrecsid,ioffsetid

      integer lnsig

c     For date
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


      namelist/fixedseaice_control/fname_sstetc,
     :                             fname_restart_fixedseaice,
     :                             outputdir_name,
     :                             dt_restart_fixedseaice,albedo_seaice
c     
!-------------------------------------------
!     open netcdf file and read in sea surface temperature
!-------------------------------------------

      IF (IFIRST.EQ.1) THEN
         fname_sstetc='xxx' 
         fname_restart_fixedseaice='xxx' 

c read DATA (i.e. namelist) file
      open(unit=10,file='data_fixedseaice',status='old',iostat=ios)
      if (ios /= 0) then
         print*,'ERROR: could not open fixedseaice namelist file'
         stop
      end if

c read in namelist
      read(UNIT=10,NML=FIXEDSEAICE_CONTROL,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read fixedseaice namelist'
         stop
      else
         close(10)
      end if
c 
         ifail=0
         if (fname_sstetc.eq.'xxx') then
            print*,' Missing filename for sst etc '
            ifail=1
         else
            ifname_sstetc=lnsig1(fname_sstetc)
            inquire(file=fname_sstetc(1:ifname_sstetc),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',fname_sstetc(1:ifname_sstetc)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         end if

         print*,' fixedseaice: Opening sst file ',
     &            fname_sstetc(1:ifname_sstetc)
         ifirst=0
         call open_file_nc(fname_sstetc(1:ifname_sstetc),ncid)
         call get3d_data_nc(ncid,'sst',mg,jgg,12,sst_clim,ifail)
         call close_file_nc(fname_sstetc(1:ifname_sstetc),ncid)


c        READ IN THE RESTART FILE
c        it's actually just the day offset from 1st January

         ifail=0
         if (fname_restart_fixedseaice.eq.'xxx') then
            print*,' Missing filename for fixedseaice restart '
            ifail=1
         else
            ifname_restart=lnsig1(fname_restart_fixedseaice)
            inquire(file=fname_restart_fixedseaice(1:ifname_restart),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :       fname_restart_fixedseaice(1:ifname_restart)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         end if

         print*,' fixedseaice: Opening restart file for read: ',
     &            fname_restart_fixedseaice(1:ifname_restart)

         call open_file_nc(
     &       fname_restart_fixedseaice(1:ifname_restart),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         call close_file_nc(
     &       fname_restart_fixedseaice(1:ifname_restart),ncid)

         day=iday
         ioffset=mod(ioffset,360)

      end if

      iday=nint(day)
      doy=mod(ioffset+istep*timestep/(24*60*60.),daysperyear)
      if (mod(istep,120).eq.0.or.istep.le.6) 
     &  print*,'Fixed seaice thinks DOY=',doy

      CALL CALNDR(DOY,Mpth1,ApFRAC)
      MPth2=mpth1+1
      if (mpth2.eq.13) mpth2=1



c     set TSTAR by interpolation from two monthly values
c     Back to grid of genie, and into celsius....
c     
       do j=1,jgg
         do i=1,mg

           tstar3(i,j)=(1.0-apfrac)*sst_clim(i,j,mpth1)+
     $              apfrac*sst_clim(i,j,mpth2)

               if (iland(i,j).eq.0) then
                  energycarry_ocn_ice(i,j)=0.
                  dtcarry_ocn_ice(i,j)=0.
                  energycarry_ice_ocn(i,j)=0.
                  if (seaice_frac(i,j).gt.0) then
c     
c     ICE AT BEGINNING OF TIMESTEP
c     
                     tstar(i,j)=tstar3(i,j)
c                     tstar(i,j)=tstar3(i,j)-abszero

                     if (tstar(i,j).gt.t2m) then
c     
c     there is ice, but there shouldn't be....
c     
                        seaice_frac(i,j)=0.
                     else
c     
c     there is ice and there should be
c     

                        albedo(i,j)=albedo_seaice
                     endif
                  else
c     
c     NO ICE AT BEGINNING OF TIMESTEP
c     
c
c     I have changed this line.  It affects the results. 
c     Use tstar(i,j) to reproduce the orginal genie_example.job.
c     tstar3(i,j) is correct I think (check this, djl).
c                     if (tstar(i,j).gt.t2m) then
                     if (tstar3(i,j).gt.t2m) then
c     
c     there is no ice and there shouldn't be any..
c     
                     else
c     
c     there's no ice but there should be...
c     
                        seaice_frac(i,j)=1.0
                        tstar(i,j)=tstar3(i,j)
c                        tstar(i,j)=tstar3(i,j)-abszero
                        albedo(i,j)=albedo_seaice
                     endif
                  end if
               endif
         enddo
      enddo
c
      
c     See if we need to write a restart....

      if (mod(istep,dt_restart_fixedseaice).eq.0) then

      write(datestring,'(i7.7)') istep
      write(yearstring,'(i4.4)') iyear
      write(monthstring,'(i2.2)') imonth
      write(daystring,'(i2.2)') iday

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      ilen=lnsig1(outputdir_name)
c      fname=outputdir_name(1:ilen)//
c     :        '/fixedseaice_restart_'//trim(datestring)//'.nc'
      fname=outputdir_name(1:ilen)//
     :        '/fixedseaice_restart_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'.nc'
      ilen=lnsig(fname)
      print*,' Opening restart file for write: ',fname(1:ilen)
      status=nf_create(fname(1:ilen), nf_clobber, ncid)
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

      return
      end
