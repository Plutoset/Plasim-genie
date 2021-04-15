c $Id: fixedocean.F 3399 2007-07-31 14:45:14Z gethin-williams $
c
c File: fixedocean.F
c
c Description:
c
c Fixedocean provides a 2d array of surface temperature
c over ocean points in which there is no seaice.  
c Monthly mean SSTs are read in from a file at initialisation.
c During a simulation the SSTs are temporally interpolated
c to the current timestep.  If sea ice is present, then the
c input field of SST is unchanged.  Also output is a 2d albedo
c field.  As for SST, if sea ice is present, then the
c input field of albedo is unchanged.
c Also modified are the 'carry' variables which originate 
c from the slabseaice module if it is used.  They are simply set to zero.
c
c subroutine: fixedocean
c
c input:
c
c     istep - OCEAN timestep number (~= koverall/kocn in genie.F)
c     seaice_frac - fraction of seaice in each gridbox (fraction)
c     iland - land-sea mask (0 or 1)
c
c input/output:
c
c     albedo - surface ocean albedo (fraction)
c
c output:
c
c     tstar - surface temperature (celcius)
c     energycarry_ocn_ice - amount of 'carry=over' energy from the last seaice timestep,
c     used to conserve energy in the slabocean-slabseaice case (joules)
c     dtcarry_ocn_ice - amount of 'carry=over' time from the last seaice timestep,
c     used to conserve energy in the slabocean-slabseaice case. (seconds)
c
      subroutine fixedocean(istep,
     :     tstar,
     :     seaice_frac,
     :     energycarry_ocn_ice,dtcarry_ocn_ice,
     :     albedo,iland)

      implicit none

#include "precision.inc"

      include 'fixedocean_com.cmn'
      include 'netcdf.inc'

c     NEED TO PUT TIMESTEP INFO INTO JOB FILE!!      

      integer mg,jg
c     mg is longitudes, jg is latitudes per hemisphere
      parameter(mg=64,jg=16)

      real timestep
c     Two days is the ocean timestep.  Timestep is in seconds.
      parameter(timestep=2.*24.*60.*60.)

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

      character datestring*7
c     
c     ****PASSED TO THIS ROUTINE
      integer istep
      real(rk_in) :: tstar(mg,jgg)
!      real(rk_in) :: latent(mg,jgg)
!      real(rk_in) :: sensible(mg,jgg)
!      real(rk_in) :: netsolar(mg,jgg)
!      real(rk_in) :: netlong(mg,jgg)
!      real(rk_in) :: stressx(mg,jgg)
!      real(rk_in) :: stressy(mg,jgg)
      real(rk_in) :: seaice_frac(mg,jgg)
      real(rk_in) :: dtcarry_ocn_ice(mg,jgg)
      real(rk_in) :: energycarry_ocn_ice(mg,jgg)
!      real(rk_in) :: energycarry_ice_ocn(mg,jgg)
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
      character fname_restart_fixedocean*200

      character outputdir_name*200

      integer dt_restart_fixedocean
      save dt_restart_fixedocean

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

      namelist/fixedocean_control/fname_sstetc,fname_restart_fixedocean,
     :                             outputdir_name,
     :                             dt_restart_fixedocean,albedo_ocean
c     
!-------------------------------------------
!     open netcdf file and read in sea surface temperature
!-------------------------------------------

      IF (IFIRST.EQ.1) THEN
         fname_sstetc='xxx'
         fname_restart_fixedocean='xxx' 

c read DATA (i.e. namelist) file
         open(unit=9,file='data_fixedocean',status='old',iostat=ios)
         if (ios /= 0) then
            print*,'ERROR: could not open fixedocean namelist file'
            stop
         end if

c read in namelist
         read(UNIT=9,NML=FIXEDOCEAN_CONTROL,IOSTAT=ios)
         if (ios /= 0) then
            print*,'ERROR: could not read fixedocean namelist'
            stop
         else
            close(9)
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

         print*,' fixedocean: Opening sst file ',
     &            fname_sstetc(1:ifname_sstetc)
         ifirst=0
         call open_file_nc(fname_sstetc(1:ifname_sstetc),ncid)
         call get3d_data_nc(ncid,'sst',mg,jgg,12,sst_clim,ifail)
         call close_file_nc(fname_sstetc(1:ifname_sstetc),ncid)


c        READ IN THE RESTART FILE
c        it's actually just the day number offset from 1st January

         ifail=0
         if (fname_restart_fixedocean.eq.'xxx') then
            print*,' Missing filename for fixedocean restart '
            ifail=1
         else
            ifname_restart=lnsig1(fname_restart_fixedocean)
            inquire(file=fname_restart_fixedocean(1:ifname_restart),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :                  fname_restart_fixedocean(1:ifname_restart)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         
         end if

         print*,' fixedocean: Opening restart file for read: ',
     &            fname_restart_fixedocean(1:ifname_restart)

         call open_file_nc(
     &       fname_restart_fixedocean(1:ifname_restart),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         call close_file_nc(
     &       fname_restart_fixedocean(1:ifname_restart),ncid)

         day=iday
         ioffset=mod(ioffset,360)

      end if

      iday=nint(day)
      doy=mod(ioffset+istep*timestep/(24*60*60.),daysperyear)
      print*,'Fixed ocean thinks DOY=',doy
      CALL CALNDR(DOY,Mpth1,ApFRAC)
      MPth2=mpth1+1
      if (mpth2.eq.13) mpth2=1



c     set TSTAR by interpolation from two monthly values
c     Back to grid of genie, and into celsius....

      do j=1,jgg
         do i=1,mg

           tstar3(i,j)=(1.0-apfrac)*sst_clim(i,j,mpth1)+
     $              apfrac*sst_clim(i,j,mpth2)

               if (iland(i,j).eq.0) then
                  energycarry_ocn_ice(i,j)=0.0
                  dtcarry_ocn_ice(i,j)=0.0
                  if (seaice_frac(i,j).eq.0) then
                     tstar(i,j)=tstar3(i,j)
c     use the next line instead if sst file is in Kelvin.
c                     tstar(i,j)=tstar3(i,j)-abszero
                     albedo(i,j)=albedo_ocean
                  endif
               endif
         enddo
      enddo

c     See if we need to write a restart....

      if (mod(istep,dt_restart_fixedocean).eq.0) then

      write(datestring,'(i7.7)') istep
      write(yearstring,'(i4.4)') iyear
      write(monthstring,'(i2.2)') imonth
      write(daystring,'(i2.2)') iday

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      fname=trim(outputdir_name)//
     :        '/fixedocean_restart_'//
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

      return
      end
