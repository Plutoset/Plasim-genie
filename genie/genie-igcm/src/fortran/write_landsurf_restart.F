      subroutine write_landsurf_restart(itspd,istep,
     :     surfhsnow_atm,
     :     surfsmstar_atm,
     :     surfsqstar_atm,
     :     surftdeep_atm,surftdeepo_atm,
     :     surftstar_atm,surftstaro_atm,
     :     surfsalb_atm,
     :     surfnetsolar_atm,surfnetlong_atm,
     :     surfprec_atm
     :                             )

      implicit none

c     This module writes restarts for the land part
c       of the igcm.
c     IT COULD BE MADE A LOT FATSER BY NEGEKCTING THE INTERMEDIATE VARIABLES
c       AND JUST USING *_ATM

#include "igcm_land_resolution.inc"

      integer i,j

#include "precision.inc"
      include 'netcdf.inc'
      include 'igcm_lsurf.cmn'

      real :: surfhsnow_atm(mg,jgg)
      real :: surfsmstar_atm(mg,jgg)
      real :: surfsqstar_atm(mg,jgg)
      real :: surftdeep_atm(mg,jgg)
      real :: surftdeepo_atm(mg,jgg)
      real :: surftstar_atm(mg,jgg)
      real :: surftstaro_atm(mg,jgg)
      real(rk_in) :: surfsalb_atm(mg,jgg)
      real(rk_in) :: surfnetsolar_atm(mg,jgg)
      real(rk_in) :: surfnetlong_atm(mg,jgg)
      real(rk_in) :: surfprec_atm(mg,jgg)
      integer itspd

      real surfhsnow(mg,jgg)
      real surfsmstar(mg,jgg)
      real surfsqstar(mg,jgg)
      real surftdeep(mg,jgg)
      real surftdeepo(mg,jgg)
      real surftstar(mg,jgg)
      real surftstaro(mg,jgg)
      real surfsalb(mg,jgg)
      real surfnetsolar(mg,jgg)
      real surfnetlong(mg,jgg)
      real surfprec(mg,jgg)

c     For date and restarts...
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
      integer ifname_restart
      integer ilen
      character outputdir_name*200
      integer nhsnowid
      integer nsmstarid
      integer nsqstarid
      integer ntdeepid,ntdeepoid
      integer ntstarid,ntstaroid
      integer nsalbid
      integer nnetsolarid,nnetlongid
      integer nprecid
      character datestring*7
      integer istep
      character fname*200
      integer status,nrecsid,ioffsetid
      integer nlonid,nlongitid,nlatid,nlatitid
      integer dimpass(2)
      real lons(mg),lats(jgg)
      save lons
      save lats
      integer ncid
      integer lnsig,lnsig1
      integer ifirst
      data ifirst/1/
      save ifirst
      integer ioffset
      save ioffset
      real(rk_libutil1) :: doy
      real timestep
      save timestep
      real daysperyear
      parameter(daysperyear=360.0)
      integer ifail
      logical lexist


      if (ifirst.eq.1) then

         timestep=24.0*3600.0/real(itspd)

         ifirst=0

c     set up dummy longitudes and latitudes
         do i=1,mg
         lons(i)=real(i)*5.625
         enddo

         do j=1,jgg
         lats(j)=real(j)*5.625
         enddo

         ifail=0
         if (fname_restart_igcmlandsurf.eq.'xxx') then
            print*,' Missing filename for igcmlandsurf restart '
            ifail=1
         else
            ifname_restart=lnsig1(fname_restart_igcmlandsurf)
            inquire(file=fname_restart_igcmlandsurf(1:ifname_restart),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :           fname_restart_igcmlandsurf(1:ifname_restart)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         endif

         print*,' igcmlandsurf: Opening restart file for read: ',
     &      fname_restart_igcmlandsurf(1:ifname_restart)

         call open_file_nc(
     &      fname_restart_igcmlandsurf(1:ifname_restart),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)

         if (coldstart_lsurf) then
         surftdeep(:,:)=12.0
         surftdeepo(:,:)=12.0
         surftstar(:,:)=12.0
         surftstaro(:,:)=12.0
         surfsalb(:,:)=0.15
         else
         call get2d_data_nc(ncid,'hsnow',mg,jgg,surfhsnow,ifail)
         call get2d_data_nc(ncid,'smstar',mg,jgg,surfsmstar,ifail)
         call get2d_data_nc(ncid,'sqstar',mg,jgg,surfsqstar,ifail)
         call get2d_data_nc(ncid,'tdeep',mg,jgg,surftdeep,ifail)
         call get2d_data_nc(ncid,'tdeepo',mg,jgg,surftdeepo,ifail)
         call get2d_data_nc(ncid,'tstar',mg,jgg,surftstar,ifail)
         call get2d_data_nc(ncid,'tstaro',mg,jgg,surftstaro,ifail)
         call get2d_data_nc(ncid,'salb',mg,jgg,surfsalb,ifail)
         call get2d_data_nc(ncid,'netsolar',mg,jgg,surfnetsolar,ifail)
         call get2d_data_nc(ncid,'netlong',mg,jgg,surfnetlong,ifail)
         call get2d_data_nc(ncid,'prec',mg,jgg,surfprec,ifail)
         endif
c
         call close_file_nc(
     &      fname_restart_igcmlandsurf(1:ifname_restart),ncid)

         day=iday

      do j=1,jgg
        do i=1,mg
         surfhsnow_atm(i,j)=surfhsnow(i,j)
         surfsmstar_atm(i,j)=surfsmstar(i,j)
         surfsqstar_atm(i,j)=surfsqstar(i,j)
         surftdeep_atm(i,j)=surftdeep(i,j)
         surftdeepo_atm(i,j)=surftdeepo(i,j)
         surftstar_atm(i,j)=surftstar(i,j)
         surftstaro_atm(i,j)=surftstaro(i,j)
         surfsalb_atm(i,j)=surfsalb(i,j)
         surfnetsolar_atm(i,j)=surfnetsolar(i,j)
         surfnetlong_atm(i,j)=surfnetlong(i,j)
         surfprec_atm(i,j)=surfprec(i,j)
        enddo
      enddo

         ioffset=mod(ioffset,360)

      end if
c     
      if (mod(istep,dt_restart_igcmlandsurf).eq.0) then

      print*,'I am now going to write a landsurface restart'
      print*,'Because mod(',istep,',',dt_restart_igcmlandsurf,'=0'

c     These statements are needed to be in i,j 
      do j=1,jgg
        do i=1,mg
          surfhsnow(i,j)=surfhsnow_atm(i,j)
          surfsmstar(i,j)=surfsmstar_atm(i,j)
          surfsqstar(i,j)=surfsqstar_atm(i,j)
          surftdeep(i,j)=surftdeep_atm(i,j)
          surftdeepo(i,j)=surftdeepo_atm(i,j)
          surftstar(i,j)=surftstar_atm(i,j)
          surftstaro(i,j)=surftstaro_atm(i,j)
          surfsalb(i,j)=surfsalb_atm(i,j)
          surfnetsolar(i,j)=surfnetsolar_atm(i,j)
          surfnetlong(i,j)=surfnetlong_atm(i,j)
          surfprec(i,j)=surfprec_atm(i,j)
        enddo
      enddo


      iday=nint(day)
      doy=mod(ioffset+istep*timestep/(24*60*60.),daysperyear)
      print*,'land restarts thinks day of year (doy,day) is',doy,day  

c     WRITE A RESTART.....

      write(datestring,'(i7.7)') istep
      write(yearstring,'(i4.4)') iyear
      write(monthstring,'(i2.2)') imonth
      write(daystring,'(i2.2)') iday

      print*,'Landsurface:'
      print*,datestring,yearstring,monthstring,daystring

      outputdir_name=outputdir_name_igcmlandsurf

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      ilen=lnsig1(outputdir_name)
      fname=outputdir_name(1:ilen)//
     :        '/igcmlandsurf_restart_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'.nc'
      ilen=lnsig(fname)
      print*,' Opening restart file for write: ',fname(1:ilen)
      status=nf_create(fname(1:ilen), nf_clobber, ncid)
      call check_err(status)
      status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
      call check_err(status)
      status=nf_def_dim(ncid, 'longitude',mg,nlonid)
      call check_err(status)
      status=nf_def_dim(ncid, 'latitude',jgg,nlatid)
      call check_err(status)
      status=nf_def_var(ncid,'longitude',nf_real,1,nlonid,nlongitid)
      call check_err(status)
      status=nf_def_var(ncid,'latitude',nf_real,1,nlatid,nlatitid)
      call check_err(status)
      dimpass(1)=nlonid
      dimpass(2)=nlatid
      status=nf_def_var(ncid,'ioffset',nf_int,1,nrecsid,ioffsetid)
      call check_err(status)
      status=nf_def_var(ncid,'iyear',nf_int,1,nrecsid,iyearid)
      call check_err(status)
      status=nf_def_var(ncid,'imonth',nf_int,1,nrecsid,imonthid)
      call check_err(status)
      status=nf_def_var(ncid,'iday',nf_int,1,nrecsid,idayid)
      call check_err(status)

      status=nf_def_var(ncid,'hsnow',nf_double,2,dimpass,
     &                     nhsnowid)
      call check_err(status)
      status=nf_def_var(ncid,'smstar',nf_double,2,dimpass,
     &                     nsmstarid)
      call check_err(status)
      status=nf_def_var(ncid,'sqstar',nf_double,2,dimpass,
     &                     nsqstarid)
      call check_err(status)
      status=nf_def_var(ncid,'tdeep',nf_double,2,dimpass,
     &                     ntdeepid)
      call check_err(status)
      status=nf_def_var(ncid,'tdeepo',nf_double,2,dimpass,
     &                     ntdeepoid)
      call check_err(status)
      status=nf_def_var(ncid,'tstar',nf_double,2,dimpass,
     &                     ntstarid)
      call check_err(status)
      status=nf_def_var(ncid,'tstaro',nf_double,2,dimpass,
     &                     ntstaroid)
      call check_err(status)
      status=nf_def_var(ncid,'salb',nf_double,2,dimpass,
     &                     nsalbid)
      call check_err(status)
      status=nf_def_var(ncid,'netsolar',nf_double,2,dimpass,
     &                     nnetsolarid)
      call check_err(status)
      status=nf_def_var(ncid,'netlong',nf_double,2,dimpass,
     &                     nnetlongid)
      call check_err(status)
      status=nf_def_var(ncid,'prec',nf_double,2,dimpass,
     &                     nprecid)
      call check_err(status)

      status=nf_enddef(ncid)
      call check_err(status)

      status=nf_put_var_int(ncid,iyearid,iyear)
      call check_err(status)
      status=nf_put_var_int(ncid,imonthid,imonth)
      call check_err(status)
      status=nf_put_var_int(ncid,idayid,iday)
      call check_err(status)
      status=nf_put_var_int(ncid,ioffsetid,ioffset)
      call check_err(status)


      if (kind(lons).eq.4) then
      status=nf_put_var_real(ncid,nlongitid,lons)
      else if (kind(lons).eq.8) then
      status=nf_put_var_double(ncid,nlongitid,lons)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(lats).eq.4) then
      status=nf_put_var_real(ncid,nlatitid,lats)
      else if (kind(lats).eq.8) then
      status=nf_put_var_double(ncid,nlatitid,lats)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surfhsnow_atm).eq.4) then
        status=nf_put_var_real(ncid,nhsnowid,surfhsnow_atm)
      else if (kind(surfhsnow_atm).eq.8) then
        status=nf_put_var_double(ncid,nhsnowid,surfhsnow_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surfsmstar_atm).eq.4) then
        status=nf_put_var_real(ncid,nsmstarid,surfsmstar_atm)
      else if (kind(surfsmstar_atm).eq.8) then
        status=nf_put_var_double(ncid,nsmstarid,surfsmstar_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surfsqstar_atm).eq.4) then
        status=nf_put_var_real(ncid,nsqstarid,surfsqstar_atm)
      else if (kind(surfsqstar_atm).eq.8) then
        status=nf_put_var_double(ncid,nsqstarid,surfsqstar_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surftdeep_atm).eq.4) then
        status=nf_put_var_real(ncid,ntdeepid,surftdeep_atm)
      else if (kind(surftdeep_atm).eq.8) then
        status=nf_put_var_double(ncid,ntdeepid,surftdeep_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surftdeepo_atm).eq.4) then
        status=nf_put_var_real(ncid,ntdeepoid,surftdeepo_atm)
      else if (kind(surftdeepo_atm).eq.8) then
        status=nf_put_var_double(ncid,ntdeepoid,surftdeepo_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surftstar_atm).eq.4) then
        status=nf_put_var_real(ncid,ntstarid,surftstar_atm)
      else if (kind(surftstar_atm).eq.8) then
        status=nf_put_var_double(ncid,ntstarid,surftstar_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surftstaro_atm).eq.4) then
        status=nf_put_var_real(ncid,ntstaroid,surftstaro_atm)
      else if (kind(surftstaro_atm).eq.8) then
        status=nf_put_var_double(ncid,ntstaroid,surftstaro_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surfsalb_atm).eq.4) then
        status=nf_put_var_real(ncid,nsalbid,surfsalb_atm)
      else if (kind(surfsalb_atm).eq.8) then
        status=nf_put_var_double(ncid,nsalbid,surfsalb_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif 
      call check_err(status)

      if (kind(surfnetsolar_atm).eq.4) then
        status=nf_put_var_real(ncid,nnetsolarid,surfnetsolar_atm)
      else if (kind(surfnetsolar_atm).eq.8) then
        status=nf_put_var_double(ncid,nnetsolarid,surfnetsolar_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surfnetlong_atm).eq.4) then
        status=nf_put_var_real(ncid,nnetlongid,surfnetlong_atm)
      else if (kind(surfnetlong_atm).eq.8) then
        status=nf_put_var_double(ncid,nnetlongid,surfnetlong_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      if (kind(surfprec_atm).eq.4) then
        status=nf_put_var_real(ncid,nprecid,surfprec_atm)
      else if (kind(surfprec_atm).eq.8) then
        status=nf_put_var_double(ncid,nprecid,surfprec_atm)
      else
         print*,"ERROR: igcm write land restart var not prec 4/8" 
         stop
      endif
      call check_err(status)

      status=nf_close(ncid) 
      call check_err(status)    

      endif

      day=day+timestep/(24*60*60.)
c     set iday or it's undefined....
      iday=nint(day)
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
c      print*,'day in landsurface restart is now',day
c      print*,'iday in landsurface restart is now',iday

      return
      end
