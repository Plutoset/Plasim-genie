c This code has timestep info hard-wired, which must be removed....
c It is pretty hoprrendous and is basically cobbled together so that
c ig_go_gs can restart properly......
c djl 31/5/2005

      subroutine genie_restarts(koverall,
     &   lrestart_genie,
     &   alon1_atm,alat1_atm,
     &   ocean_sensible_atm,
     &   ocean_latent_atm,
     &   ocean_stressx_atm,
     &   ocean_stressy_atm,
     &   ocean_sensibleinst_atm,
     &   ocean_stressxinst_atm,
     &   ocean_stressyinst_atm,
     &   ocean_evapinst_atm,
     &   ocean_latentinst_atm,
     &   ocean_tstarinst_atm,
     &   ocean_rough_atm,
     &   ocean_qstar_atm,
     &   ocean_salb_atm,
     &   tstar_atm,
     &   albedo_atm,
     &   restart_type)

      use precision
      use genie_global,only:genie_timestep
      use genie_control,only:ilon1_atm,ilat1_atm,
     &    nall,nmaxdims,nfiles
      use averages,only:fname_fluxrestart,dt_write,outputdir_name,
     &    av_alon1_atm,av_alat1_atm


      implicit none

      include 'netcdf.inc'

      integer restart_type
      integer koverall
      logical lrestart_genie

c     These are always at the same precision as the genie equivalents
      real alon1_atm(ilon1_atm),alat1_atm(ilat1_atm)

      real 
     &   ocean_sensible_atm(ilon1_atm,ilat1_atm),
     &   ocean_stressx_atm(ilon1_atm,ilat1_atm),
     &   ocean_stressy_atm(ilon1_atm,ilat1_atm),
     &   ocean_latent_atm(ilon1_atm,ilat1_atm),
     &   ocean_sensibleinst_atm(ilon1_atm,ilat1_atm),
     &   ocean_stressxinst_atm(ilon1_atm,ilat1_atm),
     &   ocean_stressyinst_atm(ilon1_atm,ilat1_atm),
     &   ocean_evapinst_atm(ilon1_atm,ilat1_atm),
     &   ocean_latentinst_atm(ilon1_atm,ilat1_atm),
     &   ocean_tstarinst_atm(ilon1_atm,ilat1_atm),
     &   ocean_rough_atm(ilon1_atm,ilat1_atm),
     &   ocean_qstar_atm(ilon1_atm,ilat1_atm),
     &   ocean_salb_atm(ilon1_atm,ilat1_atm),
     &   tstar_atm(ilon1_atm,ilat1_atm),
     &   albedo_atm(ilon1_atm,ilat1_atm)

      integer istep

c     NETCDF STUFF: 
      integer ncid

c     for data-writing
      integer ifirst
      data ifirst/1/
      save ifirst
      integer ifail
      logical lexist
      integer ioffset
      save ioffset
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
      real(rk_libutil1) :: apfrac
      real(rk_libutil1) :: doy
      real timestep
      save timestep
      real daysperyear
      parameter(daysperyear=360.0)
      integer mpth1,mpth2
      character fname*200
      integer status,nrecsid,ioffsetid

      real(rk_libnc1), dimension(ilon1_atm,ilat1_atm) ::
     &   av_ocean_sensibleinst_atm,
     &   av_ocean_stressxinst_atm,
     &   av_ocean_stressyinst_atm,
     &   av_ocean_evapinst_atm,
     &   av_ocean_latentinst_atm,
     &   av_ocean_tstarinst_atm,
     &   av_ocean_rough_atm,
     &   av_ocean_qstar_atm,
     &   av_ocean_salb_atm,
     &   av_tstar_atm,
     &   av_albedo_atm

      integer nlonid,nlatid
      integer nlongitid,nlatitid
      integer nsensibleid
      integer nstressxid
      integer nstressyid       
      integer nevapid
      integer nlatentid
      integer ntstarid
      integer nroughid
      integer nqstarid
      integer nsalbid
      integer ntstar_atmid
      integer nalbedoid
      integer dimpass(2)

      istep=koverall

      if (ifirst.eq.1) then

c     First of all, if this is the first call then read in the
c       restart file

         timestep=genie_timestep

         fname=trim(fname_fluxrestart)//'_date.nc'

c        READ IN THE RESTART FILE
         ifail=0
         if (fname.eq.'xxx') then
            print*,' Missing filename for main restart '
            ifail=1
         else
            inquire(file=trim(fname),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :           trim(fname)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         endif 

         print*,' main: Opening restart file for read: ',
     &      trim(fname)

         call open_file_nc(
     &      trim(fname),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         call close_file_nc(
     &      trim(fname),ncid)

         day=iday
         ioffset=mod(ioffset,360)

      ifirst=0

      endif  ! end if ifirst




c     Now try to read the fluxes.......

      if ( (restart_type.eq.0).or.
     &     (restart_type.eq.1.and.koverall.eq.1.and.
     &      lrestart_genie) ) then

         if (restart_type.eq.0) then
         fname=trim(fname_fluxrestart)//'_fluxes1.nc'
         endif
         if (restart_type.eq.1) then
         fname=trim(fname_fluxrestart)//'_fluxes2.nc'
         endif

c        READ IN THE RESTART FILE
         ifail=0
         if (fname.eq.'xxx') then
            print*,' Missing filenme for main restart '
            ifail=1
         else
            inquire(file=trim(fname),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :           trim(fname)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         endif 

         print*,' main: Opening restart file for read: ',
     &      trim(fname)

         call open_file_nc(
     &      trim(fname),ncid)
         call get2d_data_nc(ncid,'av_ocean_sensibleinst_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_sensibleinst_atm,ifail)
         call get2d_data_nc(ncid,'av_ocean_stressxinst_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_stressxinst_atm,ifail)
         call get2d_data_nc(ncid,'av_ocean_stressyinst_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_stressyinst_atm,ifail)
         call get2d_data_nc(ncid,'av_ocean_evapinst_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_evapinst_atm,ifail)
         call get2d_data_nc(ncid,'av_ocean_latentinst_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_latentinst_atm,ifail)
         call get2d_data_nc(ncid,'av_ocean_tstarinst_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_tstarinst_atm,ifail)
         call get2d_data_nc(ncid,'av_ocean_rough_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_rough_atm,ifail)
         call get2d_data_nc(ncid,'av_ocean_qstar_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_qstar_atm,ifail)
         call get2d_data_nc(ncid,'av_ocean_salb_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_ocean_salb_atm,ifail)
         if (restart_type.eq.0) then
         call get2d_data_nc(ncid,'av_tstar_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_tstar_atm,ifail)
         call get2d_data_nc(ncid,'av_albedo_atm',
     &      ilon1_atm,ilat1_atm,
     &      av_albedo_atm,ifail)
         endif
         call close_file_nc(
     &      trim(fname),ncid)

      ocean_sensibleinst_atm(:,:)=av_ocean_sensibleinst_atm(:,:)
      ocean_stressxinst_atm(:,:)=av_ocean_stressxinst_atm(:,:)
      ocean_stressyinst_atm(:,:)=av_ocean_stressyinst_atm(:,:)
      ocean_evapinst_atm(:,:)=av_ocean_evapinst_atm(:,:)
      ocean_latentinst_atm(:,:)=av_ocean_latentinst_atm(:,:)
      ocean_tstarinst_atm(:,:)=av_ocean_tstarinst_atm(:,:)
      ocean_rough_atm(:,:)=av_ocean_rough_atm(:,:)
      ocean_qstar_atm(:,:)=av_ocean_qstar_atm(:,:)
      ocean_salb_atm(:,:)=av_ocean_salb_atm(:,:)
      ocean_latent_atm(:,:)   = ocean_latentinst_atm(:,:)
      ocean_sensible_atm(:,:) = ocean_sensibleinst_atm(:,:)
      ocean_stressx_atm(:,:)  = ocean_stressxinst_atm(:,:)
      ocean_stressy_atm(:,:)  = ocean_stressyinst_atm(:,:)
         if (restart_type.eq.0) then
      tstar_atm(:,:)=av_tstar_atm(:,:)
      albedo_atm(:,:)=av_albedo_atm(:,:)
         endif

      endif ! end if restart_type=0 or 1 etc.


      iday=nint(day)
      doy=mod(istep*timestep/(24.0*60.0*60.0),daysperyear)
      if (mod(istep,720).eq.0) then
        print*,'genie_restarts thinks day of year is',doy
      endif
      CALL CALNDR(DOY,Mpth1,ApFRAC)
      MPth2=mpth1+1
      if (mpth2.eq.13) mpth2=1


      if (mod(nint(istep*timestep/(60.0*60.0)),dt_write).eq.0) then
      if (restart_type.gt.0) then

      write(yearstring,'(i4.4)') iyear
      write(monthstring,'(i2.2)') imonth
      write(daystring,'(i2.2)') iday


      if (restart_type.eq.1) then
      fname=trim(outputdir_name)//
     :        '/main_fluxes_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'_date.nc'
      print*,' Opening restart file for write: ',trim(fname)
      status=nf_create(trim(fname), nf_clobber, ncid)
      call f77_nc_handle_err(status)
      status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'ioffset',nf_int,1,nrecsid,ioffsetid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'iyear',nf_int,1,nrecsid,iyearid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'imonth',nf_int,1,nrecsid,imonthid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'iday',nf_int,1,nrecsid,idayid)
      call f77_nc_handle_err(status)
      status=nf_enddef(ncid)
      call f77_nc_handle_err(status)
      status=nf_put_var_int(ncid,ioffsetid,int(doy))
      call f77_nc_handle_err(status)
      status=nf_put_var_int(ncid,iyearid,iyear)
      call f77_nc_handle_err(status)
      status=nf_put_var_int(ncid,imonthid,imonth)
      call f77_nc_handle_err(status)
      status=nf_put_var_int(ncid,idayid,iday)
      call f77_nc_handle_err(status)
      status=nf_close(ncid)
      call f77_nc_handle_err(status)
      endif


      print*,'writing genie averages into directory',trim(
     :               outputdir_name)
      print*,'at time',istep*timestep/(60.*60.),
     :               '= multiple of ',dt_write      
      print*,'yearstring is',yearstring
      print*,'monthstring is',monthstring
      print*,'daystring is',daystring
      print*,'preparing atmos file'

      av_alon1_atm(:)=alon1_atm(:)
      av_alat1_atm(:)=alat1_atm(:)
      av_ocean_sensibleinst_atm(:,:)=ocean_sensibleinst_atm(:,:)
      av_ocean_stressxinst_atm(:,:)=ocean_stressxinst_atm(:,:)
      av_ocean_stressyinst_atm(:,:)=ocean_stressyinst_atm(:,:)
      av_ocean_evapinst_atm(:,:)=ocean_evapinst_atm(:,:)
      av_ocean_latentinst_atm(:,:)=ocean_latentinst_atm(:,:)
      av_ocean_tstarinst_atm(:,:)=ocean_tstarinst_atm(:,:)
      av_ocean_rough_atm(:,:)=ocean_rough_atm(:,:)
      av_ocean_qstar_atm(:,:)=ocean_qstar_atm(:,:)
      av_ocean_salb_atm(:,:)=ocean_salb_atm(:,:)
      if (restart_type.eq.1) then
      av_tstar_atm(:,:)=tstar_atm(:,:)
      av_albedo_atm(:,:)=albedo_atm(:,:)
      endif


      if (restart_type.eq.1) then
      fname=trim(outputdir_name)//
     :        '/main_fluxes_'//
     :        yearstring//'_'//monthstring//'_'//daystring//
     :        '_fluxes1.nc'
      endif
   
      if (restart_type.eq.2) then
      fname=trim(outputdir_name)//
     :        '/main_fluxes_'//
     :        yearstring//'_'//monthstring//'_'//daystring//
     :        '_fluxes2.nc'
      endif

      status=nf_create(trim(fname), nf_clobber, ncid)
      call f77_nc_handle_err(status)
      status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
      call f77_nc_handle_err(status)
      status=nf_def_dim(ncid, 'longitude',ilon1_atm,nlonid)
      call f77_nc_handle_err(status)
      status=nf_def_dim(ncid, 'latitude',ilat1_atm,nlatid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'longitude',nf_real,1,nlonid,nlongitid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'latitude',nf_real,1,nlatid,nlatitid)
      call f77_nc_handle_err(status)
      dimpass(1)=nlonid
      dimpass(2)=nlatid
      status=nf_def_var(ncid,'av_ocean_sensibleinst_atm',nf_double,2,
     &                     dimpass,
     &                     nsensibleid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_ocean_stressxinst_atm',nf_double,2,
     &                     dimpass,
     &                     nstressxid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_ocean_stressyinst_atm',nf_double,2,
     &                     dimpass,
     &                     nstressyid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_ocean_evapinst_atm',nf_double,2,
     &                     dimpass,
     &                     nevapid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_ocean_latentinst_atm',nf_double,2,
     &                     dimpass,
     &                     nlatentid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_ocean_tstarinst_atm',nf_double,2,
     &                     dimpass,
     &                     ntstarid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_ocean_rough_atm',nf_double,2,dimpass,
     &                     nroughid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_ocean_qstar_atm',nf_double,2,dimpass,
     &                     nqstarid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_ocean_salb_atm',nf_double,2,dimpass,
     &                     nsalbid)
      call f77_nc_handle_err(status)
      if (restart_type.eq.1) then
      status=nf_def_var(ncid,'av_tstar_atm',nf_double,2,dimpass,
     &                     ntstar_atmid)
      call f77_nc_handle_err(status)
      status=nf_def_var(ncid,'av_albedo_atm',nf_double,2,dimpass,
     &                     nalbedoid)
      call f77_nc_handle_err(status)
      endif
      status=nf_enddef(ncid)

      if (kind(av_alon1_atm).eq.4) then
      status=nf_put_var_real(ncid,nlongitid,av_alon1_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nlatitid,av_alat1_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nsensibleid,av_ocean_sensibleinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nstressxid,av_ocean_stressxinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nstressyid,av_ocean_stressyinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nevapid,av_ocean_evapinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nlatentid,av_ocean_latentinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,ntstarid,av_ocean_tstarinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nroughid,av_ocean_rough_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nqstarid,av_ocean_qstar_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nsalbid,av_ocean_salb_atm)
      call f77_nc_handle_err(status)
      if (restart_type.eq.1) then
      status=nf_put_var_real(ncid,ntstar_atmid,av_tstar_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_real(ncid,nalbedoid,av_albedo_atm)
      call f77_nc_handle_err(status)
      endif
      endif

      if (kind(av_alon1_atm).eq.8) then
      status=nf_put_var_double(ncid,nlongitid,av_alon1_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nlatitid,av_alat1_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nsensibleid,
     &           av_ocean_sensibleinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nstressxid,av_ocean_stressxinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nstressyid,av_ocean_stressyinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nevapid,av_ocean_evapinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nlatentid,av_ocean_latentinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,ntstarid,av_ocean_tstarinst_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nroughid,av_ocean_rough_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nqstarid,av_ocean_qstar_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nsalbid,av_ocean_salb_atm)
      call f77_nc_handle_err(status)
      if (restart_type.eq.1) then
      status=nf_put_var_double(ncid,ntstar_atmid,av_tstar_atm)
      call f77_nc_handle_err(status)
      status=nf_put_var_double(ncid,nalbedoid,av_albedo_atm)
      call f77_nc_handle_err(status)
      endif
      endif

      status=nf_close(ncid)
      call f77_nc_handle_err(status)

      endif   ! end restart type
      endif   ! end if mod

      if (restart_type.lt.2) then
      day=day+timestep/(24*60*60.)
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
      endif




      return
      end
