c This code has timestep info hard-wired, which must be removed....
      subroutine write_averages(istep,alon1_atm,alat1_atm,
     &                            alon1_ocn,alat1_ocn,
     &                            alon1_sic,alat1_sic,
     &        netsolar_atm,netsolar_ocn,netsolar_sic,
     &        netlong_atm,netlong_ocn,netlong_sic,
     &        sensible_atm,sensible_ocn,sensible_sic,
     &        latent_atm,latent_ocn,latent_sic,
     &        stressx_atm,stressx_ocn,stressx_sic,
     &        stressy_atm,stressy_ocn,stressy_sic,
     &        conductflux_atm,conductflux_ocn,conductflux_sic,
     &        evap_atm,evap_ocn,evap_sic,
     &        precip_atm,precip_ocn,precip_sic,
     &        runoff_atm,runoff_ocn,runoff_sic,
     &        waterflux_atm,waterflux_ocn,waterflux_sic,
     &        seaicefrac_atm,seaicefrac_ocn,seaicefrac_sic,
     &        tstar_atm,tstar_ocn,tstar_sic,
     &        albedo_atm,albedo_ocn,albedo_sic)

      use precision
      use genie_control
      use averages
c     for genie_timestep:
      use genie_global,only:genie_timestep

      implicit none

      include 'netcdf.inc'

c     These are always at the same precision as the genie equivalents
      real alon1_atm(ilon1_atm),alat1_atm(ilat1_atm),
     &        alon1_ocn(ilon1_ocn),alat1_ocn(ilat1_ocn),
     &        alon1_sic(ilon1_sic),alat1_sic(ilat1_sic)

      real netsolar_atm(ilon1_atm,ilat1_atm),
     &   netsolar_ocn(ilon1_ocn,ilat1_ocn),
     &   netsolar_sic(ilon1_sic,ilat1_sic),
     &   netlong_atm(ilon1_atm,ilat1_atm),
     &   netlong_ocn(ilon1_ocn,ilat1_ocn),
     &   netlong_sic(ilon1_sic,ilat1_sic),
     &   sensible_atm(ilon1_atm,ilat1_atm),
     &   sensible_ocn(ilon1_ocn,ilat1_ocn),
     &   sensible_sic(ilon1_sic,ilat1_sic),
     &   latent_atm(ilon1_atm,ilat1_atm),
     &   latent_ocn(ilon1_ocn,ilat1_ocn),
     &   latent_sic(ilon1_sic,ilat1_sic),
     &   stressx_atm(ilon1_atm,ilat1_atm),
     &   stressx_ocn(ilon1_ocn,ilat1_ocn),
     &   stressx_sic(ilon1_sic,ilat1_sic),
     &   stressy_atm(ilon1_atm,ilat1_atm),
     &   stressy_ocn(ilon1_ocn,ilat1_ocn),
     &   stressy_sic(ilon1_sic,ilat1_sic)

      real 
     &   conductflux_atm(ilon1_atm,ilat1_atm),
     &   conductflux_ocn(ilon1_ocn,ilat1_ocn),
     &   conductflux_sic(ilon1_sic,ilat1_sic)
    
      
      real evap_atm(ilon1_atm,ilat1_atm),
     &   evap_ocn(ilon1_ocn,ilat1_ocn),
     &   evap_sic(ilon1_sic,ilat1_sic),
     &   precip_atm(ilon1_atm,ilat1_atm),
     &   precip_ocn(ilon1_ocn,ilat1_ocn),
     &   precip_sic(ilon1_sic,ilat1_sic),
     &   runoff_atm(ilon1_atm,ilat1_atm),
     &   runoff_ocn(ilon1_ocn,ilat1_ocn),
     &   runoff_sic(ilon1_sic,ilat1_sic),
     &   waterflux_atm(ilon1_atm,ilat1_atm),
     &   waterflux_ocn(ilon1_ocn,ilat1_ocn),
     &   waterflux_sic(ilon1_sic,ilat1_sic)

      real seaicefrac_atm(ilon1_atm,ilat1_atm),
     &   seaicefrac_ocn(ilon1_ocn,ilat1_ocn),
     &   seaicefrac_sic(ilon1_sic,ilat1_sic),
     &   tstar_atm(ilon1_atm,ilat1_atm),
     &   tstar_ocn(ilon1_ocn,ilat1_ocn),
     &   tstar_sic(ilon1_sic,ilat1_sic),
     &   albedo_atm(ilon1_atm,ilat1_atm),
     &   albedo_ocn(ilon1_ocn,ilat1_ocn),
     &   albedo_sic(ilon1_sic,ilat1_sic)


      integer istep

      integer v

c     NETCDF STUFF: 
      integer ncid,loc_dim
      integer ndim,nvar,natts(nall),nattsvar(nall),
     :        vdims(nall),vadims(nmaxdims,nall),
     :        ndims(nall)
      character dimname(nall,nfiles)*200,varname(nall,nfiles)*200,
     :          attdimname(2,nmaxdims,nall)*200,
     :          attvarname(2,nmaxdims,nall)*200
      integer iddim(nall,nfiles),idvar(nall,nfiles)

c     for normalisation
      real dividor
      real rdividor

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

c     First of all, if this is the first call then read in the
c       restart file
      if (ifirst.eq.1) then

         timestep=kocn_loop*genie_timestep

c        READ IN THE RESTART FILE
         ifail=0
         if (fname_restart_main.eq.'xxx') then
            print*,' Missing filename for main restart '
            ifail=1
         else
            inquire(file=trim(fname_restart_main),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :           trim(fname_restart_main)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         endif 

         print*,' main: Opening restart file for read: ',
     &      trim(fname_restart_main)

         call open_file_nc(
     &      trim(fname_restart_main),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         call close_file_nc(
     &      trim(fname_restart_main),ncid)

         day=iday
         ioffset=mod(ioffset,360)

         ifirst=0

      endif

      dividor=real(dt_write/kocn_loop)
      rdividor=1.0/dividor

      iday=nint(day)
      doy=mod(istep*timestep/(24.0*60*60.),daysperyear)
      if (mod(istep,15).eq.0) then
        print*,'Main thinks day of year is',doy,iday
      endif
      CALL CALNDR(DOY,Mpth1,ApFRAC)
      MPth2=mpth1+1
      if (mpth2.eq.13) mpth2=1


      if (write_flag_atm) then

      av_netsolar_atm(:,:)=av_netsolar_atm(:,:)+netsolar_atm(:,:)
     &        *rdividor
      av_netlong_atm(:,:)=av_netlong_atm(:,:)+netlong_atm(:,:)
     &        *rdividor
      av_sensible_atm(:,:)=av_sensible_atm(:,:)+sensible_atm(:,:)
     &        *rdividor
      av_latent_atm(:,:)=av_latent_atm(:,:)+latent_atm(:,:)
     &        *rdividor
      av_stressx_atm(:,:)=av_stressx_atm(:,:)+stressx_atm(:,:)
     &        *rdividor
      av_stressy_atm(:,:)=av_stressy_atm(:,:)+stressy_atm(:,:)
     &        *rdividor
      av_conductflux_atm(:,:)=av_conductflux_atm(:,:)+
     &        conductflux_atm(:,:)
     &        *rdividor
      av_evap_atm(:,:)=av_evap_atm(:,:)+evap_atm(:,:)
     &        *rdividor
      av_precip_atm(:,:)=av_precip_atm(:,:)+precip_atm(:,:)
     &        *rdividor
      av_runoff_atm(:,:)=av_runoff_atm(:,:)+runoff_atm(:,:)
     &        *rdividor
      av_waterflux_atm(:,:)=av_waterflux_atm(:,:)+waterflux_atm(:,:)
     &        *rdividor
      av_seaicefrac_atm(:,:)=av_seaicefrac_atm(:,:)+seaicefrac_atm(:,:)
     &        *rdividor
      av_tstar_atm(:,:)=av_tstar_atm(:,:)+tstar_atm(:,:)
     &        *rdividor
      av_albedo_atm(:,:)=av_albedo_atm(:,:)+albedo_atm(:,:)
     &        *rdividor
      
      endif


      if (write_flag_ocn) then

      av_netsolar_ocn(:,:)=av_netsolar_ocn(:,:)+netsolar_ocn(:,:)
     &        *rdividor
      av_netlong_ocn(:,:)=av_netlong_ocn(:,:)+netlong_ocn(:,:)
     &        *rdividor
      av_sensible_ocn(:,:)=av_sensible_ocn(:,:)+sensible_ocn(:,:)
     &        *rdividor
      av_latent_ocn(:,:)=av_latent_ocn(:,:)+latent_ocn(:,:)
     &        *rdividor
      av_stressx_ocn(:,:)=av_stressx_ocn(:,:)+stressx_ocn(:,:)
     &        *rdividor
      av_stressy_ocn(:,:)=av_stressy_ocn(:,:)+stressy_ocn(:,:)
     &        *rdividor
      av_conductflux_ocn(:,:)=av_conductflux_ocn(:,:)+
     &        conductflux_ocn(:,:)
     &        *rdividor
      av_evap_ocn(:,:)=av_evap_ocn(:,:)+evap_ocn(:,:)
     &        *rdividor
      av_precip_ocn(:,:)=av_precip_ocn(:,:)+precip_ocn(:,:)
     &        *rdividor
      av_runoff_ocn(:,:)=av_runoff_ocn(:,:)+runoff_ocn(:,:)
     &        *rdividor
      av_waterflux_ocn(:,:)=av_waterflux_ocn(:,:)+waterflux_ocn(:,:)
     &        *rdividor
      av_seaicefrac_ocn(:,:)=av_seaicefrac_ocn(:,:)+seaicefrac_ocn(:,:)
     &        *rdividor
      av_tstar_ocn(:,:)=av_tstar_ocn(:,:)+tstar_ocn(:,:)
     &        *rdividor
      av_albedo_ocn(:,:)=av_albedo_ocn(:,:)+albedo_ocn(:,:)
     &        *rdividor

      endif


      if (write_flag_sic) then

      av_netsolar_sic(:,:)=av_netsolar_sic(:,:)+netsolar_sic(:,:)
     &        *rdividor
      av_netlong_sic(:,:)=av_netlong_sic(:,:)+netlong_sic(:,:)
     &        *rdividor
      av_sensible_sic(:,:)=av_sensible_sic(:,:)+sensible_sic(:,:)
     &        *rdividor
      av_latent_sic(:,:)=av_latent_sic(:,:)+latent_sic(:,:)
     &        *rdividor
      av_stressx_sic(:,:)=av_stressx_sic(:,:)+stressx_sic(:,:)
     &        *rdividor
      av_stressy_sic(:,:)=av_stressy_sic(:,:)+stressy_sic(:,:)
     &        *rdividor
      av_conductflux_sic(:,:)=av_conductflux_sic(:,:)+
     &        conductflux_sic(:,:)
     &        *rdividor
      av_evap_sic(:,:)=av_evap_sic(:,:)+evap_sic(:,:)
     &        *rdividor
      av_precip_sic(:,:)=av_precip_sic(:,:)+precip_sic(:,:)
     &        *rdividor
      av_runoff_sic(:,:)=av_runoff_sic(:,:)+runoff_sic(:,:)
     &        *rdividor
      av_waterflux_sic(:,:)=av_waterflux_sic(:,:)+waterflux_sic(:,:)
     &        *rdividor
      av_seaicefrac_sic(:,:)=av_seaicefrac_sic(:,:)+seaicefrac_sic(:,:)
     &        *rdividor
      av_tstar_sic(:,:)=av_tstar_sic(:,:)+tstar_sic(:,:)
     &        *rdividor
      av_albedo_sic(:,:)=av_albedo_sic(:,:)+albedo_sic(:,:)
     &        *rdividor

      endif

c      if (mod(nint(istep*timestep/(60.*60.)),dt_write).eq.0) then
      if (mod(istep*kocn_loop,dt_write).eq.0) then

      write(yearstring,'(i4.4)') iyear
      write(monthstring,'(i2.2)') imonth
      write(daystring,'(i2.2)') iday

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      fname=trim(outputdir_name)//
     :        '/main_restart_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'.nc'
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

      if (write_flag_atm.or.write_flag_ocn.or.write_flag_sic) then

      print*,'writing genie averages into directory',trim(
     :               outputdir_name)
      print*,'at time',istep*timestep/(60.*60.),
     :               '= multiple of ',dt_write      

      print*,'yearstring is',yearstring
      print*,'monthstring is',monthstring
      print*,'daystring is',daystring

      print*,'precision of netcdf library is',rk_libnc1

      av_alon1_atm(:)=alon1_atm(:)
      av_alon1_ocn(:)=alon1_ocn(:)
      av_alon1_sic(:)=alon1_sic(:)
      av_alat1_atm(:)=alat1_atm(:)
      av_alat1_ocn(:)=alat1_ocn(:)
      av_alat1_sic(:)=alat1_sic(:)

      endif


      if (write_flag_atm) then
      print*,'preparing atmos file'

      ndim=2
      dimname(1,1)='longitude'
      ndims(1)=ilon1_atm
      natts(1)=2
      attdimname(1,1,1)='long_name'
      attdimname(2,1,1)='longitude'
      attdimname(1,2,1)='units'
      attdimname(2,2,1)='degrees east'

      dimname(2,1)='latitude'
      ndims(2)=ilat1_atm
      natts(2)=2
      attdimname(1,1,2)='long_name'
      attdimname(2,1,2)='latitude'
      attdimname(1,2,2)='units'
      attdimname(2,2,2)='degrees north'

      nvar=14
      v=1
      varname(v,1)='netsolar'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='net solar flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='netlong'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='net long wave flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='sensible'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='sensible heat flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='latent'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='latent heat flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='stressx'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='u wind stress'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Nm-2'

      v=v+1
      varname(v,1)='stressy'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='v wind stress'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Nm-2'

      v=v+1
      varname(v,1)='conductflux'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='flux between the seaice and ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='seaicefrac'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='seaice fraction'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='0-1'

      v=v+1
      varname(v,1)='tstar'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='surface temperature'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='K'

      v=v+1
      varname(v,1)='albedo'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='albedo'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='0-1'

      v=v+1
      varname(v,1)='evap'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='evaporation'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='precip'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='precipitation'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='runoff'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='run off from land to ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='waterflux'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='fresh water flux from seaice to ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      fname=trim(outputdir_name)//
     :        '/genie_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'_atm.nc'
      call ininc(trim(fname),
     :     nmaxdims,ndim,nvar,
     :     natts,nattsvar,
     :     vdims,vadims,ndims,
     :     dimname(1,1),varname(1,1),
     :     attdimname,attvarname,
     :     ncid,iddim(1,1),idvar(1,1))
      call writedim(ncid,iddim(1,1),av_alon1_atm)
      call writedim(ncid,iddim(2,1),av_alat1_atm)
      call writevar(ncid,
     :        idvar(loc_dim('netsolar',varname(1,1),nall),1),
     :        av_netsolar_atm )
      call writevar(ncid,
     :        idvar(loc_dim('netlong',varname(1,1),nall),1),
     :        av_netlong_atm )
      call writevar(ncid,
     :        idvar(loc_dim('sensible',varname(1,1),nall),1),
     :        av_sensible_atm )
      call writevar(ncid,
     :        idvar(loc_dim('latent',varname(1,1),nall),1),
     :        av_latent_atm )
      call writevar(ncid,
     :        idvar(loc_dim('stressx',varname(1,1),nall),1),
     :        av_stressx_atm )
      call writevar(ncid,
     :        idvar(loc_dim('stressy',varname(1,1),nall),1),
     :        av_stressy_atm )
      call writevar(ncid,
     :        idvar(loc_dim('conductflux',varname(1,1),nall),1),
     :        av_conductflux_atm )
      call writevar(ncid,
     :        idvar(loc_dim('evap',varname(1,1),nall),1),
     :        av_evap_atm )
      call writevar(ncid,
     :        idvar(loc_dim('precip',varname(1,1),nall),1),
     :        av_precip_atm )
      call writevar(ncid,
     :        idvar(loc_dim('runoff',varname(1,1),nall),1),
     :        av_runoff_atm )
      call writevar(ncid,
     :        idvar(loc_dim('waterflux',varname(1,1),nall),1),
     :        av_waterflux_atm )
      call writevar(ncid,
     :        idvar(loc_dim('seaicefrac',varname(1,1),nall),1),
     :        av_seaicefrac_atm )
      call writevar(ncid,
     :        idvar(loc_dim('tstar',varname(1,1),nall),1),
     :        av_tstar_atm )
      call writevar(ncid,
     :        idvar(loc_dim('albedo',varname(1,1),nall),1),
     :        av_albedo_atm )
      call closenc(ncid)

      endif


      if (write_flag_ocn) then

      print*,'preparing ocean file'

      ndim=2
      dimname(1,1)='longitude'
      ndims(1)=ilon1_ocn
      natts(1)=2
      attdimname(1,1,1)='long_name'
      attdimname(2,1,1)='longitude'
      attdimname(1,2,1)='units'
      attdimname(2,2,1)='degrees east'

      dimname(2,1)='latitude'
      ndims(2)=ilat1_ocn
      natts(2)=2
      attdimname(1,1,2)='long_name'
      attdimname(2,1,2)='latitude'
      attdimname(1,2,2)='units'
      attdimname(2,2,2)='degrees north'

      nvar=14
      v=1
      varname(v,1)='netsolar'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='net solar flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='netlong'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='net long wave flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='sensible'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='sensible heat flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='latent'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='latent heat flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='stressx'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='u wind stress'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Nm-2'

      v=v+1
      varname(v,1)='stressy'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='v wind stress'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Nm-2'

      v=v+1
      varname(v,1)='conductflux'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='flux between the seaice and ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='seaicefrac'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='seaice fraction'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='0-1'

      v=v+1
      varname(v,1)='tstar'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='surface temperature'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='K'

      v=v+1
      varname(v,1)='albedo'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='albedo'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='0-1'

      v=v+1
      varname(v,1)='evap'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='evaporation'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='precip'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='precipitation'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='runoff'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='run off from land to ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='waterflux'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='fresh water flux from seaice to ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      fname=trim(outputdir_name)//
     :        '/genie_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'_ocn.nc'
      call ininc(trim(fname),
     :     nmaxdims,ndim,nvar,
     :     natts,nattsvar,
     :     vdims,vadims,ndims,
     :     dimname(1,1),varname(1,1),
     :     attdimname,attvarname,
     :     ncid,iddim(1,1),idvar(1,1))
      call writedim(ncid,iddim(1,1),av_alon1_ocn)
      call writedim(ncid,iddim(2,1),av_alat1_ocn)
      call writevar(ncid,
     :        idvar(loc_dim('netsolar',varname(1,1),nall),1),
     :        av_netsolar_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('netlong',varname(1,1),nall),1),
     :        av_netlong_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('sensible',varname(1,1),nall),1),
     :        av_sensible_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('latent',varname(1,1),nall),1),
     :        av_latent_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('stressx',varname(1,1),nall),1),
     :        av_stressx_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('stressy',varname(1,1),nall),1),
     :        av_stressy_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('conductflux',varname(1,1),nall),1),
     :        av_conductflux_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('evap',varname(1,1),nall),1),
     :        av_evap_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('precip',varname(1,1),nall),1),
     :        av_precip_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('runoff',varname(1,1),nall),1),
     :        av_runoff_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('waterflux',varname(1,1),nall),1),
     :        av_waterflux_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('seaicefrac',varname(1,1),nall),1),
     :        av_seaicefrac_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('tstar',varname(1,1),nall),1),
     :        av_tstar_ocn )
      call writevar(ncid,
     :        idvar(loc_dim('albedo',varname(1,1),nall),1),
     :        av_albedo_ocn )
      call closenc(ncid)

      endif


      if (write_flag_sic) then

      print*,'preparing sea-ice file'

      ndim=2
      dimname(1,1)='longitude'
      ndims(1)=ilon1_sic
      natts(1)=2
      attdimname(1,1,1)='long_name'
      attdimname(2,1,1)='longitude'
      attdimname(1,2,1)='units'
      attdimname(2,2,1)='degrees east'

      dimname(2,1)='latitude'
      ndims(2)=ilat1_sic
      natts(2)=2
      attdimname(1,1,2)='long_name'
      attdimname(2,1,2)='latitude'
      attdimname(1,2,2)='units'
      attdimname(2,2,2)='degrees north'

      nvar=14
      v=1
      varname(v,1)='netsolar'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='net solar flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='netlong'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='net long wave flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='sensible'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='sensible heat flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='latent'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='latent heat flux'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='stressx'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='u wind stress'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Nm-2'

      v=v+1
      varname(v,1)='stressy'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='v wind stress'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Nm-2'

      v=v+1
      varname(v,1)='conductflux'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='flux between the seaice and ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='Wm-2'

      v=v+1
      varname(v,1)='seaicefrac'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='seaice fraction'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='0-1'

      v=v+1
      varname(v,1)='tstar'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='surface temperature'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='K'

      v=v+1
      varname(v,1)='albedo'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='albedo'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='0-1'

      v=v+1
      varname(v,1)='evap'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='evaporation'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='precip'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='precipitation'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='runoff'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='run off from land to ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      v=v+1
      varname(v,1)='waterflux'
      vdims(v)=2
      vadims(1,v)=loc_dim('longitude',dimname,nall)
      vadims(2,v)=loc_dim('latitude',dimname,nall)
      nattsvar(v)=2
      attvarname(1,1,v)='long_name'
      attvarname(2,1,v)='fresh water flux from seaice to ocean'
      attvarname(1,2,v)='units'
      attvarname(2,2,v)='kgm2s-1 (I guess, need to check this)'

      fname=trim(outputdir_name)//
     :        '/genie_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'_sic.nc'
      call ininc(trim(fname),
     :     nmaxdims,ndim,nvar,
     :     natts,nattsvar,
     :     vdims,vadims,ndims,
     :     dimname(1,1),varname(1,1),
     :     attdimname,attvarname,
     :     ncid,iddim(1,1),idvar(1,1))
      call writedim(ncid,iddim(1,1),av_alon1_sic)
      call writedim(ncid,iddim(2,1),av_alat1_sic)
      call writevar(ncid,
     :        idvar(loc_dim('netsolar',varname(1,1),nall),1),
     :        av_netsolar_sic )
      call writevar(ncid,
     :        idvar(loc_dim('netlong',varname(1,1),nall),1),
     :        av_netlong_sic )
      call writevar(ncid,
     :        idvar(loc_dim('sensible',varname(1,1),nall),1),
     :        av_sensible_sic )
      call writevar(ncid,
     :        idvar(loc_dim('latent',varname(1,1),nall),1),
     :        av_latent_sic )
      call writevar(ncid,
     :        idvar(loc_dim('stressx',varname(1,1),nall),1),
     :        av_stressx_sic )
      call writevar(ncid,
     :        idvar(loc_dim('stressy',varname(1,1),nall),1),
     :        av_stressy_sic )
      call writevar(ncid,
     :        idvar(loc_dim('conductflux',varname(1,1),nall),1),
     :        av_conductflux_sic )
      call writevar(ncid,
     :        idvar(loc_dim('evap',varname(1,1),nall),1),
     :        av_evap_sic )
      call writevar(ncid,
     :        idvar(loc_dim('precip',varname(1,1),nall),1),
     :        av_precip_sic )
      call writevar(ncid,
     :        idvar(loc_dim('runoff',varname(1,1),nall),1),
     :        av_runoff_sic )
      call writevar(ncid,
     :        idvar(loc_dim('waterflux',varname(1,1),nall),1),
     :        av_waterflux_sic )
      call writevar(ncid,
     :        idvar(loc_dim('seaicefrac',varname(1,1),nall),1),
     :        av_seaicefrac_sic )
      call writevar(ncid,
     :        idvar(loc_dim('tstar',varname(1,1),nall),1),
     :        av_tstar_sic )
      call writevar(ncid,
     :        idvar(loc_dim('albedo',varname(1,1),nall),1),
     :        av_albedo_sic )
      call closenc(ncid)

      endif


      if (write_flag_atm) then
      av_netsolar_atm(:,:)=0.0
      av_netlong_atm(:,:)=0.0
      av_sensible_atm(:,:)=0.0
      av_latent_atm(:,:)=0.0
      av_stressx_atm(:,:)=0.0
      av_stressy_atm(:,:)=0.0
      av_conductflux_atm(:,:)=0.0
      av_evap_atm(:,:)=0.0
      av_precip_atm(:,:)=0.0
      av_runoff_atm(:,:)=0.0
      av_waterflux_atm(:,:)=0.0
      av_seaicefrac_atm(:,:)=0.0
      av_tstar_atm(:,:)=0.0
      av_albedo_atm(:,:)=0.0
      endif

      if (write_flag_ocn) then
      av_netsolar_ocn(:,:)=0.0
      av_netlong_ocn(:,:)=0.0
      av_sensible_ocn(:,:)=0.0
      av_latent_ocn(:,:)=0.0
      av_stressx_ocn(:,:)=0.0
      av_stressy_ocn(:,:)=0.0
      av_conductflux_ocn(:,:)=0.0
      av_evap_ocn(:,:)=0.0
      av_precip_ocn(:,:)=0.0
      av_runoff_ocn(:,:)=0.0
      av_waterflux_ocn(:,:)=0.0
      av_seaicefrac_ocn(:,:)=0.0
      av_tstar_ocn(:,:)=0.0
      av_albedo_ocn(:,:)=0.0
      endif

      if (write_flag_sic) then
      av_netsolar_sic(:,:)=0.0
      av_netlong_sic(:,:)=0.0
      av_sensible_sic(:,:)=0.0
      av_latent_sic(:,:)=0.0
      av_stressx_sic(:,:)=0.0
      av_stressy_sic(:,:)=0.0
      av_conductflux_sic(:,:)=0.0
      av_evap_sic(:,:)=0.0
      av_precip_sic(:,:)=0.0
      av_runoff_sic(:,:)=0.0
      av_waterflux_sic(:,:)=0.0
      av_seaicefrac_sic(:,:)=0.0
      av_tstar_sic(:,:)=0.0
      av_albedo_sic(:,:)=0.0
      endif

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

c     simple error handler
      subroutine f77_nc_handle_err(iret)
      implicit none
      include 'netcdf.inc'
      integer iret
c     
      if (iret.ne.0) then
         print*,' error in netcdf ',iret
         PRINT *, NF_STRERROR(iret)
         stop 1
      end if
c     
      return
      end
