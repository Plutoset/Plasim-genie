      subroutine slabocean(istep,
     :     tstar_atm,latent_atm,sensible_atm,
     :     netsolar_atm,netlong_atm,
     :     prec_atm,
     :     evap_atm,
     :     runoff_atm,
     :     seaice_frac_atm,
     :     temptop_atm,
     :     conductflux_atm,
     :     albedo_atm,iland,
     :     test_energy_ocean,
     :     test_water_ocean)
      
      use genie_util, only: check_unit

      implicit none

#include "precision.inc"

      include 'slabocean_com.cmn'
      include 'netcdf.inc'

c     NEED TO PUT TIMESTEP INFO INTO JOB FILE!! 

      integer mg,jg
c     mg is longitudes, jg is latitudes per hemisphere
      parameter(mg=64,jg=16)

      integer nhem
c     nhem is number of hemispheres
      parameter(nhem=2)

      integer jgg
c     jgg is total latitudes
      parameter(jgg=nhem*jg)

      real timestep
c     Two days is the ocean timestep.  Timestep is in seconds.
      parameter(timestep=2.*24.*60.*60.)

      real daysperyear
      parameter(daysperyear=360.0)

      real abszero
      parameter(abszero=273.16)

c     These set up some constants for the slab
      real slab_depth
c     depth of the slab ocean (metres) 
      parameter(slab_depth=25.)
      real shco
c     heat capacity per unit area = depth*density*specific heat capacity
      parameter(shco=slab_depth*1000.*4190.)

      real lonsedge(mg+1),latsedge(jgg+1)
      real lons(mg),lats(jgg)
      save lons
      save lats
      save lonsedge
      save latsedge

      integer nlonid,nlongitid,nlatid,nlatitid,nmonid,nmonthid
      integer nseaice_fracid
      integer nsensibleid,nlatentid,nnetsolarid,nnetlongid
      integer ntstarid
      integer nalbedoid
      integer ntemptopid
      integer nfluxid
      integer dimpass_2(2)
      integer dimpass_3(3)
      character datestring*7

c     ****PASSED TO/FROM THIS ROUTINE
      integer istep
      real(rk_in) :: tstar_atm(mg,jgg)
      real(rk_in) :: latent_atm(mg,jgg)
      real(rk_in) :: sensible_atm(mg,jgg)
      real(rk_in) :: netsolar_atm(mg,jgg)
      real(rk_in) :: netlong_atm(mg,jgg)
      real(rk_in) :: prec_atm(mg,jgg)
      real(rk_in) :: evap_atm(mg,jgg)
      real(rk_in) :: runoff_atm(mg,jgg)
      real(rk_in) :: seaice_frac_atm(mg,jgg)
      real(rk_in) :: temptop_atm(mg,jgg)
      real(rk_in) :: conductflux_atm(mg,jgg)
      real(rk_in) :: albedo_atm(mg,jgg)
      integer iland(mg,jgg)

c     ****BUFFER
      real :: tstar(mg,jgg)
      real :: latent(mg,jgg)
      real :: sensible(mg,jgg)
      real :: netsolar(mg,jgg)
      real :: netlong(mg,jgg)
      real :: seaice_frac(mg,jgg)
      real :: temptop(mg,jgg)
      real :: conductflux(mg,jgg)
      real :: albedo(mg,jgg)

c     ****FOR THIS ROUTINE****
c     
      character fname_ocflux*200
      character fname_siflux*200

      real ocflux(mg,jgg,12)
      real siflux(mg,jgg,12)
      save ocflux,siflux

      character fname_tempref*200
      real tempref(mg,jgg,12) 

      character fname_restart_slabocean*200

      character outputdir_name*200

      integer dt_restart_slabocean
      save dt_restart_slabocean
    
      real tref
      real ocf
      real sfc
      real dts
      integer ncid
      integer mpth1,mpth2

      real(rk_libutil1) :: apfrac
      real(rk_libutil1) :: doy

      integer j,i,ifail,ifirst
      logical lexist
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

c     For energy diagnostic
      real tot_energy
      real ini_energy
      save ini_energy
      real tot_water
      real ini_water
      save tot_water,ini_water
      real ax
      real weightcheck
      real pi
      save pi
      real weight(mg,jgg)
      save weight
c     radea must be 6370 to agree with goldstein!!!!
      real radea
      parameter(radea=6370e3)

      real(rk_in) test_energy_ocean
      real(rk_in) test_water_ocean

c     For slab-maker
      logical slab_maker
      save slab_maker
      integer start_slab_av
      integer end_slab_av
      save start_slab_av,end_slab_av
      real ocf_new
      real ocflux_new(mg,jgg,12) 
      save ocflux_new
      real ocflux_count(12)
      save ocflux_count  
      logical doav
      integer m
      real months(12)

      namelist/slabocean_control/
     :     fname_ocflux,fname_siflux,
     :     fname_tempref,fname_restart_slabocean,
     :                             outputdir_name,
     :                             dt_restart_slabocean,
     :                             albedo_ocean,
     :                             flag_checkenergy_slabocean,
     :                             flag_checkwater_slabocean,
     :                             slab_maker,
     :                             start_slab_av,
     :                             end_slab_av


      tstar(:,:)=tstar_atm(:,:)
      latent(:,:)=latent_atm(:,:)
      sensible(:,:)=sensible_atm(:,:)
      netsolar(:,:)=netsolar_atm(:,:)
      netlong(:,:)=netlong_atm(:,:)
      seaice_frac(:,:)=seaice_frac_atm(:,:)
      temptop(:,:)=temptop_atm(:,:)
      conductflux(:,:)=conductflux_atm(:,:)
      albedo(:,:)=albedo_atm(:,:)

c     This bit is to just read in the advective ocean heat flux from 
c     a file, and put it into a 3-d variable ocflux (i,j,t) 
c     (t is months). 

      if (ifirst.eq.1) then

      ocflux_new(:,:,:)=0.0
      ocflux_count(:)=0.0

      pi=4.0*atan(1.0)

c     set up dummy longitudes and latitudes
      ax=360.0/real(mg)
      do i=1,mg
         lons(i)=(i-1.0)*ax
         lonsedge(i)=(i-1.5)*ax
      end do
      lonsedge(mg+1)=(mg-0.5)*ax

      call gwtcnr(lats,jg)
      call gwtbox(latsedge,jg)

      weightcheck=0.0     
      do j=1,jgg
        do i=1,mg
          weight(i,j)=(sin(latsedge(j)*2*pi/360.)-
     &              sin(latsedge(j+1)*2*pi/360.))*
     &              (lonsedge(i+1)/360.-
     &              lonsedge(i)/360.)/2.
          weightcheck=weightcheck+weight(i,j)
        enddo
      enddo
      print*,'Check for weightings from slabocean = ',weightcheck      

         fname_ocflux='xxx'
         fname_siflux='xxx'
         fname_tempref='xxx'
         fname_restart_slabocean='xxx' 

c read DATA (i.e. namelist) file
         call check_unit(11,__LINE__,__FILE__)
         open(unit=11,file='data_slabocean',status='old',iostat=ios)
         if (ios /= 0) then
            print*,'ERROR: could not open slabocean namelist file'
            stop
         end if

c read in namelist
         read(UNIT=11,NML=SLABOCEAN_CONTROL,IOSTAT=ios)
         if (ios /= 0) then
            print*,'ERROR: could not read slabocean namelist'
            stop
         else
            close(11)
         end if
c
c     
c        read in ocean flux file.
         ifail=0
         if (fname_ocflux.eq.'xxx') then
            print*,' Missing filename for ocean heat flux '
            ifail=1
         else
            inquire(file=trim(fname_ocflux),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',trim(fname_ocflux)
               ifail=1
            end if
         end if
         if (ifail.ne.0) then
            print*,' Correct error and try again '
            stop 1
         end if
C     
         print*,' slabocean: Opening ocflux file ',
     &        trim(fname_ocflux)
         call open_file_nc(trim(fname_ocflux),ncid)
         call get3d_data_nc(ncid,'ocflux',mg,jgg,12,ocflux,ifail)
         call close_file_nc(trim(fname_ocflux),ncid)
c
c
c        read in seaice flux file.
         ifail=0
         if (fname_siflux.eq.'xxx') then
            print*,' Missing filename for seaice heat flux '
            ifail=1
         else
            inquire(file=trim(fname_siflux),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',trim(fname_siflux)
               ifail=1
            end if
         end if
         if (ifail.ne.0) then
            print*,' Correct error and try again '
            stop 1
         end if
C     
         print*,' slabocean: Opening siflux file ',
     &        trim(fname_siflux)
         call open_file_nc(trim(fname_siflux),ncid)
         call get3d_data_nc(ncid,'ocflux',mg,jgg,12,siflux,ifail)
         call close_file_nc(trim(fname_siflux),ncid)
c
c
c        read in tempref file.
         ifail=0
         if (fname_tempref.eq.'xxx') then
            print*,' Missing filename for reference temperature'
            ifail=1
         else
            inquire(file=trim(fname_tempref),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',trim(fname_tempref)
               ifail=1
            end if
         end if
         if (ifail.ne.0) then
            print*,' Correct error and try again '
            stop 1
         end if
c
C     
         print*,' slabocean: Opening tempref file ',
     &        trim(fname_tempref)
         call open_file_nc(trim(fname_tempref),ncid)
         call get3d_data_nc(ncid,'sst',mg,jgg,12,tempref,ifail)
         call close_file_nc(trim(fname_tempref),ncid)



c        READ IN THE RESTART FILE
         ifail=0
         if (fname_restart_slabocean.eq.'xxx') then
            print*,' Missing filename for slabocean restart '
            ifail=1
         else
            inquire(file=trim(fname_restart_slabocean),
     :           exist=lexist)
            if (.not.lexist) then
               print*,' Missing file ',
     :           trim(fname_restart_slabocean)
               ifail=1
            endif
         endif
         if (ifail.ne.0) then
           print*,' Correct error and try again '
           stop 1
         endif

         print*,' slabocean: Opening restart file for read: ',
     &      trim(fname_restart_slabocean)

         call open_file_nc(
     &      trim(fname_restart_slabocean),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
         call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
         call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         call get2d_data_nc(ncid,'seaice_frac',mg,jgg,seaice_frac,
     &                              ifail)
         call get2d_data_nc(ncid,'sensible',mg,jgg,sensible,ifail)
         call get2d_data_nc(ncid,'latent',mg,jgg,latent,ifail)
         call get2d_data_nc(ncid,'netsolar',mg,jgg,netsolar,ifail)
         call get2d_data_nc(ncid,'netlong',mg,jgg,netlong,ifail)
         call get2d_data_nc(ncid,'tstar',mg,jgg,tstar,ifail)
         call get2d_data_nc(ncid,'albedo',mg,jgg,albedo,ifail)
         call get2d_data_nc(ncid,'temptop',mg,jgg,temptop,ifail)
         call close_file_nc(
     &      trim(fname_restart_slabocean),ncid)

         day=iday
         ioffset=mod(ioffset,360)


      if (flag_checkenergy_slabocean) then
      tot_energy=0.0
      do j=1,jgg
         do i=1,mg
             if (iland(i,j).eq.0) then
             tot_energy=tot_energy+(temptop(i,j)+273.16)*
     :        shco*weight(i,j)
             endif
         enddo
      enddo
      print*,'Initial energy in slab ocean = ',tot_energy
      ini_energy=tot_energy
      endif

      if (flag_checkwater_slabocean) then
      tot_water=0.0
      endif

      

      ocflux(:,:,:)=ocflux(:,:,:)+siflux(:,:,:)

      end if


      iday=nint(day)
      doy=mod(istep*timestep/(24*60*60.),daysperyear)
      if (mod(istep,10).eq.0) then
      print*,'Slab ocean thinks day of year is',doy
      endif
      CALL CALNDR(DOY,Mpth1,ApFRAC)
      MPth2=mpth1+1
      if (mpth2.eq.13) mpth2=1
c     
c     **This bit steps over the lats/lons, and calculates the 
c     **temperature tendency (dts), and updates the surface temperature
c     **(tstar), including carry-overs
c

c     See if we need to write a restart......
      if (mod(istep,dt_restart_slabocean).eq.0) then

      write(datestring,'(i7.7)') istep
      write(yearstring,'(i4.4)') iyear
      write(monthstring,'(i2.2)') imonth
      write(daystring,'(i2.2)') iday

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      fname=trim(outputdir_name)//
     :        '/slabocean_restart_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'.nc'
      print*,' Opening restart file for write: ',trim(fname)
      status=nf_create(trim(fname), nf_clobber, ncid)
      call so_handle_err(status)
      status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
      call so_handle_err(status)
      status=nf_def_dim(ncid, 'longitude',mg,nlonid)
      call so_handle_err(status)
      status=nf_def_dim(ncid, 'latitude',jgg,nlatid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'longitude',nf_real,1,nlonid,nlongitid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'latitude',nf_real,1,nlatid,nlatitid)
      call so_handle_err(status)
      dimpass_2(1)=nlonid
      dimpass_2(2)=nlatid
      status=nf_def_var(ncid,'ioffset',nf_int,1,nrecsid,ioffsetid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'iyear',nf_int,1,nrecsid,iyearid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'imonth',nf_int,1,nrecsid,imonthid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'iday',nf_int,1,nrecsid,idayid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'seaice_frac',nf_double,2,dimpass_2,
     &                     nseaice_fracid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'sensible',nf_double,2,dimpass_2,
     &                     nsensibleid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'latent',nf_double,2,dimpass_2,
     &                     nlatentid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'netsolar',nf_double,2,dimpass_2,
     &                     nnetsolarid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'netlong',nf_double,2,dimpass_2,
     &                     nnetlongid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'tstar',nf_double,2,dimpass_2,
     &                     ntstarid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'albedo',nf_double,2,dimpass_2,
     &                     nalbedoid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'temptop',nf_double,2,dimpass_2,
     &                     ntemptopid)
      call so_handle_err(status)
      status=nf_enddef(ncid)
      call so_handle_err(status)
      status=nf_put_var_int(ncid,ioffsetid,int(doy))
      call so_handle_err(status)
      status=nf_put_var_int(ncid,iyearid,iyear)
      call so_handle_err(status)
      status=nf_put_var_int(ncid,imonthid,imonth)
      call so_handle_err(status)
      status=nf_put_var_int(ncid,idayid,iday)
      call so_handle_err(status)

c     Assume that precision of lons is the precision of slabocean....
      if (kind(lons).eq.4) then
      status=nf_put_var_real(ncid,nlongitid,lons)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nlatitid,lats)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nseaice_fracid,seaice_frac)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nsensibleid,sensible)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nlatentid,latent)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nnetsolarid,netsolar)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nnetlongid,netlong)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,ntstarid,tstar)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nalbedoid,albedo)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,ntemptopid,temptop)
      call so_handle_err(status)
      endif
      if (kind(lons).eq.8) then
      status=nf_put_var_double(ncid,nlongitid,lons)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nlatitid,lats)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nseaice_fracid,seaice_frac)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nsensibleid,sensible)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nlatentid,latent)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nnetsolarid,netsolar)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nnetlongid,netlong)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,ntstarid,tstar)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nalbedoid,albedo)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,ntemptopid,temptop)
      call so_handle_err(status)
      endif

      status=nf_close(ncid)
      call so_handle_err(status)

      endif

      doav=.false.
      if ( ( istep.ge.start_slab_av) .and. (istep.le.end_slab_av) ) 
     :       doav=.true.

      if (slab_maker.and.doav) then
      ocflux_count(mpth1)=ocflux_count(mpth1)+(1.0-ApFRAC)
      ocflux_count(mpth2)=ocflux_count(mpth2)+ApFRAC
      endif

      do j=1,jgg
         do i=1,mg
            if (iland(i,j).eq.0) then

               if (seaice_frac(i,j).eq.0.0) then
c     
               tstar(i,j)=temptop(i,j)

c     there is no ice at the beginning of the timestep.....
c     **the advective flux is ocf
c     **POSITIVE DOWNWARDS**
c     
                  tref=(1.0-ApFRAC)*tempref(I,J,MpTH1)+
     &                 ApFRAC*tempref(I,J,MpTH2)

                  OCF=(1.0-ApFRAC)*OCFLUX(I,J,MpTH1)+
     &                 ApFRAC*OCFLUX(I,J,MpTH2)
c     

c     **the total heat flux out of the gridbox is sfc
c     **this is advective+sensible+latent+solar+longwave+(ocean heat transport)
c     
                  SFC=sensible(i,j)+latent(i,j)+netsolar(i,j)+
     &                 netlong(i,j)+conductflux(i,j)+ocf
c     
      if (slab_maker) then
      ocf_new=((tref-tstar(i,j))*shco/timestep)-sfc
      sfc=sfc+ocf_new
      if (doav) then
      ocflux_new(i,j,mpth1)=ocflux_new(i,j,mpth1)+ocf_new*(1.0-ApFRAC)
      ocflux_new(i,j,mpth2)=ocflux_new(i,j,mpth2)+ocf_new*ApFRAC
      endif
      endif

c     **the temperature tendency (ie DT*dT/dt) is dts 
c     
                  dts=timestep*sfc/shco
c     
c     **update the temperature.....
c     
                  tstar(i,j)=tstar(i,j)+dts
                  temptop(i,j)=tstar(i,j)

c     
c     **update the albedo....
c     FROM THE ORIGINAL CODE:
c     Change albedo to cool surface- this is because Tg is
c     too high (probably because of our clouds) and mucking
c     around with albedo is better than changing the solar constant
c     
                  albedo(i,j)=albedo_ocean
               end if
            end if
         enddo
      enddo
c

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


      if (flag_checkenergy_slabocean) then
      tot_energy=0.0
      do j=1,jgg
         do i=1,mg
           if (iland(i,j).eq.0) then
             tot_energy=tot_energy+(temptop(i,j)+273.16)*
     :         shco*weight(i,j)
           endif
         enddo
      enddo
      test_energy_ocean=(tot_energy-ini_energy)*4.0*pi*radea*radea
      print*,'Tot en in slabocean rel to ini = '
      print*,test_energy_ocean
      endif

      if (flag_checkwater_slabocean) then
      do j=1,jgg
         do i=1,mg
           if (iland(i,j).eq.0) then
             tot_water=tot_water+(prec_atm(i,j)+
     :                            evap_atm(i,j)+
     :                            runoff_atm(i,j))*
     :              weight(i,j)
           endif
         enddo
      enddo
      if (ifirst.eq.1) then
        ini_water=tot_water
      endif
      test_water_ocean=(tot_water-ini_water)*4.0*pi*radea*radea*timestep
      print*,'Tot water in slabocean rel to ini = '
      print*,test_water_ocean
      endif

      tstar_atm(:,:)=tstar(:,:)
      latent_atm(:,:)=latent(:,:)
      sensible_atm(:,:)=sensible(:,:)
      netsolar_atm(:,:)=netsolar(:,:)
      netlong_atm(:,:)=netlong(:,:)
      seaice_frac_atm(:,:)=seaice_frac(:,:)
      temptop_atm(:,:)=temptop(:,:)
      conductflux_atm(:,:)=conductflux(:,:)
      albedo_atm(:,:)=albedo(:,:)

      if (slab_maker .and. (istep.eq.end_slab_av) ) then

      do m=1,12
      if (ocflux_count(m).ne.0) then
      ocflux_new(:,:,m)=ocflux_new(:,:,m)/ocflux_count(m)
      else
      ocflux_new(:,:,m)=0.0
      endif
      months(m)=m
      enddo

!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      fname=trim(outputdir_name)//
     :        '/slabocean_fluxes.nc'
      print*,' Opening flux file for write: ',trim(fname)
      status=nf_create(trim(fname), nf_clobber, ncid)
      call so_handle_err(status)
      status=nf_def_dim(ncid, 'nrecs',1,nrecsid)
      call so_handle_err(status)
      status=nf_def_dim(ncid, 'longitude',mg,nlonid)
      call so_handle_err(status)
      status=nf_def_dim(ncid, 'latitude',jgg,nlatid)
      call so_handle_err(status)
      status=nf_def_dim(ncid, 'time',12,nmonid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'longitude',nf_real,1,nlonid,nlongitid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'latitude',nf_real,1,nlatid,nlatitid)
      call so_handle_err(status)
      status=nf_def_var(ncid,'time',nf_real,1,nmonid,nmonthid)
      call so_handle_err(status)
      dimpass_3(1)=nlonid
      dimpass_3(2)=nlatid
      dimpass_3(3)=nmonid
      status=nf_def_var(ncid,'ocflux',nf_double,3,dimpass_3,
     &                     nfluxid)
      call so_handle_err(status)
      status=nf_enddef(ncid)
      call so_handle_err(status)
c     Assume that precision of lons is the precision of slabocean....
      if (kind(lons).eq.4) then
      status=nf_put_var_real(ncid,nlongitid,lons)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nlatitid,lats)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nmonthid,months)
      call so_handle_err(status)
      status=nf_put_var_real(ncid,nfluxid,ocflux_new)
      call so_handle_err(status)
      endif
      if (kind(lons).eq.8) then
      status=nf_put_var_double(ncid,nlongitid,lons)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nlatitid,lats)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nmonthid,months)
      call so_handle_err(status)
      status=nf_put_var_double(ncid,nfluxid,ocflux_new)
      call so_handle_err(status)
      endif

      status=nf_close(ncid)     
      call so_handle_err(status)

      endif

      if (ifirst.eq.1) then
        ifirst=0
      endif

      return
      end

c     simple error handler
      subroutine so_handle_err(iret)
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
