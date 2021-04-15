      subroutine fixedicesheet(istep
     :     ,iland_atm,orog_atm,albedo_atm,icefrac_atm,iconv)

      use genie_util, only : message, die

      implicit none

#include "precision.inc"

      include 'fixice.cmn'
      include 'netcdf.inc'

c     NEED TO PUT TIMESTEP INFO INTO JOB FILE!!     

#include "resolution_fixedicesheet.inc"

      real timestep
      save timestep

      real daysperyear
      parameter(daysperyear=360.0)

      real(rk_in) :: icefrac_atm(mg,jgg)
      real(rk_in) :: orog_atm(mg,jgg)
      real(rk_in) :: albedo_atm(mg,jgg)
      integer iland_atm(mg,jgg)
      integer istep
      integer iconv

c     ****FOR THIS ROUTINE****

      integer ncid

      real(rk_libutil1) :: doy

      integer j,i

      integer ifail

      real orog(mg,jgg)
      real icefrac(mg,jgg)
      real iland(mg,jgg)

      save orog,icefrac,iland

      character fname_orog*200
      character fname_icefrac*200
      character fname_iland*200
      character fname_restart_fixedicesheet*200

      character outputdir_name*200
      save outputdir_name

      integer dt_restart_fixedicesheet
      save dt_restart_fixedicesheet

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

c     for varying orog
      logical fixed_orog
      logical fixed_icefrac
      integer dt_vals
      save fixed_orog,fixed_icefrac,dt_vals
      real ntimes_1,ntimes_2,ntimes_3
      integer ntimes
      save ntimes
      integer daynum
      integer time1
      integer time2
      real timefrac
      real, allocatable, dimension(:,:,:) :: iland_vect
      real, allocatable, dimension(:,:,:) :: orog_vect
      real, allocatable, dimension(:,:,:) :: icefrac_vect
      save iland_vect,orog_vect,icefrac_vect
      integer iniday
      save iniday

      integer norogid
      integer nicefracid
      real lons(mg),lats(jgg)
      save lons
      save lats
      integer nlonid,nlongitid,nlatid,nlatitid
      integer dimpass(2)

      character msgStr*200

      namelist/FIXEDICESHEET_CONTROL/
     :        dt_restart_fixedicesheet,
     :        albedo_icesheet,
     :        outputdir_name,
     :        fname_orog,
     :        fname_icefrac,
     :        fname_iland,
     :        fname_restart_fixedicesheet,
     :        fixed_orog,
     :        fixed_icefrac,
     :        dt_vals,
     :        iniday,
     :        timestep
c     
c-------------------------------------------
c     open netcdf file and read in sea surface temperature
c------------------------------------------
      
      IF (IFIRST.EQ.1) THEN
         
c     OK, these lats and lons are not exact.  The real numbers 
c     are in the igcm files.  These are equally spaced in lat/lon, 
c     whereas igcm uses a gaussian grid.  This is just output though
c     so it doesn't matter.
         
         do i=1,mg
            lons(i)=(real(i)-1)*5.625
         enddo

         do j=1,jgg
            lats(j)=92.8125-real(j)*5.625
         enddo
         
c     Ten days is the default icesheet timestep.  Timestep is in seconds.
         timestep=10.*24.*60.*60.

         fname_orog='xxx' 
         fname_icefrac='xxx' 
         fname_iland='xxx' 
         fname_restart_fixedicesheet='xxx'
         
c     read DATA (i.e. namelist) file
         open(unit=13,file='data_fixedicesheet',status='old',iostat=ios)
         if (ios /= 0) then
            call die('could not open fiedicesheet namelist file',
     :           __LINE__,__FILE__)
         end if
         
c     read in namelist
         read(UNIT=13,NML=FIXEDICESHEET_CONTROL,IOSTAT=ios)
         if (ios /= 0) then
            call die('could not read fixedicesheet namelist',
     :           __LINE__,__FILE__)
         else
            close(13)
         end if
c     
c     **********************************
         if (fixed_orog) then
c     Reading OROG
            ifail=0
            if (fname_orog.eq.'xxx') then
               call die('missing orog filename in icesheet',
     :              __LINE__,__FILE__)
            else
               inquire(file=trim(fname_orog),exist=lexist)
               if (.not.lexist) then
                  call die('Missing file: '//trim(fname_orog),
     :                 __LINE__,__FILE__)
               endif
            endif
            call message('fixedicesheet: Opening orog file: '
     :           //trim(fname_orog),1)
            call open_file_nc(trim(fname_orog),ncid)
            call get2d_data_nc(ncid,'orog',mg,jgg,orog,ifail)
            call close_file_nc(trim(fname_orog),ncid)
            if (ifail.ne.0) then
               call die('variable orog not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if         
c     Reading ILAND
            ifail=0
            if (fname_iland.eq.'xxx') then
               call die('Missing filename for iland in icesheet',
     :              __LINE__,__FILE__)
            else
               inquire(file=trim(fname_iland),exist=lexist)
               if (.not.lexist) then
                  call die('Missing file: '//trim(fname_iland),
     :                 __LINE__,__FILE__)
               endif
            endif
            call message('fixedicesheet: Opening iland file: '
     :           //trim(fname_iland),1)
            call open_file_nc(trim(fname_iland),ncid)
            call get2d_data_nc(ncid,'lsm',mg,jgg,iland,ifail)
            call close_file_nc(trim(fname_iland),ncid)
            if (ifail.ne.0) then
               call die('variable iland not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if   
         endif
c     **********************************
         
c     **********************************
         if (fixed_icefrac) then
c     Reading ICEFRAC
            ifail=0
            if (fname_icefrac.eq.'xxx') then
               call die('Missing filename for icefrac in icesheet',
     :              __LINE__,__FILE__)
            else
               inquire(file=trim(fname_icefrac),exist=lexist)
               if (.not.lexist) then
                  call die ('Missing file: '//trim(fname_icefrac),
     :                 __LINE__,__FILE__)
               endif
            endif
            call message('fixedicesheet: Opening icefrac file: '//
     :           trim(fname_icefrac),1)
            call open_file_nc(trim(fname_icefrac),ncid)
            call get2d_data_nc(ncid,'icefrac',mg,jgg,icefrac,ifail)
            call close_file_nc(trim(fname_icefrac),ncid)
            if (ifail.ne.0) then
               call die('variable icefrac not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if   
         endif
c     **********************************
         
         
c     **********************************
         if (.not. fixed_orog) then
c     Reading OROG
            ifail=0
            if (fname_orog.eq.'xxx') then
               call die('Missing filename for orog in icesheet',
     :              __LINE__,__FILE__)
            else
               inquire(file=trim(fname_orog),
     :              exist=lexist)
               if (.not.lexist) then
                  call die('Missing file: '//trim(fname_orog),
     :                 __LINE__,__FILE__) 
               endif
            endif
            call message('fixedicesheet: Opening orog file: '//
     :           trim(fname_orog),1)
            call open_file_nc(trim(fname_orog),ncid)
            call get1d_data_nc(ncid,'ntimes',1,ntimes_1,ifail)
            if (ifail.ne.0) then
               call die('variable ntimes not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if
            
c     verbosity '2' is 'chatty'
            write(msgStr,*)'ntimes in fixedicesheet = ',ntimes_1
            call message(msgStr,2)
c     
            allocate(orog_vect(mg,jgg,nint(ntimes_1)))
            
            call get3d_data_nc(ncid,'orog',mg,jgg,nint(ntimes_1),
     :           orog_vect,ifail)
            call close_file_nc(trim(fname_orog),ncid)
            if (ifail.ne.0) then
               call die('variable orog not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if         
c     Reading ILAND
            ifail=0
            if (fname_iland.eq.'xxx') then
               call die(' Missing filename for iland in icesheet ',
     :              __LINE__,__FILE__)
            else
               inquire(file=trim(fname_iland),exist=lexist)
               if (.not.lexist) then
                  call die(' Missing file '//trim(fname_iland),
     :                 __LINE__,__FILE__)
               endif
            endif
            call message(' fixedicesheet: Opening iland file: '//
     :           trim(fname_iland),1)
            call open_file_nc(trim(fname_iland),ncid)
            call get1d_data_nc(ncid,'ntimes',1,ntimes_3,ifail)
            if (ifail.ne.0) then
               call die('variable ntimes not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if
            write(msgStr,*)'ntimes in fixedicesheet = ',ntimes_3
            call message(msgStr,2)
            
            allocate(iland_vect(mg,jgg,nint(ntimes_3)))
            
            call get3d_data_nc(ncid,'lsm',mg,jgg,nint(ntimes_3),
     :           iland_vect,ifail)
            call close_file_nc(trim(fname_iland),ncid)
            if (ifail.ne.0) then
               call die('variable iland not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if   
            
            if (ntimes_1.ne.ntimes_3) then
               call die('discrepancy in ntimes in fixedicesheet'//
     :              '(orog+lsm)',
     :              __LINE__,__FILE__)
            endif
            ntimes=int(ntimes_1)
         endif

c     **********************************
         if (.not. fixed_icefrac) then
c     Reading ICEFRAC
            ifail=0
            if (fname_icefrac.eq.'xxx') then
               call die(' Missing filename for icefrac in icesheet ',
     :              __LINE__,__FILE__)
            else
               inquire(file=trim(fname_icefrac),exist=lexist)
               if (.not.lexist) then
                  call die(' Missing file '//trim(fname_icefrac),
     :                 __LINE__,__FILE__)
               endif
            endif
            call message(' fixedicesheet: Opening icefrac file '//
     :           trim(fname_icefrac),1)
            call open_file_nc(trim(fname_icefrac),ncid)
            call get1d_data_nc(ncid,'ntimes',1,ntimes_2,ifail)
            if (ifail.ne.0) then
               call die('variable ntimes not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if
            
            write(msgStr,*) 'ntimes in fixedicesheet = ',ntimes_2
            call message(msgStr,2)
            allocate(icefrac_vect(mg,jgg,nint(ntimes_2)))
            
            call get3d_data_nc(ncid,'icefrac',mg,jgg,nint(ntimes_2),
     :           icefrac_vect,ifail)
            call close_file_nc(trim(fname_icefrac),ncid)
            if (ifail.ne.0) then
               call die('variable icefrac not found in fixedicesheet',
     :              __LINE__,__FILE__)
            end if
            ntimes=int(ntimes_2)
         endif
c     **********************************
      
      
c     **********************************
         if ( (.not. fixed_orog) .and. (.not. fixed_icefrac) ) then
            if (ntimes_1.ne.ntimes_2) then
               call die('discrepancy in ntimes in fixedicesheet (o+i)',
     :              __LINE__,__FILE__)
            endif
         endif
c     **********************************
c     READ IN THE RESTART FILE
c     it's actually just the day offset from 1st January
         ifail=0
         if (fname_restart_fixedicesheet.eq.'xxx') then
            call die(' Missing filename for fixedicesheet restart ',
     :           __LINE__,__FILE__)
         else
            inquire(file=trim(fname_restart_fixedicesheet),
     :           exist=lexist)
            if (.not.lexist) then
               call die(' Missing file '//
     :              trim(fname_restart_fixedicesheet),
     :              __LINE__,__FILE__)
            endif
         end if
         call message('fixedicesheet: '//
     :        'Opening restart file for read: '//
     :        trim(fname_restart_fixedicesheet),1)
            
         call open_file_nc(
     :        trim(fname_restart_fixedicesheet),ncid)
         call get1di_data_nc(ncid,'ioffset',1,ioffset,ifail)
         if (ifail.ne.0) then
            call die('variable ioffset not found in fixedicesheet',
     :           __LINE__,__FILE__)
         end if   
         call get1di_data_nc(ncid,'iyear',1,iyear,ifail)
         if (ifail.ne.0) then
            call die('variable iyear not found in fixedicesheet',
     :           __LINE__,__FILE__)
         end if  
         call get1di_data_nc(ncid,'imonth',1,imonth,ifail)
         if (ifail.ne.0) then
            call die('variable imonth not found in fixedicesheet',
     :           __LINE__,__FILE__)
         end if  
         call get1di_data_nc(ncid,'iday',1,iday,ifail)
         if (ifail.ne.0) then
            call die('variable iday not found in fixedicesheet',
     :           __LINE__,__FILE__)
         end if  
         call close_file_nc(
     :        trim(fname_restart_fixedicesheet),ncid)
         
         day=iday
         ioffset=mod(ioffset,360)
         ifirst=0
         
      end if

      iday=nint(day)
      doy=mod(ioffset+istep*timestep/(24*60*60.),daysperyear)
      if (mod(istep,120).eq.0.or.istep.le.6) then
         write(msgStr,*) 'Fixed icesheet thinks DOY=',doy
         call message(msgStr,2)
      end if
c     
c     Processing data.....
      
      if (fixed_orog) then
         iland_atm(:,:)=nint(iland(:,:))
         orog_atm(:,:)=orog(:,:)
      endif
      
      if (fixed_icefrac) then
         albedo_atm(:,:)=albedo_icesheet
         icefrac_atm(:,:)=icefrac(:,:)
      endif
      
      if ( (.not. fixed_orog) .or. (.not. fixed_icefrac) ) then

         daynum=iday+((imonth-1)*30)+(360*(iyear-2000))-1-iniday
         time1=int(daynum/real(dt_vals) + 1)
         time2=time1+1
         timefrac=mod(daynum,dt_vals)/real(dt_vals)
         
         if (time1.le.0) then
            call message('fixedicesheet: warning, too early for record',
     :           2)
            if (.not. fixed_orog) then
               iland_atm(:,:)=int(iland_vect(:,:,1))
               orog_atm(:,:)=orog_vect(:,:,1)
            endif
            if (.not. fixed_icefrac) then
               icefrac_atm(:,:)=icefrac_vect(:,:,1)
               albedo_atm(:,:)=albedo_icesheet
            endif
         endif
         
         if ( (time2.le.ntimes).and.(time1.gt.0) ) then
            call message('fixedicesheet: interpolating times...',2)
            write(*,msgStr) 'timefrac=',timefrac
            call message(msgStr,2)
c     OK, this case is a bit harder!!!!!
c     we want iland and icefrac to be nint!! 
            if (.not. fixed_orog) then
               iland_atm(:,:)=nint(iland_vect(:,:,time1)*(1-timefrac) + 
     :              iland_vect(:,:,time2)*(timefrac))
               orog_atm(:,:)=orog_vect(:,:,time1)*(1-timefrac) + 
     :              orog_vect(:,:,time2)*(timefrac)
            endif
            if (.not. fixed_icefrac) then
               icefrac_atm(:,:)=nint(icefrac_vect(:,:,time1)*(1-timefrac) + 
     :              icefrac_vect(:,:,time2)*(timefrac))
               albedo_atm(:,:)=albedo_icesheet
            endif
         endif
         
         if (time2.gt.ntimes) then
            call message('fixedicesheet: warning,'//
     :           'too late for gas record',2)
            if (.not. fixed_orog) then
               iland_atm(:,:)=int(iland_vect(:,:,ntimes))
               orog_atm(:,:)=orog_vect(:,:,ntimes)
            endif
            if (.not. fixed_icefrac) then
               icefrac_atm(:,:)=icefrac_vect(:,:,ntimes)
               albedo_atm(:,:)=albedo_icesheet
            endif
         endif
      endif
c
c     See if we need to write a restart....
c     
      if (mod(istep,dt_restart_fixedicesheet).eq.0) then
         
         write(yearstring,'(i4.4)') iyear
         write(monthstring,'(i2.2)') imonth
         write(daystring,'(i2.2)') iday
         
c-------------------------------------------------------
c     create a netcdf file
c-------------------------------------------------------
         fname=trim(outputdir_name)//
     :        '/fixedicesheet_restart_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'.nc'
         call message('Opening restart file for write: '//trim(fname),1)
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

c-------------------------------------------------------
c     create a netcdf file
c-------------------------------------------------------
         fname=trim(outputdir_name)//
     :        '/fixedicesheet_data_'//
     :        yearstring//'_'//monthstring//'_'//daystring//'.nc'
         call message('Opening output file for write: '//trim(fname),1)
         status=nf_create(trim(fname), nf_clobber, ncid)
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
         status=nf_def_var(ncid,'orog',nf_double,2,dimpass,
     &        norogid)
         call check_err(status)
         status=nf_def_var(ncid,'icefrac',nf_double,2,dimpass,
     &        nicefracid)
         call check_err(status)

c     ccccccccccccccccccc
c     These changes are so that certain netcdf viewers can read the netcdf fields OK.
c     DJL. 
         status=nf_put_att_text(ncid,nlongitid,
     :        'units',12, 
     :        'degrees_east')
         status=nf_put_att_text(ncid,nlongitid,
     :        'long_name',9, 
     :        'longitude')
         
         status=nf_put_att_text(ncid,nlatitid,
     :        'units',13, 
     :        'degrees_north')
         status=nf_put_att_text(ncid,nlatitid,
     :        'long_name',8, 
     :        'latitude')
c     ccccccccccccccccccc
         status=nf_enddef(ncid)
         call check_err(status)
         
         if (kind(lons).eq.4) then
            status=nf_put_var_real(ncid,nlongitid,lons)
            call check_err(status)
         endif
         if (kind(lons).eq.8) then
            status=nf_put_var_double(ncid,nlongitid,lons)
            call check_err(status)
         endif
         if (kind(lats).eq.4) then
            status=nf_put_var_real(ncid,nlatitid,lats)
            call check_err(status)
         endif
         if (kind(lats).eq.8) then
            status=nf_put_var_double(ncid,nlatitid,lats)
            call check_err(status)
         endif
         if (kind(orog_atm).eq.4) then
            status=nf_put_var_real(ncid,norogid,orog_atm)
            call check_err(status)
         endif
         if (kind(orog_atm).eq.8) then
            status=nf_put_var_double(ncid,norogid,orog_atm)
            call check_err(status)
         endif
         if (kind(icefrac_atm).eq.4) then
            status=nf_put_var_real(ncid,nicefracid,icefrac_atm)
            call check_err(status)
         endif
         if (kind(icefrac_atm).eq.8) then
            status=nf_put_var_double(ncid,nicefracid,icefrac_atm)
            call check_err(status)
         endif
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
      
c     Let's just set this to 1 anyway, to force the igcm3_adiab to 
c     re-calculate the spectral coefficients every 10 days....
      iconv=1
      
      return
      end
