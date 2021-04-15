      subroutine write_nc(lon,lat,plev,yearn,doyn,annmean,totmean,outdir)

!     ***********************************************************
!     this version for igcm-grid (see also write_netcdf_std.f)

!     [filename = output file name]
!     [data1 = data to be output]
!     [invarname = name to be given to the netcdf variable]
!     ***********************************************************

      use ichem_var
      use ichem_util
      implicit none

      include 'netcdf.inc'

      integer nmaxdims,nall,nfiles
      parameter(nfiles=4,nmaxdims=4,nall=100)
      integer ndim,nvar,natts(nall),nattsvar(nall),&
             vdims(nall),vadims(nmaxdims,nall),&
             ndims(nall)
      character dimname(nall,nfiles)*200,varname(nall,nfiles)*200,&
               attdimname(2,nmaxdims,nall)*200,&
               attvarname(2,nmaxdims,nall)*200
      integer iddim(nall,nfiles),idvar(nall,nfiles)

!     IGCM GRID:
      integer ii,jj,l
      integer ifname,lnsig,yearni
      real lon(mg),lat(jgg),plev(nl),yearn,doyn
      real annmean(mg,jgg,nl),mean(mg,jgg,nl,1)
      real meantot(mg,jgg,nl,1),totmean(mg,jgg,nl)
      real delta(mg,jgg,nl,1)
      character outdir*200
      character fname*200
      character yearstring*4

!     NETCDF + AWI STUFF: 
      integer ncid,loc_dim

      do ii=1,mg
        do jj=1,jgg
         do l=1,nl
              mean(ii,jj,l,1)=annmean(ii,jj,l)/doyn
              meantot(ii,jj,l,1)=totmean(ii,jj,l)/doyn

              delta(ii,jj,l,1) = fun_calc_isotope_delta&
                 &(meantot(ii,jj,l,1),mean(ii,jj,l,1),1.117E-12)
         enddo
        enddo
      enddo

      print*,'mean,delta:',mean(1,1,1,1),meantot(1,1,1,1),delta(1,1,1,1)
 
      yearni=nint(yearn)
      write(yearstring,'(i4.4)') yearni
      fname=trim(outdir)//'/anmn_'&
      &//yearstring//'_ichem.nc'
      ifname=lnsig(fname)
      print*,'writing ichem nc for year:',yearn

      ndim=4
      dimname(1,1)='longitude'
      ndims(1)=mg
      natts(1)=2
      attdimname(1,1,1)='long_name'
      attdimname(2,1,1)='longitude'
      attdimname(1,2,1)='units'
      attdimname(2,2,1)='degrees east'

      dimname(2,1)='latitude'
      ndims(2)=jgg
      natts(2)=2
      attdimname(1,1,2)='long_name'
      attdimname(2,1,2)='latitude'
      attdimname(1,2,2)='units'
      attdimname(2,2,2)='degrees north'

      dimname(3,1)='p'
      ndims(3)=nl
      natts(3)=2
      attdimname(1,1,3)='units'
      attdimname(2,1,3)='mb'
      attdimname(1,2,3)='positive'
      attdimname(2,2,3)='down'

      dimname(4,1)='time'
      ndims(4)=1
      natts(4)=2
      attdimname(1,1,4)='long_name'
      attdimname(2,1,4)='time'
      attdimname(1,2,4)='units'
      attdimname(2,2,4)='years'

      nvar=1
      varname(1,1)='deltac14'
      vdims(1)=4
      vadims(1,nvar)=loc_dim('longitude',dimname,nall)
      vadims(2,nvar)=loc_dim('latitude',dimname,nall)
      vadims(3,nvar)=loc_dim('p',dimname,nall)
      vadims(4,nvar)=loc_dim('time',dimname,nall)
      nattsvar(nvar)=2
      attvarname(1,1,nvar)='long_name'
      attvarname(2,1,nvar)='atmos d14C'
      attvarname(1,2,nvar)='units'
      attvarname(2,2,nvar)='per mil'

      call ininc(fname(1:ifname),&
          nmaxdims,ndim,nvar,&
          natts,nattsvar,&
          vdims,vadims,ndims,&
          dimname(1,1),varname(1,1),&
          attdimname,attvarname,&
          ncid,iddim(1,1),idvar(1,1))
    
      call writedim(ncid,iddim(1,1),lon)
      call writedim(ncid,iddim(2,1),lat)
      call writedim(ncid,iddim(3,1),plev)
      call writedim(ncid,iddim(4,1),yearn)

      call writevar(ncid,&
             idvar(loc_dim('deltac14',varname(1,1),nall),1),&
             delta)

      call closenc(ncid)


      return
      end
