      SUBROUTINE INI_NETCDF(imonth,iyear,sigma,alat,imode)
      implicit none
c     
c     imode=1  is daily data
c     imode=2  is monthly data
c     imode=3  is annual data
c     imode=4  is decadal data
c
#include "param1.cmn"
      include 'param2.cmn'
      include 'netdata.cmn'
      include 'files.cmn'
c     
      real sigma(nl),alat(jgg)
      integer imonth,iyear,imode
c     
      real xcoord(mg),ycoord(jgg),tcoord(10000)
      real zcoord1(nl),zcoord2(nl)
      integer i,j,l,itime,ifname1,lnsig,lnsig1,ilen,ilen1
      character fname1*200

c     From netdata.cmn:
      integer nmaxdims
      parameter(nmaxdims=4)
      integer ndim,nvar,natts(nall),nattsvar(nall),
     :        vdims(nall),vadims(nmaxdims,nall),
     :        ndims(nall)
      character attdimname(2,nmaxdims,nall)*200,
     :          attvarname(2,nmaxdims,nall)*200
c     
      do i=1,mg
         xcoord(i)=(i-1)*360.0/real(mg)
      end do
c     
      do j=1,jg*nhem
         ycoord(j)=alat(j)
      end do
c     
      do l=1,nl
         zcoord1(l)=1000.0*sigma(l)
      end do
c     
c     These values are hard-wired at present and must be the same 
c       as in intpr.f (but here / 100)
      zcoord2(1)=50.0
      zcoord2(2)=200.0
      zcoord2(3)=300.0
      zcoord2(4)=500.0
      zcoord2(5)=700.0
      zcoord2(6)=850.0
      zcoord2(7)=1000.0

      if (imode.eq.1) then
         itime=30
         do i=1,itime
            tcoord(i)=i
         end do
      else if (imode.eq.2) then
         itime=1
         tcoord(itime)=imonth
      else if (imode.eq.3) then
         itime=1
         tcoord(itime)=imonth
      else if (imode.eq.4) then
         itime=13
         do i=1,itime
            tcoord(i)=i
         end do
      end if
c     
      if (imode.eq.1) then
         call setup_nc1(mg,jg*nhem,nl,itime,
     :        nmaxdims,nall,
     :        ndim,nvar,natts,nattsvar,vdims,
     :        vadims,ndims,
     :        dimname(1,imode),varname(1,imode),
     :        attdimname,attvarname)
      else if (imode.eq.2) then
         call setup_nc2(mg,jg*nhem,nl,itime,
     :        nmaxdims,nall,
     :        ndim,nvar,natts,nattsvar,vdims,
     :        vadims,ndims,
     :        dimname(1,imode),varname(1,imode),
     :        attdimname,attvarname)
      else if (imode.eq.3) then
         call setup_nc3(mg,jg*nhem,nl,itime,
     :        nmaxdims,nall,
     :        ndim,nvar,natts,nattsvar,vdims,
     :        vadims,ndims,
     :        dimname(1,imode),varname(1,imode),
     :        attdimname,attvarname)
      else if (imode.eq.4) then
         call setup_nc4(mg,jg*nhem,nl,itime,
     :        nmaxdims,nall,
     :        ndim,nvar,natts,nattsvar,vdims,
     :        vadims,ndims,
     :        dimname(1,imode),varname(1,imode),
     :        attdimname,attvarname)
      end if
c     
      ilen=lnsig1(outputdir_name)
      call create_fname(fname1,iyear,imonth,ilen1)
      if (imode.eq.1) then
         fname1=outputdir_name(1:ilen)//
     :        '/igcm_da_'//fname1(1:ilen1)
      else if (imode.eq.2) then
         fname1=outputdir_name(1:ilen)//
     :        '/igcm_cl_'//fname1(1:ilen1)
      else if (imode.eq.3) then
         fname1=outputdir_name(1:ilen)//
     :        '/igcm_an_'//fname1(1:4)//'.nc'
      else if (imode.eq.4) then
         fname1=outputdir_name(1:ilen)//
     :        '/igcm_dc_'//fname1(1:4)//'.nc'
      end if
      ifname1=lnsig(fname1)
      print*,' Opening ',fname1(1:ifname1)
c     
      call ininc(fname1(1:ifname1),
     :     nmaxdims,ndim,nvar,
     :     natts,nattsvar,
     :     vdims,vadims,ndims,
     :     dimname(1,imode),varname(1,imode),
     :     attdimname,attvarname,
     :     nc(imode),iddim(1,imode),idvar(1,imode))
c     
      call writedim(nc(imode),iddim(1,imode),xcoord)
      call writedim(nc(imode),iddim(2,imode),ycoord)
      call writedim(nc(imode),iddim(3,imode),zcoord1)
      call writedim(nc(imode),iddim(4,imode),tcoord)
      call writedim(nc(imode),iddim(5,imode),zcoord2)
c     

      print*,' Finished writing dimensions '
c     
      return
      end
c
      subroutine create_fname(fname,iyear,imonth,ilen)
      implicit none
      character fname*(*)
      integer iyear,imonth,ilen,lnsig
c
      character cmon(12)*2,cyear*4
      data cmon/'01','02','03','04','05','06',
     :     '07','08','09','10','11','12'/
c
      call cblank(fname)
      write(cyear,'(i4.4)')iyear
      fname(1:10)=cyear//'_'//cmon(imonth)//'.nc'
      ilen=lnsig(fname)
c
      return
      end

