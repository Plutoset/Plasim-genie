      subroutine write_netcdf_std(filename,data1,invarname)

c     ***********************************************************
c     djl, 13/11/2003
c     subroutine to enable instantaneous easy output of 
c     grads/ferret-friendly single-level netcdf data 
c     This is mainly a debugging tool and is not necessarily 
c     ever called in a 'normal' simulation

c     this version for normal-grid (see also write_netcdf_igcm.f)

c     [filename = output file name]
c     [data1 = data to be output]
c     [invarname = name to be given to the netcdf variable]
c     ***********************************************************

      implicit none

      include 'netcdf.inc'
#include "param1.cmn"
      include 'param2.cmn'

      integer nmaxdims,nall,nfiles
      parameter(nfiles=4,nmaxdims=4,nall=100)
      integer ndim,nvar,natts(nall),nattsvar(nall),
     :        vdims(nall),vadims(nmaxdims,nall),
     :        ndims(nall)
      character dimname(nall,nfiles)*200,varname(nall,nfiles)*200,
     :          attdimname(2,nmaxdims,nall)*200,
     :          attvarname(2,nmaxdims,nall)*200
      integer iddim(nall,nfiles),idvar(nall,nfiles)

c     IGCM GRID:
      integer ilon1_atm,ilat1_atm
      parameter (ilon1_atm=mg,ilat1_atm=jg*nhem)
      real alon1(ilon1_atm),alat1(ilat1_atm)
c      real data2((mg+2)*nhem,jg)

c     DATA:
      real data1(ilon1_atm,ilat1_atm)
      character filename*(*)
      character invarname*(*)

c     NETCDF + AWI STUFF: 
      integer ncid,loc_dim

c     LOOPING:
      integer i

c     FOR GRID:
      real ax

c      call restructure1(data2,data1,mg,jg,nhem,1.,0.,1)

c     COPIED FROM INITIALISE_ATMOS.F
      ax=360.0/real(mg)
      do i=1,ilon1_atm
         alon1(i)=(i-1.0)*ax
      end do
      call gwtcnr(alat1,jg)

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

      nvar=1
      varname(1,1)=trim(invarname)
      vdims(1)=2
      vadims(1,1)=loc_dim('longitude',dimname,nall)
      vadims(2,1)=loc_dim('latitude',dimname,nall)
      nattsvar(1)=2
      attvarname(1,1,1)='long_name'
      attvarname(2,1,1)='data from the model'
      attvarname(1,2,1)='units'
      attvarname(2,2,1)='no units'

      call ininc(trim(filename),
     :     nmaxdims,ndim,nvar,
     :     natts,nattsvar,
     :     vdims,vadims,ndims,
     :     dimname(1,1),varname(1,1),
     :     attdimname,attvarname,
     :     ncid,iddim(1,1),idvar(1,1))

      call writedim(ncid,iddim(1,1),alon1)
      call writedim(ncid,iddim(2,1),alat1)

      call writevar(ncid,
     :        idvar(loc_dim(invarname,varname(1,1),nall),1),
     :        data1 )

      call closenc(ncid)

      return
      end
