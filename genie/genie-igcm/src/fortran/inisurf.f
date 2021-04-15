      subroutine INISURF
C----------------------------------------------------------------------
C     
C     Subroutine to initialise the surface model from history file
C     
C----------------------------------------------------------------------
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'bats.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
      include 'cpiers.cmn'
      include 'legau.cmn'
      include 'orog_g.cmn'
      include 'files.cmn'
      include 'flux_adjust.cmn'

      integer ncid,ifail

      if (lfluxadjust_water) then
        call open_file_nc(trim(fname_fluxadjust),ncid)
        call get2d_data_nc(ncid, 'water_mask' ,mg,jgg,water_mask,ifail)
        if (ifail.ne.0) then
          print*,' Unable to find water_mask'
          stop 1
        end if
        call close_file_nc(trim(fname_fluxadjust),ncid)
      endif

      call open_grid_restart(fname_gridpt(1:ifname_gridpt))

      print*,'Albedos read from RESTART record'

C
      RETURN
      END
c
      subroutine open_grid_restart(fname)
      implicit none
#include "param1.cmn"
      include 'param2.cmn'
      include 'cpiers.cmn'
      include 'physca.cmn'
      include 'radht.cmn'
      include 'bats.cmn'
      include 'blank.cmn'
      include 't42.cmn'
      include 'igcm_nlevs.cmn'
c
      character fname*(*)
      integer lnsig,ilen,ncid,kount2,ifail,iyear2
      real rday2,rdoy2,rntape2
c
      real, dimension(mg,jgg) :: work
      real, dimension(mg,jgg,nl) :: work1
c
      ilen=lnsig(fname)
      print*,' Opening ',fname(1:ilen)
      call open_file_nc(fname(1:ilen), ncid)
!-------------------------------------------
!     read rday
!-------------------------------------------
      call get1d_data_nc(ncid,'day',1,rday2,ifail)
      call get1d_data_nc(ncid,'doy',1,rdoy2,ifail)
      call get1d_data_nc(ncid,'rntape',1,rntape2,ifail)
      call get1di_data_nc(ncid,'kount',1,kount2,ifail)
      call get1di_data_nc(ncid,'year',1,iyear2,ifail)
      print*,' rday= ',rday2,' rdoy= ',rdoy2,' iyear = ',iyear2
      print*,' rntape = ',rntape2,' kount = ',kount2
!-------------------------------------------

      if (coldstart_grid) then
        work(:,:)=285.0/ct
      else
        call get2d_data_nc(ncid, 'tstar', mg, jgg, work, ifail)
        if (ifail.ne.0) then
          print*,' Unable to find tstar '
          stop 1
        end if
      endif
      call restructure1(tstar,work,mg,jg,nhem,1.0,0.0,-1)

      if (coldstart_grid) then
        work(:,:)=0.0
      else
        call get2d_data_nc(ncid, 'qstar', mg, jgg, work, ifail)
        if (ifail.ne.0) then
          print*,' Unable to find qstar '
          stop 1
        end if
      endif
      call restructure1(qstar,work,mg,jg,nhem,1.0,0.0,-1)

      if (coldstart_grid) then
        work(:,:)=0.15
      else
        call get2d_data_nc(ncid, 'salb', mg, jgg, work, ifail)
        if (ifail.ne.0) then
          print*,' Unable to find salb '
          stop 1
        end if
      endif
      call restructure1(salb,work,mg,jg,nhem,1.0,0.0,-1)


      if (coldstart_grid) then
        work(:,:)=0.0
      else
        call get2d_data_nc(ncid, 'sbal', mg, jgg, work, ifail)
        if (ifail.ne.0) then
          print*,' Unable to find sbal '
          stop 1
        end if
      endif
      call restructure1(sbal,work,mg,jg,nhem,1.0,0.0,-1)

      if (coldstart_grid) then
        work(:,:)=0.0
      else
        call get2d_data_nc(ncid, 'snet', mg, jgg, work, ifail)
        if (ifail.ne.0) then
          print*,' Unable to find snet '
          stop 1
        end if
      endif
      call restructure1(snet,work,mg,jg,nhem,1.0,0.0,-1)

      if (coldstart) then
        work1(:,:,:)=0.0
      else
        call get3d_data_nc(ncid, 'htnet', mg, jgg, nl, work1, ifail)
        if (ifail.ne.0) then
          print*,' Unable to find htnet in',fname
          stop
        end if
      endif
      call restructure2(htnet,work1,mg,jg,nhem,nl,1.0,0.0,-1)

!-------------------------------------------
!     close the restart.17 file
!-------------------------------------------
      call close_file_nc(fname(1:ilen), ncid)
c
      return
      end

