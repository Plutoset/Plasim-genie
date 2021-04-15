      subroutine writenc_hists
      implicit none
c     
c     THIS ROUTINE WILL ONLY WORK IF THE IGCM IS IN REAL*4!!
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'bats.cmn'
      include 'outcon.cmn'
      include 'spectr.cmn'
c     
      complex, parameter :: ic = (0.0, 1.0)
      integer, parameter :: nrecs=1
c     
      real, dimension(igb) :: z_re, z_im, d_re, d_im, t_re, t_im
      real, dimension(iga) :: sp_re, sp_im
      real, dimension(igb, ntrac) :: tra_re, tra_im

      integer :: i, kk

!     netcdf variables
      integer :: ncid, status

      integer :: igaid, igbid, ntracid, nrecsid
      integer :: dayid, doyid, rntapeid
      integer :: dimid1(2), dimid2(3), dimid3(2)
      integer :: z_reid, z_imid, d_reid, d_imid, t_reid, t_imid
      integer :: tra_reid, tra_imid
      integer :: sp_reid, sp_imid

      character fname*200
      integer ilen,ilen1,lnsig1,lnsig
 
      include 'files.cmn'
      include 'netcdf.inc'
!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      ilen=lnsig1(outputdir_name)
      call create_fname(fname,iyear,imonth,ilen1)
      fname=outputdir_name(1:ilen)//
     :        '/igcm_hs_'//fname(1:ilen1)
      ilen=lnsig(fname)
      print*,' Opening hists file ',fname(1:ilen)
      status=nf_create(fname(1:ilen), nf_clobber, ncid)
      call check_err(status)
!----------------------------------------------
!     define dimensions
!----------------------------------------------
      status=nf_def_dim(ncid, 'iga',iga,igaid)
      status=nf_def_dim(ncid, 'igb',igb,igbid)
      status=nf_def_dim(ncid, 'ntrac',ntrac,ntracid)
      status=nf_def_dim(ncid, 'nrecs',nrecs,nrecsid)
!----------------------------------------------
!     define variables
!----------------------------------------------
      status=nf_def_var(ncid,'day',nf_double,1,nrecsid,dayid)
      status=nf_def_var(ncid,'doy',nf_double,1,nrecsid,doyid)
      status=nf_def_var(ncid,'rntape',nf_double,1,nrecsid,rntapeid)
      dimid1(1)=igbid
      status=nf_def_var(ncid,'z_re',nf_double,1,dimid1,z_reid)
      status=nf_def_var(ncid,'z_im',nf_double,1,dimid1,z_imid)
      status=nf_def_var(ncid,'d_re',nf_double,1,dimid1,d_reid)
      status=nf_def_var(ncid,'d_im',nf_double,1,dimid1,d_imid)
      status=nf_def_var(ncid,'t_re',nf_double,1,dimid1,t_reid)
      status=nf_def_var(ncid,'t_im',nf_double,1,dimid1,t_imid)
      dimid2(1)=igbid
      dimid2(2)=ntracid
      status=nf_def_var(ncid,'tra_re',nf_double,2,dimid2,tra_reid)
      status=nf_def_var(ncid,'tra_im',nf_double,2,dimid2,tra_imid)
      dimid3(1)=igaid
      status=nf_def_var(ncid,'sp_re',nf_double,1,dimid3,sp_reid)
      status=nf_def_var(ncid,'sp_im',nf_double,1,dimid3,sp_imid)
!----------------------------------------------
!     end define mode
!----------------------------------------------
      status=nf_enddef(ncid)
!-------------------------------------------------------
!     separate into real and imaginary
!-------------------------------------------------------
      do i=1, igb
         z_re(i)=real(z(i))
         z_im(i)=real(-ic*z(i))
         d_re(i)=real(d(i))
         d_im(i)=real(-ic*d(i))
         t_re(i)=real(t(i))
         t_im(i)=real(-ic*t(i))
         do kk=1, ntrac
            tra_re(i, kk)=real(tra(i, kk))
            tra_im(i, kk)=real(-ic*tra(i, kk))
         enddo
      enddo
      do i=1, iga
         sp_re(i)=real(sp(i))
         sp_im(i)=real(-ic*sp(i))
      enddo

!----------------------------------------------
!     write variables to file
!----------------------------------------------
      status=nf_put_var_real(ncid,dayid,day)
      status=nf_put_var_real(ncid,doyid,doy)
      status=nf_put_var_real(ncid,rntapeid,rntape)
      status=nf_put_var_real(ncid,z_reid,z_re)
      status=nf_put_var_real(ncid,z_imid,z_im)
      status=nf_put_var_real(ncid,d_reid,d_re)
      status=nf_put_var_real(ncid,d_imid,d_im)
      status=nf_put_var_real(ncid,t_reid,t_re)
      status=nf_put_var_real(ncid,t_imid,t_im)
      status=nf_put_var_real(ncid,tra_reid,tra_re)
      status=nf_put_var_real(ncid,tra_imid,tra_im)
      status=nf_put_var_real(ncid,sp_reid,sp_re)
      status=nf_put_var_real(ncid,sp_imid,sp_im)
!----------------------------------------------
!     close file
!----------------------------------------------
      status=nf_close(ncid)

      return
      end
