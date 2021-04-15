      SUBROUTINE WRITENC_RESTART
      implicit none
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'spectr.cmn'
      include 'outcon.cmn'
      include 'physca.cmn'
      include 'bats.cmn'
      include 'blank.cmn'
      include 'radht.cmn'
      include 'cpiers.cmn'
      include 'stats.cmn'
      include 'legau.cmn'
      include 'igcm_prec.cmn'
C     
      complex, parameter :: ic = (0.0, 1.0)
      integer, parameter :: nrecs=1
c     
      real, dimension(igb) :: z_re, z_im, d_re, d_im, t_re, t_im
      real, dimension(iga) :: sp_re, sp_im
      real, dimension(igb, ntrac) :: tra_re, tra_im
      real, dimension(mg,jgg) :: work
      real, dimension(mg,jgg,nl) :: work1
      real, dimension(mg) :: along
      integer :: i, kk

!     netcdf variables
      integer :: ncid, status

      integer :: igaid, igbid, ntracid, nrecsid, kountid, 
     :           rntapeid
      integer :: dayid, doyid, yearid,monthid,idoyid
      integer :: mgid,nlid,jggid
      integer :: tstarid,tdeepid,smstarid,qstarid,hsnowid,sqstarid,
     :           salbid,sbalid,tstaroid,tdeepoid,snetid,
     :           prec_largid,prec_convid,
     :           htnetid,gmsp0id,gmspmiid,longid,latid,sigmaid
      integer :: dimid(2),dimid1(2), dimid2(3), dimid3(3)
      integer :: z_reid, z_imid, d_reid, d_imid, t_reid, t_imid
      integer :: tra_reid, tra_imid
      integer :: sp_reid, sp_imid
      integer :: zm_reid, zm_imid, dm_reid, dm_imid, tm_reid, tm_imid
      integer :: tram_reid, tram_imid
      integer :: spm_reid, spm_imid

      character fname*200
      integer ilen,ilen1,lnsig1,lnsig
 
      include 'files.cmn'
#include "netcdf.inc"
c
 2000 FORMAT(/' RESTART RECORD WRITTEN TO CHANNEL ',I3,/
     +     ' KOUNT  RNTAPE  DAY  DOY  =',I10,3F12.3)
!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      ilen=lnsig1(outputdir_name)
      call create_fname(fname,iyear,imonth,ilen1)
      fname=outputdir_name(1:ilen)//
     :        '/igcm_rs_'//fname(1:ilen1)
      ilen=lnsig(fname)
      print*,' Opening restarts file ',fname(1:ilen)
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
      status=nf_def_var(ncid,'kount',nf_int,1,nrecsid,kountid)
      status=nf_def_var(ncid,'month',nf_int,1,nrecsid,monthid)
      status=nf_def_var(ncid,'year',nf_int,1,nrecsid,yearid)
      status=nf_def_var(ncid,'idoy',nf_int,1,nrecsid,idoyid)
      status=nf_def_var(ncid,'rntape',nf_double,1,nrecsid,rntapeid)
      status=nf_def_var(ncid,'day',nf_double,1,nrecsid,dayid)
      status=nf_def_var(ncid,'doy',nf_double,1,nrecsid,doyid)
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
      dimid1(1)=igbid
      status=nf_def_var(ncid,'zm_re',nf_double,1,dimid1,zm_reid)
      status=nf_def_var(ncid,'zm_im',nf_double,1,dimid1,zm_imid)
      status=nf_def_var(ncid,'dm_re',nf_double,1,dimid1,dm_reid)
      status=nf_def_var(ncid,'dm_im',nf_double,1,dimid1,dm_imid)
      status=nf_def_var(ncid,'tm_re',nf_double,1,dimid1,tm_reid)
      status=nf_def_var(ncid,'tm_im',nf_double,1,dimid1,tm_imid)
      dimid2(1)=igbid
      dimid2(2)=ntracid
      status=nf_def_var(ncid,'tram_re',nf_double,2,dimid2,tram_reid)
      status=nf_def_var(ncid,'tram_im',nf_double,2,dimid2,tram_imid)
      dimid3(1)=igaid
      status=nf_def_var(ncid,'spm_re',nf_double,1,dimid3,spm_reid)
      status=nf_def_var(ncid,'spm_im',nf_double,1,dimid3,spm_imid)
c
      status=nf_def_var(ncid,'gmsp0',nf_double,1,nrecsid,gmsp0id)
      status=nf_def_var(ncid,'gmspmi',nf_double,1,nrecsid,gmspmiid)
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
      status=nf_put_var_int(ncid,kountid,kount)
      status=nf_put_var_int(ncid,monthid,imonth)
      status=nf_put_var_int(ncid,yearid,iyear)
      status=nf_put_var_int(ncid,idoyid,idoy)
C
C     Make assumption that the precision of z_re is the igcm precision.... 
C
      if (kind(z_re).eq.4) then
      status=nf_put_var_real(ncid,rntapeid,rntape)
      status=nf_put_var_real(ncid,dayid,day)
      status=nf_put_var_real(ncid,doyid,doy)
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
      endif

      if (kind(z_re).eq.8) then
      status=nf_put_var_double(ncid,rntapeid,rntape)
      status=nf_put_var_double(ncid,dayid,day)
      status=nf_put_var_double(ncid,doyid,doy)
      status=nf_put_var_double(ncid,z_reid,z_re)
      status=nf_put_var_double(ncid,z_imid,z_im)
      status=nf_put_var_double(ncid,d_reid,d_re)
      status=nf_put_var_double(ncid,d_imid,d_im)
      status=nf_put_var_double(ncid,t_reid,t_re)
      status=nf_put_var_double(ncid,t_imid,t_im)
      status=nf_put_var_double(ncid,tra_reid,tra_re)
      status=nf_put_var_double(ncid,tra_imid,tra_im)
      status=nf_put_var_double(ncid,sp_reid,sp_re)
      status=nf_put_var_double(ncid,sp_imid,sp_im)
      endif


!-------------------------------------------------------
!     separate into real and imaginary
!-------------------------------------------------------
      do i=1, igb
         z_re(i)=real(zmi(i))
         z_im(i)=real(-ic*zmi(i))
         d_re(i)=real(dmi(i))
         d_im(i)=real(-ic*dmi(i))
         t_re(i)=real(tmi(i))
         t_im(i)=real(-ic*tmi(i))
         do kk=1, ntrac
            tra_re(i, kk)=real(trami(i, kk))
            tra_im(i, kk)=real(-ic*trami(i, kk))
         enddo
      enddo
      do i=1, iga
         sp_re(i)=real(spmi(i))
         sp_im(i)=real(-ic*spmi(i))
      enddo
!----------------------------------------------
!     write variables to file
!----------------------------------------------
      if (kind(z_re).eq.4) then
      status=nf_put_var_real(ncid,zm_reid,z_re)
      status=nf_put_var_real(ncid,zm_imid,z_im)
      status=nf_put_var_real(ncid,dm_reid,d_re)
      status=nf_put_var_real(ncid,dm_imid,d_im)
      status=nf_put_var_real(ncid,tm_reid,t_re)
      status=nf_put_var_real(ncid,tm_imid,t_im)
      status=nf_put_var_real(ncid,tram_reid,tra_re)
      status=nf_put_var_real(ncid,tram_imid,tra_im)
      status=nf_put_var_real(ncid,spm_reid,sp_re)
      status=nf_put_var_real(ncid,spm_imid,sp_im)
      status=nf_put_var_real(ncid,gmsp0id,gmsp0)
      status=nf_put_var_real(ncid,gmspmiid,gmspmi)
      endif
      if (kind(z_re).eq.8) then
      status=nf_put_var_double(ncid,zm_reid,z_re)
      status=nf_put_var_double(ncid,zm_imid,z_im)
      status=nf_put_var_double(ncid,dm_reid,d_re)
      status=nf_put_var_double(ncid,dm_imid,d_im)
      status=nf_put_var_double(ncid,tm_reid,t_re)
      status=nf_put_var_double(ncid,tm_imid,t_im)
      status=nf_put_var_double(ncid,tram_reid,tra_re)
      status=nf_put_var_double(ncid,tram_imid,tra_im)
      status=nf_put_var_double(ncid,spm_reid,sp_re)
      status=nf_put_var_double(ncid,spm_imid,sp_im)
      status=nf_put_var_double(ncid,gmsp0id,gmsp0)
      status=nf_put_var_double(ncid,gmspmiid,gmspmi)
      endif
!----------------------------------------------
!     close file
!----------------------------------------------
      status=nf_close(ncid)
c
!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
      ilen=lnsig1(outputdir_name)
      call create_fname(fname,iyear,imonth,ilen1)
      fname=outputdir_name(1:ilen)//
     :        '/igcm_rg_'//fname(1:ilen1)
      ilen=lnsig(fname)
      print*,' Opening restartg file ',fname(1:ilen)
      status=nf_create(fname(1:ilen), nf_clobber, ncid)
      call check_err(status)
!----------------------------------------------
! define dimensions
!----------------------------------------------
      status=nf_def_dim(ncid, 'longitude',mg,mgid)
      status=nf_def_dim(ncid, 'latitude',jgg,jggid)
      status=nf_def_dim(ncid, 'sigma',nl,nlid)
      status=nf_def_dim(ncid, 'nrecs',nrecs,nrecsid)
!----------------------------------------------
! define variables
!----------------------------------------------
      status=nf_def_var(ncid,'kount',nf_int,1,nrecsid,kountid)
      status=nf_def_var(ncid,'month',nf_int,1,nrecsid,monthid)
      status=nf_def_var(ncid,'year',nf_int,1,nrecsid,yearid)
      status=nf_def_var(ncid,'idoy',nf_int,1,nrecsid,idoyid)
      status=nf_def_var(ncid,'rntape',nf_double,1,nrecsid,rntapeid)
      status=nf_def_var(ncid,'day',nf_double,1,nrecsid,dayid)
      status=nf_def_var(ncid,'doy',nf_double,1,nrecsid,doyid)
      dimid(1)=mgid
      status=nf_def_var(ncid,'longitude',nf_float,1,dimid,longid)
      dimid(1)=jggid
      status=nf_def_var(ncid,'latitude',nf_float,1,dimid,latid)
      dimid(1)=nlid
      status=nf_def_var(ncid,'sigma',nf_float,1,dimid,sigmaid)
      dimid(1)=mgid
      dimid(2)=jggid
      status=nf_def_var(ncid,'tstar',nf_double,2,dimid,tstarid)
      status=nf_def_var(ncid,'tdeep',nf_double,2,dimid,tdeepid)
      status=nf_def_var(ncid,'smstar',nf_double,2,dimid,smstarid)
      status=nf_def_var(ncid,'qstar',nf_double,2,dimid,qstarid)
      status=nf_def_var(ncid,'hsnow',nf_double,2,dimid,hsnowid)
      status=nf_def_var(ncid,'sqstar',nf_double,2,dimid,sqstarid)
      status=nf_def_var(ncid,'salb',nf_double,2,dimid,salbid)
      status=nf_def_var(ncid,'sbal',nf_double,2,dimid,sbalid)
      status=nf_def_var(ncid,'tstaro',nf_double,2,dimid,tstaroid)
      status=nf_def_var(ncid,'tdeepo',nf_double,2,dimid,tdeepoid)
      status=nf_def_var(ncid,'snet',nf_double,2,dimid,snetid)
      status=nf_def_var(ncid,'prec_larg',nf_double,2,dimid,prec_largid)
      status=nf_def_var(ncid,'prec_conv',nf_double,2,dimid,prec_convid)
      dimid3(1)=mgid
      dimid3(2)=jggid
      dimid3(3)=nlid
      status=nf_def_var(ncid,'htnet',nf_double,3,dimid3,htnetid)
!----------------------------------------------
! define attribute for variables (not necessary but helpful to user)
!----------------------------------------------
!
!----------------------------------------------
! end define mode
!----------------------------------------------
      status=nf_enddef(ncid)
!----------------------------------------------
! write variables to file
!----------------------------------------------
      status=nf_put_var_int(ncid,kountid,kount)
      status=nf_put_var_int(ncid,monthid,imonth)
      status=nf_put_var_int(ncid,yearid,iyear)
      status=nf_put_var_int(ncid,idoyid,idoy)


      if (kind(z_re).eq.4) then
      status=nf_put_var_real(ncid,rntapeid,rntape)
      status=nf_put_var_real(ncid,dayid,day)
      status=nf_put_var_real(ncid,doyid,doy)
      do i=1,mg
         along(i)=(i-1)*360.0/real(mg)
      end do
      status=nf_put_var_real(ncid,longid,along)
      status=nf_put_var_real(ncid,latid,alat)
      status=nf_put_var_real(ncid,sigmaid,sigma)
      call restructure1(tstar,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,tstarid,work)
      call restructure1(tdeep,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,tdeepid,work)
      call restructure1(smstar,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,smstarid,work)
      call restructure1(qstar,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,qstarid,work)
      call restructure1(hsnow,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,hsnowid,work)
      call restructure1(sqstar,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,sqstarid,work)
      call restructure1(salb,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,salbid,work)
      call restructure1(sbal,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,sbalid,work)
      call restructure1(tstaro,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,tstaroid,work)
      call restructure1(tdeepo,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,tdeepoid,work)
      call restructure1(snet,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,snetid,work)
      call restructure1(prec_larg,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,prec_largid,work)
      call restructure1(prec_conv,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_real(ncid,prec_convid,work)
      call restructure2(htnet,work1,mg,jg,nhem,nl,1.0,0.0,1)
      status=nf_put_var_real(ncid,htnetid,work1)
      endif
      if (kind(z_re).eq.8) then
      status=nf_put_var_double(ncid,rntapeid,rntape)
      status=nf_put_var_double(ncid,dayid,day)
      status=nf_put_var_double(ncid,doyid,doy)
      do i=1,mg
         along(i)=(i-1)*360.0/real(mg)
      end do
      status=nf_put_var_double(ncid,longid,along)
      status=nf_put_var_double(ncid,latid,alat)
      status=nf_put_var_double(ncid,sigmaid,sigma)
      call restructure1(tstar,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,tstarid,work)
      call restructure1(tdeep,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,tdeepid,work)
      call restructure1(smstar,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,smstarid,work)
      call restructure1(qstar,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,qstarid,work)
      call restructure1(hsnow,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,hsnowid,work)
      call restructure1(sqstar,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,sqstarid,work)
      call restructure1(salb,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,salbid,work)
      call restructure1(sbal,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,sbalid,work)
      call restructure1(tstaro,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,tstaroid,work)
      call restructure1(tdeepo,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,tdeepoid,work)
      call restructure1(snet,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,snetid,work)
      call restructure1(prec_larg,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,prec_largid,work)
      call restructure1(prec_conv,work,mg,jg,nhem,1.0,0.0,1)
      status=nf_put_var_double(ncid,prec_convid,work)
      call restructure2(htnet,work1,mg,jg,nhem,nl,1.0,0.0,1)
      status=nf_put_var_double(ncid,htnetid,work1)
      endif

!----------------------------------------------
! close file
!----------------------------------------------
      status=nf_close(ncid)
c
      WRITE(6,2000)11,KOUNT,RNTAPE,DAY,DOY
      KOUTR=0
c
      return
      end
