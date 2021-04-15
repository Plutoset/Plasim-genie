      SUBROUTINE WRITENC_RESTOR
      implicit none
C     
C     Write a restoration record
c     THIS ROUTINE WILL ONLY WORK IF THE IGCM IS IN REAL*4!!
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'outcon.cmn'
      include 'bats.cmn'
      include 'restij.cmn'
C     
      complex, parameter :: ic = (0.0, 1.0)
      integer, parameter :: nrecs=1
c     
      real :: rkount

      real, dimension(igb) :: ttres_re, ttres_im

      integer :: i

!     netcdf variables
      integer :: ncid, status

      integer :: igbid, nrecsid, rkountid, rntapeid
      integer :: dayid, doyid
      integer :: dimid1(2)
      integer :: ttres_reid, ttres_imid

      character fname*200
      integer ilen,ilen1,lnsig1,lnsig
      
      include 'files.cmn'
#include "netcdf.inc"
C     
 2020 FORMAT(/' RESTORATION RECORD WRITTEN TO CHANNEL ',I3,/
     +     ' RKOUNT  RNTAPE  DAY  DOY  =',4F12.3)
C     
      IF (KOUTH.EQ.KOUNTH.OR.KOUTR.EQ.KOUNTR) THEN
!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
         ilen=lnsig1(outputdir_name)
         call create_fname(fname,iyear,imonth,ilen1)
         fname=outputdir_name(1:ilen)//
     :        '/igcm_re_'//fname(1:ilen1)
         ilen=lnsig(fname)
         print*,' Opening restore file ',fname(1:ilen)
         status=nf_create(fname(1:ilen), nf_clobber, ncid)
         call check_err(status)
!----------------------------------------------
!     define dimensions
!----------------------------------------------
         status=nf_def_dim(ncid, 'igb',igb,igbid)
         status=nf_def_dim(ncid, 'nrecs',nrecs,nrecsid)
!----------------------------------------------
!     define variables
!----------------------------------------------
         status=nf_def_var(ncid,'rkount',nf_float,1,nrecsid,rkountid)
         status=nf_def_var(ncid,'rntape',nf_double,1,nrecsid,rntapeid)
         status=nf_def_var(ncid,'day',nf_double,1,nrecsid,dayid)
         status=nf_def_var(ncid,'doy',nf_double,1,nrecsid,doyid)
         dimid1(1)=igbid
         status=nf_def_var(ncid,'ttres_re',nf_double,1,dimid1,
     :                     ttres_reid)
         status=nf_def_var(ncid,'ttres_im',nf_double,1,dimid1,
     :                     ttres_imid)
!----------------------------------------------
!     end define mode
!----------------------------------------------
         status=nf_enddef(ncid)
!-------------------------------------------------------
!     separate into real and imaginary
!-------------------------------------------------------
         do i=1, igb
            ttres_re(i)=real(ttres(i))
            ttres_im(i)=real(-ic*ttres(i))
         enddo
!----------------------------------------------
!     write variables to file
!----------------------------------------------
         rkount=kount
         status=nf_put_var_real(ncid,rkountid,rkount)
         status=nf_put_var_real(ncid,rntapeid,rntape)
         status=nf_put_var_real(ncid,dayid,day)
         status=nf_put_var_real(ncid,doyid,doy)
         status=nf_put_var_real(ncid,ttres_reid,ttres_re)
         status=nf_put_var_real(ncid,ttres_imid,ttres_im)
!----------------------------------------------
!     close file
!----------------------------------------------
         status=nf_close(ncid)
         WRITE(6,2020)13,RKOUNT,RNTAPE,DAY,DOY
      ENDIF
C     
      RETURN
      END
