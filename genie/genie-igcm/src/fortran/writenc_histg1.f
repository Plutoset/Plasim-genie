      subroutine writenc_histg1(imode)
C
c     THIS ROUTINE WILL ONLY WORK IF THE IGCM IS IN REAL*4!!
C
      implicit none
#include "param1.cmn"
      include 'param2.cmn'
      include 'outcon.cmn'
      include 'bats.cmn'
      include 'blank.cmn'
      include 'gridpp.cmn'
      include 'ptendz.cmn'
      include 'fluxes.cmn'
      include 'legau.cmn'
C     
      integer, parameter :: nrecs=1
      integer :: i,imode

!     netcdf variables
      integer :: ncid, status

      integer :: mgid, nrecsid, nlid, jggid
      integer :: dayid, doyid,sigmaid,longid,latid
      integer :: dimid(2), dimid3(3),dimid4(2),dimid5(1)
      integer :: ssblid, shblid,slblid,rrcrid,rrlrid,
     &     rflux1id, rflux2id, rflux3id, rflux4id,
     &     rflux5id, rflux6id, salbid, tstarid, tdeepid,
     &     qstarid, smstarid, hsnowid, cld1id, cld2id,
     &     cld3id, cld4id, cld5id, tnlgid,
     &     autvdzid,avtvdzid,attvdzid,aqtvdzid,attcrzid,
     &     aqtcrzid,attlrzid,aqtlrzid,attrdzid,actcrzid,
     &     actlrzid,autblzid,avtblzid,attblzid,aqtblzid
      real work(mg,jgg),work1(mg,jgg,nl)
      real along(mg)

      character fname*200
      integer ilen,ilen1,lnsig1,lnsig
      
      include 'files.cmn'
      include 'netcdf.inc'
c     
      integer iddz
      PARAMETER(IDDZ=(11*NL+4)*JGG)
      REAL ADDZ(IDDZ)
      EQUIVALENCE (ADDZ(1),AUTVDZ(1))
c     
      real rkp
c     
      save
c     
      if (imode.eq.0) then
!-------------------------------------------------------
!     create a netcdf file
!-------------------------------------------------------
         ilen=lnsig1(outputdir_name)
         call create_fname(fname,iyear,imonth,ilen1)
         fname=outputdir_name(1:ilen)//
     :        '/igcm_hg_'//fname(1:ilen1)
         ilen=lnsig(fname)
         print*,' Opening histg file ',fname(1:ilen)
         status=nf_create(fname(1:ilen), nf_clobber, ncid)
         call check_err(status)
!----------------------------------------------
!     define dimensions
!----------------------------------------------
         status=nf_def_dim(ncid, 'longitude',mg,mgid)
         status=nf_def_dim(ncid, 'latitude',jgg,jggid)
         status=nf_def_dim(ncid, 'sigma',nl,nlid)
         status=nf_def_dim(ncid, 'nrecs',nrecs,nrecsid)
!----------------------------------------------
!     define variables
!----------------------------------------------
         status=nf_def_var(ncid,'day',nf_float,1,nrecsid,dayid)
         status=nf_def_var(ncid,'doy',nf_float,1,nrecsid,doyid)
         dimid(1)=mgid
         status=nf_def_var(ncid,'longitude',nf_float,1,dimid,longid)
         dimid(1)=jggid
         status=nf_def_var(ncid,'latitude',nf_float,1,dimid,latid)
         dimid(1)=nlid
         status=nf_def_var(ncid,'sigma',nf_float,1,dimid,sigmaid)
         dimid(1)=mgid
         dimid(2)=jggid
         status=nf_def_var(ncid,'ssbl',nf_float,2,dimid,ssblid)
         status=nf_def_var(ncid,'shbl',nf_float,2,dimid,shblid)
         status=nf_def_var(ncid,'slbl',nf_float,2,dimid,slblid)
         status=nf_def_var(ncid,'rrcr',nf_float,2,dimid,rrcrid)
         status=nf_def_var(ncid,'rrlr',nf_float,2,dimid,rrlrid)
         status=nf_def_var(ncid,'rflux1',nf_float,2,dimid,rflux1id)
         status=nf_def_var(ncid,'rflux2',nf_float,2,dimid,rflux2id)
         status=nf_def_var(ncid,'rflux3',nf_float,2,dimid,rflux3id)
         status=nf_def_var(ncid,'rflux4',nf_float,2,dimid,rflux4id)
         status=nf_def_var(ncid,'rflux5',nf_float,2,dimid,rflux5id)
         status=nf_def_var(ncid,'rflux6',nf_float,2,dimid,rflux6id)
         status=nf_def_var(ncid,'salb',nf_float,2,dimid,salbid)
         status=nf_def_var(ncid,'tstar',nf_float,2,dimid,tstarid)
         status=nf_def_var(ncid,'tdeep',nf_float,2,dimid,tdeepid)
         status=nf_def_var(ncid,'qstar',nf_float,2,dimid,qstarid)
         status=nf_def_var(ncid,'smstar',nf_float,2,dimid,smstarid)
         status=nf_def_var(ncid,'hsnow',nf_float,2,dimid,hsnowid)
         status=nf_def_var(ncid,'cld1',nf_float,2,dimid,cld1id)
         status=nf_def_var(ncid,'cld2',nf_float,2,dimid,cld2id)
         status=nf_def_var(ncid,'cld3',nf_float,2,dimid,cld3id)
         status=nf_def_var(ncid,'cld4',nf_float,2,dimid,cld4id)
         status=nf_def_var(ncid,'cld5',nf_float,2,dimid,cld5id)
         dimid3(1)=mgid
         dimid3(2)=jggid
         dimid3(3)=nlid
         status=nf_def_var(ncid,'tnlg',nf_float,3,dimid3,tnlgid)
         dimid4(1)=jggid
         dimid4(2)=nlid
         status=nf_def_var(ncid,'autvdz',nf_float,2,dimid4,autvdzid)
         status=nf_def_var(ncid,'avtvdz',nf_float,2,dimid4,avtvdzid)
         status=nf_def_var(ncid,'attvdz',nf_float,2,dimid4,attvdzid)
         status=nf_def_var(ncid,'aqtvdz',nf_float,2,dimid4,aqtvdzid)
         status=nf_def_var(ncid,'attcrz',nf_float,2,dimid4,attcrzid)
         status=nf_def_var(ncid,'aqtcrz',nf_float,2,dimid4,aqtcrzid)
         status=nf_def_var(ncid,'attlrz',nf_float,2,dimid4,attlrzid)
         status=nf_def_var(ncid,'aqtlrz',nf_float,2,dimid4,aqtlrzid)
         status=nf_def_var(ncid,'attrdz',nf_float,2,dimid4,attrdzid)
         status=nf_def_var(ncid,'actcrz',nf_float,2,dimid4,actcrzid)
         status=nf_def_var(ncid,'actlrz',nf_float,2,dimid4,actlrzid)
         dimid5(1)=jggid
         status=nf_def_var(ncid,'autblz',nf_float,1,dimid5,autblzid)
         status=nf_def_var(ncid,'avtblz',nf_float,1,dimid5,avtblzid)
         status=nf_def_var(ncid,'attblz',nf_float,1,dimid5,attblzid)
         status=nf_def_var(ncid,'aqtblz',nf_float,1,dimid5,aqtblzid)
!----------------------------------------------
!     define attribute for variables (not necessary but help to user)
!----------------------------------------------

!----------------------------------------------
!     end define mode
!----------------------------------------------
         status=nf_enddef(ncid)
!----------------------------------------------
!     write variables to file
!----------------------------------------------
         status=nf_put_var_real(ncid,dayid,day)
         status=nf_put_var_real(ncid,doyid,doy)
         do i=1,mg
            along(i)=(i-1)*360.0/real(mg)
         end do
         status=nf_put_var_real(ncid,longid,along)
         status=nf_put_var_real(ncid,latid,alat)
         status=nf_put_var_real(ncid,sigmaid,sigma)
         call restructure1(assbl,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,ssblid,work)
         call restructure1(ashbl,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,shblid,work)
         call restructure1(aslbl,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,slblid,work)
         call restructure1(arrcr,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,rrcrid,work)
         call restructure1(arrlr,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,rrlrid,work)
         call restructure1(arflux(1,1,1),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,rflux1id,work)
         call restructure1(arflux(1,1,2),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,rflux2id,work)
         call restructure1(arflux(1,1,3),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,rflux3id,work)
         call restructure1(arflux(1,1,4),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,rflux4id,work)
         call restructure1(arflux(1,1,5),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,rflux5id,work)
         call restructure1(arflux(1,1,6),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,rflux6id,work)
         call restructure1(asalb,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,salbid,work)
         call restructure1(atstar,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,tstarid,work)
         call restructure1(atdeep,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,tdeepid,work)
         call restructure1(aqstar,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,qstarid,work)
         call restructure1(asmstar,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,smstarid,work)
         call restructure1(ahsnow,work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,hsnowid,work)
         call restructure1(acld(1,1,1),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,cld1id,work)
         call restructure1(acld(1,1,2),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,cld2id,work)
         call restructure1(acld(1,1,3),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,cld3id,work)
         call restructure1(acld(1,1,4),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,cld4id,work)
         call restructure1(acld(1,1,5),work,mg,jg,nhem,1.0,0.0,1)
         status=nf_put_var_real(ncid,cld5id,work)
         call restructure3(atnlg,work1,mg,jg,nhem,nl,1.0,0.0,1)
         status=nf_put_var_real(ncid,tnlgid,work1)
c     
      else
         RKP=1.0/REAL(KOUNTH)
         DO I=1,IDDZ
            ADDZ(I)=ADDZ(I)*RKP
         END DO
         status=nf_put_var_real(ncid,autvdzid,autvdz)
         status=nf_put_var_real(ncid,avtvdzid,avtvdz)
         status=nf_put_var_real(ncid,attvdzid,attvdz)
         status=nf_put_var_real(ncid,aqtvdzid,aqtvdz)
         status=nf_put_var_real(ncid,attcrzid,attcrz)
         status=nf_put_var_real(ncid,aqtcrzid,aqtcrz)
         status=nf_put_var_real(ncid,attlrzid,attlrz)
         status=nf_put_var_real(ncid,aqtlrzid,aqtlrz)
         status=nf_put_var_real(ncid,attrdzid,attrdz)
         status=nf_put_var_real(ncid,actcrzid,actcrz)
         status=nf_put_var_real(ncid,actlrzid,actlrz)
         status=nf_put_var_real(ncid,autblzid,autblz)
         status=nf_put_var_real(ncid,avtblzid,avtblz)
         status=nf_put_var_real(ncid,attblzid,attblz)
         status=nf_put_var_real(ncid,aqtblzid,aqtblz)
         status=nf_close(ncid)
      end if
c     
      return
      end
