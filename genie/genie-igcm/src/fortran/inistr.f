      SUBROUTINE INISTR
      implicit none
C     
C     Reads in data for a start/restart run
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'bats.cmn'
      include 'outcon.cmn'
      include 'restor.cmn'
      include 'restij.cmn'
      include 'stats.cmn'
      include 'balan.cmn'
      include 'orog_g.cmn'
      include 'radht.cmn'
      include 'files.cmn'
C     
C     maximum number of days stored in restart.12 - marc 28/5/03
C     netcdf ID for water vapour file - marc 28/5/03
C     
      integer :: i,kk,l,j,ihem,id,ktemp
      real :: daynear,rkount,rm1tape,rm2tape
C     
 2000 FORMAT('***WARNING*** KITS < 1 FOR AN INITIAL RUN.'/)
 2040 FORMAT(/' HISTORY RECORD READ',/
     +     ' KOUNT  RMTAPE  DAY  DOY =',I8,3F12.3)
 2041 FORMAT(/' RESTART RECORD READ FROM CHANNEL ',I3,/
     +     ' KOUNT  RMTAPE  DAY  DOY =',I8,3F12.3)
 2012 FORMAT(/'***ABORT*** THE RESTORATION RECORDS READ FROM CHANNEL '
     +     ,I3,/' ARE NOT IN CORRECT FORMAT ')
 2022 FORMAT(/'***ABORT*** THE RUN NUMBER IN THE RESTORATION RECORD '
     +     ,F10.3/' IS NOT THE SAME AS RNTAPO IN THE NAMELIST ',F10.3)
 2032 FORMAT(/'***ABORT*** CANNOT FIND THE CORRECT RESTORATION RECORD.'
     +     /' LOOKING FOR DAY',F8.2/,' BUT THE NEAREST RECORD FOUND',
     +     ' IS FOR DAY',F8.2)
 2042 FORMAT(' RESTORATION RECORD READ FROM CHANNEL ',I3,/
     +     ' KOUNT  RMTAPE  DAY  DOY =',I8,3F12.3)
 2050 FORMAT(/' SPECTRAL ARRAYS ARE SET TO ZERO ')
 2100 FORMAT(/' MASS INFORMATION READ AT RESTART:'
     :     /'    INITIAL (REFERENCE) MASS     = ',1PE20.12,' (PA)'
     :     /'    TIME-LAGGED MASS             = ',1PE20.12,' (PA)')
C     
C     Initialize spectral arrays to zero and overwrite as desired
C     

      DO 1 I=1,IGA
         SP(I)=(0.0,0.0)
         SPMI(I)=(0.,0.)
         GS(I)=(0.0,0.0)
    1 CONTINUE
      DO I=1,IGB
         Z(I)=(0.0,0.0)
         D(I)=(0.0,0.0)
         T(I)=(0.0,0.0)
         ZMI(I)=(0.0,0.0)
         DMI(I)=(0.0,0.0)
         TMI(I)=(0.0,0.0)
      END  DO
      DO KK=1,NTRAC
         DO I=1,IGB
            TRA(I,KK)=(0.0,0.0)
            TRAMI(I,KK)=(0.0,0.0)
         END DO
      END DO
      DO l=1,nl
         DO i=1,mg
            DO j=1,jg
               DO ihem=1,nhem
                  htnet(ihem,j,i,l)=0.
               END DO
            END DO
         END DO
      END DO
C     
C     Initialise current and global reference mass to zero (unset).
C     
      GMSP0=0.0
      GMSPMI=0.0
C     
      IF (.NOT.LRSTRT) THEN
C     
C     Initial run
C     
         IF ( KITS .LT. 1) THEN
            WRITE(6,2000)
         ENDIF
         DAY=0.0
         IF (KITS.EQ.0) THEN
            KTOTAL=KRUN
            KOUTD=0
            KOUTE=0
            KOUTH=0
            KOUTR=0
         ELSE
            KTOTAL=KRUN+KITS-1
            KOUTD=1-KITS
            KOUTE=1-KITS
            KOUTH=1-KITS
            KOUTR=1-KITS
         END IF
C     
C     Initialise restoration array
C     
         IF (LRESTIJ) CALL SETZT
C     
C     Initialise spectral arrays
C     
         IF (LBALAN) THEN
            CALL INIBAL
            KOUNT=-KBAL
            KSTART=0
         ELSE
            IF (LRESTIJ) THEN
               CALL INISP
            ELSE
               WRITE (6,2050)
            ENDIF
            KOUNT=0
            KSTART=KOUNT
         ENDIF
      ELSE
C     
C     Code for restart and normal mode perturbation runs.
C     assume spectral data is set up non-dimensionalised
C     on a history (LSHORT) or restart (.NOT.LSHORT) record.
C     
         ID=10
         DAYNEAR=-9999.5        ! allows desired day no. = 0
         IF (LSHORT) THEN
            call open_spec_restart(fname_spectral(1:ifname_spectral),
     :           kount,day,doy,iyear,imonth,idoy,
     :           itspd,
     :           lannual_restart,
     :           ldecadal_restart)
            KOUNT=NINT(RKOUNT)
            WRITE(6,2040) KOUNT,BEGDAY,DOY
c     
            KOUNT=0
            idoy=0
            imonth=1
            iyear=2000
            day=0.0
            doy=0.0
c     
            IF (KITS.EQ.0) THEN
               KTOTAL=KRUN
               KOUTD=0
               KOUTE=0
               KOUTH=0
               KOUTR=0
            ELSE
               KTOTAL=KRUN+KITS-1
               KOUTD=1-KITS
               KOUTE=1-KITS
               KOUTH=1-KITS
               KOUTR=1-KITS
            ENDIF
            KSTART=KOUNT
            DO 183 I=1,IGA
               SPMI(I)=SP(I)
 183        CONTINUE
            DO J=1,IGB
               ZMI(J)=Z(J)
               DMI(J)=D(J)
               TMI(J)=T(J)
            END DO
            DO KK=1,NTRAC
               DO J=1,IGB
                  TRAMI(J,KK)=TRA(J,KK)
               END DO
            END DO
         ELSE
            call open_spec_restart(fname_spectral(1:ifname_spectral),
     :           kount,day,doy,iyear,imonth,idoy,
     :           itspd,
     :           lannual_restart,
     :           ldecadal_restart)
C     
            WRITE(6,2041) ID,KOUNT,RM1TAPE,BEGDAY,DOY
            KTOTAL=KOUNT+KRUN
            KSTART=KOUNT
            KTEMP=KOUNT
            IF(KITS.GT.0) KTEMP=KOUNT+1-KITS
            KOUTD=KTEMP-KOUNTD*(KTEMP/KOUNTD)
            KOUTE=KTEMP-KOUNTE*(KTEMP/KOUNTE)
            KOUTH=KTEMP-KOUNTH*(KTEMP/KOUNTH)
            KOUTR=KTEMP-KOUNTR*(KTEMP/KOUNTR)
            IF ((KTEMP.GT.0).AND.(KOUTH.EQ.0)) THEN
               KOUTH=KOUNTH
            ENDIF
         END IF
C     
         IF (LMASCOR.AND.GMSP0.NE.0.0) THEN
            WRITE(6,2100) GMSP0*P0,GMSPMI*P0
         ENDIF
C     
C     Read in restoration state from separate file
C     
         IF (LRESTIJ) THEN
            ID=13
 182        READ(ID,END=1002)RKOUNT,RM1TAPE,DAY,DOY,TTRES,RM2TAPE
            IF (ABS(RM1TAPE-RM2TAPE) .GT. 1.0E-03) THEN
               WRITE(6,2012) ID
               CALL ABORT
            ENDIF
            IF (ABS(RM1TAPE-RNTAPO) .GT. 1.0E-03) THEN
               WRITE(6,2022) RM1TAPE,RNTAPO
               CALL ABORT
            ENDIF
            IF (ABS(DAY-BEGDAY) .GT. 1.0E-02) THEN
               IF (ABS(DAY-BEGDAY) .LT. ABS(DAYNEAR-BEGDAY)) THEN
                  DAYNEAR = DAY
               ENDIF
               GOTO 182
            ELSE
               GOTO 202
            ENDIF
 1002       WRITE(6,2032) BEGDAY,DAYNEAR
            CALL ABORT
C     
 202        WRITE(6,2042) ID,NINT(RKOUNT),RM1TAPE,BEGDAY,DOY
         ELSE
            IF(DAMP.GT.0.0) THEN
               REWIND 13
               READ(13)ZRES,DRES,TRES,SPRES
            END IF
         ENDIF
C     
C     Initialise any new tracer fields and surface state.
C     
         CALL ICTRAC
C     
      END IF
C     
C     READS in T21 orography  part
C     Now in icesheet module and in initialise_atmos.F ....     

C     
      return
      END
c     
      subroutine open_spec_restart(fname,kount,day,doy,iyear,
     :                             imonth,idoy,itspd,
     :                             lannual_restart,
     :                             ldecadal_restart)
      implicit none
#include "param1.cmn"
      include 'param2.cmn'
      include 'spectr.cmn'
      include 'blank.cmn'
      include 'stats.cmn'
      include 'igcm_nlevs.cmn'
      include 't42.cmn'
 
      character fname*(*)
      integer ncid,ilen,lnsig,kount2,ifail,kount,iyear,iyear2,
     :        idoy,idoy2,imonth,imonth2,iyear2a,imonth2a,itspd
      real day2,doy2,rntape2,day,doy
      logical lannual_restart,ldecadal_restart
c     
      integer i,i1

      print*,'Do we have a cold start...?',coldstart

      ilen=lnsig(fname)
      print*,' Opening yes ',fname(1:ilen)
      call open_file_nc(fname(1:ilen), ncid)
!     
!     read day
!     
      call get1di_data_nc(ncid,'kount',1,kount2,ifail)
      call get1di_data_nc(ncid,'month',1,imonth2,ifail)
      call get1di_data_nc(ncid,'year',1,iyear2,ifail)
      call get1di_data_nc(ncid,'idoy',1,idoy2,ifail)
      call get1d_data_nc(ncid,'rntape',1,rntape2,ifail)
      call get1d_data_nc(ncid,'day',1,day2,ifail)
      call get1d_data_nc(ncid,'doy',1,doy2,ifail)
      print*,' day= ',day2,' doy= ',doy2
      print*,' imonth= ',imonth2,' iyear = ',iyear2
      print*,' rntape= ',rntape2,' kount = ',kount2
      if (lrstrt) then
         imonth2a=int(idoy2/(itspd*30))+1     
         iyear2a=iyear2
         if (imonth2a.gt.12) then
            imonth2a=imonth2a-12
            iyear2a=-(iyear2+1)
         else
            lannual_restart=.false.
         end if
         if (mod(-iyear2a,10).ne.0) ldecadal_restart=.false.
         print*,' imonth_a= ',imonth2a,' iyear_a= ',iyear2a
      end if
!     
!     read data at the right rday
!     

      if (.not.coldstart) then

      call get1d_slice_comp_nc(ncid, 'z', igb, z,ifail)
      if (ifail.ne.0) then
         print*,' Error reading vorticty ',ifail
         stop 1
      end if
      call get1d_slice_comp_nc(ncid, 'd', igb, d,ifail)
      if (ifail.ne.0) then
         print*,' Error reading divergence ',ifail
         stop 1
      end if
      call get1d_slice_comp_nc(ncid, 't', igb, t,ifail)
      if (ifail.ne.0) then
         print*,' Error reading temperature ',ifail
         stop 1
      end if

      if (ntrac.ne.0) then
         call get2d_slice_comp_nc(ncid, 'tra', igb, ntrac, tra,ifail)
         if (ifail.ne.0) then
            print*,' Error reading tracer ',ifail
            stop 1
         end if
      endif
!     
      endif ! ends if (.not.coldstart)

       if (coldstart_gmsp) then
         gmsp0=0.0
         gmspmi=gmsp0
         sp(:)=0.0
       else
        call get1d_slice_comp_nc(ncid, 'sp', iga, sp,ifail)
        if (ifail.ne.0) then
          print*,' Error reading surface pressure ',ifail
          stop 1
        end if
        if (coldstart) then
          gmsp0=0.0
          gmspmi=gmsp0
        else
          call get1d_data_nc(ncid, 'gmsp0',1,gmsp0,ifail)
          if (ifail.ne.0) gmsp0=0.0
          call get1d_data_nc(ncid, 'gmspmi',1,gmspmi,ifail)
          if (ifail.ne.0) gmspmi=gmsp0
        endif
       endif

      if (.not.lshort) then

      if (.not.coldstart) then

         call get1d_slice_comp_nc(ncid, 'zm', igb, zmi,ifail)
         if (ifail.ne.0) then
            print*,' Error reading vorticity mi ',ifail
            stop 1
         end if
         call get1d_slice_comp_nc(ncid, 'dm', igb, dmi,ifail)
         if (ifail.ne.0) then
            print*,' Error reading divergence mi ',ifail
            stop 1
         end if
         call get1d_slice_comp_nc(ncid, 'tm', igb, tmi,ifail)
         if (ifail.ne.0) then
            print*,' Error reading temperature mi ',ifail
            stop 1
         end if

         if (ntrac.ne.0) then
            call get2d_slice_comp_nc(ncid, 'tram', igb, ntrac, trami,
     :           ifail)
            if (ifail.ne.0) then
               print*,' Error reading tracer mi ',ifail
               stop 1
            end if
         end if
c
      endif ! ends if (.not.coldstart)

         call get1d_slice_comp_nc(ncid, 'spm', iga, spmi,ifail)
         if (ifail.ne.0) then
            print*,' Error reading surface pressure mi ',ifail
            stop 1
         end if

         kount=kount2
         if (lrstrt) then 
            imonth=imonth2a
            iyear=iyear2a
         else
            imonth=imonth2
            iyear=iyear2
         end if 
         idoy=idoy2
         day=day2
         doy=doy2
c
      end if
!     

      if (coldstart) then

      do i=1,idb*nhem
      t(i)=0.0
      d(i)=0.0
      z(i)=0.0
      tmi(i)=0.0
      dmi(i)=0.0
      zmi(i)=0.0
      enddo

      do i=1,nl
      i1=(i-1)*iga+1
      z(i1)=real(ez)
      t(i1)=tempzero/700.0
      enddo

      endif ! ends if (coldstart)

      call close_file_nc(fname(1:ilen), ncid)
!     
      return
      end
