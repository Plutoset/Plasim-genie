      SUBROUTINE SFCT(JH,IFIRST,TROPHT)

      IMPLICIT NONE

C     
C     Subroutine to find new T and Q based on DOY and climatology
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'bats.cmn'
      include 'physca.cmn'
      include 'orog_g.cmn'
      include 'blank.cmn'
      include 'files.cmn'
      include 'climatologies.cmn'
C     
      INTEGER JH,IFIRST,JH1,MTH1,MTH2,K,J
      REAL    AMFRAC
      integer cmth
      character mn(12)*3
      data mn/'jan','feb','mar','apr','may','jun','jul','aug'
     &     ,'sep','oct','nov','dec'/
      data mth1/0/
      real tropht(mg,nhem,jg)
      save mth1
c     
      CALL CALNDR_igcm(DOY,MTH2,AMFRAC)
!-------------------------------------------
!     open netcdf file and read in sea surface temperature
!-------------------------------------------
      IF (IFIRST.EQ.1) THEN
         cmth=mth2
         if (jh.eq.1) print*,' Reading tqpap month ',cmth,mn(cmth)
         tropht(1:mg,1,jh)=tropht_clim(1:mg,jh,cmth)
         if (nhem.eq.2) then
            jh1=jgg+1-jh
            tropht(1:mg,2,jh)=tropht_clim(1:mg,jh1,cmth)
         end if
c     
         IF (JH.EQ.JG) THEN
            write(6,*)'CLIMATOLOGICAL TROPOPAUSE HEIGHT'
            do k=1,nhem
               write(6,*)(tropht(1,k,j),j=1,jg)
            enddo
         ENDIF
      ENDIF                     ! IFIRST
c     
      IF (MTH2.NE.MTH1) THEN    !time to change month
         cmth=mth2+1
         IF (cmth.eq.13) cmth=1
         if (jh.eq.1) print*,' Reading tqpap month ',cmth,mn(cmth)
         tropht(1:mg,1,jh)=tropht_clim(1:mg,jh,cmth)
         if (nhem.eq.2) then
            jh1=jgg+1-jh
            tropht(1:mg,2,jh)=tropht_clim(1:mg,jh1,cmth)
         end if
         IF (JH.EQ.JG) THEN
            MTH1=MTH2           ! stop doing loop until next month
         ENDIF
      ENDIF                     ! MTH2.NE.MTH1
c     
      RETURN
      END
