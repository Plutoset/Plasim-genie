*     DECK SURFM
C**********************************************************
C     SUBROUTINE SURFM
C**********************************************************
      subroutine SURFM

      implicit none

C-----------------------------------------------------------------------
C     perform soil moisture / snow depth update for timestep
C     have to do this after the CONVEC and LSCRN subs
C-----------------------------------------------------------------------
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'bats.cmn'
      include 'physca.cmn'
      include 'cpiers.cmn'
      include 'orog_g.cmn'
      include 'fluxes.cmn'

      INTEGER IHEM,J,I
      real    tzc,tz,frn,ffl
      REAL    TSCUR,CTQUSE,DWATER

      parameter (tzc=273.15)

C     
C     snow covered lookup table
C     
c      DIMENSION SNOLOOK(24)
c      DATA SNOLOOK /0.8, 0.8, 0.2, 0.2, 0.3, 0.3,
c     $     0.3, 0.2, 0.3, 0.5, 0.5, 0.5,
c     $     0.7, 0.6, 0.8, 0.7, 0.7, 0.7,
c     $     0.7, 0.7, 0.4, 0.8, 0.7, 0.8/
C     

      tz=tzc/ct

      FRN=SDW/(1000.*RADEA)
      FFL=AKAP*DELT


      do ihem=1,nhem
         do i=1,mg
            j=(ihem-1)*mgpp+i

            if (iland(j,jh).eq.1) then

               TSCUR=TSTAR(J,JH) ! bug fix RF 10/4/2000
               if (tscur.lt.tz) then
                  ctquse=ctqi
               else
                  ctquse=ctq
               endif

C     amount of water change in time DELT

               dwater=(-FFL*slbl(j)/ctquse+(rrcr(j)+rrlr(j))*FRN)

               if ((hsnow(j,jh).gt.0.).or.
     $              ((tstar(j,jh).lt.tz).and.(dwater.gt.0.))) then
                  hsnow(j,jh)=hsnow(j,jh)+dwater/sdsn
               else
                  smstar(j,jh)=smstar(j,jh)+dwater/sdw
               endif

c               if (hsnow(j,jh).le.0.) then
c                  salb(j,jh)=sbal(j,jh)
c               else
c                  salb(j,jh)=sbal(j,jh)+
c     $                    (sasnow-salb(j,jh))*hsnow(j,jh)/
c     $                    (hsnow(j,jh)+shsstar)
C     snow-covered albedo depends on vegetation
c     $                 (snolook(NINT(SVEGE(J,JH)))-salb(j,jh))
c     $                 *hsnow(j,jh)/(hsnow(j,jh)+shsstar)
C
c               endif

            endif
         enddo
      enddo

      return
      end
