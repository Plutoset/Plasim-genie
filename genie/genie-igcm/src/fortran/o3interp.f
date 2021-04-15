*DECK O3INTERP
C**********************************************************
C             SUBROUTINE O3INTERP
C**********************************************************
      subroutine O3INTERP(o3clim,o3mod,ps)

      implicit none

c
c This subroutine takes in a profile from climatology (o3clim) and
c interpolates to the model vertical grid (nl.gt.15), computing the
c model profile by constraining its column to agree with climatology.
c The model profile is passed back in o3mod.
c N.B. The o3clim passed to the routine must be on the pressure levels
c defined in the routine (ocliml).
c
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'spectr.cmn'
c
      real o3clim(15),ocliml(15),halflev(14),deltap(15),ohdcol(15)
      real o3mod(nl),dpmod(nl),modcol(nl)
      real ps, pressu
      logical ifirst
      save
      data ocliml/1.0,3.0,10.0,30.0,50.0,70.0,100.0,150.0,200.0,
     &              250.0,300.0,400.0,500.0,700.0,850.0/
      data ifirst/.true./
c

      INTEGER L,LO
      REAL    RHOO3,GR,COL,SUMRDP,FRACTION

      rhoo3=2.14 ! density of O3 at STP
      GR=GA*rhoo3 !g*density of O3 at STP
      if (ifirst) then
        ifirst=.false.
        do l=1,14
          halflev(l)=(ocliml(l+1)+ocliml(l))/2.0    ! in mb
        enddo
        deltap(1)=halflev(1)                        ! Top
        do l=2,14
          deltap(l)=halflev(l)-halflev(l-1)         ! In between
        enddo
        deltap(15)=1.0e3-halflev(14)                ! Bottom
        do l=1,15
          deltap(l)=deltap(l)*100.0          ! Convert all to Pa
        enddo
      endif

                               ! Compute overhead column of climatology
      col=0.0                  ! for this particular profile.
      do l=1,15
        col=col+o3clim(l)*deltap(l)
        ohdcol(l)=col*1.0e5/(GR)    ! In DU
      enddo

      do l=1,nl                    ! model delta pressure
        dpmod(l)=dsigma(l)*ps      ! in Pa
      enddo


      sumrdp=0.0     ! MMR*dp summed down column

      do 269 l=1,nl
        IF (l.LT.NL)THEN
           pressu=sigmah(l)*ps/100.0   ! in mb
        ELSE
           pressu=ps/100.0
        ENDIF
        do 270 lo=1,14
          if (pressu.lt.halflev(1)) then

            modcol(l)=(pressu/halflev(1))*ohdcol(1)  ! Model column (DU)
                                               ! forced from climatology
            o3mod(l)=(modcol(l)*GR/1.0e5-sumrdp)/dpmod(l)
            goto 271

          elseif (pressu.eq.halflev(lo)) then

            modcol(l)=ohdcol(lo)
            o3mod(l)=(modcol(l)*GR/1.0e5-sumrdp)/dpmod(l)
            goto 271

          elseif (pressu.gt.halflev(lo).and.pressu.lt.halflev(lo+1))
     &         then

            fraction=(pressu-halflev(lo))/(halflev(lo+1)-halflev(lo))
            modcol(l)=ohdcol(lo)+fraction*(ohdcol(lo+1)-ohdcol(lo))
            o3mod(l)=(modcol(l)*GR/1.0e5-sumrdp)/dpmod(l)
            goto 271

          elseif (pressu.ge.halflev(14)) then

            fraction=(pressu-halflev(14))/(1.0e3-halflev(14))
            modcol(l)=ohdcol(14)+fraction*(ohdcol(15)-ohdcol(14))
            o3mod(l)=(modcol(l)*GR/1.0e5-sumrdp)/dpmod(l)
            goto 271

          endif

 270  continue
 271  sumrdp=sumrdp+o3mod(l)*dpmod(l)
 269  continue
      RETURN
      END
