*DECK INIRESIJ
C*************************************************************
C               SUBROUTINE INIRESIJ
C*************************************************************
      SUBROUTINE INIRESIJ

      IMPLICIT NONE

C
C     Sets up restoration variables and arrays. Sets NAMELIST
C     variables to their default settings, then reads NAMELIST
C     Sets up Rayleigh friction coefficients.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'restij.cmn'
      include 'blank.cmn'
      include 'bats.cmn'

      INTEGER L
      REAL    RESTIM,TTROP,STPS,THING
      integer ios

      DIMENSION RESTIM(NL)
C
      NAMELIST/INPRSIJ/ DTNS,DTEP,ALR,DTTRP,ZTROP,TGR,YRLEN
C
C     TFRC and RESTIM removed from namelist
      TFRC(NL)=1.
      DO 19 L=1,NL-1
         TFRC(L) = 0.! Rayleigh friction time scale IN DAYS
! NB zero here equates to infinity - no friction.
 19   CONTINUE
      DO 20 L=1,NL
         RESTIM(L) = 15.0
 20   CONTINUE
c
c     namelist defaults 
      DTNS=0.
      DTEP=60.
      ALR=6.5E-03
      DTTRP=2.
      ZTROP=12.0E03
      TGR=288.
      YRLEN=0.
C
      read(UNIT=7,NML=INPRSIJ,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INPRSIJ namelist'
         stop
      end if
      WRITE(6,INPRSIJ)
C
C     Dimensionless coefficient for Newtonian cooling friction
C     and timestep. A day is 2*pi in non dimensional
C     units using omega as the unit of frequency.
C
      DO 22 L=1,NL
         IF (RESTIM(L).GT.0.0) THEN
            DDAMP(L)=1.0/(PI2*RESTIM(L))
         ELSE
            DDAMP(L)=0.0
         ENDIF
         IF (TFRC(L).GT.0.0) THEN
            TFRC(L)=1.0/(PI2*TFRC(L))
         ELSE
            TFRC(L)=0.0
         ENDIF
 22   CONTINUE
C
C     Make temperatures dimensionless
C
      DTNS=DTNS/CT
      DTEP=DTEP/CT
      DTTRP=DTTRP/CT
C
C     Loop to set array FAC - this controls temperature gradients
C     as a function of SIGMA in TTRES. It is a sine wave from one
C     at SIGMA = 1 to zero at STPS (SIGMA at the tropopause).
C
C     First find SIGMA at ZTROP
C
      TTROP = TGR - ZTROP*ALR
      STPS = (TTROP/TGR)**(GA/(ALR*GASCON))
      DO 600 L=1,NL
         THING=SIN(0.5*PI*(SIGMA(L)-STPS)/(1.-STPS))
         IF (THING.LT.0.) THEN
            FAC(L)=0.
         ELSE
            FAC(L)=THING
         ENDIF
600   CONTINUE
C
      END
