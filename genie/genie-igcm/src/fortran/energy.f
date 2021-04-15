      SUBROUTINE ENERGY
      implicit none
C     
C     Calculates various global diagnostic quantities
C     every itstp timesteps.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'outcon.cmn'
      include 'bats.cmn'
      include 'stats.cmn'
C
      COMPLEX Q(IGB)
      EQUIVALENCE (Q(1),TRA(1,1))
      COMPLEX TIG,SPIP,CTIG,CRGS
C
      integer :: i,l,il,iofs,ihem,ig,ip,jp,m
      real :: psitot,chitot,tmptot,totp,toti,totq,amq,st2b,st,
     :        tpsitt,tchitt,ttmptt,ttoti,ttotq,dsig,dsigh,rdig,rzig,
     :        rtig,rspip,rtl,amsp
C     
C     First remove planetary vorticity so Z contains relative vorticity
C     
      I=1
      DO L=1,NL
         Z(I)=Z(I)-EZ
         I=I+IGA
      END DO
C     
C     Calculate means - PSITOT RMS vorticity
C     CHITOT RMS divergence
C     TMPTOT RMS temperature
C     TOTP  IE+PE potential energy
C     AMSP mean surface pressure
C     
      PSITOT=0.0
      CHITOT=0.0
      TMPTOT=0.0
      TOTP=0.0
      TOTI=0.0
      TOTQ=0.0                  ! Total moisture diagnostic
      AMQ=0.0                   ! Accounts for p0 term 
C                               ! excluded from SPA(1) in MGRMLT.
      IL=1
      ST2B=0.
      ST=0.
      IOFS=0
      DO 800 IHEM=1,NHEM
         IG=IOFS
         DO 30 L=1,NL
            TPSITT=0.
            TCHITT=0.
            TTMPTT=0.
            TTOTI=0.
            TTOTQ=0.
            DSIG=DSIGMA(L)
            DSIGH=0.5*DSIG
            IP=IOFS
            DO 10 JP=1,NFP,MH
               IG=IG+1
               IP=IP+1
               RTIG=REAL(T(IG))
               RZIG=REAL(Z(IG))
               RDIG=REAL(D(IG))
               TPSITT=TPSITT+RZIG*RZIG
               TCHITT=TCHITT+RDIG*RDIG
               TTMPTT=TTMPTT+RTIG*RTIG
               RSPIP=REAL(SPA(IP))
               IF (L.EQ.1) THEN
                  TOTP=TOTP+0.5*RSPIP*REAL(GS(IP))
               ENDIF
               TTOTI=TTOTI+RSPIP*RTIG
               TTOTQ=TTOTQ+RSPIP*REAL(Q(IG))
 10         CONTINUE
            PSITOT=PSITOT+DSIGH*TPSITT
            CHITOT=CHITOT+DSIGH*TCHITT
            TMPTOT=TMPTOT+DSIGH*TTMPTT
            TOTI=TOTI+DSIGH*TTOTI
            TOTQ=TOTQ+DSIGH*TTOTQ
            TPSITT=0.
            TCHITT=0.
            TTMPTT=0.
            TTOTI=0.
            TTOTQ=0.
            DO 25 M=MOCT,MF,MOCT
               DO 20 JP=M,NF,MH
                  IG=IG+1
                  IP=IP+1
                  TIG=T(IG)
                  CTIG=CONJG(TIG)
                  SPIP=SPA(IP)
                  TPSITT=TPSITT+REAL(Z(IG)*CONJG(Z(IG)))
                  TCHITT=TCHITT+REAL(D(IG)*CONJG(D(IG)))
                  TTMPTT=TTMPTT+REAL(TIG*CTIG)
                  IF (L.EQ.1) THEN
                     CRGS=CONJG(GS(IP))
                     TOTP=TOTP+REAL(SPIP*CRGS)
                  ENDIF
                  TTOTI=TTOTI+REAL(SPIP*CTIG)
                  TTOTQ=TTOTQ+REAL(SPIP*CONJG(Q(IG)))
 20            CONTINUE
 25         CONTINUE
            PSITOT=PSITOT+DSIG*TPSITT
            CHITOT=CHITOT+DSIG*TCHITT
            TMPTOT=TMPTOT+DSIG*TTMPTT
            TOTI=TOTI+DSIG*TTOTI
            TOTQ=TOTQ+DSIG*TTOTQ
            IF (IHEM.EQ.1) THEN
               RTL=REAL(T(IL))
               ST2B=ST2B+T0(L)*RTL*DSIG
               ST=ST+RTL*DSIG
               AMQ=AMQ+DSIG*REAL(Q(IL))
               IL=IL+IGA
            ENDIF
            IG=IG+IGA-NWJ2
 30      CONTINUE
         IOFS=NWJ2
 800  CONTINUE
      AMSP=1.0+REAL(SPA(1))*RSQR2
      AMQ=CQ*RSQR2*AMQ
      TOTQ=CQ*TOTQ+AMQ
      PSITOT=SQRT(PSITOT)
      CHITOT=SQRT(CHITOT)
      TMPTOT=SQRT(TMPTOT+TOUT2+ST2B*SQR2)
      TOTP=TOTP+RSQR2*REAL(GS(1))+(AMSP*TOUT1+TOTI+RSQR2*ST)/AKAP
C     
      print*,'from energy.f in igcm:'
      WRITE(6,40)
      WRITE (6,50) KOUNT,PSITOT,CHITOT,TMPTOT,TOTP,TOTQ,AMSP
C
 40   FORMAT(3X,'KOUNT',3X,'RMSVORT',8X,'RMSDIV',8X,'RMSTEMP'
     +     ,8X,'PE+IE',8X,'TOTQ',9X,'MSP')
 50   FORMAT(I6,1X,4E15.6,2F13.10)
C     
C     Restore Z to absolute vorticity
C     
      I=1
      DO L=1,NL
         Z(I)=Z(I)+EZ
         I=I+IGA
      END DO
C     
      RETURN
      END
