      SUBROUTINE ICTRAC

      IMPLICIT NONE

C     
C     *********PMF Version 1.0 (8.4.97)*******************
C     Subroutine which initialises the tracer fields.
C     Tracers can be started as any function of the usual grid
C     point fields.
C     Mass mixing ratios converted to volume mixing ratios for
C     flux form of advection.
C     John Methven, 10.04.95
C     Slighly modified to add more tracers and
C     add array GWORK Piers Forster 22.01.97
C     
C     Changed again to combine with phys2 update
C     to include surface water and temperature
C     initilization.
C     Also lets water vapour profile be cacluated by setting
C     surface RH     Piers Forster  18.02.97
C     
C     Flag LFLUX added for FLUX form of advection
C     

c     Beware with this routine, now I have taken out qstar
c       from the igcm, it may behave oddly, or not restart.
c     I don't think it's a problem though because qstar is only used
c       to output ascii, and we always have lnsurf=.false.

#include "param1.cmn"
      include 'param2.cmn'

      INTEGER NLTRG,NITRWG,IDDAGFL,IDPLG,L,KK,I,IH,NTALL,NRSTALL,K
C      INTEGER DP,DQ
      INTEGER IOFM,IHEM,J,LL,NTTR,NRSTTR,NCHAN
      REAL    SPZ,ESCON,PQSAT
      REAL,EXTERNAL :: SSUM

      integer ios

      PARAMETER(NLTRG=NLTR*NHEM,NITRWG=7*NL*NHEM
     :     ,IDDAGFL=NITRWG*MGPP,IDPLG=3*IGC)
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'legau.cmn'
      include 'comfft.cmn'
      include 'polyno.cmn'
      include 'gridp.cmn'
      include 'bats.cmn'
      include 'physca.cmn'
      include 'comgrm.cmn'
C     
      LOGICAL LNSURF,LRH
      REAL RH(NL),TDUM(JG),QDUM(JG)
      COMPLEX GWORK(IGL,(3+NTRAC)*NL+2)
      REAL DAGFL(IDDAGFL),DAGPLG(IDPLG),TEMPTR(IGD),DAFTR(NLTRG*MGPP)
      EQUIVALENCE (DAGFL(1),CHIG(1)),(DAGPLG(1),PLG(1))
     :     ,(DAFTR(1),TRAG(1,1))
      CHARACTER LHEM(2)*25
      DATA LHEM/'  N HEM  EQUATOR.....POLE'
     :     ,'  S HEM  EQUATOR.....POLE'/
C     
      NAMELIST/INQ/LRH,RH,LNSURF
C     
 2010 FORMAT('0SURFACE TEMPERATURE IN DEGC',A25/20(1X,F8.2))
 2020 FORMAT('0SURFACE SPECIFIC HUMIDITY IN G/KG',A25/20(1X,f8.4))
 2030 FORMAT(3X,20(1X,F8.2))
 2040 FORMAT(3X,20(1X,F8.4))
C     
      LRH=.FALSE.
      LNSURF=.FALSE.
      DO L=1,NL
         RH(L)=90.0*SIGMA(L)
      ENDDO
      read(UNIT=7,NML=INQ,IOSTAT=ios)
      if (ios /= 0) then
         print*,'ERROR: could not read IGCM INQ namelist'
         stop
      end if
      WRITE(6,INQ)
C     
      IF (.NOT.LRH.AND.NTRACO.LT.NTRAC) THEN
         WRITE(6,'(A25)') 'No water Vapour. Need to set LRH to TRUE'
         CALL ABORT
      ENDIF
C     convert RH from % to fraction
      DO L=1,NL
         RH(L)=RH(L)/100.0
      ENDDO
C     Surface characteristics now read in in INISURF, not here
C     
C     Save old tracers in the array TRAT and reset TRA to zero.
C     
      IF (NTRACO.GT.0) THEN
         DO KK=1,NTRACO
            DO I=1,IGB
               TRAT(I,KK)=TRA(I,KK)
               TRA(I,KK)=0.0
            END DO
         END DO
      ENDIF
C     
C     Loop over latitude
C     
      JL=1
      IF(JGL.EQ.1) REWIND 25
      DO 400 IH=1,JG
         JH=IH
         IF(JGL.EQ.1) THEN
            WRITE(6,*) 'ERROR: you should not reach here'
            WRITE(6,*) 'not expected to have only 2 N-S grid boxes'
            STOP
C           DAGW: vestigial code:
C           READ(25) ALP,DALP,DP,DQ
         ENDIF
C     
C     Spectral to grid transforms for initial model variables.
C     Calculate the horizontal derivatives of temperature as well
C     as the usual derivatives done by LTI.
C     
         CALL LTI
         CALL LTIDT
         NTALL=(NITRWG-1)/NCRAY
         NRSTALL=NITRWG-NCRAY*NTALL
C     
C     Sets up surface temperature here
C     

C     
         IF(NTALL.EQ.0) GOTO 40
         DO I=1,NTALL
            CALL FFT991(DAGFL(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX
     :           ,1,MGPP,MG,NCRAY,1)
         END DO
 40      CONTINUE
         CALL FFT991(DAGFL(1+NTALL*NCRAY*MGPP),WORK,TRIG,IFAX
     :        ,1,MGPP,MG,NRSTALL,1)
         CALL FFT991(DAGPLG(1),WORK,TRIG,IFAX,1,MGPP,MG,3*NHEM,1)
C     
C     Grid point calculations for surface pressure,
C     temperature T (=TO+T') and vertical derivatives of U,V and T.
C     
         DO I=1,IGC
            PLG(I)=EXP(PLG(I))
         END DO
         K=0
         DO L=1,NL
            DO I=1,IGC
               K=K+1
               TG(K)=TG(K)+T0(L)
               IF (LNSURF.AND.L.EQ.NL) THEN
                  TSTAR(I,IH)=TG(K)/SKAP(NL)
               ENDIF
            END DO
         END DO
C     
         CALL DLSGCR(IGC,UG,RGG,DUDLSG,IGC,NL)
         CALL DLSGCR(IGC,VG,RGG,DVDLSG,IGC,NL)
         CALL DLSGCR(IGC,TG,RGG,DTDLSG,IGC,NL)
C     
C     Grid point calculations for new tracer fields at one
C     latitude (IH).
C     
C     initilise water vapour if LRH set  depends on RH
C     goes into tracer No. 1
         IOFM=0
         DO 810 IHEM=1,NHEM
            SPZ=SSUM(MG,PLG(1+IOFM),1)/REAL(MG)
            IF (LRH) THEN
               DO I=1,MG
                  J=I+IOFM
                  ESCON=1./PLG(J)
                  K=J
                  DO L=1,NL
                     TRAG(K,1)=ESCON*RH(L)*PQSAT(TG(K))/SIGMA(L)
                     K=K+IGC
                  ENDDO
               ENDDO
            ENDIF
            IF (LNSURF) THEN
               ESCON=1./SPZ
               DO K=1,IGC
                  QSTAR(K,IH)=ESCON*PQSAT(TSTAR(K,IH))
               ENDDO
            ENDIF
            IOFM=MGPP
 810     CONTINUE

         DO 70 KK=NTRACO+1,NTRAC
            DO I=1,IGD
               TEMPTR(I)=TRAG(I,KK)
            END DO
            IF(KOLOUR(KK).EQ.1) THEN
               CALL PVCR(TEMPTR,1)
            ENDIF
            IF(KOLOUR(KK).EQ.2) THEN
               CALL PVCR(TEMPTR,2)
            ENDIF
            DO I=1,IGD
               TRAG(I,KK)=TEMPTR(I)
            END DO
C     
C     Convert mass mixing ratios to volume mixing ratios.
C     If LFLUX set
            IF (LFLUX) THEN
               DO L=1,NL
                  LL=(L-1)*IGC
                  DO I=1,IGC
                     TRAG(LL+I,KK)=PLG(I)*TRAG(LL+I,KK)
                  END DO
               END DO
            ENDIF
 70      CONTINUE
C     
C     Transform all tracer fields to spectral space at each latitude.
C     First do inverse FFT to give half-transforms and then call
C     HANAL for direct Legendre transform.
C     
         NTTR=(NLTRG-1)/NCRAY
         NRSTTR=NLTRG-NCRAY*NTTR
         IF(NTTR.EQ.0) GO TO 110
         DO I=1,NTTR
            CALL FFT991(DAFTR(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX
     :           ,1,MGPP,MG,NCRAY,-1)
         END DO
 110     CONTINUE
         CALL FFT991(DAFTR(1+ NTTR*NCRAY*MGPP),WORK,TRIG,IFAX
     :        ,1,MGPP,MG,NRSTTR,-1)
         CALL HANAL(CMPLX(TRAG),GWORK,TRA,NLTR,2)
C     
C     End of latitude loop. Note that if JGL=JG then JINC=1
C     if JGL=1  then JINC=0
C     Therefore JL=JH(=IH) if JGL=JG, or JL=1 if JGL=1.
C     
         JL=JL+JINC
 400  CONTINUE
C     
C     Return old tracers from the restart record into array TRA and
C     set the t-1 record for the new tracers.
C     
      IF (NTRACO.GT.0) THEN
         DO KK=1,NTRACO
            DO I=1,IGB
               TRA(I,KK)=TRAT(I,KK)
               TRAT(I,KK)=0.0
            END DO
         END DO
      ENDIF
      DO KK=NTRACO+1,NTRAC
         DO I=1,IGB
            TRAMI(I,KK)=TRA(I,KK)
         END DO
      END DO
C     
C     writes out surface temp and humdidity to channel 14
C     Continue to write out restart in old format....
      if (LNSURF) WRITE(14,*) ((tstar(i,j),j=1,jg),i=1,igc),
     $     ((qstar(i,j),j=1,jg),i=1,igc)
      WRITE(6,'(A40)') 'O DEG LONG X-SECTS SHOWN BELOW'

      DO 820 IHEM=1,NHEM
         DO J=1,JG
            TDUM(J)=CT*TSTAR(IHEM,J)-273.15
            QDUM(J)=CQ*QSTAR(IHEM,J)
         END DO
         IF(JG.LE.20) THEN
            DO NCHAN=6,6
               WRITE(NCHAN,2010) LHEM(IHEM),(TDUM(J),J=JG,1,-1)
               WRITE(NCHAN,2020) LHEM(IHEM),(QDUM(J),J=JG,1,-1)
            END DO
         ELSE
            DO NCHAN=6,6
               WRITE(NCHAN,2010) LHEM(IHEM),(TDUM(J),J=JG,2,-2)
               WRITE(NCHAN,2030)            (TDUM(J),J=JG-1,1,-2)
               WRITE(NCHAN,2020) LHEM(IHEM),(QDUM(J),J=JG,2,-2)
               WRITE(NCHAN,2040)            (QDUM(J),J=JG-1,1,-2)
            END DO
         ENDIF
 820  CONTINUE
C     
      RETURN
      END
