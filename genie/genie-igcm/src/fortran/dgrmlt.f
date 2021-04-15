      SUBROUTINE DGRMLT

      IMPLICIT NONE

C     
C     COMPUTE DIABATIC TENDENCIES IN GRID POINT SPACE FOR PRESENT LAT.
C     ACCUMULATE TIME AVERAGES FOR PRINTED OUTPUT AND HISTORY
C     ASSUMES OUTPUT AND HISTORY TIMESTEPS ARE SYNCHRONISED
C     
C     Full Physics
C     Piers 6/2/97 Version 2.0
C     Water vapour (things beginning with Q have changed
C     into tracer no 1)
C     and converted from Volume mixing  ratio to Mass mixing ratio
C     As water vapour advection is now done in Flux form
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'outcon.cmn'
      include 'gridpp.cmn'
      include 'bats.cmn'
      include 'physca.cmn'
      include 'ptendz.cmn'
      include 'cpiers.cmn'
      include 'orog_g.cmn'
      include 'gridsss.cmn'
      include 'fluxes.cmn'
      include 'igcm_prec.cmn'
      include 'gridpf.cmn'
      REAL QNLG(IGC,NL),QTLR(IGC,NL),QTCR(IGC,NL)
      EQUIVALENCE (QNLG(1,1),TRANLG(1,1,1))
     *            ,(QTLR(1,1),UTRAG(1,1,1)),(QTCR(1,1),VTRAG(1,1,1))

C     
      LOGICAL LSUM
      REAL UTBL(IGC),VTBL(IGC),TTBL(IGC),QTBL(IGC)
      INTEGER IFIRST
      REAL TROPHT(MG,NHEM,JG)
      REAL TTVDSC(IGC,NL)       ! temperature inc. from VDIFSC
      REAL QTVDSC(IGC,NL)       ! spec. hum, inc. from VDIFSC      
C     
      EQUIVALENCE (UTBL(1),UNLG(1,NL)),(VTBL(1),VNLG(1,NL))
     :     ,(TTBL(1),TNLG(1,NL)),(QTBL(1),TRANLG(1,NL,1))
C     
      SAVE IFIRST
      DATA IFIRST/1/
C     

      REAL,EXTERNAL :: SSUM

      INTEGER L,J,IHEM,K,IOF,K1
      REAL    RKP,THRESH,ASSBL1,ASHBL1,ARRCR1,ARRLR1,ARFLUX1
      REAL    ACLD1,ASSBLY1,ASLBL1,ASSBLX1

      DO L=1,NL-1
         DO J=1,IGC
            UNLG(J,L)=0.0
            VNLG(J,L)=0.0
            TNLG(J,L)=0.0
            QNLG(J,L)=0.0
         END DO
      END DO


      DELT2C=DELT2


      DO 61 IHEM=1,NHEM
         DO L=1,NL
            CTCR(L,IHEM)=0.0
            CTLR(L,IHEM)=0.0
         END DO
 61   CONTINUE





      DO J=1,IGC
         DO L=1,5
C     
C     sets ICFLAG and CFRAC to ZERO at each timestep
C     bottom level is complex for ICFLAG
C     need to make sure that max and min commands in MORC.632
C     change things if cloud is there
C     PMF bug fix 11-5-98
C     
            ICFLAG(J,L,1)=2
            ICFLAG(J,L,2)=NLP
            CFRAC(J,L)=0.0
         ENDDO
         RRCR(J)=0.0
         RRLR(J)=0.0
      END DO

      DO L=1,NL
         DO J=1,IGC
            QTVD(J,L)=0.0
            UTVD(J,L)=0.0
            VTVD(J,L)=0.0
            TTVD(J,L)=0.0
            TTCR(J,L)=0.0
            QTCR(J,L)=0.0
            TTLR(J,L)=0.0
            QTLR(J,L)=0.0
            TTRD(J,L)=0.0
            TTVDSC(J,L)=0.0
            QTVDSC(J,L)=0.0
          END DO
      END DO



      LSUM=.FALSE.
      IF(KOUTD.GE.1.AND.KOUTD.LE.KOUNTD) LSUM=.TRUE.
C     
      RKP=1.0/KOUNTD


      IF (LSUM) THEN
         IF (KOUTD.EQ.1) THEN
            do k=1,8
               do j=1,igc
                  arflux(j,jh,k)=0.
               enddo
            enddo
            do k=1,5
               do j=1,igc
                  acld(j,jh,k)=0.
               enddo
            enddo
            DO J=1,IGC
               ASSBL(J,JH)=0.0
               assblx(j,jh)=0.0
               assbly(j,jh)=0.0
               ASHBL(J,JH)=0.0
               ASNOWD(J,JH)=0.0
               ATEMP2M(J,JH)=0.0
               ALANDEVAP(J,JH)=0.0
               ASLBL(J,JH)=0.0
               ARRCR(J,JH)=0.0
               ARRLR(J,JH)=0.0
               asalb(j,jh)=0.0
               atstar(j,jh)=0.0
               atdeep(j,jh)=0.0
               aqstar(j,jh)=0.0
               asmstar(j,jh)=0.0
               ahsnow(j,jh)=0.0
            end do
         ENDIF
      ENDIF

      IF (LCSFCT) THEN
         IF (.NOT.LPERPET.OR.IFIRST.EQ.1)
     +        CALL SFCT(JH,IFIRST,TROPHT)
         IF (JH.EQ.JG) IFIRST=0
      ENDIF


      thresh=(273.16+60.0)/ct
      do j=1,igc
         if (tstar(j,jh).gt.thresh) tstar(j,jh)=thresh
      end do
c
      
      DO J=1,IGC
      rrcr(j)=0.
      DO L=1,NL
      TTCR(J,l)=0.
      QTCR(J,l)=0.
      enddo
      enddo
       
      IF(LVD) CALL VDIFF

      IF(LCR) CALL CONVEC
      IF(LCR_TIEDTKE) CALL CONVEC_TIEDTKE(TTVDSC,QTVDSC)
      IF(LLR) CALL LSCRN
      IF(LRD) CALL RADN(TROPHT)

c     Store the precipitation for use in the landsurface scheme
c     at the next timestep.  Do this instead of a separate call to surfm.
      do j=1,igc
      prec_conv(j,jh)=rrcr(j)
      prec_larg(j,jh)=rrlr(j)
      enddo
c      if (.NOT.LOLDBL) CALL SURFM
C     


      if (lsum) then
         do j=1,igc
            asalb(j,jh)=asalb(j,jh)+salb(j,jh)*rkp
            atstar(j,jh)=atstar(j,jh)+tstar(j,jh)*rkp
            atdeep(j,jh)=atdeep(j,jh)+tdeep(j,jh)*rkp
            aqstar(j,jh)=aqstar(j,jh)+qstar(j,jh)*rkp
            asmstar(j,jh)=asmstar(j,jh)+smstar(j,jh)*rkp
            ahsnow(j,jh)=ahsnow(j,jh)+hsnow(j,jh)*rkp
         end do
         do k=1,8
            do j=1,igc
               arflux(j,jh,k)=arflux(j,jh,k)+rrflux(j,jh,k)*RKP
            enddo
         enddo
C     arflux(j,2) is +downwards surface flux Wm-2
c     The following lines have been removed because they're wrong.
c         do j=1,igc
c            arflux(j,jh,2)=arflux(j,jh,2)+
c     :           (rrflux(j,jh,1)-rrflux(j,jh,2)+
c     :            rrflux(j,jh,3)-rrflux(j,jh,4)-
c     :            cv*p0*(shbl(j)+slbl(j)))*RKP
c         enddo
         do k=1,5
            do j=1,igc
               acld(j,jh,k)=acld(j,jh,k)+cfrac(j,k)*RKP
            enddo
         enddo
         DO J=1,IGC
            assblx(j,jh)=assblx(j,jh)+txbl(j)*rkp
            assbly(j,jh)=assbly(j,jh)+tybl(j)*rkp
            ASSBL(J,JH)=ASSBL(J,JH)+
     :          SQRT(TXBL(J)*TXBL(J)+TYBL(J)*TYBL(J))*RKP
            ASHBL(J,JH)=ASHBL(J,JH)+SHBL(J)*RKP
            ASNOWD(J,JH)=ASNOWD(J,JH)+SNOWD(J)*RKP
            ATEMP2M(J,JH)=ATEMP2M(J,JH)+TEMP2M(J)*RKP
            ALANDEVAP(J,JH)=ALANDEVAP(J,JH)+LANDEVAP(J)*RKP
            ASLBL(J,JH)=ASLBL(J,JH)+SLBL(J)*RKP
            ARRCR(J,JH)=ARRCR(J,JH)+RRCR(J)*RKP
            ARRLR(J,JH)=ARRLR(J,JH)+RRLR(J)*RKP
         END DO
         DO IHEM=1,NHEM
            IOF=(IHEM-1)*MGPP+1
            K1=JH*(2-IHEM)+(JGG+1-JH)*(IHEM-1)
            K=K1
            DO L=1,NL
               UTVDZ(K)=SSUM(MG,UTVD(IOF,L),1)*RMG
               VTVDZ(K)=SSUM(MG,VTVD(IOF,L),1)*RMG
               TTVDZ(K)=SSUM(MG,TTVD(IOF,L),1)*RMG
               QTVDZ(K)=SSUM(MG,QTVD(IOF,L),1)*RMG
               TTCRZ(K)=SSUM(MG,TTCR(IOF,L),1)*RMG
               QTCRZ(K)=SSUM(MG,QTCR(IOF,L),1)*RMG
               TTLRZ(K)=SSUM(MG,TTLR(IOF,L),1)*RMG
               QTLRZ(K)=SSUM(MG,QTLR(IOF,L),1)*RMG
               TTRDZ(K)=SSUM(MG,TTRD(IOF,L),1)*RMG
               CTCRZ(K)=CTCR(L,IHEM)
               CTLRZ(K)=CTLR(L,IHEM)
               K=K+JGG
            END DO
            UTBLZ(K1)=SSUM(MG,UTBL(IOF),1)*RMG
            VTBLZ(K1)=SSUM(MG,VTBL(IOF),1)*RMG
            TTBLZ(K1)=SSUM(MG,TTBL(IOF),1)*RMG
            QTBLZ(K1)=SSUM(MG,QTBL(IOF),1)*RMG
         END DO
      ENDIF                     ! IF(LSUM)
C     
      DO L=1,NL
         DO J=1,IGC
            UNLG(J,L)=UNLG(J,L)+UTVD(J,L)
            VNLG(J,L)=VNLG(J,L)+VTVD(J,L)
            TNLG(J,L)=TNLG(J,L)+TTVD(J,L)+TTCR(J,L)+
     :           TTLR(J,L)+TTRD(J,L)
     :           +TTVDSC(J,L)
            ATNLG(J,L,JH)=TNLG(J,L)
            QNLG(J,L)=QNLG(J,L)+QTVD(J,L)+QTCR(J,L)+QTLR(J,L)
     :                +QTVDSC(J,L)
         END DO
      END DO
C     
C     Convert to volume mixing ratio from mass mixing ratio.
C     
      IF (LFLUX) THEN
         DO 211 L=1,NL
            DO 210 J=1,IGC
               QNLG(J,L)=PLG(J)*QNLG(J,L)
 210        CONTINUE
 211     CONTINUE
      ENDIF
C     

c      enddo

      RETURN
      END
