      SUBROUTINE CONVEC

      IMPLICIT NONE

C
C     CONVECTION SCHEME. THIS ROUTINE FINDS UNSTABLE LAYERS AND
C     CALLS RELEVANT ROUTINES TO CALCULATE TENDENCIES ETC.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
      include 'bats.cmn'
C
      LOGICAL LCDRY,LCWET
      REAL ESCON(NL),TMIN
      REAL QG(IGC,NL),QTLR(IGC,NL),QTCR(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))
     *            ,(QTLR(1,1),UTRAG(1,1,1)),(QTCR(1,1),VTRAG(1,1,1))

C
c 28-05-97 PMF Water vapour fix to stop too low values
c takes water vapour from level below
c this produces a slight vertical advection of Q
c assumes no -ve water vapour in bottom layer
c
      REAL QGO(NL)
c
      real vapourfix
      parameter(vapourfix=1e-6)

      INTEGER LTOP,IHEM,IOFM,I,J,L,NCRB,NCRT,LP,NIT,NCT
      REAL    PRESSMIN,QEXS,QEX,TCLP,QCL,TCL,QSL,PQSAT,DQN,DQ

      PRESSMIN=20000./p0
                        ! p0=100000.
                        !  Minimum parcel pressure for
                        !  which moist convection is attempted.

      TMIN=233.1278244/ct  
                        ! ct=752.0151
                        !  Minimum parcel temperature (dedim) for
                        !  which moist convection is attempted.
      LTOP=NL-NLCR      !  NLCR gives highest level for which
                        !  convection is attempted.
!djl nlcr=nlm=nl-1, so ltop=1



      DO 800 IHEM=1,NHEM
        IOFM=(IHEM-1)*MGPP
        DO 700 I=1,MG
          J=I+IOFM
C         IPH=J
          DO 10 L=1,NL
            TTDC(L)=0.0
c           ttdc is assigned in subroutine dryadj.
            QTDC(L)=0.0
c           qtdc is assigned in subroutine dryadj.
            TTMC(L)=0.0
            QTMC(L)=0.0
            ESCON(L)=1./(PLG(J)*SIGMA(L))
            QGO(L)=QG(J,L)
   10     CONTINUE
c Water vapour fix
          DO L=1,NL-1
            IF (QG(J,L).LT.vapourfix) THEN
              QEXS=vapourfix-QG(J,L)
              IF (L.EQ.1) THEN
                QEXS=QEXS*SIGMAH(1)
              ELSE
                QEXS=QEXS*(SIGMAH(L)-SIGMAH(L-1))
              ENDIF
              IF (L.NE.NL-1) THEN
                QEX=QEXS/(SIGMAH(L+1)-SIGMAH(L))
              ELSE
                QEX=QEXS/(1.0-SIGMAH(L))
              ENDIF
c assumes no large scale rain here in layer
              QTLR(J,L)=(vapourfix-QGO(L))/DELT2C
              QTLR(J,L+1)=QTLR(J,L+1)-QEX/DELT2C
              QG(J,L)=vapourfix
              QG(J,L+1)=QG(J,L+1)-QEX
            ENDIF
          ENDDO
C
C     DRY ADJUSTMENT
C
          L=NL
C     BEGIN NEW PARCEL CURVE
   20     NCRB=L
          LCDRY=.FALSE.
          TCLP=TG(J,L)
          QCL=QG(J,L)
c         tclp is parcel temp, set to ambient temp at beginning of parcel curve. 
C     DRY ADIABAT TO NEXT LEVEL
   30     L=L-1
          IF(L.LT.LTOP) GOTO 40
          TCL=TCLP*SK(L)
       
c         sk=(sigma(l)/sigma(l+1))^kappa (iniphys.f)
c         therefore tcl is the temperature at level l which corresponds to
c         a dry adiabat from level l+1 
          IF(TCL.LE.TG(J,L)) GOTO 40  !If stable layer
C     BUOYANT - IF SATURATED - MOIST CONVECTION - IGNORE
          QSL=ESCON(L)*PQSAT(TCL)
          IF ( QCL.GE.QSL ) THEN   ! Is parcel supersaturated?
            IF ( ( (SIGMA(NCRB)*PLG(J)).GE.PRESSMIN )
     :        .AND.( TG(J,NCRB).GE.TMIN ) ) GOTO 40
C If parcel is in stratosphere, do dry rather than moist convection
C as the dry and moist adiabats are approximately parallel and
C dry convection is a lot cheaper.
          ENDIF
C     DRY CONVECTION - CONTINUE PARCEL CURVE UP
          LCDRY=.TRUE.
          TCLP=TCL
          GOTO 30
   40     CONTINUE
C     STABLE LAYER OR MODEL TOP - ADJUST ANY UNSTABLE LAYER BELOW
          IF(LCDRY) THEN
            NCRT=L+1
            CALL DRYADJ(NCRB,NCRT,J,IHEM)
c     in subroutine dryadj, TG and QG are redistributed in the 
c     levels between ncrb (bottom) and ncrt (top).
c     the rate of change of these variables is in TTDC and QTDC.

          ENDIF
          IF(L.GT.LTOP) GOTO 20
C
C     MOIST CONVECTION
C
          L=NL
C         BEGIN NEW PARCEL CURVE
  100     NCRB=L
          LCWET=.FALSE.
          IF ( (SIGMA(NCRB)*PLG(J)).LT.PRESSMIN ) THEN
            L=LTOP
            GOTO 150
          ELSEIF ( TG(J,NCRB).LT.TMIN ) THEN
            L=L-1
            GOTO 100
          ENDIF
          TC(L)=TG(J,L)
          QC(L)=QG(J,L)
C     UP ONE LEVEL - DRY ADIABAT AS FIRST GUESS
  110     LP=L
          L=L-1
          IF(L.LT.LTOP) GOTO 150
          TC(L)=TC(LP)*SK(L)
c         therefore tc is the temperature a parcle at level l+1 would have if
c         it rose by a dry adiabat.
          QC(L)=QC(LP)
          QSL=ESCON(L)*PQSAT(TC(L))
c         qsl is the humidity of the upper layer
C     IF SATURATED MAY BE MOIST CONVECTION
          IF(QC(L).GE.QSL) GOTO 120
C     TEST FOR DRY INSTABILITY (SHOULD BE STABLE)
          GOTO 150  !No moist convection
C     POSSIBLE MOIST CONVECTION - ITERATE FOR THETAE (T,Q)
  120     NIT=0
  130     NIT=NIT+1
          DQN=QC(L)-QSL
          DQ=DQN/(1.0+CCC*QSL/(TC(L)*TC(L)))
c     
          QC(L)=QC(L)-DQ
          TC(L)=TC(L)+CTQ*DQ
c     this looks like a newton-raphson calculation
          IF(ABS(DQN).LT.EPSIQ) GOTO 140
c     epsiq=0.01/cq (defined in iniphys.f)
c     CCC=CLATNT*CLATNT/(RV*CPD*CT*CT)
c     clatnt=2.5e6
c     rv=461.51
c     cpd=gascon/akap
c     ct=cg/gascon

          IF(NIT.GT.10) GOTO 140
          QSL=ESCON(L)*PQSAT(TC(L))
          GOTO 130
C     BUOYANCY TEST - IF STABLE GO TO 150
  140     IF(TC(L).LT.TG(J,L)) GOTO 150
C     MOIST CONVECTION - CONTINUE PARCEL CURVE UP
          LCWET=.TRUE.
          GOTO 110
  150     CONTINUE
C     STABLE LAYER OR MODEL TOP - ADJUST ANY UNSTABLE LAYER BELOW
          IF(LCWET) THEN
            NCRT=LP
            IF(NCRT.LT.NCUTOP) THEN
              IF(.NOT.LCBADJ) CALL CBCON(NCRB,NCRT,J,IHEM)
c     we have always had lcbadj=.true.  Therefore use cbadj. 
              IF(LCBADJ)      CALL CBADJ(NCRB,NCRT,J,IHEM)
            ELSE
              NCT=NCRT
              IF(.NOT.LCUBM)  CALL CUDIF(NCRB,NCT,J,IHEM)
c     we have always had lcubm=.true.  Therefore use cubm. 
              IF(LCUBM)       CALL CUBM (NCRB,NCT,J,IHEM)
            ENDIF
          ENDIF
          IF(L.GT.LTOP) GOTO 100
C
C     COLLECT TENDENCIES AND INCREMENT T,Q FOR MOIST CONVECTION
C
          DO 200 L=1,NL
            TTCR(J,L)=TTDC(L)+TTMC(L)
            QTCR(J,L)=QTDC(L)+QTMC(L)+QTLR(J,L)
            QTLR(J,L)=0.0
  200     CONTINUE
  700   CONTINUE
  800 CONTINUE
C
      RETURN
      END
