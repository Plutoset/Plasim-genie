      SUBROUTINE XSINX(ALAT,X,PI)
      IMPLICIT NONE
      DOUBLE PRECISION EPS,DX,AA,X0,X1,XX,DPI
      REAL ALAT,X,PI
      INTEGER ICOUNT
C
C     I know that for this problem, x is in range 0- pi
C
      EPS=1.E-8
      DPI=2.*DASIN(1.D0)
      IF(ABS(ALAT).LT.EPS)THEN
      X=0.
      RETURN
      ELSEIF(ALAT.GT.0.5*PI-EPS)THEN
      X=0.5*PI
      RETURN
      ELSEIF(ALAT.LT.-0.5*PI+EPS)THEN
      X=-0.5*PI
      RETURN
      ENDIF
      XX=0.
      DX=0.2
      IF(ALAT.LT.0.)DX=-DX
      AA=DPI*SIN(ALAT)
      X0=XX+SIN(XX)-AA
      ICOUNT=1

10    XX=XX+DX
      X1=XX+SIN(XX)-AA
      ICOUNT=ICOUNT+1
      IF(ABS(X1).LT.EPS)GO TO 30
      IF(X0*X1.LE.0.)THEN
      XX=XX-DX
      DX=0.1*DX
      ELSE
      X0=X1
      ENDIF
      GO TO 10
30    CONTINUE
      X=0.5*XX
      END
