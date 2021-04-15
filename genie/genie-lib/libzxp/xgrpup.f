      SUBROUTINE XGRPUP(X,Y)
C Position pen at the starting point of a sigle valued curve y=y(x).
C
      REAL X2
      REAL X
      REAL Y2
      REAL Y
      INTEGER NPTS
      INTEGER KEND
      INTEGER NEND
      INTEGER MTD
      INTEGER NSUBDV
      INTEGER NEND1
      REAL X1
      REAL Y1
      REAL X3
      REAL Y3
      REAL X4
      REAL Y4
      REAL A
      REAL B
      REAL C
      INTEGER KOUNT
      INTEGER J
      REAL D21
      REAL D32
      REAL DD31
      REAL SL2
      REAL AA
      REAL SLINV
      REAL SL1
      REAL A1
      REAL A2
      REAL XSUB
      REAL XINC
      REAL XC
      INTEGER ISUB
      REAL A12
      REAL YSUB
C
      COMMON /SVCURV/ X1,Y1,X2,Y2,X3,Y3,NPTS, SL1,SL2
      COMMON /XCDV23/ NSUBDV
      COMMON /XCMD24/ MTD
      X2=X
      Y2=Y
      NPTS=1
      CALL XLPNUP(X2,Y2)
      RETURN
      ENTRY XGRPDN(X,Y,NEND)
C Join (x,y(x)) with a smooth seamed quadratic curve, where y=y(x) is a
C single valued function.
      KEND=NEND
      IF( MTD.EQ.0.OR .NSUBDV.LE.1) THEN
      CALL XLPNDN(X,Y)
      RETURN
      ENDIF
      NEND1=NEND
      IF( NPTS.LT.3)GOTO 8
      IF( X.EQ.X2)THEN
      IF(Y.NE.Y2)GOTO 999
      IF( NEND1.EQ.1) GOTO 22
      GOTO 5
      ENDIF
      NPTS=NPTS+1
      X1=X2
      Y1=Y2
      X2=X3
      Y2=Y3
      X3=X
      Y3=Y
      GOTO 13
 8    IF(NPTS.EQ.2) GOTO 10
      IF( X.EQ.X2) THEN
      IF(Y.NE.Y2)GOTO 999
      GOTO 5
      ENDIF
       NPTS=NPTS+1
       X3=X
       Y3=Y
       GOTO 5
 10   IF( X.EQ.X3) THEN
      IF(Y.NE.Y3)GOTO 999
      GOTO 5
      ENDIF
      NPTS=NPTS+1
      X4=X
      Y4=Y
      CALL XQUADR(X2,Y2,X3,Y3,X4,Y4,A,B,C)
      X1=X2-(X4-X3)
      Y1=C+(B+A*X1)*X1
 13   IF( (X3-X2)*(X2-X1).LT.0.0) GOTO 997
      KOUNT=1
      IF( NPTS.EQ.3) KOUNT=2
 21   DO 20 J=1,KOUNT
      D21=(Y2-Y1)/(X2-X1)
      D32=(Y3-Y2)/(X3-X2)
      IF(MTD.EQ.2) THEN
        DD31=(D32-D21)/(X3-X1)
        SL2=( D21+D32-DD31*(X1-2*X2+X3))*0.5
        GOTO 16
      ELSEIF( MTD.EQ.1) THEN
          IF( D21*D32.GT. 0.0) THEN
            AA=(1.0+(X3-X2)/(X3-X1))/3.0
            SLINV=AA/D21+(1-AA)/D32
            SL2=1.0/SLINV
          ELSE
            SL2=0.0
          ENDIF
          GOTO 16
      ENDIF
 16   IF( KOUNT.EQ.2.AND.J.EQ.1) THEN
      SL1=SL2
      X1=X2
      Y1=Y2
      X2=X3
      Y2=Y3
      X3=X4
      Y3=Y4
      ENDIF
 20   CONTINUE
      CALL XSEAMQ( X1,Y1,SL1,X2,Y2,SL2,A1,A2,B,C)
      XSUB=X1
      XINC=(X2-X1)/NSUBDV
      XC=(X2+X1)*0.5
      KEND=0
      DO 15 ISUB=2,NSUBDV
      XSUB=XSUB+XINC
      A12=A1
      IF( ISUB.GT.NSUBDV/2 ) A12=A2
      YSUB=C +(B +A12*(XSUB-XC))*(XSUB-XC)
 15   CALL XLPNDN( XSUB,YSUB)
      IF( NEND.EQ.1.AND.NEND1.EQ.0) KEND=1
      CALL XLPNDN( X2,Y2)
      SL1=SL2
 22   IF(NEND1.EQ.0) THEN
      RETURN
      ELSE
      CALL XQUADR(X1,Y1,X2,Y2,X3,Y3,A,B,C)
      X4=X3-(X1-X2)
      Y4=C+(B+A*X4)*X4
 25     X1=X2
      Y1=Y2
      X2=X3
      Y2=Y3
      X3=X4
      Y3=Y4
      NEND1=0
      KOUNT=1
      GOTO 21
      ENDIF
 5    IF( NEND1.NE.1) RETURN
      IF( NPTS.EQ.2) THEN
        KEND=1
        CALL XLPNDN( X2,Y2)
      ELSE
        RETURN
      ENDIF
      RETURN
 999  PRINT*,' Input data are controdicting! Curve plotting aborted.'
      RETURN
 997  PRINT*,' Input data not in correct order! Plotting aborted.'
      RETURN
      END
