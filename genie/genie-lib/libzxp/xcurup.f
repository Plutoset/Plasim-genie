      SUBROUTINE XCURUP(X,Y)
C
      REAL X2
      REAL X
      REAL Y2
      REAL Y
      INTEGER NPTS
      INTEGER IEND
      INTEGER MTD
      INTEGER NSUBDV
      INTEGER NEND1
      INTEGER NEND
      INTEGER KLOSE
      INTEGER KCLOSE
      REAL T2
      REAL X1
      REAL Y1
      REAL X3
      REAL Y3
      REAL X4
      REAL Y4
      REAL X01
      REAL X02
      REAL X03
      REAL Y01
      REAL Y02
      REAL Y03
      INTEGER KOUNT
      REAL T1
      REAL A
      REAL B
      REAL C
      INTEGER J
      REAL SL2X
      REAL SL2Y
      REAL D21X
      REAL D32X
      REAL D21Y
      REAL D32Y
      REAL SLINV
      REAL SL1X
      REAL SL1Y
      REAL A1X
      REAL A2X
      REAL BX
      REAL CX
      REAL T
      REAL TC
      REAL XX
      REAL A1Y
      REAL A2Y
      REAL BY
      REAL CY
      REAL TINC
      INTEGER KEND
      INTEGER ISUB
      REAL A12X
      REAL A12Y
      REAL XSUB
      REAL YSUB
      REAL T4
C
      COMMON /MVCURV/ X1,Y1,X2,Y2,X3,Y3,T2,NPTS ,SL1X,SL1Y,SL2X,SL2Y
      SAVE X01,Y01,X02,Y02,X03,Y03,IEND
      COMMON /XCDV23/ NSUBDV
      COMMON /XCMD24/ MTD
      X2=X
      Y2=Y
      NPTS=1
      IEND=0
      CALL XLPNUP(X2,Y2)
      RETURN
      ENTRY XCURDN(X,Y,KCLOSE,NEND)
      IF( MTD.EQ.0.OR .NSUBDV.LE.1) THEN
      CALL XLPNDN(X,Y )
      RETURN
      ENDIF
      NEND1=NEND
      KLOSE=KCLOSE
      IF( NPTS.LT.3)GOTO 8
      IF( X.EQ.X2.AND.Y.EQ.Y2) THEN
      IF( NEND1.EQ.1) GOTO 22
      GOTO 5
      ENDIF
      NPTS=NPTS+1
      T2=T2+1.
      X1=X2
      Y1=Y2
      X2=X3
      Y2=Y3
      X3=X
      Y3=Y
      GOTO 13
 8    IF(NPTS.EQ.2) GOTO 10
      IF( X.EQ.X2.AND.Y.EQ.Y2) GOTO 5
       NPTS=NPTS+1
       X3=X
       Y3=Y
       IF( KLOSE.EQ.1) CALL XLPNUP(X3,Y3)
       GOTO 5
 10   IF( X.EQ.X3.AND.Y.EQ.Y3) GOTO 5
      NPTS=NPTS+1
      X4=X
      Y4=Y
      T2=2.0
      IF( KLOSE.EQ.1) THEN
      X1=X2
      Y1=Y2
      X2=X3
      Y2=Y3
      X3=X4
      Y3=Y4
      X01=X1
      X02=X2
      X03=X3
      Y01=Y1
      Y02=Y2
      Y03=Y3
      KOUNT=1
      GOTO 21
      ENDIF
      T1=0.0
      CALL XQUADR(T2-1,X2,T2,X3,T2+1,X4,A,B,C)
      X1=C
      CALL XQUADR(T2-1,Y2,T2,Y3,T2+1,Y4,A,B,C)
      Y1=C
 13   KOUNT=1
      IF( NPTS.EQ.3.AND.KLOSE.NE.1)  KOUNT=2
 21   DO 20 J=1,KOUNT
      IF(MTD.EQ.2) THEN
        SL2X=(X3-X1)*0.5
        SL2Y=(Y3-Y1)*0.5
        GOTO 16
      ELSEIF( MTD.EQ.1) THEN
        D21X= X2-X1
        D32X= X3-X2
        D21Y= Y2-Y1
        D32Y= Y3-Y2
        IF( D21X*D32X.GT. 0.0) THEN
          SLINV=0.5/D21X+0.5/D32X
          IF(SLINV.EQ.0.0) THEN
            SL2X=0.0
          ELSE
            SL2X=1.0/SLINV
          ENDIF
        ELSE
          SL2X=0.0
        ENDIF
        IF( D21Y*D32Y.GT. 0.0) THEN
          SLINV=0.5/D21Y+0.5/D32Y
          IF(SLINV.EQ.0.0) THEN
            SL2Y=0.0
          ELSE
            SL2Y=1.0/SLINV
          ENDIF
        ELSE
          SL2Y=0.0
        ENDIF
        GOTO 16
      ENDIF
 16   CONTINUE
      IF(KLOSE.EQ.1.AND.NPTS.LE.3) THEN
      SL1X=SL2X
      SL1Y=SL2Y
      RETURN
      ENDIF
      IF( KOUNT.EQ.2.AND.J.EQ.1) THEN
      SL1X=SL2X
      SL1Y=SL2Y
      X1=X2
      Y1=Y2
      X2=X3
      Y2=Y3
      X3=X4
      Y3=Y4
      T2=2.0
      ENDIF
 20   CONTINUE

      CALL XSEAMQ( T2-1.0,X1,SL1X,T2,X2,SL2X,A1X,A2X,BX,CX)
      T=T2-1
      TC=T2-0.5
      XX=CX+(BX+A1X*(T-TC))*(T-TC)
      CALL XSEAMQ( T2-1.0,Y1,SL1Y,T2,Y2,SL2Y,A1Y,A2Y,BY,CY)
      TINC=1.0/NSUBDV
      KEND=0
      DO 15 ISUB=2,NSUBDV
      T=(ISUB-1)*TINC-0.5
      A12X=A1X
      A12Y=A1Y
      IF( ISUB.GT.NSUBDV/2 ) THEN
        A12Y=A2Y
        A12X=A2X
      ENDIF
      XSUB=CX+(BX+A12X*T)*T
      YSUB=CY+(BY+A12Y*T)*T
 15   CALL XLPNDN( XSUB,YSUB)
      IF(KLOSE.EQ.1) THEN
       IF( IEND.EQ.3) KEND=1
      ELSE
       IF( NEND .EQ.1.AND.NEND1.EQ.0  ) KEND=1
      ENDIF
      CALL XLPNDN( X2,Y2)
      SL1X=SL2X
      SL1Y=SL2Y

 22   IF(NEND1.EQ.0) THEN
      RETURN
      ELSEIF(KLOSE.EQ.1) THEN
      IF(IEND.GE.3) RETURN
      X1=X2
      X2=X3
      Y1=Y2
      Y2=Y3
 27     IEND=IEND+1
      IF( IEND.EQ.1) THEN
        IF(X3.EQ.X01.AND.Y3.EQ.Y01) GOTO 27
        X3=X01
        Y3=Y01
      ELSEIF( IEND.EQ.2) THEN
        X3=X02
        Y3=Y02
      ELSEIF( IEND.EQ.3) THEN
        X3=X03
        Y3=Y03
      ENDIF
      NPTS=NPTS+1
      T2=T2+1
      KOUNT=1
        GOTO 21
      ELSE
      T4=NPTS+1.0
      CALL XQUADR(T2-1,X1,T2,X2,T2+1,X3,A,B,C)
      X4=C+(B+A*T4)*T4
      CALL XQUADR(T2-1,Y1,T2,Y2,T2+1,Y3,A,B,C)
      Y4=C+(B+A*T4)*T4
 25     X1=X2
      Y1=Y2
      X2=X3
      Y2=Y3
      X3=X4
      Y3=Y4
      T2=T2+1
      NEND1=0
      KOUNT=1
      GOTO 21
      ENDIF
 5    IF( NEND1.EQ.1.AND. NPTS.EQ.2) THEN
        KEND=1
        CALL XLPNDN( X3,Y3)
      ENDIF
      RETURN
      END
