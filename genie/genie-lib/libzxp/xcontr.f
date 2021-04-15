C
      SUBROUTINE XCONTR(ZG,X,Y,IWRK,MD,MG,JG,CV)
C
      INTEGER MD
      INTEGER MG
      REAL D
      REAL CV
      INTEGER MGP
      INTEGER JGP
      INTEGER JG
      INTEGER J
      INTEGER I
      INTEGER IWRK
      INTEGER JJ
      INTEGER I4
      INTEGER J4
      INTEGER ISW
      INTEGER INI
      INTEGER INJ
      INTEGER I1
      INTEGER J1
      REAL H1
      REAL ZG
      REAL H4
      REAL X1
      REAL X
      REAL X4
      REAL Y1
      REAL Y
      REAL Y4
      REAL XA
      REAL YA
      INTEGER I2
      INTEGER J2
      INTEGER I3
      INTEGER J3
      REAL H2
      REAL H3
      REAL H5
      REAL X2
      REAL X3
      REAL Y2
      REAL Y3
      INTEGER ISA
      REAL XB
      REAL YB
      INTEGER I10
      INTEGER J10
      INTEGER I40
      INTEGER J40
C
      DIMENSION ZG(MD ,*),X(MD ,*),Y(MD,*),IWRK(MG ,*)
C* The final edition of the contouring package  2nd ed
C*  Zhang Zuojun, Jan. 1988
C     IFUN1(K)=K+MG*((MGP-K)/MGP-K/MGP)
      D(P1,P2,B1,B2   )=B1+(CV-P1)*(B2-B1)/(P2-P1)
      MGP=MG+1
      JGP=JG+1
      DO 4 J=1,JG
      DO 4 I=1,MG
    4 IWRK(I,J)=0
      DO 1 JJ=1,2*(MG+JG-2)
      IF(JJ.LT.MG) THEN
        I4=JJ
        J4=1
        ISW=1
      ELSEIF(JJ.LT.MG+JG-1) THEN
        I4=MG
        J4=JJ-MG+1
        ISW=4
      ELSEIF(JJ.LT.MG+MG+JG-2) THEN
        I4=MG+MG+JG-JJ-1
        J4=JG
        ISW=3
      ELSEIF(JJ.LT.MG+MG+JG+JG-3) THEN
        I4=1
        J4=MG+MG+JG+JG-2-JJ
        ISW=2
      ENDIF
      INI=MOD(ISW  ,2)*(1-2*(MOD(ISW,4)/2))
      INJ=MOD(ISW+1,2)*(1-2*(MOD(ISW,4)/2))
      I1=I4+INI
      J1=J4+INJ
      IF(I1.EQ.0.OR.I1.EQ.MGP.OR.J1.EQ.0.OR.J1.EQ.JGP)GOTO 1
      H1=ZG(I1,J1)
      H4=ZG(I4,J4)
      IF(H1.GE.CV.OR.H4.LT.CV ) GOTO 1
      X1= X(I1,J1)
      X4= X(I4,J4)
      Y1= Y(I1,J1)
      Y4= Y(I4,J4)
      XA=D(H4,H1,X4,X1)
      YA=D(H4,H1,Y4,Y1)
      CALL XCURUP( XA, YA )
      I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J2=J1+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      I3=I4+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J3=J4+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
  201 H1=ZG(I1,J1)
      H2=ZG(I2,J2)
      H3=ZG(I3,J3)
      H4=ZG(I4,J4)
      H5=0.25*(H1+H2+H3+H4)
      X1= X(I1,J1)
      X2= X(I2,J2)
      X3= X(I3,J3)
      X4= X(I4,J4)
      Y1= Y(I1,J1)
      Y2= Y(I2,J2)
      Y3= Y(I3,J3)
      Y4= Y(I4,J4)
      IF(H2-CV) 52,53,53
   52 IF(H3-CV) 63,62,62
   53 IF(H3-CV) 54,61,61
   54 IF(H5-CV) 63,61,61
   61 ISA=1
      XB=D(H1,H2,X1,X2)
      YB=D(H1,H2,Y1,Y2)
      I4=I2
      J4=J2
      GOTO 60
   62 ISA=2
      XB=D(H2,H3,X2,X3)
      YB=D(H2,H3,Y2,Y3)
      I1=I2
      J1=J2
      I4=I3
      J4=J3
      GOTO 60
   63 ISA=3
      XB=D(H3,H4,X3,X4)
      YB=D(H3,H4,Y3,Y4)
      I1=I3
      J1=J3
   60 ISW=MOD(ISW-ISA+5,4)+1
      I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J2=J1+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      I3=I4+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J3=J4+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      IF( I2.EQ.0.OR.I3.EQ.0.OR.I2.EQ.MGP.OR.I3.EQ.MGP  .OR.
     :    J2.EQ.0.OR.J3.EQ.0.OR.J2.EQ.JGP.OR.J3.EQ.JGP) THEN
        CALL XCURDN( XB, YB,0 , 1 )
      ELSE
        IF(XB.NE.XA. OR.YB.NE.YA) CALL XCURDN( XB , YB, 0 ,0)
        XA=XB
        YA=YB
        IWRK(I1,J1)=1
        IWRK(I4,J4)=1
        GOTO 201
      ENDIF
   1  CONTINUE
      DO 2 J=2,JG-1
      DO 2 I=1,MG-1
      ISW=1
      I10=I+1
      J10=J
      I40=I
      J40=J
      IF(IWRK(I10,J10).EQ.1.AND.IWRK(I40,J40).EQ.1) GOTO 2
      H1=ZG(I10,J10)
      H4=ZG(I40,J40)
      IF(H1.GE.CV.OR.H4.LT.CV ) GOTO 2
      I1=I10
      J1=J10
      I4=I40
      J4=J40
      X1= X(I1,J1)
      X4= X(I4,J4)
      Y1= Y(I1,J1)
      Y4= Y(I4,J4)
      XA=D(H4,H1,X4,X1)
      YA=D(H4,H1,Y4,Y1)
      CALL XCURUP( XA, YA )
  101 I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J2=J1+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      I3=I4+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J3=J4+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      H1=ZG(I1,J1)
      H2=ZG(I2,J2)
      H3=ZG(I3,J3)
      H4=ZG(I4,J4)
      H5=0.25*(H1+H2+H3+H4)
      X1= X(I1,J1)
      X2= X(I2,J2)
      X3= X(I3,J3)
      X4= X(I4,J4)
      Y1= Y(I1,J1)
      Y2= Y(I2,J2)
      Y3= Y(I3,J3)
      Y4= Y(I4,J4)
      IF(H2-CV) 12,13,13
   12 IF(H3-CV) 23,22,22
   13 IF(H3-CV) 14,21,21
   14 IF(H5-CV) 23,21,21
   21 ISA=1
      XB=D(H1,H2,X1,X2)
      YB=D(H1,H2,Y1,Y2)
      I4=I2
      J4=J2
      GOTO 30
   22 ISA=2
      XB=D(H2,H3,X2,X3)
      YB=D(H2,H3,Y2,Y3)
      I1=I2
      J1=J2
      I4=I3
      J4=J3
      GOTO 30
   23 ISA=3
      XB=D(H3,H4,X3,X4)
      YB=D(H3,H4,Y3,Y4)
      I1=I3
      J1=J3
   30 IF( I1.EQ.I10.AND.J1.EQ.J10.AND.I4.EQ.I40.AND.J4.EQ.J40) THEN
        CALL XCURDN(XB,YB,1,1)
      ELSE
        IF(XB.NE.XA. OR.YB.NE.YA) CALL XCURDN( XB , YB, 1 ,0)
        XA=XB
        YA=YB
        IWRK(I1,J1)=1
        IWRK(I4,J4)=1
        ISW=MOD(ISW-ISA+5,4)+1
        GOTO 101
      ENDIF
    2 CONTINUE
      CALL XLPNUP( X(1,1) ,Y(1,1) )
      RETURN
      END
