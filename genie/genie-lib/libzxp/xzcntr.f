      SUBROUTINE XZCNTR(ZG,Z,IWRK,MD,MG,JG,CV)
C
      INTEGER MD
      INTEGER MG
      REAL CV
      INTEGER MODE
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
      INTEGER I2
      INTEGER J2
      INTEGER I3
      INTEGER J3
      REAL H2
      REAL H3
      REAL H5
      INTEGER ISA
      INTEGER I10
      INTEGER J10
      INTEGER I40
      INTEGER J40
C
      DIMENSION ZG(MD ,*),Z(MD ,*),IWRK(MG ,*)
C*   The second edition of the contour tracing
C*   Zhang Zuojun, Jan. 1988
C*   New update including contouring on triagle grids
C*   When MODE=0 contouring perform on retangular grids (default)
C*   When MODE=1 contouring perform on triangular grids .
      COMPLEX   Z,B1,B2,ZA,ZB,ZC,Z1,Z2,Z3,Z4,Z5,D
C     IFUN1(K)=K+MG*((MGP-K)/MGP-K/MGP)
      D(P1,P2,B1,B2   )=B1+(CV-P1)*(B2-B1)/(P2-P1)
      CALL ZQCONM(MODE)
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
      Z1= Z(I1,J1)
      Z4= Z(I4,J4)
      ZA=D(H4,H1,Z4,Z1)
      CALL XCURUP(REAL(ZA),AIMAG(ZA))
      I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J2=J1+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      I3=I4+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J3=J4+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
  201 H1=ZG(I1,J1)
      H2=ZG(I2,J2)
      H3=ZG(I3,J3)
      H4=ZG(I4,J4)
      H5=0.25*(H1+H2+H3+H4)
      Z1= Z(I1,J1)
      Z2= Z(I2,J2)
      Z3= Z(I3,J3)
      Z4= Z(I4,J4)
      IF(MODE.EQ.1) Z5=0.25*(Z1+Z2+Z3+Z4)
      IF(H2-CV) 52,53,53
   52 IF(H3-CV) 63,62,62
   53 IF(H3-CV) 54,61,61
   54 IF(H5-CV) 63,61,61
   61 ISA=1
      IF(MODE.EQ.1) THEN
        IF(H5.LT.CV.AND.H3.GE.CV) THEN
            ZC=D(H4,H5,Z4,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
            ZC=D(H3,H5,Z3,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
            ZC=D(H2,H5,Z2,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
        ELSE
            ZC=D(H1,H5,Z1,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
        ENDIF
      ENDIF
      ZB=D(H1,H2,Z1,Z2)
      I4=I2
      J4=J2
      GOTO 60
   62 ISA=2
      IF(MODE.EQ.1) THEN
        IF(H5.LT.CV) THEN
            ZC=D(H4,H5,Z4,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
            ZC=D(H3,H5,Z3,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
        ELSE
            ZC=D(H1,H5,Z1,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
            ZC=D(H2,H5,Z2,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
        ENDIF
      ENDIF
      ZB=D(H2,H3,Z2,Z3)
      I1=I2
      J1=J2
      I4=I3
      J4=J3
      GOTO 60
   63 ISA=3
      IF(MODE.EQ.1) THEN
        IF(H5.GE.CV.AND.H2.LT.CV) THEN
            ZC=D(H1,H5,Z1,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
            ZC=D(H2,H5,Z2,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
            ZC=D(H3,H5,Z3,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
        ELSE
            ZC=D(H4,H5,Z4,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),0,0)
            ZA=ZC
        ENDIF
      ENDIF
      ZB=D(H3,H4,Z3,Z4)
      I1=I3
      J1=J3
   60 ISW=MOD(ISW-ISA+5,4)+1
      I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J2=J1+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      I3=I4+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J3=J4+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      IF( I2.EQ.0.OR.I3.EQ.0.OR.I2.EQ.MGP.OR.I3.EQ.MGP  .OR.
     :    J2.EQ.0.OR.J3.EQ.0.OR.J2.EQ.JGP.OR.J3.EQ.JGP) THEN
        CALL XCURDN(REAL(ZB),AIMAG(ZB),0,1)
      ELSE
        IF(ZB.NE.ZA) CALL XCURDN(REAL(ZB),AIMAG(ZB),0,0)
        ZA=ZB
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
      Z1= Z(I1,J1)
      Z4= Z(I4,J4)
      ZA=D(H4,H1,Z4,Z1)
      CALL XCURUP(REAL(ZA),AIMAG(ZA))
  101 I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J2=J1+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      I3=I4+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J3=J4+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      H1=ZG(I1,J1)
      H2=ZG(I2,J2)
      H3=ZG(I3,J3)
      H4=ZG(I4,J4)
      H5=0.25*(H1+H2+H3+H4)
      Z1= Z(I1,J1)
      Z2= Z(I2,J2)
      Z3= Z(I3,J3)
      Z4= Z(I4,J4)
      IF(MODE.EQ.1) Z5=0.25*(Z1+Z2+Z3+Z4)
      IF(H2-CV) 12,13,13
   12 IF(H3-CV) 23,22,22
   13 IF(H3-CV) 14,21,21
   14 IF(H5-CV) 23,21,21
   21 ISA=1
      IF(MODE.EQ.1) THEN
        IF(H5.LT.CV.AND.H3.GE.CV) THEN
            ZC=D(H4,H5,Z4,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
            ZC=D(H3,H5,Z3,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
            ZC=D(H2,H5,Z2,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
        ELSE
            ZC=D(H1,H5,Z1,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
        ENDIF
      ENDIF
      ZB=D(H1,H2,Z1,Z2)
      I4=I2
      J4=J2
      GOTO 30
   22 ISA=2
      IF(MODE.EQ.1) THEN
        IF(H5.LT.CV) THEN
            ZC=D(H4,H5,Z4,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
            ZC=D(H3,H5,Z3,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
        ELSE
            ZC=D(H1,H5,Z1,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
            ZC=D(H2,H5,Z2,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
        ENDIF
      ENDIF
      ZB=D(H2,H3,Z2,Z3)
      I1=I2
      J1=J2
      I4=I3
      J4=J3
      GOTO 30
   23 ISA=3
      IF(MODE.EQ.1) THEN
        IF(H5.GE.CV.AND.H2.LT.CV) THEN
            ZC=D(H1,H5,Z1,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
            ZC=D(H2,H5,Z2,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
            ZC=D(H3,H5,Z3,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
        ELSE
            ZC=D(H4,H5,Z4,Z5)
            IF(ZC.NE.ZA) CALL XCURDN(REAL(ZC),AIMAG(ZC),1,0)
            ZA=ZC
        ENDIF
      ENDIF
      ZB=D(H3,H4,Z3,Z4)
      I1=I3
      J1=J3
   30 IF( I1.EQ.I10.AND.J1.EQ.J10.AND.I4.EQ.I40.AND.J4.EQ.J40) THEN
        CALL XCURDN(REAL(ZB),AIMAG(ZB),1,1)
      ELSE
        IF(ZB.NE.ZA) CALL XCURDN(REAL(ZB),AIMAG(ZB),1,0)
        ZA=ZB
        IWRK(I1,J1)=1
        IWRK(I4,J4)=1
        ISW=MOD(ISW-ISA+5,4)+1
        GOTO 101
      ENDIF
    2 CONTINUE
      CALL XLPNUP( REAL(Z(1,1)), AIMAG(Z(1,1)) )
      RETURN
      END
