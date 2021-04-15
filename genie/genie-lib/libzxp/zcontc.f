      SUBROUTINE ZCONTC(ZG,Z,IWRK,X,Y,MD,MG,JG,C1,C2)
C
      INTEGER MD,MG,MGP,J,JG,I,NP,JJ,ISA
      INTEGER IWRK,I1,J1,I2,J2,I3,J3,I4,J4,ISIG
      INTEGER ISW,I10,J10,I40,J40,J0,KORNER,INI,INJ
      REAL    CV,CV1,C1,C2,CV2,HMN,ZG,HMX,X,Y
      REAL    H1,H2,H3,H4,CVC,H5
C   filling colour between two contour values
      DIMENSION ZG(MD,*),Z(MD,*),IWRK(MG,*),X(*),Y(*)
      COMPLEX   Z,B1,B2,ZA,ZB,Z1,Z2,Z3,Z4,D
            D(P1,P2,B1,B2   )=B1+(CV-P1)*(B2-B1)/(P2-P1)
      MGP=MG+1
      CV1=MIN(C1,C2)
      CV2=MAX(C1,C2)
      DO 3 J=1,JG-1
      HMN=MIN(ZG(1,J),ZG(1,J+1))
      HMX=MAX(ZG(1,J),ZG(1,J+1))
      DO 50 I=2,MG
      HMN=MIN(HMN,ZG(I,J),ZG(I,J+1))
   50 HMX=MAX(HMX,ZG(I,J),ZG(I,J+1))
      IF(HMN.GE.CV1.AND.HMX.LE.CV2) THEN
        NP=1
        ZA=Z(1,J)
        X(NP)= REAL(ZA)
        Y(NP)=AIMAG(ZA)
        DO 51 I=2,MG
        IF(Z(I,J).NE.ZA) THEN
            NP=NP+1
            ZA=Z(I,J)
            X(NP)= REAL(ZA)
            Y(NP)=AIMAG(ZA)
        ENDIF
   51     CONTINUE
        DO 52 I=1,MG
        IF(Z(MG-I+1,J+1).NE.ZA) THEN
            NP=NP+1
            ZA=Z(MG-I+1,J+1)
            X(NP)= REAL(ZA)
            Y(NP)=AIMAG(ZA)
        ENDIF
   52     CONTINUE
        CALL ZFILLN(X,Y,NP)
      ELSEIF(HMN.GT.CV2.OR.HMX.LT.CV1) THEN
        GOTO 3
      ENDIF
      DO 4 JJ=1,4
      DO 4 I=1,MG
    4 IWRK(I,JJ)=0
      DO 2 I=1,MG-1
      I1=I
      J1=J
      I2=I+1
      J2=J
      I3=I+1
      J3=J+1
      I4=I
      J4=J+1
      ISIG= 1
   29 CV=0.5*(1+ISIG)*CV1+0.5*(ISIG-1)*CV2
      H1=ZG(I1,J1)*ISIG
      H2=ZG(I2,J2)*ISIG
      H3=ZG(I3,J3)*ISIG
      H4=ZG(I4,J4)*ISIG
      IF(H1-CV)31,36,36
   31 IF(H2-CV)32,34,34
   32 IF(H3-CV)33,35,35
   33 IF(H4-CV)46,42,42
   34 IF(H3-CV)44,35,35
   35 IF(H4-CV)43,42,42
   36 IF(H2-CV)41,37,37
   37 IF(H3-CV)44,38,38
   38 IF(H4-CV)43,46,46
   41 ISW=1
      I10=I2
      J10=J2
      I40=I1
      J40=J1
      GOTO 45
   42 ISW=2
      I10=I1
      J10=J1
      I40=I4
      J40=J4
      GOTO 45
   43 ISW=3
      I10=I4
      J10=J4
      I40=I3
      J40=J3
      GOTO 45
   44 ISW=4
      I10=I3
      J10=J3
      I40=I2
      J40=J2
      GOTO 45
   46 IF(ISIG.EQ.1) THEN
       IF(H1.LT.CV ) GOTO 2
       ISIG=-1
       GOTO 29
      ENDIF
      GOTO 2
   45 J0=J+ISIG-2
      IF(IWRK(I10,J10-J0).EQ.1.AND.IWRK(I40,J40-J0).EQ.1) GOTO 2
      I1=I10
      J1=J10
      I4=I40
      J4=J40
      CV=0.5*(1+ISIG)*CV1+0.5*(ISIG-1)*CV2
      H1=ZG(I1,J1)*ISIG
      H4=ZG(I4,J4)*ISIG
      Z1= Z(I1,J1)
      Z4= Z(I4,J4)
      ZA=D(H4,H1,Z4,Z1)
      NP=1
      X(NP)=REAL(ZA)
      Y(NP)=AIMAG(ZA)
  101 I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J2=J1+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      I3=I4+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J3=J4+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      IF(I2.EQ.0.OR.I3.EQ.0.OR.I2.EQ.MGP.OR.I3.EQ.MGP)GOTO 103
      IF(J2.EQ.J-1.OR.J3.EQ.J-1.OR.J2.EQ.J+2.OR.J3.EQ.J+2)GOTO 103
      GOTO 104
  103 ISW=MOD(ISW+1,4)+1
      KORNER=0
  112 INI=MOD(ISW  ,2)*(1-2*(MOD(ISW,4)/2))
      INJ=MOD(ISW+1,2)*(1-2*(MOD(ISW,4)/2))
      CVC=0.5*(ISIG+1)*CV2+0.5*(ISIG-1)*CV1
      H4=ZG(I4,J4)*ISIG
      IF(KORNER.EQ.0.AND.H4.GT.CVC) THEN
        ISIG=-ISIG
        CV=0.5*(1+ISIG)*CV1+0.5*(ISIG-1)*CV2
        I4=I1
        J4=J1
        I1=I4+INI
        J1=J4+INJ
        H1=ZG(I1,J1)*ISIG
        H4=ZG(I4,J4)*ISIG
        Z1= Z(I1,J1)
        Z4= Z(I4,J4)
        ZA=D(H4,H1,Z4,Z1)
        IWRK(I1,J1-J-ISIG+2)=1
        IWRK(I4,J4-J-ISIG+2)=1
        NP=NP+1
        X(NP)=REAL(ZA)
        Y(NP)=AIMAG(ZA)
        IF (I1.NE.I10.OR.J1.NE.J10.OR.I4.NE.I40.OR.J4.NE.J40) GOTO 101
        GOTO 100
      ELSE
        I1=I4
        J1=J4
        NP=NP+1
        X(NP)=REAL(Z(I1,J1))
        Y(NP)=AIMAG(Z(I1,J1))
      ENDIF
  111 I4=I1
      J4=J1
      I1=I4+INI
      J1=J4+INJ
      IF(I1.EQ.0.OR.I1.EQ.MGP.OR.J1.EQ.J-1.OR.J1.EQ.J+2)GOTO 113
      GOTO 114
  113 ISW=MOD(ISW+2,4)+1
      KORNER=1
      GOTO 112
  114 H1=ZG(I1,J1)*ISIG
      IF(H1.GT.CV2*(1+ISIG)*0.5+CV1*(ISIG-1)*0.5) THEN
        ISIG=-ISIG
        H1=-H1
        CV=0.5*(1+ISIG)*CV1+0.5*(ISIG-1)*CV2
      ELSEIF(H1-CV.GE.0.) THEN
        NP=NP+1
        X(NP)=REAL(Z(I1,J1))
        Y(NP)=AIMAG(Z(I1,J1))
        GOTO 111
      ENDIF
      H4=ZG(I4,J4)*ISIG
      Z1= Z(I1,J1)
      Z4= Z(I4,J4)
      ZA=D(H4,H1,Z4,Z1)
      IWRK(I1,J1-J-ISIG+2)=1
      IWRK(I4,J4-J-ISIG+2)=1
      NP=NP+1
      X(NP)=REAL(ZA)
      Y(NP)=AIMAG(ZA)
      IF (I1.NE.I10.OR.J1.NE.J10.OR.I4.NE.I40.OR.J4.NE.J40) GOTO 101
      GOTO 100
  104 H1=ZG(I1,J1)*ISIG
      H2=ZG(I2,J2)*ISIG
      H3=ZG(I3,J3)*ISIG
      H4=ZG(I4,J4)*ISIG
      H5=0.25*(H1+H2+H3+H4)
      Z1= Z(I1,J1)
      Z2= Z(I2,J2)
      Z3= Z(I3,J3)
      Z4= Z(I4,J4)
      IF(H1-CV) 11, 2,15
   11 IF(H2-CV) 12,13,13
   12 IF(H3-CV) 23,22,22
   13 IF(H3-CV) 14,21,21
   14 IF(H5-CV) 23,21,21
   15 IF(H2-CV) 16,16,18
   16 IF(H3-CV) 21,21,17
   17 IF(H5-CV) 21,21,23
   18 IF(H3-CV) 22,22,23
   21 ISA=1
      ZB=D(H1,H2,Z1,Z2)
      I4=I2
      J4=J2
      GOTO 30
   22 ISA=2
      ZB=D(H2,H3,Z2,Z3)
      I1=I2
      J1=J2
      I4=I3
      J4=J3
      GOTO 30
   23 ISA=3
      ZB=D(H3,H4,Z3,Z4)
      I1=I3
      J1=J3
   30 IF(ZB.NE.ZA) THEN
        NP=NP+1
        X(NP)=REAL(ZB)
        Y(NP)=AIMAG(ZB)
      ENDIF
      IWRK(I1,J1-J-ISIG+2)=1
      IWRK(I4,J4-J-ISIG+2)=1
      ZA=ZB
      ISW=MOD(ISW-ISA+5,4)+1
      IF (I1.NE.I10.OR.J1.NE.J10.OR.I4.NE.I40.OR.J4.NE.J40) GOTO 101
  100 CALL ZFILLN(X,Y,NP-1)
    2 CONTINUE
    3 CONTINUE
      RETURN
      END
