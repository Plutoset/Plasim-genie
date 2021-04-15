      SUBROUTINE ZCONTR(ZG,Z,IWRK,MD,MG,JG,CV)
C
      INTEGER MD
      INTEGER MG
      REAL CV
      INTEGER MGP
      INTEGER JGP
      INTEGER JG
      INTEGER J
      INTEGER I
      INTEGER IWRK
      INTEGER I1
      INTEGER J1
      INTEGER I2
      INTEGER J2
      INTEGER I3
      INTEGER J3
      INTEGER I4
      INTEGER J4
      REAL H1
      REAL ZG
      REAL H2
      REAL H3
      REAL H4
      INTEGER ISW
      INTEGER I10
      INTEGER J10
      INTEGER I40
      INTEGER J40
      INTEGER INI
      INTEGER INJ
      REAL H5
      INTEGER ISA
C
      DIMENSION ZG(MD ,*),Z(MD ,*),IWRK(MG ,*)
C*           The final edition of the contouring package
C*                                  Zhang Zuojun, 12.6.1987
      COMPLEX   Z,B1,B2,ZA,ZB,Z1,Z2,Z3,Z4,D
C               IFUN1(K)=K+MG*((MGP-K)/MGP-K/MGP)
       D(P1,P2,B1,B2   )=B1+(CV-P1)*(B2-B1)/(P2-P1)
      MGP=MG+1
      JGP=JG+1
      DO 4 J=1,JG
      DO 4 I=1,MG
    4 IWRK(I,J)=0
      DO 2 J=1,JG-1
      DO 2 I=1,MG-1
      I1=I
      J1=J
      I2=I+1
      J2=J
      I3=I+1
      J3=J+1
      I4=I
      J4=J+1
      H1=ZG(I1,J1)
      H2=ZG(I2,J2)
      H3=ZG(I3,J3)
      H4=ZG(I4,J4)
      IF(H1-CV)31,36,36
   31 IF(H2-CV)32,34,34
   32 IF(H3-CV)33,35,35
   33 IF(H4-CV) 2,42,42
   34 IF(H3-CV)44,35,35
   35 IF(H4-CV)43,42,42
   36 IF(H2-CV)41,37,37
   37 IF(H3-CV)44,38,38
   38 IF(H4-CV)43, 2, 2
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
   45 IF(IWRK(I10,J10).EQ.1.AND.IWRK(I40,J40).EQ.1) GOTO 2
      I1=I10
      J1=J10
      I4=I40
      J4=J40
      H1=ZG(I1,J1)
      H4=ZG(I4,J4)
      Z1= Z(I1,J1)
      Z4= Z(I4,J4)
      ZA=D(H4,H1,Z4,Z1)
      CALL ZPENUP(REAL(ZA),AIMAG(ZA))
  101 CONTINUE
      I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J2=J1+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      I3=I4+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
      J3=J4+MOD(ISW  ,2)*(1-2*((ISW-1)/2))
      IF(I2.EQ.0.OR.I3.EQ.0.OR.I2.EQ.MGP.OR.I3.EQ.MGP)GOTO 103
      IF(J2.EQ.0.OR.J3.EQ.0.OR.J2.EQ.JGP.OR.J3.EQ.JGP)GOTO 103
      GOTO 104
  103 ISW=MOD(ISW+1,4)+1
  112 INI=MOD(ISW  ,2)*(1-2*(MOD(ISW,4)/2))
      INJ=MOD(ISW+1,2)*(1-2*(MOD(ISW,4)/2))
      I1=I4
      J1=J4
  111 I4=I1
      J4=J1
      I1=I4+INI
      J1=J4+INJ
      IF(I1.EQ.0.OR.I1.EQ.MGP.OR.J1.EQ.0.OR.J1.EQ.JGP)GOTO 113
      GOTO 114
  113 ISW=MOD(ISW+2,4)+1
      GOTO 112
  114 H1=ZG(I1,J1)
      IF(H1-CV.GE.0.) GOTO 111
      H4=ZG(I4,J4)
      Z1= Z(I1,J1)
      Z4= Z(I4,J4)
      ZA=D(H4,H1,Z4,Z1)
      IWRK(I1,J1)=1
      IWRK(I4,J4)=1
      CALL ZPENUP(REAL(ZA),AIMAG(ZA))
      IF (I1.NE.I10.OR.J1.NE.J10.OR.I4.NE.I40.OR.J4.NE.J40) GOTO 101
      GOTO 100
C*
  104 CONTINUE
      H1=ZG(I1,J1)
      H2=ZG(I2,J2)
      H3=ZG(I3,J3)
      H4=ZG(I4,J4)
      H5=0.25*(H1+H2+H3+H4)
      Z1= Z(I1,J1)
      Z2= Z(I2,J2)
      Z3= Z(I3,J3)
      Z4= Z(I4,J4)
      IF(H1-CV) 11,24,15
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
      GOTO 30
   24 ISA=5
      PRINT*,'ISA=',ISA,' ISW=',ISW
      STOP      999
   30 CONTINUE
      IF(ZB.NE.ZA) CALL ZPENDN(REAL(ZB),AIMAG(ZB))
C*
      IWRK(I1,J1)=1
      IWRK(I4,J4)=1
      ZA=ZB
      ISW=MOD(ISW-ISA+5,4)+1
      IF (I1.NE.I10.OR.J1.NE.J10.OR.I4.NE.I40.OR.J4.NE.J40) GOTO 101
  100 CONTINUE
    2 CONTINUE
      RETURN
      END
