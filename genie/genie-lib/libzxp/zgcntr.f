C
C-----------------------------------------------------------------------
C
C     This is XZXPlot 0.1 (ZXPlotPC 1.4)
C
C     Minor modifications to allow use of Xlib based graphics library,
C     only uses line drawing, open, close, clear screen at the moment,
C     still need to patch in colour selection, polygon filling and
C     dashed line drawing. Probably could do with some tidying up too,
C     but hell, it works!
C
C     Robin Glover (R.W.Glover@reading.ac.uk) 23/4/95
C
C-----------------------------------------------------------------------
C
      SUBROUTINE ZGCNTR(ZG,ALO,ALA,IWK,MD,MG,JG,CV)
C
      INTEGER MD,MG,MGP,JGP,JG,J,I,IWK,I1,J1,I2,J2
      INTEGER I3,J3,I4,J4,H1,H2,H3,H4,ISW,I10,J10
      INTEGER I40,J40,IA,INI,INJ,H5,ISA,LABON
      INTEGER NLABEL,IHTLAB
      REAL    DMASK,ZG,CV,ALO,ALA,PIH,ZSTEP,ZLEN,ZCHAR
C
      DIMENSION  ZG(MD ,*),IWK(MG ,*),ALO(*),ALA(*)
      COMPLEX    ZA,ZB,Z1,Z2,Z3,Z4,DZGCNT
      PARAMETER  (DMASK= -2.54E+34)
C
      COMMON/ZLABL/PIH,ZSTEP,ZLEN,ZCHAR,NLABEL,LABON,IHTLAB
C
C
      MGP=MG+1
      JGP=JG+1
      DO 4 J=1,JG
      DO 4 I=1,MG
      IWK(I,J)=0
 4    CONTINUE
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
      IF (H1.EQ.DMASK.OR.H2.EQ.DMASK.OR.
     :    H3.EQ.DMASK.OR.H4.EQ.DMASK) GO TO 2
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
   45 IF(IWK(I10,J10).EQ.1.AND.IWK(I40,J40).EQ.1) GOTO 2
      I1=I10
      J1=J10
      I4=I40
      J4=J40
      H1=ZG (I1,J1)
      H4=ZG (I4,J4)
      IF (H1.EQ.DMASK.OR.H2.EQ.DMASK.OR.
     :    H3.EQ.DMASK.OR.H4.EQ.DMASK) GO TO 2
      Z1=CMPLX(ALO(I1),ALA(J1))
      Z4=CMPLX(ALO(I4),ALA(J4))
      ZA=DZGCNT(H4,H1,Z4,Z1,CV)
      CALL ZPROJC(REAL(ZA),AIMAG(ZA),-1,IA)
  101 I2=I1+MOD(ISW-1,2)*(1-2*((ISW-1)/2))
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
  114 H1=ZG (I1,J1)
      IF (H1.EQ.DMASK.OR.H2.EQ.DMASK.OR.
     :    H3.EQ.DMASK.OR.H4.EQ.DMASK) GO TO 2
      IF(H1-CV.GE.0.) GOTO 111
      H4=ZG (I4,J4)
      IF (H1.EQ.DMASK.OR.H2.EQ.DMASK.OR.
     :    H3.EQ.DMASK.OR.H4.EQ.DMASK) GO TO 2
      Z1=CMPLX(ALO(I1),ALA(J1))
      Z4=CMPLX(ALO(I4),ALA(J4))
      ZA=DZGCNT(H4,H1,Z4,Z1,CV)
      IWK(I1,J1)=1
      IWK(I4,J4)=1
      CALL ZPROJC(REAL(ZA),AIMAG(ZA),-1,IA)
      IF(I1.NE.I10.OR.J1.NE.J10.OR.I4.NE.I40.OR.J4.NE.J40) GOTO 101
      GOTO 2
  104 H1=ZG(I1,J1)
      H2=ZG(I2,J2)
      H3=ZG(I3,J3)
      H4=ZG(I4,J4)
      IF (H1.EQ.DMASK.OR.H2.EQ.DMASK.OR.
     :    H3.EQ.DMASK.OR.H4.EQ.DMASK) GO TO 2
      H5=0.25*(H1+H2+H3+H4)
      Z1=CMPLX(ALO(I1),ALA(J1))
      Z2=CMPLX(ALO(I2),ALA(J2))
      Z3=CMPLX(ALO(I3),ALA(J3))
      Z4=CMPLX(ALO(I4),ALA(J4))
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
      ZB=DZGCNT(H1,H2,Z1,Z2,CV)
      I4=I2
      J4=J2
      GOTO 30
   22 ISA=2
      ZB=DZGCNT(H2,H3,Z2,Z3,CV)
      I1=I2
      J1=J2
      I4=I3
      J4=J3
      GOTO 30
   23 ISA=3
      ZB=DZGCNT(H3,H4,Z3,Z4,CV)
      I1=I3
      J1=J3
   30 IF(ZB.NE.ZA) CALL ZPROJC(REAL(ZB),AIMAG(ZB), 1,IA)
      IWK(I1,J1)=1
      IWK(I4,J4)=1
      ZA=ZB
      ISW=MOD(ISW-ISA+5,4)+1
      IF(I1.NE.I10.OR.J1.NE.J10.OR.I4.NE.I40.OR.J4.NE.J40) GOTO 101
    2 CONTINUE
      IF(LABON.EQ.1) THEN
      CALL ZBUF
      CALL ZANGLE(0.)
      ENDIF
      RETURN
      END
