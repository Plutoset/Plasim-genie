
      SUBROUTINE XSPACE(NUMPH,NUMPV,ROTANG,XLIM, YLIM)
C
C  SUBROUTINE TO SET A GRAPHIC SPACE CONTAINING  NUMPH*NUMPV
C  PICTURES IN ONE FRAME OF FILM BY MOVING AND ROTATING COORDINATES.
C  INPUT : NUMPH,NUMPV- NUMBER OF PICTURES IN HORIZONATL AND VERTICAL
C                       IN EACH FRAME
C          ROTANG- THE ANGLE ATHAT EACH PICTURE IS ROTATED THROUGH
C  OUTPUT: XLIMIT,YLIMIT--
C          DEFINE THE MAXIMUM PLOTTING AREA FOR EACH PICTURE
C          (-XLIMIT/2,XLIMIT/2,-YLIMIT/2,YLIMIT/2),
C  ENTRIES: XNWPIC, XNWFRM, XPMAGN
C
C Option to switch off annotation for certain sub-pictures are included
C This is controled by Entry XFAUTO for automatic frame setting.
      INTEGER NCOUNT
      INTEGER NUMPX
      INTEGER NUMPY
      INTEGER NUMPIC
      REAL XLIMIT
      REAL YLIMIT
      REAL XMAGIN
      REAL YMAGIN
      REAL PANGLE
      INTEGER KFAUTO
      INTEGER IGRORDER
      INTEGER NCALLS
      INTEGER NUMPH
      INTEGER NUMPV
      REAL PL1
      REAL PR1
      REAL PB1
      REAL PT1
      REAL ROTANG
      REAL XLIM
      REAL YLIM
      REAL XS
      REAL XSIDE
      REAL YS
      REAL YSIDE
      INTEGER INUMX
      INTEGER INUMY
      INTEGER IORIGN
      INTEGER NOPIC
      REAL XOR
      REAL YOR
      REAL XRANGE
      REAL YRANGE
      REAL XC
      REAL YC
      REAL PPANG
      INTEGER NX
      INTEGER NY
      INTEGER NSEQH
      INTEGER KANY
      INTEGER KANX
      INTEGER IOD
      REAL XM
      REAL YM
      INTEGER IFRAME
      INTEGER NPIC
      INTEGER KFAU
      INTEGER KTKX
      INTEGER KTKY
C
      COMMON /XPSD01/ XSIDE, YSIDE
      COMMON /XAXS18/ KANX,KANY, KTKX,KTKY
      SAVE NCALLS, NOPIC , KFAUTO, IGRORDER
      SAVE NCOUNT,NUMPX,NUMPY,NUMPIC,XLIMIT,YLIMIT,XMAGIN,YMAGIN,PANGLE
      DATA NCOUNT,NUMPX,NUMPY,NUMPIC,XLIMIT,YLIMIT,XMAGIN,YMAGIN,PANGLE
     ;     /  0,    1,    1,    1,     1.5,   1.0,  0.0,   0.0 ,   0.0/
      DATA KFAUTO,IGRORDER /0,0/
      DATA NCALLS /0/
      NUMPX=NUMPH
      NUMPY=NUMPV
      NUMPIC=NUMPX*NUMPY
      NCOUNT=0
CPV
      CALL XQPSPC(PL1,PR1,PB1,PT1)
      XLIMIT=(PR1-PL1)/NUMPX
      YLIMIT=(PT1-PB1)/NUMPY
C     XLIMIT=XSIDE/NUMPX
C     YLIMIT=YSIDE/NUMPY
CPV
      PANGLE=ROTANG
      IF(PANGLE.EQ.90.0) THEN
      XLIM=YLIMIT
      YLIM=XLIMIT
      ELSE
      XLIM=XLIMIT
      YLIM=YLIMIT
      ENDIF
      RETURN
CPV
      ENTRY XPSIDE(XS,YS)
      XS=XSIDE
      YS=YSIDE
      RETURN
CPV
      ENTRY XQSPAC(INUMX,INUMY)
      INUMX=NUMPX
      INUMY=NUMPY
      RETURN
CPV
      ENTRY XNWPIC
C Used in relating to XSPACE to define the picture plotting space
C for next picture.
      NCALLS=NCALLS+1
      NCOUNT=NCOUNT+1
      IORIGN= MOD( NCOUNT, NUMPIC)
      IF((IORIGN.EQ. 1.OR.NUMPIC.EQ.1).AND.NCALLS.GT.1) CALL XFRAME
      NOPIC=IORIGN
      IF(IORIGN.EQ.0) NOPIC=NUMPIC
C      PRINT*, 'Picture No. ', NOPIC,' in the frame.'

      IF (igrorder.eq.0) then
         IF(PANGLE.EQ.90.0) THEN
         XOR=XLIMIT*(INT((NOPIC-1)/NUMPY)+0.5)
         YOR=YLIMIT*(MOD(NOPIC-1, NUMPY)+0.5)
         ELSE
         XOR=XLIMIT*(MOD(NOPIC-1,NUMPX)+0.5)
         YOR=YLIMIT*(INT(NUMPY-(NOPIC-1)/NUMPX)-0.5)
         ENDIF
      else
         IF(PANGLE.EQ.90.0) THEN
         XOR=XLIMIT*(MOD(NOPIC-1, NUMPY)+0.5)
         YOR=YLIMIT*(INT((NOPIC-1)/NUMPY)+0.5)
         ELSE
         XOR=XLIMIT*(INT(NUMPX-(NOPIC-1)/NUMPY)-0.5)
         YOR=YLIMIT*(NUMPY-MOD(NOPIC-1, NUMPY)-0.5)
         ENDIF
      end if
      XRANGE=XLIMIT-2*XMAGIN
      YRANGE=YLIMIT-2*YMAGIN
      XC=XRANGE/2
      YC=YRANGE/2
C
      CALL XPSPAC( XOR-XC,XOR+XC,YOR-YC,YOR+YC)
C
      IF( PANGLE.NE.0.0)  THEN
      CALL XDREFP( XOR,YOR)
      CALL XDRANG( PANGLE)
      ENDIF
      PPANG=PANGLE

      IF( KFAUTO.EQ.0) RETURN
       CALL XAXANT(-1,-1)
       IF(PANGLE.NE.90.0) THEN
       NX=NUMPX
       NY=NUMPY
       ELSE
       NX=NUMPY
       NY=NUMPX
       ENDIF
       NSEQH=MOD(NOPIC,NX)
       IF( NSEQH.EQ.0) NSEQH=NX
       IF( NSEQH.NE.1) KANY=0
       IF( NOPIC.LE. (NY-1)*NX ) KANX=0
      RETURN
C
      ENTRY XGORDER(IOD)
      IGRORDER=IOD
      RETURN
C
      ENTRY XPMAGN( XM,YM)
C Used in XSPACE to set the margins of graghic in the picture space
C provided. If not called ,default values of zero are provided.
      XMAGIN=XM
      YMAGIN=YM
      RETURN
      ENTRY XNWFRM
C Used in XSPACE to terminate the current picture frame and move on
C to the next page
      IFRAME=1
      NCOUNT=0
      RETURN
      ENTRY XQNPIC(NPIC)
      NPIC=NOPIC
      RETURN
      ENTRY XFAUTO(KFAU)
C* ADDED IN ZXPLOTI *
      KFAUTO=KFAU
      RETURN
      END
