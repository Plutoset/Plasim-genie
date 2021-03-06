      FUNCTION XCHLEN(STRING)
C
      REAL XCHLEN
      INTEGER IXCHR
      INTEGER IYCHR
      REAL CXY
      REAL XSC
      REAL YSC
      REAL SY
      REAL YFACTR
      REAL SX
      REAL XFACTR
      REAL CRATIO
      INTEGER KFONT
      REAL FACTOR
      INTEGER N
      INTEGER ITX
      INTEGER ICHR
      INTEGER I
      INTEGER ICRAM
      INTEGER IASCII
      INTEGER NCD
      INTEGER IX
      REAL XWIDTH
      REAL XP1
      REAL YP1
      REAL XP2
      REAL YP2
      REAL PL
      REAL PR
      REAL PB
      REAL PT
      REAL XRANGE
      REAL YRANGE
      REAL XL
      REAL XR
      REAL YB
      REAL YT
      REAL XSCALE
      REAL YSCALE
      REAL HCTR
      REAL SCTR
      INTEGER NUNDLN
C
      COMMON /XPHY01/ PL,PR,PB,PT,XRANGE,YRANGE
      COMMON /XMAP04/ XL,XR,YB,YT,XSCALE,YSCALE
      COMMON /XFTR06/ XFACTR,YFACTR
      COMMON /XCHA20/ HCTR,SCTR,CRATIO, KFONT,NUNDLN
      COMMON /XASC12/ IASCII(300)
      COMMON /XCHR30/ ICRAM(256)
      COMMON /XCHR31/ CHDATA
      COMMON /XCHR32/ ICDATA
      INTEGER ICDATA (0:150, 32:127)
      CHARACTER     CTEMP*1, CH*5,CHDATA(127)*300
      CHARACTER*(*) STRING

      CH=CHDATA(2)
      READ(CH  ,103)IXCHR,IYCHR
      CXY=0.75
      CALL XQPSCL( XSC, YSC )
      SY = 1.0/ ( IYCHR* YFACTR)*YSC
      SX = 1.0/ ( IYCHR* XFACTR)*XSC      *CRATIO/CXY
      IF( KFONT.EQ.2) THEN
      FACTOR=6.0/4.2
      SX=SX*FACTOR
      SY=SY*FACTOR
      ENDIF
      N =   LEN (STRING)
      ITX=0
      DO 8 ICHR=1,N
      CTEMP = STRING (ICHR:ICHR)
      I = ICHAR(CTEMP)
      I = ICRAM(I)
      I = IASCII(I)
      IF( I.EQ.0) I=32
      IF( ICDATA(0,I).NE.KFONT) THEN
      CALL XCHDEC(ICDATA,CHDATA,I)
      ICDATA(0,I)=KFONT
      ENDIF
      NCD=ICDATA(1,I)
      IX= ICDATA(NCD-1,I)
      IF( IX.GE.50)   IX=IX-50
      ITX=ITX+IX
  8   CONTINUE
      XWIDTH= ITX* SX
      XP1= 0.0
      YP1= 0.0
      XP2= XWIDTH
      YP2= 0.0
      CALL XCTRAN(XP1,YP1)
      CALL XCTRAN(XP2,YP2)
      XCHLEN=SQRT( (XP2-XP1)*(XP2-XP1)+ (YP2-YP1)*(YP2-YP1))
      RETURN
  103 FORMAT(I2,1X,I2)
      END
