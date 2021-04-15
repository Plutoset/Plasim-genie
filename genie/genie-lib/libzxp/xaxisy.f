      SUBROUTINE XAXISY(XO,YO,YSTEP)
C  To draw Y-axis through (XO,YO) with tick interval of YSTEP.
C  If YSTEP=0.0,the interval is set automatically
      INTEGER JUMP
      REAL UNITH
      REAL XRANGE
      REAL YRANGE
      REAL UH
      INTEGER NTMAG
      REAL ANMAG
      REAL ANSIZ
      REAL YFACTR
      REAL HX
      REAL XFACTR
      REAL HY
      REAL HOLD
      REAL XO
      REAL YB
      REAL YT
      REAL YSTEP
      REAL YSTEPJ
      REAL XAXINC
      INTEGER KTKY
      REAL HTICK
      REAL AYB
      REAL YO
      INTEGER J
      REAL Y
      INTEGER JUMPY
      INTEGER LAXFMT
      INTEGER LCH
      INTEGER KCH
      INTEGER ICLENG
      INTEGER KANY
      INTEGER KANX
      INTEGER KANTX
      INTEGER KANTY
      INTEGER KTKX
      INTEGER KTIKX
      INTEGER KTIKY
      REAL XL
      REAL XR
      REAL XSCALE
      REAL YSCALE
      REAL PL
      REAL PR
      REAL PB
      REAL PT
      INTEGER LLBFMT
C
      PARAMETER( JUMP =2 )
      COMMON /XMAP04/ XL,XR,YB,YT,XSCALE,YSCALE
      COMMON /XPHY01/PL,PR,PB,PT,XRANGE,YRANGE
      COMMON /XFTR06/ XFACTR,YFACTR
      COMMON /XAXS18/ KANX,KANY, KTKX,KTKY
      COMMON /XFMT33/ LBFMT, AXFMT
      COMMON /XFMT34/ LLBFMT,LAXFMT
      CHARACTER LBFMT*50, AXFMT*10,AXFM*(*)
      CHARACTER CH*20
      COMMON /XAXM35/ NTMAG, ANMAG, ANSIZ
      UNITH=SQRT( ABS(XRANGE*YRANGE))*0.01
      UH= MIN( ABS(XRANGE), ABS(YRANGE) )*0.03
      IF( NTMAG.EQ.0) ANMAG=UH
      IF( NTMAG.EQ.2) ANMAG=ANSIZ*YFACTR
      HX= ANMAG/XFACTR
      HY= ANMAG/YFACTR
      CALL XQCHMG( HOLD )
      CALL XCHMAG( ANMAG )
      CALL XPENUP( XO, YB)
      CALL XPENDN( XO, YT)
      IF( YSTEP.EQ.0.0) THEN
      YSTEPJ=XAXINC(YT-YB)
      GOTO 215
      ENDIF
      IF( KTKY.EQ.0) GOTO 210
      HTICK=UNITH/XFACTR*KTKY
      AYB=YO+INT((YB-YO)/YSTEP)*YSTEP
      DO 200 J=0,1000
       Y=AYB+ J *YSTEP
       IF( (Y-YB)*(Y-YT).GT.0.0 ) GOTO 210
       CALL XPENUP(XO,Y)
       CALL XPENDN(XO+HTICK,Y)
200   CONTINUE
210   JUMPY=JUMP
      IF( NINT((YT-YB) /YSTEP).GT.6  ) JUMPY=JUMP*2
      YSTEPJ=YSTEP*JUMPY
215   AYB=YO+INT((YB-YO)/YSTEPJ)*YSTEPJ
      HTICK=UNITH*1.5/XFACTR*KTKY
      DO 250 J=0,1000
       Y=AYB+J*YSTEPJ
       IF( (Y-YB)*(Y-YT).GT.0.0 ) GOTO 260
       IF( AXFMT(1:LAXFMT).EQ.'*') THEN
         CALL XRCH(Y, CH, LCH)
       ELSE
         DO 503 KCH=1,LAXFMT
         IF((AXFMT(KCH:KCH).EQ.'I')
     :       .or.(axfmt(kch:kch).eq.'i')) THEN
           WRITE(CH,AXFMT(1:LAXFMT)) NINT(Y)
           GOTO 504
         ENDIF
 503       CONTINUE
           WRITE(CH,AXFMT(1:LAXFMT)) Y
 504       LCH=ICLENG( CH )
         CALL XCHLJ(CH(1:LCH), LCH)
       ENDIF
       IF( KANY ) 301,300,302
 301     CALL XCHARR(XO-HX*0.7,Y-0.4*HY, CH(1:LCH) )
       GOTO 300
 302     CALL XCHARL(XO+HX   ,Y-0.4*HY, CH(1:LCH) )
 300     IF(KTKY.NE.0.AND.YSTEP.EQ.0.0 .OR. KTKY.NE.0.AND.KANY.NE.0)THEN
       CALL XPENUP(XO,Y)
       CALL XPENDN (XO+HTICK,Y)
       ENDIF
 250  CONTINUE
 260  CONTINUE
      CALL XCHMAG( HOLD )
      RETURN
      ENTRY XAXANT(KANTX,KANTY)
C KANTX KANTY--  Axis annotation parameters.
C KANTX=1  annotation lacated above x-axis
C KANTX=-1 annotation lacated below x-axis
C KANTX=0  annotation on x-axis is suppressed
C KANTY=1  annotation lacated to the right of y-axis
C KANTY=-1 annotation lacated to the left  of y-axis
C KANTY=0  annotation on y-axis is suppressed
C Default: KANTX=-1, KANTY=-1
      KANX=KANTX
      KANY=KANTY
      RETURN
      ENTRY XAXTIK(KTIKX,KTIKY)
C KTIKX KTIKY--  Axis ticking parameters.
C KTIKX=1  ticking lacated above x-axis
C KTIKX=-1 ticking lacated below x-axis
C KTIKX=0  ticking on x-axis is suppressed
C KTIKY=1  ticking lacated to the right of y-axis
C KTIKY=-1 ticking lacated to the left  of y-axis
C KTIKY=0  ticking on y-axis is suppressed
C Default: KTIKX= 1, KTIKY= 1
      KTKX=KTIKX
      KTKY=KTIKY
      RETURN
      ENTRY XAXDEF
C* ZXPLOTI *
C To restore the default values of parameters for axis annotation
C and ticking.
      KTKX=1
      KTKY=1
      KANX=-1
      KANY=-1
      RETURN
      ENTRY XAXFMT( AXFM )
C* MODIFIED IN ZXPLOTI, INTEGER FORMAT ALLOWED. *
      LAXFMT=LEN(AXFM)
      AXFMT=AXFM
      END
