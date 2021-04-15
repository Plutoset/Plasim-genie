      SUBROUTINE XDRSET(X1,Y1)
C Perform rotation around device reference point.
C
      REAL DRANG
      REAL X2
      REAL X1
      REAL XPREF
      REAL Y2
      REAL Y1
      REAL YPREF
      REAL COSDRA
      REAL SINDRA
      REAL XP
      REAL YP
      REAL XP1
      REAL YP1
      REAL ANG
      REAL XANGLE
      REAL CRANG
      REAL SRANG
      INTEGER KSR
      REAL XA
      REAL RADANG
      REAL ANG1
      REAL DXPO
      REAL XPLOC
      REAL DYPO
      REAL YPLOC
      REAL XPLOC1
      REAL YPLOC1
      REAL PL
      REAL PR
      REAL PB
      REAL PT
      REAL XRANGE
      REAL YRANGE
      REAL XSYMAN
      REAL YA
      REAL SINMRA
      REAL COSMRA
      REAL SINSRA
      REAL COSSRA
      REAL SINXA
      REAL COSXA
      REAL SINYA
      REAL COSYA
      REAL CHSIN
      REAL CHCOS
C
      COMMON /XPHY01/PL,PR,PB,PT,XRANGE,YRANGE
      COMMON /XPHO03/ DXPO,DYPO
      COMMON /XPRF07/ XPREF,YPREF
      COMMON /XAGS09/ DRANG,CRANG,XANGLE,XSYMAN,SRANG,KSR,XA,YA
      COMMON /XSCS10/ SINDRA,COSDRA,SINMRA,COSMRA,SINSRA,COSSRA
     :              ,SINXA,COSXA,SINYA,COSYA ,CHSIN,CHCOS

      IF( DRANG.EQ.0.0) RETURN
      X2=X1-XPREF
      Y2=Y1-YPREF
      X1=X2*COSDRA-Y2*SINDRA+XPREF
      Y1=X2*SINDRA+Y2*COSDRA+YPREF
      RETURN
      ENTRY XDREFP(XP,YP)
C Define the device reference point in ND-space for overall picture
C rotation.
      XPREF=XP
      YPREF=YP
      RETURN
      ENTRY XQDREF(XP1,YP1)
      XP1=XPREF
      YP1=YPREF
      RETURN
      ENTRY XDRANG(ANG)
C Set the angle the overall picture is rotated through around the
C device reference point. (Defined by XDREFP )
      DRANG=ANG
      XANGLE=CRANG+DRANG+(90-SRANG)*KSR+XA
      RADANG= ATAN(1.)/45.0*ANG
      SINDRA= SIN( RADANG)
      COSDRA= COS( RADANG)
      RETURN
      ENTRY XDROFF
C Switch off rotation around the device reference point.
      DRANG=0.0
      XANGLE=CRANG+DRANG+(90-SRANG)*KSR+XA
      SINDRA=0.0
      COSDRA=1.0
      RETURN
      ENTRY XQDRAG(ANG1)
      ANG1=DRANG
      RETURN
      ENTRY XDLOCA(XPLOC,YPLOC)
C Define the position in ND-space to which the device reference point
C is moved. ( Picture translation in ND-space )
      DXPO=XPLOC-XPREF
      DYPO=YPLOC-YPREF
      RETURN
      ENTRY XUNDLC
C Cancel picture translation in ND-space.
      DXPO=0.0
      DYPO=0.0
      RETURN
      ENTRY XQDLOC(XPLOC1,YPLOC1)
      XPLOC1=XPREF+DXPO
      YPLOC1=YPREF+DYPO
      RETURN
      END
