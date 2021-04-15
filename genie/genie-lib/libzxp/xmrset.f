c
      SUBROUTINE  XMRSET(X2,Y2)
C Perform rotation around picture reference point (XMREF,YMREF)
C
      REAL CRANG
      REAL X1
      REAL X2
      REAL XMPREF
      REAL Y1
      REAL Y2
      REAL YMPREF
      REAL COSMRA
      REAL SINMRA
      REAL ANG
      REAL RADANG
      REAL XANGLE
      REAL DRANG
      REAL SRANG
      INTEGER KSR
      REAL XA
      REAL ANG1
      REAL COSSRA
      REAL SINSRA
      REAL XYANGL
      INTEGER KSHEAR
      REAL RAD
      REAL XMREF
      REAL YMREF
      REAL XSYMAN
      REAL YA
      REAL SINDRA
      REAL COSDRA
      REAL SINXA
      REAL COSXA
      REAL SINYA
      REAL COSYA
      REAL CHSIN
      REAL CHCOS
C
      COMMON /XMRF08/ XMREF,YMREF,XMPREF,YMPREF
      COMMON /XAGS09/ DRANG,CRANG,XANGLE,XSYMAN,SRANG,KSR,XA,YA
      COMMON /XSCS10/ SINDRA,COSDRA,SINMRA,COSMRA,SINSRA,COSSRA
     :              ,SINXA,COSXA,SINYA,COSYA ,CHSIN,CHCOS
      IF( CRANG.EQ.0.0) RETURN
      X1=X2-XMPREF
      Y1=Y2-YMPREF
      X2=X1*COSMRA-Y1*SINMRA +XMPREF
      Y2=X1*SINMRA+Y1*COSMRA +YMPREF
      RETURN
      ENTRY XMRANG(ANG)
C Set coordinate rotation angle (It supercedes the previous value.)
      CRANG=ANG
      RADANG= ATAN(1.)/45.0*ANG
      SINMRA= SIN( RADANG)
      COSMRA= COS( RADANG)
      XANGLE=CRANG+DRANG+(90-SRANG)*KSR+XA
      RETURN
      ENTRY XMROFF
C Turn off coordinate rotation
      CRANG=0.0
      XANGLE=CRANG+DRANG+(90-SRANG)*KSR+XA
      SINMRA=0.0
      COSMRA=1.0
      RETURN
      ENTRY XQMRAG(ANG1)
      ANG1=CRANG
      RETURN
      ENTRY XSRSET(X2,Y2)
C Perform shearing of the picture in x or y direction
      IF( SRANG.EQ.90.0) RETURN
      IF( KSR.EQ.0) THEN
        Y1=Y2-YMPREF
        X2=X2+Y1*COSSRA
        Y2=YMPREF+Y1*SINSRA
      ELSE
        X1=X2-XMPREF
        Y2=Y2+X1*COSSRA
        X2=XMPREF+X1*SINSRA
      ENDIF
      RETURN
      ENTRY XSHEAR( XYANGL, KSHEAR)
C Shear the picture in x or y direction.
C XYANGL  The angle between x and y-axis after shearing (default 90.0)
C KSHEAR  =0 when X-axis to be is fixed, =1 when Y-axis is to be fixed
      SRANG=XYANGL
      KSR=KSHEAR
      RAD=ATAN(1.0)/45.0*SRANG
      SINSRA=SIN(RAD)
      COSSRA=COS(RAD)
      XANGLE=CRANG+DRANG+(90-SRANG)*KSR+XA
      RETURN
      ENTRY XSROFF
C Switch off picture shearing
      SRANG=90.0
      KSR=0
      SINSRA=1.0
      COSSRA=0.0
      XANGLE=CRANG+DRANG+(90-SRANG)*KSR+XA
      RETURN
      END
