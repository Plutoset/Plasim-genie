      SUBROUTINE XOBSET(X2,Y2)
C Perform non-orthogonal rotation of coordinate system. (Deformation)
C
      REAL XA
      REAL YA
      REAL X1
      REAL X2
      REAL XMPREF
      REAL Y1
      REAL Y2
      REAL YMPREF
      REAL COSXA
      REAL SINYA
      REAL SINXA
      REAL COSYA
      REAL XANG
      REAL YANG
      REAL DR
      REAL XANGLE
      REAL CRANG
      REAL DRANG
      REAL SRANG
      INTEGER KSR
      REAL XANG1
      REAL YANG1
      REAL XMREF
      REAL YMREF
      REAL XSYMAN
      REAL SINDRA
      REAL COSDRA
      REAL SINMRA
      REAL COSMRA
      REAL SINSRA
      REAL COSSRA
      REAL CHSIN
      REAL CHCOS
C
      COMMON /XMRF08/ XMREF,YMREF,XMPREF,YMPREF
      COMMON /XAGS09/ DRANG,CRANG,XANGLE,XSYMAN,SRANG,KSR,XA,YA
      COMMON /XSCS10/ SINDRA,COSDRA,SINMRA,COSMRA,SINSRA,COSSRA
     :              ,SINXA,COSXA,SINYA,COSYA ,CHSIN,CHCOS
      IF( XA.EQ.0.0.AND.YA.EQ.0.0) RETURN
      X1=X2-XMPREF
      Y1=Y2-YMPREF
      X2=X1*COSXA-Y1*SINYA+XMPREF
      Y2=X1*SINXA+Y1*COSYA+YMPREF
      RETURN
      ENTRY XOBANG( XANG, YANG)
C Define angles for non-orthogonal coordiante rotation.
C XANG -- The angle x-axis is rotated through relative to old x-axis.
C YANG -- The angle y-axis is rotated through relative to old y-axis.
      XA=XANG
      YA=YANG
      DR =ATAN(1.0)/45.0
      SINXA=SIN(XA*DR)
      COSXA=COS(XA*DR)
      SINYA=SIN(YA*DR)
      COSYA=COS(YA*DR)
      XANGLE=CRANG+DRANG+(90-SRANG)*KSR +XA
      RETURN
      ENTRY XOBOFF
C Switch off non-orthogonal rotation
      XA=0.0
      YA=0.0
      SINXA=0.0
      COSXA=1.0
      SINYA=0.0
      COSYA=1.0
      XANGLE=CRANG+DRANG+(90-SRANG)*KSR+XA
      RETURN
      ENTRY XQOBAG( XANG1, YANG1)
      XANG1=XA
      YANG1=YA
      RETURN
      END
