      SUBROUTINE XMAP(XL,XR,YB,YT)
C Map the picture space. (Define maths coordinates on the picture space)
C Transformations in vector space are reset as default when remaped.
C
      REAL XSCALE
      REAL XR
      REAL XL
      REAL YSCALE
      REAL YT
      REAL YB
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2
      REAL XFACTR
      REAL XRANGE
      REAL YFACTR
      REAL YRANGE
      REAL XL0
      REAL XR0
      REAL YB0
      REAL YT0
      REAL PL
      REAL PR
      REAL PB
      REAL PT
C
      COMMON /XPHY01/PL,PR,PB,PT,XRANGE,YRANGE
      COMMON /XMAP04/ X1,X2,Y1,Y2,XSCALE,YSCALE
      COMMON /XFTR06/ XFACTR,YFACTR

      XSCALE=XR-XL
      YSCALE=YT-YB
      X1=XL
      X2=XR
      Y1=YB
      Y2=YT
      XFACTR=XRANGE/XSCALE
      YFACTR=YRANGE/YSCALE
      CALL XMREFP(X1,Y1)
      CALL XPSCOF
      CALL XUNMLC
      CALL XMROFF
      CALL XSROFF
      CALL XOBOFF
      RETURN
      ENTRY XQMAP (XL0,XR0,YB0,YT0)
C Return the range of the current mapping space
      XL0=X1
      XR0=X2
      YB0=Y1
      YT0=Y2
      RETURN
      END
