      SUBROUTINE XPSPAC( PL0,PR0,PB0,PT0)
C Define the individual picture plotting area in ND-space
C Its arguments should be in the range of (0.0,1.5,0.0,1.0)
C By default this area covers the whole ND-space .
C All transformations in vector space are reset as default (cancelled)
C when XPSPAC is called.
C
      REAL PL
      REAL PL0
      REAL PR
      REAL PR0
      REAL PB
      REAL PB0
      REAL PT
      REAL PT0
      REAL XRANGE
      REAL YRANGE
      REAL XFACTR
      REAL XSCALE
      REAL YFACTR
      REAL YSCALE
      REAL X1
      REAL Y1
      REAL X2
      REAL Y2
      REAL DXPO
      REAL DYPO
      REAL DXMOP
      REAL DYMOP
      REAL XPREF
      REAL YPREF
C
      COMMON /XPHY01/PL,PR,PB,PT,XRANGE,YRANGE
      COMMON /XMAP04/ X1,X2,Y1,Y2,XSCALE,YSCALE
      COMMON /XFTR06/ XFACTR,YFACTR
      COMMON /XPHO03/ DXPO,DYPO
      COMMON /XMAO05/ DXMOP,DYMOP
      COMMON /XPRF07/XPREF,YPREF
      PL=PL0
      PR=PR0
      PB=PB0
      PT=PT0
      XRANGE=PR-PL
      YRANGE=PT-PB
      XFACTR=XRANGE/XSCALE
      YFACTR=YRANGE/YSCALE
      CALL XPSCOF
      CALL XMREFP(X1,Y1)
      CALL XUNMLC
      CALL XMROFF
      CALL XSROFF
      CALL XOBOFF
      RETURN
      END
