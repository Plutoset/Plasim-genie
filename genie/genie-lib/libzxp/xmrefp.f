
      SUBROUTINE XMREFP(XREF,YREF)
      IMPLICIT NONE
C
C Define the picture reference point in mapped vector space as the
C center of picture scaling, deformation, rotation, and translation.
C All these transformations are cancelled when either XPSPAC or XMAP
C is called  (But the transformations defined in ND-space remain in
C effect).
C
      REAL PL,PR,PB,PT,XRANGE,YRANGE
      COMMON /XPHY01/PL,PR,PB,PT,XRANGE,YRANGE
      REAL X1,X2,Y1,Y2,XSCALE,YSCALE
      COMMON /XMAP04/ X1,X2,Y1,Y2,XSCALE,YSCALE
      REAL DXMOP,DYMOP
      COMMON /XMAO05/ DXMOP,DYMOP
      REAL XFACTR,YFACTR
      COMMON /XFTR06/ XFACTR,YFACTR
      REAL XMREF,YMREF,XMPREF,YMPREF
      COMMON /XMRF08/ XMREF,YMREF,XMPREF,YMPREF
C
      REAL XLOC1,YLOC1,SCALX1,SCALY1,
     :     XLOC,YLOC,SCALEX,SCALEY,XREF,YREF
C
      XMREF=XREF
      YMREF=YREF
      XMPREF=PL+(XMREF-X1)*(PR-PL)/(X2-X1)
      YMPREF=PB+(YMREF-Y1)*(PT-PB)/(Y2-Y1)
      RETURN

      ENTRY XPSCAL(SCALEX,SCALEY)
C Define scaling factors
      XRANGE=(PR-PL)*SCALEX
      YRANGE=(PT-PB)*SCALEY
      XFACTR=XRANGE/XSCALE
      YFACTR=YRANGE/YSCALE
      RETURN

C Switch off scaling
      ENTRY XPSCOF
      XRANGE= PR-PL
      YRANGE= PT-PB
      XFACTR=XRANGE/XSCALE
      YFACTR=YRANGE/YSCALE
      RETURN

      ENTRY XQPSCL(SCALX1,SCALY1)
      SCALX1=XRANGE/(PR-PL)
      SCALY1=YRANGE/(PT-PB)
      RETURN

      ENTRY XMLOCA(XLOC,YLOC)
C Translate the picture reference point to the location (XLOC,YLOC)
C defined in mapped vetor space.
      DXMOP=(XLOC-XREF) *XRANGE/XSCALE
      DYMOP=(YLOC-YREF) *YRANGE/YSCALE
      RETURN

      ENTRY XUNMLC
C Switch off the translation in mapped vector space
      DXMOP=0.0
      DYMOP=0.0
      RETURN

      ENTRY XQMLOC(XLOC1,YLOC1)
      XLOC1=XREF+DXMOP*XSCALE/XRANGE
      YLOC1=YREF+DYMOP*YSCALE/YRANGE
      RETURN
      END
