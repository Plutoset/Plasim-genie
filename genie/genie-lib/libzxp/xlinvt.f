C
      SUBROUTINE XLINVT(X,Y)
C Perform inverse linear transformation (from ND-space to mathe space).
C
      REAL X
      REAL XMREF
      REAL XMPREF
      REAL XFACTR
      REAL Y
      REAL YMREF
      REAL YMPREF
      REAL YFACTR
C
      COMMON /XFTR06/ XFACTR,YFACTR
      COMMON /XMRF08/ XMREF,YMREF,XMPREF,YMPREF
      X=XMREF+(X-XMPREF)/XFACTR
      Y=YMREF+(Y-YMPREF)/YFACTR
      RETURN
      END
