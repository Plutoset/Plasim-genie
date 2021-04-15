      SUBROUTINE XPENUP(X,Y)
C position pen at point (x,y) defined in maths space
C
      REAL XPP
      REAL X
      REAL YPP
      REAL Y
      REAL XMPEN
      REAL YMPEN
      REAL FLEN
      REAL BLEN
      INTEGER NPD
      REAL XPEN
      REAL YPEN
C
      COMMON /XPEN11/ XPEN,YPEN,FLEN,BLEN,NPD,XMPEN,YMPEN
      XPP= X
      YPP= Y
      XMPEN= X
      YMPEN= Y
      CALL XTRANS(XPP,YPP)
      CALL XTPNUP(XPP,YPP)
      FLEN=0.0
      BLEN=0.0
      NPD=0
      RETURN
      END
