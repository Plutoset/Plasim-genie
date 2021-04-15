      SUBROUTINE XTPNUP(X,Y)
C position pen at point (x,y) defined in maths space
C
      REAL X
      REAL Y
      REAL XPEN
      REAL YPEN
      REAL FLEN
      REAL BLEN
      INTEGER NPD
      REAL XMPEN
      REAL YMPEN
C
      COMMON /XPEN11/ XPEN,YPEN,FLEN,BLEN,NPD,XMPEN,YMPEN
      CALL PPENUP( X,Y)
      XPEN= X
      YPEN= Y
      RETURN
      END
