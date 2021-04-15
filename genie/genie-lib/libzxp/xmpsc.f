
      SUBROUTINE XMPSC(XSIDE,YSIDE)
C
C    Set mapping between penup/dn coordinates and postscript
C
      include 'vpsc.inc'
C
      REAL XSIDE
      REAL YSIDE
C
      XL=0.0
      YB=0.0
      XR=XSIDE
      YT=YSIDE

      XR=1./(XR-XL)
      YT=1./(YT-YB)

      END
