C
      SUBROUTINE XBORDR
C DRAW A BORDER AROUND MAPPED AERA
      REAL X1
      REAL X2
      REAL Y1
      REAL Y2
      REAL XSCALE
      REAL YSCALE
C
      COMMON /XMAP04/ X1,X2,Y1,Y2,XSCALE,YSCALE
      CALL XBOX(X1,X2,Y1,Y2)
      RETURN
      END