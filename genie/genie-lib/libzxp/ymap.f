
      FUNCTION YMAP(Y)
      include 'mappings.inc'
C
      REAL YMAP
      REAL Y
C
      YMAP= (Y-YBOT)*YRATIO + YSCRB
      END
