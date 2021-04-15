
      FUNCTION IYMAP(Y)
      include 'mappings.inc'
C
      INTEGER IYMAP
      REAL Y
C
      IYMAP=NINT( (Y-YBOT)*YRATIO + YSCRB )
      END
