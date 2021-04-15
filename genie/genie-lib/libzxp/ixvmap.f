      FUNCTION IXVMAP(X)
      include 'mappings.inc'
C
      INTEGER IXVMAP
      REAL X
C
      IXVMAP=NINT( (X-XLHS)*XRATIO + XSCRL )
      END
