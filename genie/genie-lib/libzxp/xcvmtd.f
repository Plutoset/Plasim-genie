      SUBROUTINE XCVMTD( METHOD )
C Set the method for curve plotting when using XGRPUP,XGRPDN,XGRAPH,
C XCURUP,XCURUP,XCURDN. By default METHOD=0
C
      INTEGER MTD
      INTEGER METHOD
      INTEGER MTD1
C
      COMMON /XCMD24/ MTD
      MTD= METHOD
      RETURN
      ENTRY XQCVMD( MTD1 )
      MTD1=MTD
      RETURN
      END
