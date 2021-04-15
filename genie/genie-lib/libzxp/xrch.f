      SUBROUTINE XRCH( R,CH,LCH)
C Return real number R as a character string in automatically set format
C
      REAL ABSR
      REAL R
      INTEGER LCH
C
      CHARACTER CH*20
      ABSR=ABS(R)
      IF(ABSR.GE.1.0E5.OR.(ABSR.GT.0.0.AND.ABSR.LT.1.0E-2))THEN
        WRITE(CH,'(1P,E20.2)') R
      ELSEIF( ABSR.LT.0.1.AND. ABSR.NE.0.0) THEN
        WRITE(CH,'(F20.2)') R
      ELSE
        WRITE(CH,'(F20.1)') R
      ENDIF
      LCH=20
      CALL XCHLJ( CH, LCH)
      END
