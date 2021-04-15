C
      SUBROUTINE XICH( I,CH, LCH)
      INTEGER I
      INTEGER LCH
      CHARACTER CH*20
      WRITE(CH,'( I20  )')  I
      LCH=20
      CALL XCHLJ( CH, LCH )
      END
