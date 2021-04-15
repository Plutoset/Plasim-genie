      SUBROUTINE XINUMB(X,Y,I, FORM )
C
      INTEGER I
      INTEGER LCH
      INTEGER ICLENG
      REAL X
      REAL Y
C
      CHARACTER CH*132 , FORM*(*)
      IF( FORM.EQ. '*' ) THEN
      CALL XICH(I,CH,LCH)
      ELSE
      WRITE( CH, FORM )  I
      LCH= ICLENG ( CH )
      ENDIF
      CALL XCHARL(X,Y,CH(1:LCH) )
      END
