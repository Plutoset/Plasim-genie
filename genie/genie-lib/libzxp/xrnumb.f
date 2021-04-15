      SUBROUTINE XRNUMB(X,Y,R, FORM )
C
      REAL R
      INTEGER LCH
      INTEGER ICLENG
      REAL X
      REAL Y
C
      CHARACTER CH*132 , FORM*(*)
      IF( FORM.EQ. '*' ) THEN
      CALL XRCH(R,CH,LCH)
      ELSE
      WRITE( CH, FORM )  R
      LCH= ICLENG ( CH )
      ENDIF
      CALL XCHARL(X,Y,CH(1:LCH) )
      END
