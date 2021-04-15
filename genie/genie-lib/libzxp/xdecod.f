      FUNCTION XDECOD(CH)
C
      REAL XDECOD
      INTEGER J
      INTEGER ICRAM
      INTEGER K
C
      CHARACTER   CH*2 ,F*1
      COMMON /XCHR30/ ICRAM(256)
  100 FORMAT(1A)
  101 FORMAT(1X,1A)
      READ(CH,100) F
      J = ICHAR(F)
      J=  ICRAM( J )
      IF (J .GT. 200) THEN
        J=J-240
      ELSE
        J=J-183
      END IF
      READ(CH,101) F
      K = ICHAR(F)
      K=  ICRAM( K )
      IF (K .GT. 200) THEN
       K = K - 240
      ELSE
       K=K-183
      ENDIF
      XDECOD = 16*J+K
      END
