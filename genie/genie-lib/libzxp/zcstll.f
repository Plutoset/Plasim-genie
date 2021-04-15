      SUBROUTINE ZCSTLL(NCHNL)
C
      INTEGER NCHNL,ISW
      REAL    X,Y
C     Plot pre-computed polar stereographic coastlines.
  200 READ (NCHNL,END=400) ISW,X,Y
      IF (ISW.EQ.-1) THEN
       CALL ZPENUP(X,Y)
      ELSE
       CALL  ZPENDN(X,Y)
      ENDIF
      GO TO 200
  400 REWIND NCHNL
      RETURN
      END
