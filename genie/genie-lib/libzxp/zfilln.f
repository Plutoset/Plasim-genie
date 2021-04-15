C
      SUBROUTINE ZFILLN(X,Y,NP)
C
C     To fill in pre-specified colour in an area closed by a circuit
C
      INTEGER IUNMAP
      INTEGER NP
C
      REAL X(*),Y(*)
      SAVE IUNMAP
      DATA IUNMAP/0/
C
      IF (NP.GE.3.AND.NP.LE.8000) THEN
         CALL ZFILLNN(X,Y,NP,IUNMAP)
      ELSE
c           PRINT*,' Invalid NP in ZFILL ',NP
           RETURN
      ENDIF
C
      RETURN
C
      ENTRY ZFILLUN
      IUNMAP=1-IUNMAP
      RETURN
      END
