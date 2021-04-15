      SUBROUTINE ZCSTCH(IU)
C
      INTEGER IU,K
C     Plot cheap polar stereographic coastlines.
      REAL FNN(5,4)
  200 CONTINUE
      READ (IU,END=400) FNN
      DO 300 K=1,4
      IF (FNN(1,K).GT.0.) THEN
       CALL ZPENUP(FNN(2,K),FNN(3,K))
       CALL ZPENDN(FNN(4,K),FNN(5,K))
      ENDIF
  300 CONTINUE
      GO TO 200
  400 REWIND IU
      RETURN
      END
