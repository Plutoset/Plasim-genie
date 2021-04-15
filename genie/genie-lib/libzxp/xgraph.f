C
      SUBROUTINE XGRAPH( X,Y,N )
C Plot single valued curve y=y(x), where for each x,there is a unique y.
C METHOD-- parameter controling curve plotting pattern
C          =0 ,points are joined up by straight lines
C          =1 or 2, two points are joined up using seamed quadratics.
C when =1,slope at data points are calculated using monotonic method
C when =2, using Bessel methed.
C
      INTEGER N
      INTEGER MTD
      INTEGER NEND
      INTEGER I
C
      REAL X(*), Y(*)
      IF( N.LE.1) RETURN
      IF( N.EQ.2) THEN
      CALL XLPNUP( X(1),Y(1))
      CALL XLPNDN( X(2),Y(2))
      RETURN
      ENDIF
      CALL XQCVMD( MTD )
      IF( MTD.EQ.0) THEN
      CALL XLPNUP(X(1),Y(1))
      ELSE
      CALL XGRPUP(X(1),Y(1))
      ENDIF
      NEND=0
      DO 10 I=2,N
      IF( MTD.EQ.0) THEN
        CALL XLPNDN(X(I),Y(I))
      ELSE
        IF( I.EQ.N) NEND=1
        CALL XGRPDN(X(I),Y(I),NEND)
      ENDIF
 10   CONTINUE
      CALL XLPNUP( X(N),Y(N) )
      RETURN
      END
