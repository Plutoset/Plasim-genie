      SUBROUTINE XCURVE( X,Y,N, KLOSE)
C Plot multiple-valued curve X(t), Y(t).
C METHOD-- parameter controling curve plotting pattern.
C          =0 ,points are joined up by straight lines
C  =1 or 2, two points are joined up using parametric seamed quadratics.
C when =1,slope at data points are calculated using monotonic method
C when =2, using Bessel methed.
C
      INTEGER N
      INTEGER MTD
      INTEGER NEND
      INTEGER I
      INTEGER KLOSE
C
      REAL X(*), Y(*)
      IF( N.LE.1) RETURN
      IF( N.EQ.2) THEN
      CALL XLPNUP( X(1),Y(1))
      CALL XLPNDN( X(2),Y(2))
      RETURN
      ENDIF
      CALL XQCVMD(MTD)
      IF( MTD.EQ.0) THEN
      CALL XLPNUP(X(1),Y(1))
      ELSE
      CALL XCURUP(X(1),Y(1))
      ENDIF
      NEND=0
      DO 10 I=2,N
      IF( MTD.EQ.0) THEN
        CALL XLPNDN(X(I),Y(I))
      ELSE
        IF( I.EQ.N) NEND=1
        CALL XCURDN(X(I),Y(I), KLOSE, NEND)
      ENDIF
 10   CONTINUE
      IF( MTD.EQ.0.AND.KLOSE.EQ.1.AND.(X(1).NE.X(N).OR.Y(1).NE.Y(N)))
     :  CALL XLPNDN(X(1), Y(1))
      CALL XLPNUP( X(1),Y(1) )
      RETURN
      END
