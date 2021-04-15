
      SUBROUTINE XQUADR(X1,Y1,X2,Y2,X3,Y3,A,B,C)
C Return the coefficients A,B,C of a quadratic polynomial fitting
C points (X1,Y1),(X2,Y2),(X3,Y3)
C
      REAL D21
      REAL Y2
      REAL Y1
      REAL X2
      REAL X1
      REAL D32
      REAL Y3
      REAL X3
      REAL A
      REAL B
      REAL C
C
      D21=(Y2-Y1)/(X2-X1)
      D32=(Y3-Y2)/(X3-X2)
      A=(D32-D21)/(X3-X1)
      B=(D21*(X3+X2)-D32*(X1+X2))/(X3-X1)
      C=(X3*Y1-X1*Y3+X1*X3*(D32-D21))/(X3-X1)
      RETURN
      END
