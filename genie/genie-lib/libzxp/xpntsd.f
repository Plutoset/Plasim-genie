      FUNCTION XPNTSD(X1,Y1,X2,Y2)
C Measure the distance in ND-space between two points
C  (X1,Y1) and (X2,Y2) defined in maths space
C
      REAL XPNTSD
      REAL PX1
      REAL X1
      REAL PY1
      REAL Y1
      REAL PX2
      REAL X2
      REAL PY2
      REAL Y2
C
      PX1=X1
      PY1=Y1
      CALL XTRANS(PX1,PY1)
      PX2=X2
      PY2=Y2
      CALL XTRANS(PX2,PY2)

      XPNTSD=SQRT((PX2-PX1)*(PX2-PX1)+(PY2-PY1)*(PY2-PY1))
      RETURN
      END
