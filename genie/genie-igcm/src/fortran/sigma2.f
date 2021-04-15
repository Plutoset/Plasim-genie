C*************************************
C   FUNCTION SIGMA2
C*************************************
C
       REAL FUNCTION SIGMA2(N, SX, INCX)
C
       INTEGER N, INCX, I
       REAL SX(N*INCX)
C
       SIGMA2 = 0.
       DO 1 I=1,N
          SIGMA2 = SIGMA2 + SX(I*INCX)
 1     CONTINUE
C
       END
