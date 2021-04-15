C
      SUBROUTINE GIFSWAP(A,B,NRASX,NRASY)
C
      INTEGER NRASX
      INTEGER NRASY
      INTEGER J
      INTEGER I
C
      CHARACTER A(NRASX*NRASY),B(NRASY*NRASX)
C
      DO J=1,NRASY
         DO I=1,NRASX
            B(J+(NRASX-I)*NRASY)=A(I+(J-1)*NRASX)
         END DO
      END DO
      DO I=1,NRASX*NRASY
         A(I)=B(I)
      END DO
C      
      RETURN
      END
