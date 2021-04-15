C
      SUBROUTINE IMAXS(II,N,IIMAX,ICOUNT)
C
      INTEGER N
      INTEGER IIMAX
      INTEGER ICOUNT
      INTEGER I
C
      INTEGER II(N)
C
C     This routine finds the maximum value of an integer array and the
C     corresponding number of the array element
C
      IIMAX=II(1)
      ICOUNT=1
C
      DO 10 I=2,N
      IF(II(I).GT.IIMAX)THEN
        IIMAX=II(I)
        ICOUNT=I
      ENDIF
 10   CONTINUE
C
      END
