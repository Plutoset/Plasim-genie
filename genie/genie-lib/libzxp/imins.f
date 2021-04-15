C
      SUBROUTINE IMINS(II,N,IIMIN,ICOUNT)
C
      INTEGER N
      INTEGER IIMIN
      INTEGER ICOUNT
      INTEGER I
C
      INTEGER II(N)
C
C     This routine finds the minimum value of an integer array and the
C     corresponding number of the array element
C
      IIMIN=II(1)
      ICOUNT=1
C
      DO 10 I=2,N
      IF(II(I).LT.IIMIN)THEN
        IIMIN=II(I)
        ICOUNT=I
      ENDIF
 10   CONTINUE
C
      END
