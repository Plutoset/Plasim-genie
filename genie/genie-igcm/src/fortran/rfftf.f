      SUBROUTINE RFFTF (N,R,WSAVE,IFAX)
      IMPLICIT NONE
      INTEGER N
      REAL    R,WSAVE
      DIMENSION       R(*)       ,WSAVE(*)
      INTEGER IFAX(*)
      IF (N .EQ. 1) RETURN
      CALL RFFTF1 (N,R,WSAVE,WSAVE(N+1),IFAX)
      RETURN
      END
C
      SUBROUTINE RFFTF8 (N,R,WSAVE,IFAX)
      IMPLICIT NONE
      INTEGER N
      REAL*8 R,WSAVE
      DIMENSION       R(*)       ,WSAVE(*)
      INTEGER IFAX(*)
      IF (N .EQ. 1) RETURN
      CALL RFFTF18 (N,R,WSAVE,WSAVE(N+1),IFAX)
      RETURN
      END
