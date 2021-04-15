      SUBROUTINE FFT(A,JUMP,N,KLOT,ISIGN,TRIG,IFAX)
      IMPLICIT NONE
C     
C     Call ECMWF FFT routine FFT991
C     
C     Input arguments:
C     
C     A     - Array containing input data
C     JUMP  - Increment between the start of each data vector
C     N     - Length of data vectors
C     KLOT  - Number of data vectors
C     ISIGN - +1 for inverse transform (Fourier to Gridpoint)
C     -1 for direct transform (Gridpoint to Fourier)
C     
      INTEGER JUMP,KLOT,ISIGN,J,JJ,N,IFAX(*)
      REAL TRIG(*),SCALE
      REAL A(JUMP,KLOT)
C     
      IF (ISIGN.GT.0) THEN
         DO 100 J=1,KLOT
            DO 150 JJ=2,N
               A(JJ,J)=A(JJ+1,J)
 150        CONTINUE
            CALL RFFTB(N,A(1,J),TRIG,IFAX)
 100     CONTINUE
      ELSE
         SCALE=1.0/REAL(N)
         DO 200 J=1,KLOT
            CALL RFFTF(N,A(1,J),TRIG,IFAX)
            A(N+2,J)=0.0
            DO 250 JJ=N+1,3,-1
               A(JJ,J)=SCALE*A(JJ-1,J)
 250        CONTINUE
            A(2,J)=0.0
            A(1,J)=SCALE*A(1,J)
 200     CONTINUE
      ENDIF
C     
      END
C
      SUBROUTINE FFT8(A,JUMP,N,KLOT,ISIGN,TRIG,IFAX)
      IMPLICIT NONE
C     
C     Call ECMWF FFT routine FFT991
C     
C     Input arguments:
C     
C     A     - Array containing input data
C     JUMP  - Increment between the start of each data vector
C     N     - Length of data vectors
C     KLOT  - Number of data vectors
C     ISIGN - +1 for inverse transform (Fourier to Gridpoint)
C     -1 for direct transform (Gridpoint to Fourier)
C     
      INTEGER JUMP,KLOT,ISIGN,J,JJ,N,IFAX(*)
      REAL*8 TRIG(*),SCALE
      REAL*8 A(JUMP,KLOT)
C     
      IF (ISIGN.GT.0) THEN
         DO 100 J=1,KLOT
            DO 150 JJ=2,N
               A(JJ,J)=A(JJ+1,J)
 150        CONTINUE
            CALL RFFTB8(N,A(1,J),TRIG,IFAX)
 100     CONTINUE
      ELSE
         SCALE=1.0/REAL(N)
         DO 200 J=1,KLOT
            CALL RFFTF8(N,A(1,J),TRIG,IFAX)
            A(N+2,J)=0.0
            DO 250 JJ=N+1,3,-1
               A(JJ,J)=SCALE*A(JJ-1,J)
 250        CONTINUE
            A(2,J)=0.0
            A(1,J)=SCALE*A(1,J)
 200     CONTINUE
      ENDIF
C     
      END
