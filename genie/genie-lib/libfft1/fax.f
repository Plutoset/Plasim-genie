c
c	These are a set of dummy routines which allow
c       fax and trig to be called, even with the fortran fft
c
c     This routine is never called so I have removed the 'mode'
c       argument which is flagged up as not being used.
c
c      SUBROUTINE FAX(IFAX,N,MODE)
      SUBROUTINE FAX(IFAX,N)
      IMPLICIT NONE
c      INTEGER NMAX,N,IFAX,MODE
      INTEGER NMAX,N,IFAX
      REAL    TRIGS
      PARAMETER(NMAX=241)
      DIMENSION IFAX(*)
      DIMENSION TRIGS(NMAX)
C
      IF (NMAX.LT.(3*N)/2+1) THEN
         PRINT*,' IN FAX INCREASE NMAX TO ',(3*N)/2+1
         STOP
      ENDIF
      CALL SET99(TRIGS,IFAX,N)
      RETURN
      END
