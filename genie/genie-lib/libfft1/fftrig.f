c      SUBROUTINE FFTRIG(TRIGS,N,MODE)
      SUBROUTINE FFTRIG(TRIGS,N)
c     I have removed mode from the argument list here as it isn't used.
c     No routine calls this subroutine as far as I can tell.....
      IMPLICIT NONE
c      INTEGER IFAX,N,MODE
      INTEGER IFAX,N
      REAL    TRIGS
      DIMENSION TRIGS(*)
      DIMENSION IFAX(10)
      CALL SET99(TRIGS,IFAX,N)
      RETURN
      END
