C
      SUBROUTINE XEPSC
C
C     Ends postscript file
C
      include 'vpsc.inc'
C
      INTEGER IPSC
      INTEGER I
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      IF(IPSC.EQ.0)THEN
      PRINT*,' POSTSCRIPT OUTPUT NOT TURNED ON: ERROR '
      STOP
      ENDIF
C
      IF (IMACH.NE.1) THEN
c
c     Dont think I need the next bit now
c
c        IF (IPAGE.NE.1) THEN
c           BACKSPACE NOUT
c           BACKSPACE NOUT
c           BACKSPACE NOUT
c        END IF
      WRITE(NOUT,'(A)')'%%Trailer'
      WRITE(NOUT,'(A)')'x'
      DO I=1,10
           WRITE(NOUT,'(A)')' '
      END DO
      CLOSE(UNIT=NOUT)
      ENDIF
      END
