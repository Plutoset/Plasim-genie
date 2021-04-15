C
      SUBROUTINE XPSBLN(R,G,B)
C
C     Begin postscript file, overwrites existing filename
C
C
      include 'vpsc.inc'
C
      INTEGER IPV
      INTEGER IPSC
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      include 'colours.inc'
      REAL R,G,B
C
      CPSBKG=' -215 -500 m 0 4000 L 6000 0 L 0 -4000 '//
     :       'L -6000 0 L c 1.00  .00  .00 q'
C
      IF (R.LT.0.0.OR.G.LT.0.0.OR.B.LT.0.0) THEN
         ICPSBKG=0
      ELSE
         ICPSBKG=1
         WRITE(CPSBKG(54:57),'(F4.2)')R
         WRITE(CPSBKG(59:62),'(F4.2)')G
         WRITE(CPSBKG(64:67),'(F4.2)')B
         CALL IFILZERO(CPSBKG)
      END IF
C
      IF (IPAGE.EQ.1.AND.ILINE.EQ.1) THEN
         WRITE(NOUT,'(A)')CPSBKG
      END IF
C
      RETURN
      END
