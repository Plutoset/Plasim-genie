C
      SUBROUTINE XFPSC(ITYPE)
C
C     Calls new frame
C
C
      include 'vpsc.inc'
C
      INTEGER IPSC
      INTEGER IFAIL
      INTEGER III
      INTEGER I
      INTEGER ICH
      INTEGER NIN
      INTEGER N1
      INTEGER NAN
      INTEGER N2
      INTEGER NOUT1
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      INTEGER ITYPE
      CHARACTER LABEL*4,LINE100*100
      LOGICAL LOPENED
C
      IF (IPSC.EQ.0) RETURN
C
      CALL XBUFPS(IFAIL)
      IF(IFAIL.NE.0) RETURN
      ICHR=0
C
      IF (NOUTA.NE.0) THEN
         rewind nouta
         DO III=1,ICNOUT
            WRITE(LINE100,'(100X)')
            READ (NOUTA,'(A)')LINE100
            DO I=LEN(LINE100),1,-1
               IF (LINE100(I:I).NE.' ') GO TO 150
            END DO
 150        IF (I.GE.1) WRITE(NOUT,'(A)')LINE100(1:I)
         END DO
         rewind nouta
         ICNOUT=0
      END IF
C
      IF(IPSCRM.EQ.1.AND.IMACH.NE.1) THEN
         CALL XPSCRM
         RETURN
      ENDIF
C
      WRITE(NOUT,'(A)')' p'
      IF (ITYPE.EQ.0) THEN
         IPAGE=IPAGE+1
         CALL XLAB(IPAGE,LABEL,ICH)
         WRITE(NOUT,'(A)')'%%Page:'//LABEL//LABEL
         WRITE(NOUT,'(A)')'/c {closepath} def'
         WRITE(NOUT,'(A)')
     :         '/r {1 1 sethsbcolor fill 0 0 0 setrgbcolor} def'
         IPSCON=0
C
         ILINE=ILINE+2
C
         IF (ICPSBKG.NE.0) THEN
            WRITE(NOUT,'(A)')CPSBKG
            ILINE=ILINE+1
         END IF
C
         ILINE=1
         ISLINE=1
         ICHR=0
         NOUTA=0
         ICNOUT=0
      END IF
C
      RETURN
C
      ENTRY XLPRFX(N1,N2)
      NIN=N1
      NAN=N2
      RETURN
C
      ENTRY XPSCSP(NOUT1)
      IF (NOUTA.NE.0.AND.NOUTA.NE.NOUT1) THEN
         CLOSE(UNIT=NOUTA)
         ICNOUT=0
      END IF
      NOUTA=NOUT1
      INQUIRE(UNIT=NOUTA,OPENED=LOPENED)
      IF (.NOT.LOPENED) THEN
         OPEN(UNIT=NOUTA,FORM='FORMATTED',STATUS='SCRATCH')
         ICNOUT=0 
      END IF
      RETURN
C
      END
