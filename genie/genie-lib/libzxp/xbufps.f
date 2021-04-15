      SUBROUTINE XBUFPS(IFAIL)
C
C    Clears buffer before closing or going to new frame
C
C
      include 'vpsc.inc'
C
      INTEGER IFAIL
C
      IFAIL=1
      IF(ILINE.EQ.1.AND.ICHR.GT.ICHAR1)THEN
        IF (NOUTA.EQ.0) THEN
         WRITE(NOUT,'(A)')LINE(1:ICHR)//' s '//CHARCL
        ELSE
           ICNOUT=ICNOUT+1
         WRITE(NOUTA,'(A)')LINE(1:ICHR)//' s '//CHARCL
        END IF
C
      IPSCON=IPSCON+1
C
      ILINE=ILINE+1
      ISLINE=1
      IFAIL=0
      ELSEIF(ILINE.GT.1.AND.ICHR.GT.0)THEN
        IF (NOUTA.EQ.0) THEN
         WRITE(NOUT,'(A)')LINE(1:ICHR)//' s '//CHARCL
        ELSE
           ICNOUT=ICNOUT+1
         WRITE(NOUTA,'(A)')LINE(1:ICHR)//' s '//CHARCL
        END IF
C
      IPSCON=IPSCON+1
C
      ILINE=ILINE+1
      ISLINE=1
      IFAIL=0
      ENDIF

      IF(ILINE.GT.1)IFAIL=0

      END
