C
      SUBROUTINE XFLPSC
C
C   Change postscript line back to full
C
      include 'vpsc.inc'
C
      INTEGER IPSC
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      IF (IPSC.EQ.0) RETURN

      LINE1=' b  0  0  0  0 '//CHARCL

      IF (ICHR.GT.0.AND.IPSCLT.NE.1.AND.
     :    (ISLINE.GT.1.OR.ICHR.GT.ICHAR1) )THEN
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
         IPSCLT=1
         CALL XCOL(LINE,LINE1,LNCOL,ICHR,ICHAR1)
      ENDIF
C
      RETURN
      END
