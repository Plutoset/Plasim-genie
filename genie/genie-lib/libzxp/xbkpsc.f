C
      SUBROUTINE XBKPSC(IF1,IB1,IF2,IB2)
C
C    Change post-script line to any type of line
C
      include 'vpsc.inc'
C
      INTEGER IPSC
      INTEGER III
      INTEGER IF1
      INTEGER IB1
      INTEGER IF2
      INTEGER IB2
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C

      IF (IPSC.EQ.0) RETURN
C
      III=IF1+IB1+IF2+IB2
      IF (III.EQ.IPSCLT) RETURN
      IPSCLT=III
C
      LINE1=' b            '//' '//CHARCL
      IF(IF1.GT.99)THEN
      PRINT*,' ERROR IN broken lines in postscript, IF1 MUST lt.100 '
      IF1=10
      ENDIF
      IF(IB1.GT.99)THEN
      PRINT*,' ERROR IN broken lines in postscript, IB1 MUST lt.100 '
      IB1=10
      ENDIF
      IF(IF2.GT.99)THEN
      PRINT*,' ERROR IN broken lines in postscript, IF2 MUST lt.100 '
      IF1=10
      ENDIF
      IF(IB2.GT.99)THEN
      PRINT*,' ERROR IN broken lines in postscript, IB2 MUST lt.100 '
      IB2=10
      ENDIF

      WRITE(LINE1(4:5),'(I2)')IF1
      WRITE(LINE1(7:8),'(I2)')IB1
      WRITE(LINE1(10:11),'(I2)')IF2
      WRITE(LINE1(13:14),'(I2)')IB2

      IF (ICHR.GT.0.AND.
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
         CALL XCOL(LINE,LINE1,LNCOL,ICHR,ICHAR1)
      ENDIF
C
      RETURN
      END
