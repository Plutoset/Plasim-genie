C
      SUBROUTINE XPDPSC(X,Y)
C
C    Moves postscript pointer to new position without drawing line
C
      include 'vpsc.inc'
C
      INTEGER IPSC
      INTEGER IX
      REAL X
      INTEGER IY
      REAL Y
      INTEGER ICH
      INTEGER IXOLD
      INTEGER IYOLD
      INTEGER IXR
      INTEGER IYR
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
C
      CHARACTER LABEL*14
      SAVE IXOLD,IYOLD

      IF (IPSC.EQ.0) RETURN

      IF (ICHR.EQ.0) CALL XCOL(LINE,LINE1,LNCOL,ICHR,ICHAR1)
      IX=NINT((X-XL)*XR*IXB)+50
      IY=NINT((Y-YB)*YT*IYB)

      CALL XPREPN(IX,IY,LABEL,ICH)
      LABEL=LABEL(1:ICH)//' m'
      ICH=ICH+2
      IF(ICHR+ICH.LT.76)THEN
      LINE=LINE(1:ICHR)//LABEL(1:ICH)
      ICHR=ICHR+ICH
      ELSE
      IF(MOD(ISLINE,10).EQ.0)THEN
           IF (NOUTA.EQ.0) THEN
            WRITE(NOUT,'(A)')LINE(1:ICHR)//' s '//CHARCL
           ELSE
              ICNOUT=ICNOUT+1
            WRITE(NOUTA,'(A)')LINE(1:ICHR)//' s '//CHARCL
           END IF
      ELSE
           IF (NOUTA.EQ.0) THEN
            WRITE(NOUT,'(A)')LINE(1:ICHR)
           ELSE
              ICNOUT=ICNOUT+1
            WRITE(NOUTA,'(A)')LINE(1:ICHR)
           END IF
      ENDIF
c
      IPSCON=IPSCON+1
c
      ILINE=ILINE+1
      ISLINE=ISLINE+1
      LINE=LABEL(1:ICH)
      ICHR=ICH
      ENDIF
      IXOLD=IX
      IYOLD=IY
      RETURN
      ENTRY XPOPSC(X,Y)
C
C    Moves to new position drawing line
C
      IF (ICHR.EQ.0) CALL XCOL(LINE,LINE1,LNCOL,ICHR,ICHAR1)
C
      IX=NINT((X-XL)*XR*IXB)+50
      IY=NINT((Y-YB)*YT*IYB)
C
      IXR=IX-IXOLD
      IYR=IY-IYOLD
C
      CALL XPREPN(IXR,IYR,LABEL,ICH)
      LABEL=LABEL(1:ICH)//' L'
      ICH=ICH+2
C
      IF(ICHR+ICH.LT.76)THEN
      LINE=LINE(1:ICHR)//LABEL(1:ICH)
      ICHR=ICHR+ICH
      ELSE
      IF(MOD(ISLINE,10).EQ.0)THEN
           IF (NOUTA.EQ.0) THEN
            WRITE(NOUT,'(A)')LINE(1:ICHR)//' s'
           ELSE
              ICNOUT=ICNOUT+1
            WRITE(NOUTA,'(A)')LINE(1:ICHR)//' s'
           END IF
      ELSE
           IF (NOUTA.EQ.0) THEN
            WRITE(NOUT,'(A)')LINE(1:ICHR)
           ELSE
              ICNOUT=ICNOUT+1
            WRITE(NOUTA,'(A)')LINE(1:ICHR)
           END IF
      ENDIF
C
      IPSCON=IPSCON+1
C
      ILINE=ILINE+1
      ISLINE=ISLINE+1
      LINE=LABEL(1:ICH)
      ICHR=ICH
      ENDIF
      IXOLD=IX
      IYOLD=IY
C
      RETURN
C
      ENTRY XPSCLR
C
      IF(MOD(ISLINE,10).EQ.0)THEN
           IF (NOUTA.EQ.0) THEN
            WRITE(NOUT,'(A)')LINE(1:ICHR)//' s'
           ELSE
              ICNOUT=ICNOUT+1
            WRITE(NOUTA,'(A)')LINE(1:ICHR)//' s'
           END IF
      ELSE
           IF (NOUTA.EQ.0) THEN
            WRITE(NOUT,'(A)')LINE(1:ICHR)
           ELSE
              ICNOUT=ICNOUT+1
            WRITE(NOUTA,'(A)')LINE(1:ICHR)
           END IF
      ENDIF
C
      IPSCON=IPSCON+1
C
      ILINE=ILINE+1
      ISLINE=ISLINE+1
      LINE=LABEL(1:ICH)
      ICHR=ICH
C
      RETURN
      END
