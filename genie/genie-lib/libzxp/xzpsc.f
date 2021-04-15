C
      SUBROUTINE XZPSC(X,Y,NPO)
C
C     Produces postscript output of filled area with colour
C
      include 'mappings.inc'
      include 'vpsc.inc'
      include 'colours.inc'
C
      REAL X(*),Y(*)
      INTEGER IX(IJOINM1),IY(IJOINM1)
      CHARACTER LABEL*20
C
      INTEGER IPSC
      INTEGER NP
      INTEGER NPO
      INTEGER I
      INTEGER IFAIL
      INTEGER ICH
      REAL COLPS1
      REAL COLPS2
      REAL COLPS3
      INTEGER III
      INTEGER II
      INTEGER IMASKCOL
      REAL R
      REAL G
      REAL B
      INTEGER IPV,IPSCTMP,ICOLTX,IPSCEXP
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      SAVE COLPS1,COLPS2,COLPS3
C
      IF (IPSC.EQ.0) RETURN
C
      NP=IABS(NPO)
C
      IF (NP.LT.3.OR.NP.GT.IJOINM1) THEN
         PRINT*,' XZPSC : NP.NE.3 : NP = ',NP
         STOP
      END IF
C
      IF (NPO.GT.0) THEN
         DO 10 I=1,NP
         IX(I)=NINT((X(I)-XL)*XR*IXB)+50
         IY(I)=NINT((Y(I)-YB)*YT*IYB)
 10      CONTINUE
      ELSE
         DO 12 I=1,NP
         IX(I)=NINT(X(I))
         IY(I)=NINT(Y(I))
 12      CONTINUE
      ENDIF
C
      CALL XBUFPS(IFAIL)
      ICHR=0
C
      CALL XPREPN(IX(1),IY(1),LABEL,ICHR)
      LINE=LABEL(1:ICHR)//' m'
      ICHR=ICHR+2
C
      DO 40 I=2,NP
      CALL XPREPN(IX(I)-IX(I-1),IY(I)-IY(I-1),LABEL,ICH)
      LABEL=LABEL(1:ICH)//' L'
      ICH=ICH+2
      IF(ICHR+ICH.LT.76)THEN
        IF (ICHR.EQ.0) THEN
           LINE=LABEL(1:ICH)
        ELSE
           LINE=LINE(1:ICHR)//LABEL(1:ICH)
        ENDIF
        ICHR=ICHR+ICH
      ELSE
        WRITE(NOUT,'(A)')LINE(1:ICHR)
        LINE=LABEL(1:ICH)
        IPSCON=IPSCON+1
        ILINE=ILINE+1
        ICHR=ICH
      ENDIF
 40   CONTINUE
C
      IF (ICHR.GE.62) THEN
        WRITE(NOUT,'(A)')LINE(1:ICHR)
        LINE=LABEL(1:ICH)
        IPSCON=IPSCON+1
        ILINE=ILINE+1
        ICHR=0
      ENDIF
C
      WRITE(LINE(ICHR+1:ICHR+19),'(A3,F4.2,A1,F4.2,A1,F4.2,A2)')
     :         ' c ',COLPS1,' ',COLPS2,' ',COLPS3,' q'
      CALL IFILZERO(LINE(ICHR+1:ICHR+19))
      ICHR=ICHR+19
C
      WRITE(NOUT,'(A)')LINE(1:ICHR)
      IPSCON=IPSCON+1
      ILINE=ILINE+1
      ICHR=0
C
      RETURN
C
      ENTRY XZCCOL(III)
C
C     This routine sets the colour to be used in the post-script file
C     If III = -1   then set to background color
C     If III = -2   then set to foreground color
C
      IF (III.LE.-10) THEN
         II=III+512
      ELSE IF (III.LE.0) THEN
         II=0
      ELSE
         II=ICV(III)
      END IF
C
      IF (III.LE.-10) THEN
         COLPS1=COLARRAY(1,IABS(II))/256.0
         COLPS2=COLARRAY(2,IABS(II))/256.0
         COLPS3=COLARRAY(3,IABS(II))/256.0
      ELSE IF (III.LE.-5) THEN
         COLPS1=COLARRAY(1,IABS(III))/256.0
         COLPS2=COLARRAY(2,IABS(III))/256.0
         COLPS3=COLARRAY(3,IABS(III))/256.0
      ELSE IF (III.EQ.-1) THEN
         COLPS1=1.00
         COLPS2=1.00
         COLPS3=1.00
      ELSE IF (III.EQ.-2) THEN
         COLPS1=0.00
         COLPS2=0.00
         COLPS3=0.00
      ELSE IF (III.EQ.-3) THEN
         IF (IMASKCOL.NE.-1) THEN
            COLPS1=COLARRAY(1,IABS(IMASKCOL))/256.0
            COLPS2=COLARRAY(2,IABS(IMASKCOL))/256.0
            COLPS3=COLARRAY(3,IABS(IMASKCOL))/256.0
         else
            COLPS1=1.00
            COLPS2=1.00
            COLPS3=1.00
         end if
      ELSE
         COLPS1=COLARRAY(1,II)/256.0
         COLPS2=COLARRAY(2,II)/256.0
         COLPS3=COLARRAY(3,II)/256.0
      END IF
c
      IF (COLPS1.LE.0.0) COLPS1=0.00
      IF (COLPS1.GT.1.0) COLPS1=1.00
      IF (COLPS2.LE.0.0) COLPS2=0.00
      IF (COLPS2.GT.1.0) COLPS2=1.00
      IF (COLPS3.LE.0.0) COLPS3=0.00
      IF (COLPS3.GT.1.0) COLPS3=1.00
      RETURN
C
      ENTRY XZCRGB(R,G,B)
C
C     User has complete control on colours using RGB scale
C
      COLPS1=R
      COLPS2=G
      COLPS3=B
      IF (COLPS1.LE.0.0) COLPS1=0.00
      IF (COLPS1.GT.1.0) COLPS1=1.00
      IF (COLPS2.LE.0.0) COLPS2=0.00
      IF (COLPS2.GT.1.0) COLPS2=1.00
      IF (COLPS3.LE.0.0) COLPS3=0.00
      IF (COLPS3.GT.1.0) COLPS3=1.00
      RETURN
C
      END
