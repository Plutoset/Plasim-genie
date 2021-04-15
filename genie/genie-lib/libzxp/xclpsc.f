C
      SUBROUTINE XCLPSC(LINTHI,LINCOL)
C
C   Change line intensity by making it think it is coloured!
C
C
      include 'colours.inc'
      include 'vpsc.inc'
C
      INTEGER IPSC
      INTEGER LINTHI
      INTEGER LINCOL
      INTEGER IL
      REAL COLPS1
      REAL COLPS2
      REAL COLPS3
      INTEGER ITSIZE
      INTEGER ITCOL
      INTEGER IFONT
      REAL X1
      REAL XO
      REAL Y1
      REAL YO
      INTEGER IX
      INTEGER IY
      INTEGER IROTT
      INTEGER IROT
      REAL ANG
      INTEGER ITA
      INTEGER ITB
      INTEGER ILEN
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      CHARACTER CHAR*2,TEMPLINE*30,STRING*(*),CHAR6A*6,CHAR6B*6,
     :          CHAR3*3,CHAR3A*3
C
      IF (IPSC.EQ.0) RETURN
C
      IF (LINTHI.EQ.IPSTHI.AND.LINCOL.EQ.IPSCCL) RETURN
C
      IF (LINTHI.NE.IPSTHI) THEN
         IL=LINTHI
         IF(IL.EQ.0)IL=1
         WRITE(CHAR,'(I2)')IL
         LNCOL=' '//CHAR//' t'
      END IF
      IPSTHI=LINTHI
C
      IF (LINCOL.NE.IPSCCL.AND.IPSCCL.NE.-100) THEN
         IF (LINCOL.GE.0) THEN
            CALL ZPSCOL(LINCOL,CHARCL)
            IF (ICHR.GT.0)THEN
               IF (ICHR+6.LT.76)THEN
                  LINE=LINE(1:ICHR)//' s '//CHARCL
                  ISLINE=1
                ICHR=ICHR+6
               ELSE
                  IF (NOUTA.EQ.0) THEN
                   WRITE(NOUT,'(A)')LINE(1:ICHR)
                  ELSE
                     ICNOUT=ICNOUT+1
                   WRITE(NOUTA,'(A)')LINE(1:ICHR)
                  END IF
                     IPSCON=IPSCON+1
                    ILINE=ILINE+1
                   ISLINE=1
                  CALL ZPSCOL(LINCOL,CHARCL)
                  LINE=' s '//CHARCL
                ICHR=6
               END IF
            ELSE 
               LINE1(16:18)=CHARCL
            END IF
         ELSE 
            COLPS1=COLARRAY(1,LINCOL)/256.0
            COLPS2=COLARRAY(2,LINCOL)/256.0
            COLPS3=COLARRAY(3,LINCOL)/256.0
            WRITE(TEMPLINE,1000)COLPS1,COLPS2,COLPS3
            CALL IFILZERO(TEMPLINE)
            IF (ICHR.GT.0.AND.ICHR+30.GE.76) THEN
               IF (NOUTA.EQ.0) THEN
                WRITE(NOUT,'(A)')LINE(1:ICHR)
               ELSE
                  ICNOUT=ICNOUT+1
                WRITE(NOUTA,'(A)')LINE(1:ICHR)
               END IF
               IPSCON=IPSCON+1
               ILINE=ILINE+1
             ISLINE=1
             LINE=TEMPLINE
               ICHR=30
            ELSE
               LINE=LINE(1:ICHR)//TEMPLINE
               ICHR=ICHR + 30 
            END IF
         END IF
      END IF
      IPSCCL=LINCOL
 1000 FORMAT(1X,' s ',F4.2,1X,F4.2,1X,F4.2,' setrgbcolor')
C
      IF (ICHR.GT.0) THEN
         IF (LINE(ICHR:ICHR).EQ.'t') THEN
            LINE(ICHR-4:ICHR)=LNCOL
            LINE(ICHR+1:ICHR+4)=' '//CHARCL
            ICHR=ICHR+4
         ELSE
            IF (ICHR+11.LT.76) THEN
             LINE=LINE(1:ICHR)//' s'//LNCOL//' '//CHARCL
             ISLINE=1
                ICHR=ICHR+11
            ELSE
               IF (NOUTA.EQ.0) THEN
                WRITE(NOUT,'(A)')LINE(1:ICHR)
               ELSE
                  ICNOUT=ICNOUT+1
                WRITE(NOUTA,'(A)')LINE(1:ICHR)
               END IF
             IPSCON=IPSCON+1
                  ILINE=ILINE+1
             ISLINE=1
             LINE=' s'//LNCOL//' '//CHARCL
             ICHR=11
            END IF
         END IF
      ELSE 
          LINE1(16:18)=CHARCL
      ENDIF
c
      RETURN
C
      ENTRY XPSTITL(XO,YO,STRING,ITSIZE,ITCOL,IROT,IFONT)
C
      IF (ICHR.GT.0.AND.
     :   (ISLINE.GT.1.OR.ICHR.GT.ICHAR1) )THEN
         if (line(ichr:ichr).ne.'s') THEN
            line(1:ichr+6)=line(1:ichr)//' s '//CHARCL
            ichr=ichr+6
         end if
         IF (NOUTA.EQ.0) THEN
            WRITE(NOUT,'(A)')LINE(1:ICHR)
         ELSE
            ICNOUT=ICNOUT+1
            WRITE(NOUTA,'(A)')LINE(1:ICHR)
         END IF
         IPSCON=IPSCON+1
           ILINE=ILINE+1
         ISLINE=1
         ICHR=0
      END IF
C
      ITSIZE=IABS(ITSIZE)
      IF (ITSIZE.GT.9999) ITSIZE=9999
      WRITE(CHAR6A,'(I6)')ITSIZE
C
      CALL ZPSCOL(ITCOL,CHAR6B(2:4))
      CHAR6B(1:1)=' '
C
      IF (NOUTA.EQ.0) THEN
         WRITE(NOUT,'(A,I1,A)')'fn',IFONT,
     :              char6a//' scalefont setfont'
     :                         //char6b(1:4)
      ELSE
         ICNOUT=ICNOUT+1
         WRITE(NOUTA,'(A,I1,A)')'fn',IFONT,
     :              char6a//' scalefont setfont'
     :                         //char6b(1:4)
      END IF
C
      IPSCON=IPSCON+1
      ILINE=ILINE+1
C       
      X1=XO
      Y1=YO
      IF (IFONT.EQ.3) Y1=Y1+0.01
      CALL XCTRAN(X1,Y1)
      IX=NINT((X1-XL)*XR*IXB)+50
      IY=NINT((Y1-YB)*YT*IYB)
C
      IROTT=IROT
      CHAR3A='-90'
C
      CALL XQCHOR(ANG)
      IF (ABS(ANG).GT.45.0) THEN
         IX=IX+1381
         IY=IY+981
         IF (IROTT.EQ.1) THEN
            IROTT=2
            CHAR3A='+90'
         END IF 
      END IF
C
      CALL XLAB(IX,CHAR6A,ITA)
      CALL XLAB(IY,CHAR6B,ITB)
      IF (ITA.GT.6.OR.ITB.GT.6) THEN
         PRINT*,' In xpstitl : ITA,ITB error ',ITA,ITB
         print*,' In xpstit1 : ',ix,iy,':',char6a,':',char6b
         STOP
      END IF
C
      CALL ZPSCOL(IPSCCL,CHAR3)
C
      ILEN=LEN(STRING)
      IF (NOUTA.EQ.0) THEN
         IF (IROTT.NE.2) THEN
            WRITE(NOUT,'(A)')CHAR6A(1:ITA)//' '//CHAR6B(1:ITB)//' m (',
     *                    STRING(1:ILEN),
     *                    ') show '//CHAR3
         ELSE
            WRITE(NOUT,'(A)')CHAR6A(1:ITA)//' '//CHAR6B(1:ITB)//' m ',
     *                    'gsave ',CHAR6A(1:ITA)//' '//CHAR6B(1:ITB),
     *                    ' translate '//CHAR3A//' rotate 0 0 m (',
     *                    STRING(1:ILEN),
     *                    ') show grestore '//CHAR3
         END IF
      ELSE
         IF (IROTT.NE.2) THEN
            ICNOUT=ICNOUT+3
            WRITE(NOUTA,'(A)')CHAR6A(1:ITA)//' '//CHAR6B(1:ITB)//' m (',
     *                    STRING(1:ILEN),
     *                    ') show '//CHAR3
         ELSE
            ICNOUT=ICNOUT+6
            WRITE(NOUTA,'(A)')CHAR6A(1:ITA)//' '//CHAR6B(1:ITB)//' m ',
     *                    'gsave ',CHAR6A(1:ITA)//' '//CHAR6B(1:ITB),
     *                    ' translate '//CHAR3A//' rotate 0 0 m (',
     *                    STRING(1:ILEN),
     *                    ') show grestore '//CHAR3
         END IF
      END IF
      IPSCON=IPSCON+1
      ILINE=ILINE+1
C      
      RETURN
      END
