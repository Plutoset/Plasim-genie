      SUBROUTINE XBPSC(NCHAN,FNAME)
C
C     Begin postscript file, overwrites existing filename
C
C
      include 'vpsc.inc'
      include 'colours.inc'
C
      CHARACTER FNAME*(*)
      LOGICAL LOPEN,LEXIST
C
      INTEGER IPSC
      INTEGER NCHAN
      INTEGER ILENFN
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      IMACH=0
      IPSCON=0
      IPSCRM=0
      IPSCLT=-1
      IPSCCL=-100
      IPSTHI=1
      IFLPS=0
      NOUTA=0
      ICNOUT=0
      ICPSBKG=0
      CHARCL='sbk'
C
      IF (IPSC.EQ.0) THEN
       INQUIRE(UNIT=NCHAN,OPENED=LOPEN)
       IF (LOPEN)THEN
          return
       ENDIF
      ENDIF
C
      IF(IPSC.EQ.1)THEN
      IF(IMACH.NE.1)THEN
c        PRINT*,' Postscript output already turned on '
        RETURN
      ENDIF
      ELSE
      IPSC=1
      ILENFN=LEN(FNAME)
      FNAM=FNAME
      ENDIF
C
      LINE1=' b  0  0  0  0 '//CHARCL
      LNCOL='  1 t'
C
      NOUT=NCHAN
      IPAGE=1
      ILINE=1
      ICHR=0
      IXB=2784/0.954
      IYB=2325
      ISLINE=1
C
      INQUIRE(FILE=FNAM(1:ILENFN),EXIST=LEXIST)
      IF (LEXIST) THEN
         OPEN(UNIT=NOUT,FILE=FNAM(1:ILENFN),
     :        FORM='FORMATTED',STATUS='UNKNOWN')
         CLOSE(UNIT=NOUT,STATUS='DELETE')
      END IF
C
      OPEN(UNIT=NOUT,FILE=FNAM(1:ILENFN),
     :     FORM='FORMATTED',STATUS='UNKNOWN')
      WRITE(NOUT,'(A)')'%!PS-Adobe-2.0 EPSF-2.0'
      WRITE(NOUT,'(A)')'%%BoundingBox: -100 45 700 712'
      WRITE(NOUT,'(A)')'%%EndComments'
      WRITE(NOUT,'(A,A)')'/SavedState save def 39 dict begin ',
     :'/st 1 string def'
      WRITE(NOUT,'(A,A)')'/bd {bind def} bind def '
      WRITE(NOUT,'(A)')'/fn1 /Times-Roman findfont def'
      WRITE(NOUT,'(A,A)')'/fn2 /Times-Bold findfont def',
     :                 ' /fn3 /Helvetica findfont def'
      WRITE(NOUT,'(A)')
     : '/sbk {0 0 0 setrgbcolor} bd /sgr {0 1 0 setrgbcolor} bd'
      WRITE(NOUT,'(A)')
     : '/sbl {0 0 1 setrgbcolor} bd /srd {1 0 0 setrgbcolor} bd'
      WRITE(NOUT,'(A)')
     : '/swh {1 1 1 setrgbcolor} bd /scy {0 1 1 setrgbcolor} bd'
      WRITE(NOUT,'(A,A)')
     : '/sg1 {0.25 0.25 0.25 setrgbcolor} bd',
     : '/sg2 {0.5 0.5 0.5 setrgbcolor} bd'
      WRITE(NOUT,'(A)')
     : '/sg3 {0.75 0.75 0.75 setrgbcolor} bd ',
     : '/sa1 {1 1 0.5 setrgbcolor} bd'
      WRITE(NOUT,'(A)')
     : '/sa2 {1 0.5 0.5 setrgbcolor} bd /sa3 {0.5 1 1 setrgbcolor} bd'
      WRITE(NOUT,'(A)')
     : '/sa4 {1 0.5 1 setrgbcolor} bd /sa5 {0.5 0.5 1 setrgbcolor} bd'
      WRITE(NOUT,'(A)')
     : '/sa6 {0.5 1 0.5 setrgbcolor} bd'
      WRITE(NOUT,'(A)')'/r {1 1 sethsbcolor fill 0 0 0 setrgbcolor} def'
      WRITE(NOUT,'(A)')'/q {setrgbcolor fill 0 0 0 setrgbcolor} def'
      WRITE(NOUT,'(A,A)')'/T {/marked true def}bd ',
     :'/F {/marked false def}bd'
      WRITE(NOUT,'(A,A)')'/t {setlinewidth}bd /n {newpath}bd ',
     :'/gs {gsave}bd /gr {grestore}bd'
      WRITE(NOUT,'(A,A)')'/i {1 setlinecap 1 setlinejoin ',
     :'0.24 0.24 scale 2404 215'
      WRITE(NOUT,'(A,A)')'    translate 90 rotate 0 F}bd ',
     :'/S {/sv save def i}bd'
      WRITE(NOUT,'(A,A)')'/p {marked {showpage} if sv restore S}bd'
      WRITE(NOUT,'(A,A)')'/m {moveto}bd /L {rlineto}bd'
      WRITE(NOUT,'(A,A)')'/g {currentfile token ',
     :'pop 2.329 mul dup 0 eq {pop} if}bd'
      WRITE(NOUT,'(A)')'/b {[g g g g] 0 setdash}bd'
      WRITE(NOUT,'(A,A)')'/s {currentpoint stroke T m}bd'
      WRITE(NOUT,'(A,A)')'/x {p end SavedState restore}bd S'
      WRITE(NOUT,'(A)')'/c {closepath} def'
      WRITE(NOUT,'(A)')'%%EndProlog'
      WRITE(NOUT,'(A)')'%%Page: 1 1'
C
      RETURN
      END
