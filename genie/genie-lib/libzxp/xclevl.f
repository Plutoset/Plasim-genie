      SUBROUTINE XCLEVL(Z,MD, M,N,ZZMAX,ZZMIN,ZZINC,CL,NCNT)
C   TO DETERMINE CONTOUR INCRMENT AND CONTOUR VALUES FOR Z(M,N)
C
      INTEGER MD
      INTEGER NCMIN
      INTEGER NMIN
      INTEGER NCMAX
      INTEGER NMAX
      REAL ZINC
      REAL ZZINC
      REAL ZMIN1
      REAL ZMAX1
      INTEGER J
      INTEGER N
      INTEGER I
      INTEGER M
      REAL ZK
      REAL DIFF
      INTEGER NCNT
      REAL ZZMIN
      REAL ZZMAX
      INTEGER KCOUNT
      REAL EPS
      INTEGER KZINC
      REAL CLREF
      REAL ZMIN
      REAL ZMAX
      REAL CLV
      REAL CREF
      INTEGER NMAX1
      INTEGER NMIN1
      INTEGER LCPTN
      INTEGER LABTYP
      INTEGER ICLF
      INTEGER LHILIT
      INTEGER IHLF
      INTEGER KCT0
C
      REAL Z(MD,1 ),CL(*)
      COMMON /XCLM19/ NMIN, NMAX
      COMMON /XCRF17/CLREF,LCPTN,LABTYP,ICLF,LHILIT,IHLF,KCT0
      NCMIN=NMIN
      NCMAX=NMAX
      ZINC=ZZINC
      ZMIN1=Z(1,1)
      ZMAX1=ZMIN1
      DO 2 J=1,N
      DO 2 I=1,M
      ZK=Z(I,J)
      ZMAX1= MAX (ZMAX1,ZK)
      ZMIN1= MIN (ZMIN1,ZK)
 2    CONTINUE
      DIFF=ZMAX1-ZMIN1
      IF(DIFF-ABS( ZINC)*1.0E-6 )999,999,4
 999  PRINT*,' Bad first guess of contour increment or field is constant
     : , number of contours is one.'
      NCNT=1
      CL(1)= ZMIN1
      ZZMIN= ZMIN1
      ZZMAX= ZMAX1
      ZZINC= 0.0
 899  RETURN
 4    KCOUNT=0
 1    CONTINUE
      EPS=0.1*ZINC
      KCOUNT=KCOUNT+1
      IF( KCOUNT.GT.20) GOTO 998
      KZINC=(ZMIN1-CLREF)/ZINC
      ZMIN=KZINC*ZINC+CLREF
      KZINC=(ZMAX1-CLREF)/ZINC
      ZMAX=KZINC*ZINC+CLREF
      IF(ZMIN1-CLREF.GT.0.0) ZMIN=ZMIN+ZINC
      IF(ZMAX1-CLREF.LT.0.0) ZMAX=ZMAX-ZINC
C
      CLV=ZMIN-ZINC
      NCNT=0
 6    CLV=CLV+ZINC
      IF(CLV-ZMAX-EPS) 3,3,8
 3    NCNT=NCNT+1

      IF(NCNT.GT.NCMAX) THEN
      ZINC=ZINC*2
      WRITE(6,1000) NCMAX, ZINC
 1000 FORMAT(' Number of contours > ',I3,' ,Zinc is doubled. Zinc='
     :   ,E10.3)
      GO TO 1
      ENDIF
      IF( ABS( CLV-CLREF ).LT.EPS ) CLV=CLREF
      CL(NCNT)=CLV
      GOTO 6
 8    CONTINUE

      IF( NCNT.LT.NCMIN) THEN
      ZINC=ZINC/2
      WRITE(6,2000) NCMIN,ZINC
 2000 FORMAT(' Number of contours < ',I3,' ,Zinc is halved. Zinc='
     :  ,E10.3)
      GO TO 1
      ENDIF
      WRITE(*,'('' * Number of contours= '',I5,''  MIN='',E12.4,
     : '' MAX='', E12.4,'' INC='',E12.5 )')
     ;    NCNT,ZMIN1,ZMAX1,ZINC
      ZZMAX=ZMAX
      ZZMIN=ZMIN
      ZZINC=ZINC
      RETURN
 998  PRINT*,' Contour levels can not be selected by XCNTLV. please '
      PRINT*,
     :' alter input contour interval or limits of number of contours.'
      RETURN
      ENTRY XCTREF( CREF)
C Set reference contour level. Default is 0.0 .
      CLREF=CREF
      RETURN
      ENTRY XNCTRS(NMIN1, NMAX1)
C Set upper and lower limit of the number of contours
      NMAX=NMAX1
      NMIN=NMIN1
      RETURN
      END
