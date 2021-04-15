C
      SUBROUTINE XUDRAW(XJOIN,YJOIN,IJOIN,RWIDTH,UNICOL,DASHNO)
      REAL XJOIN(*),YJOIN(*)
      INCLUDE 'f77pc.inc'
      include 'mappings.inc'
C
      INTEGER UNICOL
      INTEGER DASHNO
C
      INTEGER ICOPY
      INTEGER IDISOFF
      INTEGER IJOIN
      REAL XTEMP
      REAL YTEMP
      INTEGER IPSC
      INTEGER I
      INTEGER IXVMAP
      INTEGER IYMAP
      REAL XVMAP
      REAL YMAP
      INTEGER IJ
      REAL RWIDTH
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      save icopy,idisoff,xtemp,ytemp
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      data icopy/0/,idisoff/0/
C
C     to avoid unused warning
      RWIDTH = RWIDTH
      DASHNO = DASHNO
C
      if (icopy.eq.1) then
         call zsavln(xjoin,yjoin,ijoin)
         XJOIN(1)=XTEMP
         YJOIN(1)=YTEMP
         IJOIN=1
         return
      end if
      IF(IPSC.EQ.1)THEN
      CALL XPDPSC(XJOIN(1),YJOIN(1))
      DO 100 I=2,IJOIN
      CALL XPOPSC(XJOIN(I),YJOIN(I))
100     CONTINUE
      ENDIF

      XTEMP=XJOIN(IJOIN)
      YTEMP=YJOIN(IJOIN)
      CALL VIDLIN(xjoin,yjoin,ijoin)

      IF(LSAVE.AND.ISAVE.EQ.1)THEN
       DO 400 I=1,IJOIN
       IXJ(I)=IXVMAP(XJOIN(I))
       IYJ(I)=IYMAP(YJOIN(I))
400      CONTINUE
      ELSEIF(LSAVE.AND.ISAVE.EQ.2)THEN
       DO 500 I=1,IJOIN
       XJ(I)=XVMAP(XJOIN(I))
       YJ(I)=YMAP(YJOIN(I))
500      CONTINUE
      ENDIF
      IJ=IJOIN
      CALL SSSOOO(2,IJ)
C
      DO 200 I=1,IJOIN
       IXJ(I)=IXVMAP(XJOIN(I))
       IYJ(I)=IYMAP(YJOIN(I))
200   CONTINUE
C
      if (idisoff.eq.0)
     :   CALL X_POLYLINE(IJOIN,IXJ,IYJ,UNICOL)
C
      XJOIN(1)=XTEMP
      YJOIN(1)=YTEMP
      IJOIN=1
      RETURN
C
      ENTRY ZRESLN
         icopy=1-icopy
      RETURN
C
      ENTRY XDISOFF
         IDISOFF=1
      RETURN
C
      ENTRY XDISON
         IDISOFF=0
      RETURN
C
      END
