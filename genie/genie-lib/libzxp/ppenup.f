C
      SUBROUTINE PPENUP(XX,YY)
C
      include 'uniras.inc'
      include 'mappings.inc'
C
      INTEGER IBROK
      REAL X
      REAL XX
      REAL Y
      REAL YY
      INTEGER IX
      INTEGER IXVMAP
      INTEGER IY
      INTEGER IYMAP
      INTEGER IX2
      INTEGER IX1
      INTEGER IX3
      INTEGER IX4
      INTEGER IPV
      INTEGER IPSC
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
C
      SAVE IBROK
      DATA IBROK/0/
C Connect pen up routines
      X=XX
      Y=YY
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
C
      IJOIN=1
      XJOIN(IJOIN)=X
      YJOIN(IJOIN)=Y
      LMOD=.TRUE.
      RETURN
C ******************************************************************
      ENTRY PPENDN(XX,YY)
C Connect pen down routines
      X=XX
      Y=YY
      IJOIN=IJOIN+1
      XJOIN(IJOIN)=X
      YJOIN(IJOIN)=Y
      IF(IJOIN.GE.IJOINM)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
      LMOD=.TRUE.
      RETURN
C ******************************************************************
      ENTRY XFTRAN(XX,YY)
C Find pixel coordinates of point
      CALL XTRANS(XX,YY)
      IX=IXVMAP(XX)
      IY=IYMAP(YY)
      XX=REAL(IX)/XSCRR
      YY=1.0 - REAL(IY)/YSCRB
      RETURN
C ******************************************************************
      ENTRY XQPVOS(XX,YY)
C Find current position of pen
      XX=XJOIN(IJOIN)
      YY=YJOIN(IJOIN)
      RETURN
C ******************************************************************
      ENTRY FULL
C
C     DRAW FULL LINE
C
      LMOD=.TRUE.
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
      DASHNO=0
      IDASH=0
      IF (IBROK.EQ.0) CALL X_SETLINESTYLE(0,0,10,4,10,4)
      RETURN
C*********************************************************************
      ENTRY BROKEN(IX1,IX2,IX3,IX4)
C
C     DRAW DOTTED LINE USING HOME GROWN SUBROUTINE
C
      LMOD=.TRUE.
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
      IF (IBROK.EQ.1) THEN
        IDASH=0
      ELSE IF (IX2.GT.IX1) THEN
      IDASH=1
      ELSE
      IDASH=2
      ENDIF
C
      IF (IBROK.EQ.0) THEN
         IF (IDASH.EQ.0) THEN
            CALL X_SETLINESTYLE(0,0,10,4,10,4)
         ELSE
            CALL X_SETLINESTYLE(0,1,ix1,ix2,ix3,ix4)
         ENDIF
      ENDIF
      RETURN
C
      ENTRY BROKOF
      IBROK=1
      RETURN
C
      ENTRY BROKON
      IBROK=0
      RETURN
      END
