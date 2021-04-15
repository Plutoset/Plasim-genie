C
      SUBROUTINE DPORT(ZX1,ZX2,ZY1,ZY2,ZIX,ZIY)
      include 'mappings.inc'
      include 'vpsc.inc'
C     
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      include 'colours.inc'
C
C
      REAL ZX1
      REAL ZX2
      REAL Z1X1
      REAL Z1X2
      REAL Z1Y1
      REAL Z1Y2
      REAL X1
      REAL Y1
      REAL ZY1
      REAL X2
      REAL Y2
      REAL ZY2
      INTEGER IXVMAP
      INTEGER IYMAP
      INTEGER IPSC
      INTEGER J1
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      INTEGER ZIX,ZIY,ZIX1,ZIX2,ZIY1,ZIY2
C
1000  FORMAT(6I4)
C
      IF(ZX1.EQ.ZX2.AND.ZX1.EQ.0)THEN
      IXRASO=0
      IYRASO=0
        Z1X1=0
        Z1X2=0
        Z1Y1=0
        Z1Y2=0
        ZIY=0
      ELSE
      X1=ZX1
      Y1=ZY1
      CALL XTRANS(X1,Y1)
      X2=ZX2
      Y2=ZY2
      CALL XTRANS(X2,Y2)
      ZIX1=IXVMAP(X1)
      ZIX2=IXVMAP(X2)
      ZIY1=IYMAP(Y1)
      ZIY2=IYMAP(Y2)
      ZIX=ZIX2-ZIX1
      IF(ZIX.LT.0)ZIX=-ZIX
      ZIY=ZIY2-ZIY1
      IF(ZIY.LT.0)ZIY=-ZIY
      ZIX=ZIX+1
      ZIY=ZIY+1
      IXRASO=ZIX1
      IYRASO=ZIY1
      IF(ZIY2.GT.ZIY1)THEN
        IOFRAS=1
      ELSE
        IOFRAS=-1
      ENDIF
        IF (IPSC.EQ.1) THEN
           IRASX1=NINT((X1-XL)*XR*IXB) + 50
           IRASX2=NINT((X2-XL)*XR*IXB) + 50
           IRASY1=NINT((Y1-YB)*YT*IYB)
           IRASY2=NINT((Y2-YB)*YT*IYB)
        END IF
      ENDIF
C
      IF(LSAVE.AND.ISAVE.GT.0)THEN
      J1=-3
      WRITE(NCHAN,1000)J1,ZIX1,ZIX2,ZIY1,ZIY2,ZIY
      ENDIF
C
      RETURN
      END
