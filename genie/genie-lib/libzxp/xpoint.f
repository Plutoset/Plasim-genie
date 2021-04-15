      SUBROUTINE XPOINT(X,Y,IFILL)
C Plot a point at position (X,Y) of mathematical space with predefined
C size. ( The size can be defined by XPNTSZ).
C
      REAL X1
      REAL X
      REAL Y1
      REAL Y
      INTEGER II
      REAL RPOINT
      REAL XCIR
      REAL YCIR
      INTEGER I
      INTEGER IFILL
      REAL XP
      REAL YP
      REAL R
C
      COMMON /XCIR25/ XCIR(9) ,YCIR(9) , RPOINT
      REAL XT(25),YT(25)
C
      X1=X
      Y1=Y
      CALL XTRANS(X1,Y1)
C      
      II=1
      XT(II)=X1+RPOINT*XCIR(1)
      YT(II)=Y1+RPOINT*YCIR(1) 
      CALL PPENUP(X1+RPOINT*XCIR(1), Y1+RPOINT*YCIR(1) )
      DO 6 I=3,9,2
      II=II+1
      XT(II)=X1+RPOINT*XCIR(I)
      YT(II)=Y1+RPOINT*YCIR(I) 
 6    CALL PPENDN(X1+RPOINT*XCIR(I), Y1+RPOINT*YCIR(I) )
C
      IF (IFILL.EQ.1) THEN
         II=II+1
         XT(II)=X1+RPOINT*XCIR(1)
         YT(II)=Y1+RPOINT*YCIR(1) 
         CALL ZFILLNN(XT,YT,II,1)
      END IF
C
      RETURN
      ENTRY XPPONT(XP,YP)
      CALL PPENUP(XP+RPOINT*XCIR(1), YP+RPOINT*YCIR(1) )
      DO 5 I=3,9,2
 5    CALL PPENDN(XP+RPOINT*XCIR(I), YP+RPOINT*YCIR(I) )
      RETURN
      ENTRY XPNTSZ(R)
C Define the size of points to be plotted by XPOINT by their radius
C in ND-space.  By default R=0.0005
      RPOINT=R
      RETURN
      ENTRY QXPNTSZ(R)
C Define the size of points to be plotted by XPOINT by their radius
C in ND-space.  By default R=0.0005
      R=RPOINT
      RETURN
      END
