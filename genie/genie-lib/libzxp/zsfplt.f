C* SURFACE VIEWING ROUTINES
      SUBROUTINE ZSFPLT(SURFAS,MD,M,N,WORK)
C      isometric surface viewing
C
      INTEGER MD,IQ,MODE,IQUAD,M1,N,M,N1,I,II,JJ,J
      INTEGER IPLOT,II1,JJ1,MODES,IQS
      REAL    ANGISM,SCALE,XX0,YY0,XRANGE,EPS
      REAL    DX,DY,HS,X,Y,Y0,PMAX1,PMAX2,PMIN1,PMIN2
      REAL    Y2,Y1,SCALES,ANGS,RANGEX,X0S,Y0S
C
      REAL SURFAS(MD,*),WORK(2,*)
      SAVE IQ,ANGISM,SCALE,XRANGE,XX0,YY0,MODE
      DATA IQ/1/,ANGISM/1.043862/,SCALE/1./,XX0,YY0,XRANGE/2*0.,2./
     :    ,MODE/0/
C        MD: the first dimension of array to be viewed
C         M: array dimension of x direction
C         N: array dimension of y direction
C      WORK: working space of total dimension at least 2*max(M,N)
C
       IQUAD=IQ
       EPS=1.E-4
      M1=(MOD(IQUAD,4)/2)*(M-1)+1
      N1=((IQUAD-1)/2)*(N-1)   +1
      DX=XRANGE/(M+N)
      DY=DX/TAN(ANGISM)
      IF( MOD(IQUAD,2).EQ.0) DX=-ABS(DX)
      HS=   SCALE
C     plot parallel along x direction
C     first line
C     loop 101,102 draw line in x-direction
      IF(MODE.EQ.2) GOTO 301
      DO 101 I=1,M
      X=(I-1)*DX       +XX0
      Y=(I-1)*DY       +YY0
      II=ABS(I-M1)+1
      JJ=ABS(1-N1)+1
      Y=Y+SURFAS(II,JJ)*HS
      WORK(1,I)=           Y
      WORK(2,I)=           Y
      IF(I.EQ.1) THEN
        CALL XPENUP(X,Y)
      ELSE
        CALL XPENDN(X,Y)
      ENDIF
  101 CONTINUE
      DO 102 J=2,N
C     loop 103 : upper surface
      DO 103 I=M,2,-1
      X= (I-1)*DX-(J-1)*DX    +XX0
      Y0= (I-1)*DY+(J-1)*DY   +YY0
      II=ABS(I-M1)+1
      JJ=ABS(J-N1)+1
      Y=Y0+SURFAS(II,JJ  )*HS
      PMAX1=WORK(1, I)
      PMAX2=WORK(1, I-1)
      IF(Y.GE.WORK(1,I-1)) THEN
          IPLOT=1
          WORK(1,I)=Y
          IF(I.EQ.M) THEN
            CALL ZSF002(X,Y,IPLOT)
          ELSE
            II1=ABS(I  -M1)+1
            JJ1=ABS(J-1-N1)+1
            Y2=Y0+SURFAS(II1,JJ1)*HS-DY
            IF(ABS(PMAX1-Y2).LT.EPS) Y1=Y2
            CALL ZSF001(X,Y,IPLOT,PMAX2,PMAX1,Y1,DX)
          ENDIF
      ELSE
          IPLOT=0
          IF(I.EQ.M) THEN
            CALL ZSF002(X,Y,0)
          ELSE
            CALL ZSF001(X,Y,IPLOT,PMAX2,PMAX1,Y1,DX)
          ENDIF
          WORK(1,I)=WORK(1,I-1)
      ENDIF
      Y1=Y
  103 CONTINUE
      X=         -(J-1)*DX    +XX0
      Y=         +(J-1)*DY    +YY0
      II=ABS(1-M1)+1
      JJ=ABS(J-N1)+1
      Y=Y+SURFAS(II,JJ  )*HS
      WORK(1,1)=Y
      CALL ZSF003(X,Y)

C     loop 104 : Lower surface
      DO 104 I=M,2,-1
      X= (I-1)*DX-(J-1)*DX    +XX0
      Y0=(I-1)*DY+(J-1)*DY    +YY0
      II=ABS(I-M1)+1
      JJ=ABS(J-N1)+1
      Y=Y0+SURFAS(II,JJ  )*HS
      PMIN1=WORK(2, I)
      PMIN2=WORK(2, I-1)
      IF(Y.LE.WORK(2,I-1)) THEN
          IPLOT=1
          WORK(2,I)=Y
          IF(I.EQ.M) THEN
            CALL ZSF002(X,Y,IPLOT)
          ELSE
            II1=ABS(I  -M1)+1
            JJ1=ABS(J-1-N1)+1
            Y2=Y0+SURFAS(II1,JJ1)*HS-DY
            IF(ABS(PMIN1-Y2).LT.EPS) Y1=Y2
            CALL ZSF001(X,Y,IPLOT,PMIN2,PMIN1,Y1,DX)
          ENDIF
      ELSE
          IPLOT=0
          IF(I.EQ.M) THEN
            CALL ZSF002(X,Y,IPLOT)
          ELSE
            CALL ZSF001(X,Y,IPLOT,PMIN2,PMIN1,Y1,DX)
          ENDIF
          WORK(2,I)=WORK(2,I-1)
      ENDIF
      Y1=Y
  104 CONTINUE
      X=         -(J-1)*DX    +XX0
      Y=         +(J-1)*DY    +YY0
      II=ABS(1-M1)+1
      JJ=ABS(J-N1)+1
      Y=Y+SURFAS(II,JJ  )*HS
      WORK(2,1)=Y
      CALL ZSF003(X,Y)
  102 CONTINUE

C     loop 201,202 draw line in y-direction
  301 IF(MODE.EQ.1) RETURN
      DO 201 J=1,N
      X=-(J-1)*DX         +XX0
      Y= (J-1)*DY         +YY0
      II=ABS(1-M1)+1
      JJ=ABS(J-N1)+1
      Y=Y+SURFAS(II,JJ)*HS
      WORK(1,J)=           Y
      WORK(2,J)=           Y
      IF(J.EQ.1) THEN
        CALL XPENUP(X,Y)
      ELSE
        CALL XPENDN(X,Y)
      ENDIF
  201 CONTINUE

      DO 202 I=2,M
C     loop 203 : upper surface
      DO 203 J=N,2,-1
      X= (I-1)*DX-(J-1)*DX     +XX0
      Y0= (I-1)*DY+(J-1)*DY    +YY0
      II=ABS(I-M1)+1
      JJ=ABS(J-N1)+1
      Y=Y0+SURFAS(II,JJ  )*HS
      PMAX1=WORK(1, J)
      PMAX2=WORK(1, J-1)
      IF(Y.GE.WORK(1,J-1)) THEN
          IPLOT=1
          WORK(1,J)=Y
          IF(J.EQ.N) THEN
            CALL ZSF002(X,Y,IPLOT)
          ELSE
            II1=ABS(I-1-M1)+1
            JJ1=ABS(J  -N1)+1
            Y2=Y0+SURFAS(II1,JJ1)*HS-DY
            IF(ABS(PMAX1-Y2).LT.EPS) Y1=Y2
            CALL ZSF001(X,Y,IPLOT,PMAX2,PMAX1,Y1,-DX)
          ENDIF
      ELSE
          IPLOT=0
          IF(J.EQ.N) THEN
            CALL ZSF002(X,Y,0)
          ELSE
            CALL ZSF001(X,Y,IPLOT,PMAX2,PMAX1,Y1,-DX)
          ENDIF
          WORK(1,J)=WORK(1,J-1)
      ENDIF
      Y1=Y
  203 CONTINUE
      X=(I-1)*DX        +XX0
      Y=(I-1)*DY        +YY0
      II=ABS(I-M1)+1
      JJ=ABS(1-N1)+1
      Y=Y+SURFAS(II,JJ  )*HS
      WORK(1,1)=Y
      CALL ZSF003(X,Y)
C     loop 204 : lower surface
      DO 204 J=N,2,-1
      X= (I-1)*DX-(J-1)*DX       +XX0
      Y0= (I-1)*DY+(J-1)*DY      +YY0
      II=ABS(I-M1)+1
      JJ=ABS(J-N1)+1
      Y=Y0+SURFAS(II,JJ  )*HS
      PMIN1=WORK(2, J)
      PMIN2=WORK(2, J-1)
      IF(Y.LE.WORK(2,J-1)) THEN
          IPLOT=1
          WORK(2,J)=Y
          IF(J.EQ.N) THEN
            CALL ZSF002(X,Y,IPLOT)
          ELSE
            II1=ABS(I-1-M1)+1
            JJ1=ABS(J  -N1)+1
            Y2=Y0+SURFAS(II1,JJ1)*HS-DY
            IF(ABS(PMIN1-Y2).LT.EPS) Y1=Y2
            CALL ZSF001(X,Y,IPLOT,PMIN2,PMIN1,Y1,-DX)
          ENDIF
      ELSE
          IPLOT=0
          IF(J.EQ.N) THEN
            CALL ZSF002(X,Y,IPLOT)
          ELSE
            CALL ZSF001(X,Y,IPLOT,PMIN2,PMIN1,Y1,-DX)
          ENDIF
          WORK(2,J)=WORK(2,J-1)
      ENDIF
      Y1=Y
  204 CONTINUE
      X=(I-1)*DX        +XX0
      Y=(I-1)*DY        +YY0
      II=ABS(I-M1)+1
      JJ=ABS(1-N1)+1
      Y=Y+SURFAS(II,JJ  )*HS
      WORK(2,1)=Y
      CALL ZSF003(X,Y)
  202 CONTINUE
      RETURN
      ENTRY     ZSFSTL(MODES)
C  MODE=0 draw surface lines along both axes directions (default).
C  MODE=1 draw surface lines along the x-direction.
C  MODE=2 draw surface lines along the y-direction.
      MODE=MODES
      RETURN
      ENTRY     ZSFVEW(IQS)
C  Define the corner through which the surface is viewed
C     IQ=1,2,3 & 4
      IQ=IQS
      RETURN
      ENTRY     ZSFSCL(SCALES)
C  Set the scaler which scales the surplot data
C  Decreasing SCALE results in decreasing in size of the plot
      SCALE=SCALES
      RETURN
      ENTRY     ZSFANG(ANGS)
C  Set the isometric angle of the surface viewing
      ANGISM=4.*ATAN(1.)*ANGS/180.
      RETURN
      ENTRY     ZSFLOC(X0S,Y0S,RANGEX)
C     left to right range in plotting space
C     (X0S,Y0S) : the position of reference point
C     i.e. the closest grid point to the viewer
      XRANGE=RANGEX
      XX0=X0S
      YY0=Y0S
      RETURN
      END
