      SUBROUTINE ZSTCT2(NCHNL,NOUT,AZ,AY,Z1,Z2,YL,YR)
C     Pre-compute coastlines in latitude-longitude map space.
C
      REAL PI
      REAL DEGREE
      REAL TW
      INTEGER ILAST
      INTEGER IOF
      INTEGER ION
      INTEGER NCEN
      REAL YR
      REAL YL
      INTEGER NCHNL
      INTEGER ISEG
      REAL X
      REAL Y
      REAL X1
      REAL X11
      REAL AY
      REAL Y1
      REAL AZ
      REAL Z1
      REAL Z2
      INTEGER NOUT
      REAL XO
C
      PI=4.*ATAN(1.0)
      DEGREE=180./PI
      TW=20./DEGREE
      ILAST=-999
      IOF=-1
      ION=1
      NCEN=NINT(0.5*(YR+YL))
      IF (NCEN.NE.180.AND.NCEN.NE.0) THEN
       PRINT*,' *ZSTCT2* LONGITUDE OFFSET NOT SUPPORTED: USE ZCSTLN'
       RETURN
      ENDIF
      ION=1
  200 CONTINUE
      READ (NCHNL,*,END=400) ISEG,X,Y
      X1=X
      IF (X.LT.0.) X1=X+2*PI
      X11=AY*(X1*DEGREE-YL)/(YR-YL)
      Y1=AZ*(Y*DEGREE-Z1)/(Z2-Z1)
      IF (X11.LT.0.OR.Y1.LT.0.OR.X11.GT.AY.OR.Y1.GT.AZ) THEN
       ILAST=-999
      ELSE
       IF (NCEN.EQ.180) THEN
        IF (ISEG.NE.ILAST) THEN
           WRITE (NOUT) IOF,X11,Y1
           ILAST=ISEG
           XO=X1
        ELSEIF (ABS(X1-XO).GT.(2.*PI-TW)) THEN
           WRITE (NOUT) IOF,X11,Y1
           XO=X1
        ELSE
           WRITE (NOUT) ION,X11,Y1
           XO=X1
        ENDIF
       ELSE
        IF (ISEG.NE.ILAST) THEN
           WRITE (NOUT) IOF,X11,Y1
           ILAST=ISEG
        ELSE
           WRITE (NOUT) ION,X11,Y1
        ENDIF
       ENDIF
      ENDIF
      GOTO 200
  400 CONTINUE
      REWIND NCHNL
      REWIND NOUT
      RETURN
      END
