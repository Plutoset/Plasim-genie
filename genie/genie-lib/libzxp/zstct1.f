      SUBROUTINE ZSTCT1(NCHNL,NOUT,IHEM,AR,DOFF)
C     Pre-compute coastlines in polar stereographic map space.
C
      REAL PI
      REAL PIH
      INTEGER ILAST
      INTEGER IOF
      INTEGER ION
      REAL S
      INTEGER IHEM
      REAL ROFF
      REAL DOFF
      INTEGER NCHNL
      INTEGER ISEG
      REAL X
      REAL Y
      REAL ARR
      REAL AR
      REAL XX
      REAL X1
      REAL Y1
      INTEGER NOUT
C
      PI=4.*ATAN(1.)
      PIH=0.5*PI
      ILAST=-999
      IOF=-1
      ION=1
      S=FLOAT(3-2*IHEM)
      ROFF=DOFF*PI/180.
  200 CONTINUE
      READ (NCHNL,*,END=400) ISEG,X,Y
      IF (Y*S.GT.0.) THEN
       ARR=AR*TAN(0.5*(PIH-ABS(Y)))
       XX=S*(X-PIH-ROFF)
       X1=ARR*COS(XX)
       Y1=ARR*SIN(XX)
       IF (ISEG.NE.ILAST) THEN
        WRITE (NOUT) IOF,X1,Y1
        ILAST=ISEG
       ELSE
        WRITE (NOUT) ION,X1,Y1
       ENDIF
      ELSE
       ILAST=-999
      ENDIF
      GO TO 200
 400  CONTINUE
      REWIND NOUT
      REWIND NCHNL
      RETURN
      END
