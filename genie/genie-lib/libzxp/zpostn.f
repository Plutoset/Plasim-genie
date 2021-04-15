      SUBROUTINE ZPOSTN(Z,ALO,ALA,MD,MN,JN)
C
      include 'zpltpm.cmn'
c      COMMON/ZPLTPM/CX,CY,ALONS,ALATS,SALATS,CALATS,RC,AMP,SNWD,CSWD
c     :,CENT,ISTL,SWITCH,XH,YH,RAD,YEND,DX,DY,YTOP,RDX,RDY,RTOP,NM,NJ
c     :,MG,JG,JG2,JG2P,MGP,MGHP,DLONG,DLAT,PI,SVAGL,CVAGL,SROT,CROT,Q
c     :,ROUSN,ROUCS,PI2,ALFA,BETA,FANST,FANED,ITRUNC,AMQ,AMR,AK,YB,AB
c     :,ICOASRD,INCCST,ICOORD,CENTY,X1LIM,X2LIM,Y1LIM,Y2LIM
c     :,Z1STEP,Z1CHAR,IZTYPE,IVECTC,ARM
C
      INTEGER MD
      INTEGER J
      INTEGER JN
      INTEGER I
      INTEGER MN
      REAL X
      REAL Y
      REAL X1
      REAL Y1
      REAL DELT1
      REAL R1
C
      REAL ALO(*),ALA(*)
      COMPLEX Z(MD,*)
C---------------------------------------------------------------------
C     This option provides an economic run for Istyle=1,3,4 ( simple
C     longitude-latitude and Polar-stereographic projection ) by
C     evaluating contours in projection space rather than on spherical
C     lat.-long. space.
C NOTE: needs be called once for each each projection, then call
C       ZCONTA or ZCONTR instead of ZGCNTA or ZGCNTR.
C       When Istyle=1, cx has no effect, taken as 180.
C---------------------------------------------------------------------
      IF(ISTL.LT.1.OR.ISTL.GT.4.OR.ISTL.EQ.2) RETURN
      DO 100 J=1,JN
      DO 100 I=1,MN
      X=ALO(I)
      Y=ALA(J)
      IF(ISTL.EQ.1) THEN
       Z(I,J)=CMPLX(RAD*X-XH,2.*Y*YH/PI)
      ELSE
       X1=X-ALONS
       Y1=Y
       DELT1=SIN(Y1*RC)-CSWD
       R1=COS(Y1)*AMP*AMQ/(1+(SWITCH-SVAGL*(1-SWITCH))*(DELT1+CSWD))
       Z(I,J)=CMPLX(SIN(X1)*R1,-COS(X1)*R1*RC)
      ENDIF
  100 CONTINUE
      RETURN
      END
