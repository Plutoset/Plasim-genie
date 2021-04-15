c
      SUBROUTINE SETCV (FMIN,FMAX,CINC,CV,NCNT,ITYPE,FINC)

      IMPLICIT NONE

C======================================================================
C                                                                     =
C  Set up contour levels for a data array with known min and max      =
C  values. Three options, depending on sign and magnitude of input    =
C  contour interval, CINC :                                           =
C     a) CINC.GT.0.       : Use input contour interval CINC.          =
C     b) CINC.LT.-1.      : Contour interval is (-CINC) in normalised =
C                           units, defined such that max. abs. value  =
C                           is 1000 units.                            =
C     c) -1.LT.CINC.LT.0. : Contour interval is (-CINC) in normalised =
C                           units, defined such that next power of 10 =
C                           greater than max. abs. value is 1 unit.   =
C  In (c) the contour interval is halved if the max. abs. value is    =
C  less than half the next power of ten, or divided by 5 if less than =
C  1/5th of it.                                                       =
C                                                                     =
C   If itype.eq.0 then (b) and (c) is calculated using maximum value  =
C   If itype.eq.1 then (b) and (c) uses range                         =
C======================================================================
C
      INTEGER NCNT,ITYPE,IEXP,ILO,I
      REAL    FAMAX,FMAX,FMIN,FRANGE,FINC,CINC,FA10,FRACT,CLV

      REAL CV(NCNT)
C
      FAMAX=AMAX1(ABS(FMAX),ABS(FMIN))
      FRANGE=FMAX-FMIN
      IF (FRANGE.LE.FAMAX*1.E-10) THEN
         FINC=0.0
         NCNT=0
         RETURN
      END IF
c
      IF (CINC.GT.0.) THEN
         FINC=CINC
      ELSE IF (CINC.LE.-1.) THEN
         FINC=-FAMAX*CINC/1000.
      ELSE IF (CINC.LT.0.) THEN
         IF (ITYPE.EQ.0) THEN
            IEXP=NINT(ALOG10(FAMAX)*0.99999)
            FA10=10.**IEXP
            FRACT=FAMAX/FA10
         ELSE
            IEXP=NINT(ALOG10(FRANGE)*0.99999)
            FA10=10.**IEXP
            FRACT=FRANGE/FA10
         END IF
         IF (FRACT.GT.0.5) THEN
            FINC=-FA10*CINC
         ELSE IF (FRACT.GT.0.2) THEN
            FINC=-FA10*CINC*0.5
         ELSE
            FINC=-FA10*CINC*0.2
         END IF
      END IF
c      print*,cinc,iexp,fmin,fmax,frange,fract,fa10,finc
C
      ILO=INT(FMIN/FINC)
      IF (FMIN.GE.0.) ILO=ILO+1
      CLV=FLOAT(ILO-1)*FINC
      I=0
 10   CLV=CLV+FINC
      I=I+1
      IF ((I.GT.NCNT).OR.(CLV.GE.FMAX)) GO TO 20
      CV(I)=CLV
      GO TO 10
 20   NCNT=I-1
C
      RETURN
      END
