C
      SUBROUTINE VIDINI(ITYOUT)
C
      include 'uniras.inc'
      include 'colours.inc'
      include 'cframe.inc'
C
      INTEGER ITYOUT
      INTEGER NX1
      INTEGER NY1
C
      IF (ITYOUT.EQ.0.OR.ITYOUT.EQ.2) LVID=.TRUE.
      IF (ITYOUT.EQ.1.OR.ITYOUT.EQ.2) THEN
         IGIF=1
      ELSE IF (ITYOUT.EQ.3) THEN
         IGIF=2
      END IF
C
      IF (LVID) THEN
         OPEN(UNIT=NUNITV,FORM='UNFORMATTED',
     :     FILE='vidi.for',status='unknown')
      END IF
C
      IF (ITYOUT.NE.3) THEN
         PRINT*,' ENTER NUMBER OF PIXELS IN X AND Y DIRECTION '
         READ(5,*)NRASX,NRASY
         IF(NRASX.GT.NRASMX.OR.NRASY.GT.NRASMY)THEN
          PRINT*,' Too many pixels max = ',NRASMX,NRASMY
          STOP
         ENDIF
         IF(NRASX.LE.0.OR.NRASY.LE.0)THEN
          PRINT*,' NUMBER OF PIXELS MUST BE .GT. 0 '
          STOP
         ENDIF
         NRASX=2*(NRASX/2)
         NRASY=2*(NRASY/2)
      ELSE
         CALL XRESOL(NX1,NY1)
         NRASX=NX1
         NRASY=NY1
      END IF
C
      IF (LVID) THEN
         NRASCL=245
      ELSE IF (IGIF.NE.0) THEN
         NRASCL=NCOL
      END IF
C
      CALL VCLEAR(0)
      END
