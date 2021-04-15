C
      SUBROUTINE XDEVIC
C
      include 'uniras.inc'
      include 'cframe.inc'
      include 'mappings.inc'
      INCLUDE 'f77pc.inc'
C
      INTEGER NZXCAL
      REAL XBOUND
      REAL YBOUND
      INTEGER IEG
      INTEGER IPSC
      INTEGER IPSCTMP
      INTEGER I1
      INTEGER IPV
      INTEGER ICOLTX
      INTEGER IPSCEXP
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
C
      SAVE NZXCAL
      DATA NZXCAL /0/
C Set up device ( Paper for GHOST )
C
C     INITIALISE GRAPHICAL OUTPUT
C
      CALL XICHAR
      LMOD=.FALSE.
      PICNO=1
      UNICOL=-2
      RLINWI=-1.0
      LININT=1
      DASHNO=0
      IJOIN=0
      XUSIZE=1.9
      YUSIZE=1.5
      XBOUND=1.9
      YBOUND=XBOUND*1.5/1.9
      LVID=.FALSE.
      IGIF=0
C XDSPAC must precede all other calls to ZXPLOT routines.
      IEG=3
      CALL INIGRA(ieg)
      CALL XDSPAC( 1.0 )
      CALL XMINIT
      NZXCAL=1
      RETURN
C ******************************************************************
      ENTRY XFRAME
C Advance plotting onto next frame
C
C     MOVE TO NEXT FRAME
C
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
      IF(LMOD)THEN
      IF (LVID.OR.(IGIF.NE.0) ) CALL VFRAME(LVID,IGIF)
        IF (IGIF.EQ.2) IGIF=0
      IF (IPSC.EQ.1) CALL XFPSC(0)
        IF (IPSCTMP.EQ.1) THEN
           IPSC=0
           IPSCTMP=0
        END IF
      LMOD=.FALSE.
1000  FORMAT(6I4)
C
      IF(LSAVE.AND.ISAVE.GT.0)THEN
        I1=-1
        WRITE(NCHAN,1000)I1,ICOLPC,IDASH,IDASH,IDASH,IDASH
      ENDIF
C
        CALL X_Clear
      ENDIF
      RETURN
C ******************************************************************
      ENTRY XGREND
C Terminate graphic plotting
      IF( NZXCAL.EQ.0)  THEN
       PRINT*,' XGREND called before device is set up.'
       RETURN
      ENDIF
C
C     END GRAPHICAL OUTPUT
C
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
      IF (LMOD) THEN
      IF (IPSC.EQ.1) CALL XFPSC(1)
      IF (LVID.OR.(IGIF.NE.0)) CALL VFRAME(LVID,IGIF)
        IF (IGIF.EQ.2) IGIF=0
      IF(LSAVE.AND.ISAVE.GT.0)THEN
        I1=-1
        WRITE(NCHAN,1000)I1,ICOLPC,IDASH,IDASH,IDASH,IDASH
      ENDIF
      ENDIF
      IF (IPSC.EQ.1) CALL XEPSC
      IF (IPSCTMP.EQ.1) THEN
         IPSC=0
         IPSCTMP=0
      END IF
      IF(ISAVE.GT.0)CLOSE(UNIT=NCHAN)
      IF (LVID) CLOSE(UNIT=NUNITV)
      CALL X_DisplayDone
C
      CALL CLOSEDOWN
C
      RETURN
      END
