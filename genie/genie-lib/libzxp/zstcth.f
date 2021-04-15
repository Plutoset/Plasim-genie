      SUBROUTINE ZSTCTH(AR,NAME)
C     Open files for cheap polar stereographic coastlines
C     on units fort.18 and fort.19.
      REAL AR
      CHARACTER NAME*(*)
      IF (AR.NE.0.675) THEN
       PRINT*,' *ZSTCTH*  AR NOT CORRECT FOR CHEAP POLAR COASTLINES'
     :         ,'  - USE EXPENSIVE VERSION.'
       STOP
      ENDIF
      IF (NAME.EQ.'RAL') THEN
       OPEN(18,FILE='/atlas/kd/diagn/nhemcoast1',FORM='UNFORMATTED')
       OPEN(19,FILE='/atlas/kd/diagn/shemcoast1',FORM='UNFORMATTED')
      ELSEIF(NAME.EQ.'ULCC')THEN
       OPEN(18,FILE='/u/gbar361/diagn/nhemcoast1',FORM='UNFORMATTED')
       OPEN(19,FILE='/u/gbar361/diagn/shemcoast1',FORM='UNFORMATTED')
      ELSEIF(NAME.EQ.'RDG.CMS')THEN
       OPEN(18,FILE='/NHEMCOAS DATA D',FORM='UNFORMATTED')
       OPEN(19,FILE='/SHEMCOAS DATA D',FORM='UNFORMATTED')
      ELSE
       PRINT*,' *ZPLOT*ZSTCTH*  TYPE OF MAINFRAME NOT ALLOWED '
       STOP
      ENDIF
      RETURN
      END
