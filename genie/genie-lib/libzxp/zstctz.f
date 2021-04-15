      SUBROUTINE ZSTCTZ(NAME)
C     Open file for full resolution coastline data on fort.17.
      CHARACTER NAME*(*)
      IF (NAME.EQ.'RAL') THEN
       OPEN(17,FILE='/atlas/kd/diagn/zjmap2',FORM='FORMATTED')
      ELSEIF(NAME.EQ.'ULCC')THEN
       OPEN(17,FILE='/u/gbar361/diagn/zjmap2',FORM='FORMATTED')
      ELSEIF(NAME.EQ.'RDG.CMS')THEN
       OPEN(17,FILE='/ZJMAP DATA D',FORM='FORMATTED')
      ELSE
       PRINT*,' *ZPLOT*ZSTCTZ*  TYPE OF MAINFRAME NOT ALLOWED '
       STOP
      ENDIF
      RETURN
      END
