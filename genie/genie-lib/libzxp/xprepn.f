C
      SUBROUTINE XPREPN(IX,IY,LABEL,ICH)
C
C   Prepares text for output
C
      INTEGER IX
      INTEGER IZ
      INTEGER ICH
      INTEGER IY
C
      CHARACTER LABEL*(*)

      CALL XLAB(IX,LABEL,IZ)
      ICH=IZ
      CALL XLAB(IY,LABEL(IZ+1:LEN(LABEL)),IZ)
      ICH=ICH+IZ
      END
