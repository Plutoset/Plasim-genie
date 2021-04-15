      SUBROUTINE XCHDEC(ICDATA,CHDATA,I)
C To decode character set data for charactere No. I.
C
      INTEGER ILEN
      INTEGER I
      INTEGER ICLENG
      INTEGER I0
      INTEGER J0
      INTEGER ICD
      INTEGER INUM
      INTEGER JX
      REAL XDECOD
      INTEGER JY
      INTEGER I00
C
      CHARACTER CHDATA(127)*300,XCH*2,YCH*2,STR2*4
      INTEGER ICDATA (0:150, 32:127)
      ILEN = ICLENG(CHDATA(I))
      I0=0
      J0=0
      ICDATA(1,I)=ILEN/2+1
      ICD=1
      DO 5 INUM=1,ILEN,4
       STR2 = CHDATA(I) (INUM:INUM+3)
       READ(STR2,100)XCH,YCH
       JX = XDECOD(XCH)
       JY = XDECOD(YCH)
       I00=0
       IF (JX .GT. 127) THEN
          JX = JX - 128
          I00=1
       ENDIF
       I0=I0+JX-64
       J0=J0+JY-64
       IF( I00.EQ.1) THEN
         ICDATA(ICD+1,I)=I0
       ELSE
         ICDATA(ICD+1,I)=I0+50
       ENDIF
       ICDATA(ICD+2,I)=J0
       ICD=ICD+2
 5    CONTINUE
  100 FORMAT(2A,2A)
      RETURN
      END
