      SUBROUTINE XCHLJ( CH,LCH)
C Left justify a character string.
      INTEGER K
      INTEGER LCH
      INTEGER L
C
      CHARACTER CH*(*) , CH1*20
      K=1
      LCH=LEN( CH )
      DO 1 L=1,LCH
      IF( CH(L:L).NE.' ') THEN
        K=L
        GOTO 2
      ENDIF
 1    CONTINUE
 2    CH1=CH
      CH=' '
      CH(1:LCH-K+1)=CH1(K:LCH)
      LCH=LCH-K+1
      RETURN
      END
