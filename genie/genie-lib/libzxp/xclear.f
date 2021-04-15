C *******************************************************
      SUBROUTINE XCLEAR
C
      include 'uniras.inc'
C
      IF(IJOIN.GT.1)
     :   CALL XUDRAW(XJOIN,YJOIN,IJOIN,RLINWI,UNICOL,DASHNO)
      call x_displayflush
C
      RETURN
      END
