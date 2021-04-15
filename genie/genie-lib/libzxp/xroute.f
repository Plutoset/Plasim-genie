
C *******************************************************
      SUBROUTINE XROUTE(TEXT)
      CHARACTER TEXT*(*),TEXT1*20
      SAVE TEXT1
      DATA TEXT1/'s ldummy;e'/
      TEXT=TEXT1
      RETURN
      ENTRY XIROUT(TEXT)
      TEXT1=TEXT
      END
