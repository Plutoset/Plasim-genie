      SUBROUTINE XPRJON
      INTEGER KPROJC
      COMMON /XPRJ26/ KPROJC
C Switch on user defined projection through XPROJC.
      KPROJC=1
      RETURN
      ENTRY      XPRJOF
C Switch off user defined projection through XPROJC.
      KPROJC=0
      RETURN
      END
