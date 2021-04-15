C
      SUBROUTINE XTHICK(ITHICK)
C
      INTEGER LTHICK
      INTEGER ITHICK
      INTEGER KTHICK
      REAL HF1
      REAL HB1
      REAL HF2
      REAL HB2
      INTEGER LFULL
      REAL DTHICK
CPV  change for efficient dashed lines
      COMMON /XLPN13/ HF1,HB1,HF2,HB2,LFULL,LTHICK, DTHICK
C Set thickness of lines. ITHICK=1 or 2.
      LTHICK=min(ITHICK,2)
      RETURN
      ENTRY XQTHIK(KTHICK)
C Enquiry routine of line thickness.
      KTHICK=LTHICK
      RETURN
      END
