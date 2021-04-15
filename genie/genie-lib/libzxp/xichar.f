*DECK XICHAR
      SUBROUTINE XICHAR
C To build an equivalence between ICHAR/CHAR functions using ASCII
C and EBCDIC code.
C  CHAR( I ) (ASCII ) =  CHAR( ICRAM(I) ) (EBCDIC)
C ICHAR( C ) (EBCDIC) = ICRAM( ICHAR(C) ) (ASCII )
      COMMON /XCHR30/ NCRAM
      INTEGER ICRAM(127) ,NCRAM(256), I
      DATA ICRAM /
C 1-30
     :   32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     :   32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
C 31-60
     :   32, 32, 90,127,123, 91,108, 80,125, 77, 93, 92, 78,107, 96,
     :   75, 97,240,241,242,243,244,245,246,247,248,249,122, 94, 76,
C 61-90
     :  126,110,111, 32,193,194,195,196,197,198,199,200,201,209,210,
     :  211,212,213,214,215,216,217,226,227,228,229,230,231,232,233,
C 91-120
     :  173,224,189,113,109,121,129,130,131,132,133,134,135,136,137,
     :  145,146,147,148,149,150,151,152,153,162,163,164,165,166,167,
C 121-127
     :  168,169,192, 32,208, 95, 32  /
C
      IF (ICHAR('0').EQ.48) THEN
C       For machines/systems using ASCII code (eg Cray)
        DO 5 I=1,127
  5     NCRAM(I)=ICRAM(I)
        DO 6 I=128,256
  6     NCRAM(I)= 32
      ELSE
C       For machines/systems using EBCDIC code (eg IBM CMS)
        DO 15 I=1,256
  15    NCRAM(I)=I
      ENDIF
      RETURN
      END
