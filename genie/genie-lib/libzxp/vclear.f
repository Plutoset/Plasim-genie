
      SUBROUTINE VCLEAR(ICOLOUR)
      include 'cframe.inc'
C
      INTEGER II
      INTEGER IY
      INTEGER IX
      INTEGER ICOLOUR
C
      ii=0
      DO 10 IY=1,nrasx
      DO 10 IX=1,nrasy
         ii=ii+1
         CFRAME(ii)=CHAR(ICOLOUR)
 10   CONTINUE

      RETURN
      END
