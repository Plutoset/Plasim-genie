      SUBROUTINE SSSLLL(IX1,IY1,IX2,IY2,ICOLLL)
      include 'mappings.inc'
C
      INTEGER ITEMP
      INTEGER ITEMP1
      INTEGER ICOLLL
      INTEGER IX1
      INTEGER IY1
      INTEGER IX2
      INTEGER IY2
C
1000  FORMAT(6I4)
1001  FORMAT(20I3)
1002  FORMAT(11F7.2)

      IF(LSAVE.AND.ISAVE.GT.0.) THEN
      ITEMP=2
        ITEMP1=-2
      WRITE(NCHAN,1000)ITEMP,ICOLLL,IDASH,IDASH,IDASH,ITEMP1
      IF(ISAVE.EQ.1)THEN
         WRITE(NCHAN,1001)IX1,IY1,IX2,IY2
        ELSE IF (ISAVE.EQ.2) THEN
          WRITE(NCHAN,1002)REAL(IX1),REAL(IY1),REAL(IX2),REAL(IY2)
        ENDIF
      ENDIF
      END
