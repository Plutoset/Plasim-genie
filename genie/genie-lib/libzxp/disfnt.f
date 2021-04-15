C
      SUBROUTINE DISFNT(X1,Y1,STRING,IPOS,ILEN,IFONT,IFNTCOL,IFAIL)
C
      INTEGER IVERT
      REAL XP1
      REAL X1
      REAL YP1
      REAL Y1
      REAL ANG1
      INTEGER IXJ
      INTEGER IXVMAP
      INTEGER IYJ
      INTEGER IYMAP
      INTEGER IPOS
      INTEGER ILEN
      INTEGER IFAIL
      INTEGER IFONT
      INTEGER IFNTCOL
C
      CHARACTER*(*) STRING
C
      call xqmrag(ang1)
c
      IF (ABS(ANG1).LE.1.0) THEN
         IVERT=0
      ELSE
         IVERT=1
      END IF
C
      call xqchor(ang1)
      if (abs(ang1).gt.1.0) then
         if (ivert.eq.0) then
            ivert=-1
         else
            ivert=0
         end if
      end if
C
      XP1=X1
      YP1=Y1
      CALL XTRANS(XP1,YP1)
c
      IXJ=IXVMAP(XP1)
      IYJ=IYMAP(YP1)
      CALL X_FNT(IPOS,ILEN,IXJ,IYJ,IFAIL,STRING,IFONT,IFNTCOL,IVERT)
C
      RETURN
      END
