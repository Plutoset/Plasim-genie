
      SUBROUTINE VLINE(IX1,IY1,IX2,IY2,ICOLOUR)
C
      include 'cframe.inc'
C
      INTEGER IY1
      INTEGER IY2
      INTEGER ISTEP
      INTEGER IX1
      INTEGER IX2
      INTEGER IX
      INTEGER II
      INTEGER ICOLOUR
      INTEGER IY
      REAL A
      REAL C
C
       IF(IY1.EQ.IY2)THEN
         ISTEP=1
         IF(IX1.GT.IX2)ISTEP=-1
         DO 20 IX=IX1,IX2,ISTEP
            II=(IY1-1)*NRASX + IX
            if(ii.gt.0)CFRAME(II)=CHAR(ICOLOUR)
  20       CONTINUE
       ELSEIF(IX1.EQ.IX2)THEN
         ISTEP=1
         IF(IY1.GT.IY2)ISTEP=-1
         DO 30 IY=IY1,IY2,ISTEP
            II=(IY-1)*NRASX + IX1
            if(ii.gt.0)CFRAME(II)=CHAR(ICOLOUR)
  30       CONTINUE
       ELSE
         A=float(IY2-IY1)/float(IX2-IX1)
         C=IY1-A*IX1
         IF(ABS(A).LE.1.)THEN
           ISTEP=1
           IF(IX1.GT.IX2)ISTEP=-1
           DO 40 IX=IX1,IX2,ISTEP
             IY=NINT(A*IX+C)
             II=(IY-1)*NRASX + IX
             if(ii.gt.0)CFRAME(II)=CHAR(ICOLOUR)
  40         CONTINUE
         ELSE
           A=1./A
           C=IX1-A*IY1
           ISTEP=1
           IF(IY1.GT.IY2)ISTEP=-1
           DO 50 IY=IY1,IY2,ISTEP
             IX=NINT(A*IY+C)
             II=(IY-1)*NRASX +IX
             if(ii.gt.0)CFRAME(II)=CHAR(ICOLOUR)
  50         CONTINUE
         ENDIF
       ENDIF
       RETURN
       END
