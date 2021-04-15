C
      SUBROUTINE VIDPIX(VIDIW,IXO,IYO,IXSC,IYSC,LGIF)
C
      include 'cframe.inc'
C
      INTEGER IXSC
      INTEGER IYSC
      INTEGER J
      INTEGER IFR
      INTEGER IXO
      INTEGER IYO
      INTEGER I
      INTEGER IYLIM
      INTEGER IXRASO
      INTEGER IYRASO
      INTEGER ITYSCR
      INTEGER III
      INTEGER IRESCALE
      INTEGER IX2
      INTEGER IX1
      INTEGER IXSTEP
      INTEGER IY2
      INTEGER IY1
      INTEGER IYSTEP
      INTEGER IY
      INTEGER IOFF
      INTEGER IX
      INTEGER II1
C
      REAL VIDIW(IXSC,IYSC)
      INTEGER IVIDIT(IXSC,IYSC),IMSK(*),DIX,ICV(*)
      LOGICAL LGIF
C
      IF (LGIF) THEN
         DO J=1,IYSC
            IFR=IXO + (IYO+1-j)*nrasx
            DO I=1,IXSC
               IFR=IFR+1
               if (vidiw(i,j).ne.999.999.and.ifr.gt.0)
     :             CFRAME(IFR)=CHAR(INT(VIDIW(I,J)))
            END DO
         END DO
      ELSE
C
         iylim=nint(nrasy*1.5/1.9)+1
         DO J=1,IYSC
            IFR=IXO + (iylim-iyo+1-j)*nrasx
            DO I=1,IXSC
               IFR=IFR+1
               if (vidiw(i,j).ne.999.999.and.ifr.gt.0)
     :             CFRAME(IFR)=CHAR(INT(VIDIW(i,j)))
            END DO
         END DO
C
      END IF
      RETURN
C
      ENTRY VIDPIXI(IVIDIT,IXO,IYO,IXSC,IYSC,LGIF)
C
      DO J=1,IYSC
         IFR=IXO + (IYO-j)*nrasx
         DO I=1,IXSC
            IFR=IFR+1
            if (ividit(i,j).ne.999999.and.ifr.gt.0)
     :             CFRAME(IFR)=CHAR(IVIDIT(I,J))
         END DO
      END DO
C
      RETURN
C
      ENTRY VIDFIL(IMSK,DIX,IXRASO,IYRASO,IRESCALE,ITYSCR,ICV)
C
      IFR=IXRASO + IYRASO*NRASX + 1
      DO I=1,DIX
         IF (IMSK(I).NE.0.AND.ITYSCR.LT.5) THEN
            III=IMSK(I)
            IF (III.LT.-3) THEN
               III=0
            ELSE IF (III.EQ.-3) THEN
               III=1
            ELSE IF (III.EQ.-2) THEN
               III=1
            ELSE IF (III.EQ.-1) THEN
               III=0
            ELSE IF (III.EQ.0) THEN
               III=0
            ELSE
               IF (IRESCALE.EQ.0) III=ICV(III)
            ENDIF
         END IF
         CFRAME(IFR)=CHAR(III)
         IFR=IFR+1
      END DO
C
      RETURN
C
      ENTRY VIDFIX(IX1,IY1,IX2,IY2,II1)
C
      IF (IX2.LT.IX1) THEN
         IXSTEP=-1
      ELSE
         IXSTEP=1
      END IF
      IF (IY2.LT.IY1) THEN
         IYSTEP=-1
      ELSE
         IYSTEP=1
      END IF
C
      DO IY=IY1,IY2,IYSTEP
         IOFF=(IY-1)*NRASX
         DO IX=IX1,IX2,IXSTEP
            CFRAME(IX+IOFF)=CHAR(II1)
         END DO
      END DO
C
      RETURN
      END
