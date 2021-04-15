C
      SUBROUTINE XZRPSC(IMSK,DIX)
C
C     This routine produces post-script colour fill output from the
C     PC DPICT routine. It will colour fill squares of a given dimension
C     Each square is equivalent to a pixel on the PC screen
C
      include 'mappings.inc'
      include 'vpsc.inc'
      include 'colours.inc'
C
      INTEGER DIX,IMSK(DIX)
      REAL X(4),Y(4)
C
      INTEGER IPSC
      INTEGER ITEMP
      REAL RINCX
      REAL RINCY
      REAL RINC
      INTEGER I
      INTEGER NOPLOT
      INTEGER IPSCEXP
      INTEGER IN_EXP
      INTEGER IPV
      INTEGER IPSCTMP
      INTEGER ICOLTX
      INTEGER IMASKCOL
C
      COMMON /XPVD01/ IPV,IPSC,IPSCTMP,ICOLTX(4),IPSCEXP,IMASKCOL
C
      IF (IPSC.EQ.0) RETURN
C
      IF (IRASX1.GT.IRASX2) THEN
         ITEMP=IRASX1
         IRASX1=IRASX2
         IRASX2=ITEMP
      END IF
      IF (IRASY1.GT.IRASY2) THEN
         ITEMP=IRASY1
         IRASY1=IRASY2
         IRASY2=ITEMP
      END IF
C
      ITEMP=IXRASO
C
      RINCX=REAL(IXB)/ABS(XSCRR-XSCRL)
      RINCY=-REAL(IYB)/ABS(YSCRT-YSCRB)
C
      X(1)=RINCX * (REAL(ITEMP)  - XSCRL) + 50
      IF (X(1).LT.IRASX1) X(1)=IRASX1
      IF (X(1).GT.IRASX2) X(1)=IRASX2
C
      Y(1)=RINCY * (REAL(IYRASO) - YSCRB) - RINCY/2
      IF (Y(1).LT.IRASY1) Y(1)=IRASY1
      IF (Y(1).GT.IRASY2) Y(1)=IRASY2
C
      Y(2)=Y(1)
C
      Y(3)=Y(2) + RINCY
      IF (Y(3).LT.IRASY1) Y(3)=IRASY1
      IF (Y(3).GT.IRASY2) Y(3)=IRASY2
C
      Y(4)=Y(3)
C
      RINC=0.0
      ITEMP=ITEMP+1
      DO 10 I=2,DIX
C
      RINC=RINC+RINCX
C
      noplot=0
      IF (IMSK(I).NE.IMSK(I-1)) THEN
         if (imsk(i-1).eq.-20) then
            noplot=1
         else if (ipscexp.eq.1) then
            if (imsk(i-1).eq.-3) then
               call xzccol(-1)
            else
               noplot=1
            end if
         else if (imsk(i-1).le.-5) then
            call xzccol(imsk(i-1))
         else if (imsk(i-1).eq.-4) then
            call xzccol(-1)
         else
            CALL XZCCOL  (IMSK(I-1))
         endif
         X(2)=X(1)+RINC
         IF (X(2).LT.IRASX1) X(2)=IRASX1
         IF (X(2).GT.IRASX2) X(2)=IRASX2
         X(3)=X(2)
         X(4)=X(3) - RINC
         IF (X(4).LT.IRASX1) X(4)=IRASX1
         IF (X(4).GT.IRASX2) X(4)=IRASX2
         if (noplot.eq.0) CALL XZPSC(X,Y,-4)
         RINC=0.0
         X(1)=RINCX * (REAL(ITEMP)  - XSCRL) + 50 - RINCX/2
         IF (X(1).LT.IRASX1) X(1)=IRASX1
         IF (X(1).GT.IRASX2) X(1)=IRASX2
      ENDIF
      ITEMP=ITEMP+1
 10   CONTINUE
C
      RINC=RINC+RINCX
      noplot=0
      if (imsk(i-1).eq.-20) then
         noplot=1
      else if (ipscexp.eq.1) then
         if (imsk(i-1).eq.-3) then
            call xzccol(-1)
         else
            noplot=1
         end if
      else if (imsk(i-1).le.-5) then
         call xzccol(imsk(i-1))
      else if (imsk(dix).eq.-4) then
         call xzccol(-1)
      else
         CALL XZCCOL  (IMSK(DIX))
      endif
C
      X(2)=X(1) + RINC
      IF (X(2).LT.IRASX1) X(2)=IRASX1
      IF (X(2).GT.IRASX2) THEN
         X(2)=IRASX2
         RINC=X(2)-X(1)
      ENDIF
      X(3)=X(2)
      X(4)=X(3) - RINC
      IF (X(4).LT.IRASX1) X(4)=IRASX1
      IF (X(4).GT.IRASX2) X(4)=IRASX2
      if (noplot.eq.0) CALL XZPSC(X,Y,-4)
C
      RETURN
C
      ENTRY ZPSCEXP(IN_EXP)
      IPSCEXP=IN_EXP
      RETURN
C
      END
