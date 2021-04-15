C
      SUBROUTINE VIDLIN(X,Y,N)
C
      include 'uniras.inc'
      include 'cframe.inc'
C
      INTEGER N
      INTEGER I1
      INTEGER I2
      INTEGER I3
      INTEGER ICOLOR
      REAL X1
      REAL Y1
      INTEGER IX1
      INTEGER IXVMAP
      INTEGER IY1
      INTEGER IYMAP
      INTEGER I
      REAL X2
      REAL Y2
      INTEGER IX2
      INTEGER IY2
      REAL RX
      REAL XSIDE
      REAL RY
      REAL YSIDE
C
      COMMON /XPSD01/ XSIDE, YSIDE
C
      REAL X(N),Y(N)
C
      IF (.NOT.LVID.AND..NOT.(IGIF.NE.0)) RETURN
C
      CALL XQCOL(I1,I2,I3,ICOLOR)
      if (lvid) then
       if(icolor.eq.0)then
         icolor=127
      elseif(icolor.eq.1)then
         icolor=1
      elseif(icolor.eq.2)then
         icolor=192
      elseif(icolor.eq.3)then
         icolor=128
      elseif(icolor.eq.-10)then
         if(ishade.ne.0)then
           icolor=255
         else
           icolor=90
         endif
      elseif(icolor.eq.-2)then
         icolor=245
      elseif(icolor.eq.-1)then
         icolor=140
      elseif(icolor.eq.-3)then
         icolor=20
      elseif(icolor.eq.-4)then
         icolor=50
      elseif(icolor.eq.-5)then
         icolor=220
        elseif (icolor.eq.-999)then
           icolor=0
        elseif (icolor.eq.-9999) then
           icolor=255
      else
         icolor=20
      endif
      else
         icolor=unicol
      endif
C
      IF (IGIF.NE.0) THEN
         X1=X(1)
         Y1=Y(1)
         IX1=IXVMAP(X1) + 1
         IY1=IYMAP(Y1)  + 1
         do i=2,n
            X2=X(I)
            Y2=Y(I)
            IX2=IXVMAP(X2) + 1
            IY2=IYMAP(Y2)  + 1
            call vline(ix1,iy1,ix2,iy2,ICOLOR)
          ix1=ix2
          iy1=iy2
         end do
      else
         rx=real(nrasx)/xside
         ry=real(nrasy)/yside*1.5/1.9
         ix1=nint(x(1)*rx)+1
         iy1=nint((yside-y(1))*ry)+1
         if(ix1.gt.nrasx)ix1=nrasx
         if(iy1.gt.nrasy)iy1=nrasy
         do i=2,n
          ix2=nint(x(i)*rx)+1
           iy2=nint((yside-y(i))*ry)+1
          if(ix2.gt.nrasx)ix2=nrasx
          if(iy2.gt.nrasy)iy2=nrasy
          call vline(ix1,iy1,ix2,iy2,ICOLOR)
          ix1=ix2
          iy1=iy2
         end do
      end if
C
      RETURN
      END
