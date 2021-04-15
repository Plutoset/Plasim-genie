C
      subroutine xnear(xout,yout,i,xend,yend,xnr,ynr,dist)
C
      INTEGER I
      REAL XNR
      REAL YNR
      REAL YEND
      REAL XEND
      REAL SLOPE
      REAL DIST
      REAL XDIST
C
      real xout(*),yout(*)
c
      if (xout(i+1)-xout(i).eq.0) then
c         dist=(xend-xout(i))*(xend-xout(i))
         xnr=xout(i)
         ynr=yend
      else if (yout(i+1)-yout(i).eq.0) then
c         dist=(yend-yout(i))*(yend-yout(i))
         xnr=xend
         ynr=yout(i)
      else
         slope=(yout(i+1)-yout(i))/(xout(i+1)-xout(i))
         xnr=(yend+xend/slope-yout(i)+slope*xout(i))/
     :                       (slope+1.0/slope)
         ynr=slope*(xnr-xout(i)) + yout(i)
      end if
      dist=xdist(xend,xnr,yend,ynr)
      return
      end
