c
      subroutine zcstfl(xout,yout,iout)
      real xout(*),yout(*)
      integer iout
c
      integer idim,icount_seq,ijoin,ilines,i,ic,ii,ibr
      integer imin,imin1,imin2,ilen,ibr1,itemp
      integer iloop,ibrst,ist,ien,ic1
      real    distmin,distmn,xstart,ystart,xnr,ynr,dist
      real    xmin,ymin,xend,yend,totald,xdist
      real    xaim,yaim,xst,yst,xen,yen,x1out
      real    x2out,y2out,ang1,ang2,y1out
c
      integer iseg
      parameter(idim=20000,iseg=30)
      real x1(idim),y1(idim),xloop(idim),yloop(idim)
      real xmin1(iseg),xmin2(iseg),ymin1(iseg),ymin2(iseg)
      integer istart(iseg),iend(iseg),iline(iseg),istout(iseg),
     :        iendout(iseg),ijoine(iseg)
      integer isilent,icheck,itest
      save icount_seq
      data icount_seq/0/
c
      icount_seq=icount_seq+1
c
      isilent=-1
      distmin=1.e-6
c
      call xclear
      if (isilent.gt.0) then
         call xpages(3,0)
      end if
c
      if (iout.gt.idim) then
         print*,' zcstfl: redimension arrays 1 ',iout,idim
         stop 1
      end if
c
c     First get coordinates of coastline
c
      if (isilent.gt.0) call xclear
      call zgetln(x1,y1,ijoin)
      if (isilent.gt.0) print*,' ijoin ',ijoin
c
      if (ijoin.gt.idim) then
         print*,' zcstfl: redimension arrays 2 ',ijoin,idim
         stop 1
      end if
c
      if (ijoin.le.1) return
c
c    First find out if the coastline is one continuous line
c    or broken into several segments (due to intersections
c    with the domain boundary and the line
c
      if (x1(ijoin).ne.-999.999.or.y1(ijoin).ne.-999.999) then
         print*,' There has been some error in getting           '
         print*,' coastline points. Values of ijoin and x,y are: '
         print*,ijoin,x1(ijoin),y1(ijoin)
         print*,' Press any key to continue '
         read(5,*)
         return
      end if
      ijoin=ijoin-1
      if (x1(ijoin).eq.-999.999.or.y1(ijoin).eq.-999.999) then
         print*,' There has been some error in getting           '
         print*,' coastline points. Values of ijoin and x,y are: '
         print*,' ij',ijoin,x1(ijoin),y1(ijoin)
         print*,' Press any key to continue '
         read(5,*)
         return
      end if
      if (x1(1).eq.-999.999.or.y1(1).eq.-999.999) then
         print*,' There has been some error in getting           '
         print*,' coastline points. Values of x(1),y are: '
         print*,'1',x1(1),y1(1)
         print*,' Press any key to continue '
         read(5,*)
         return
      end if
c
      istart(1)=1
      ilines=1
      do i=1,ijoin
         if (x1(i).eq.-999.999.and.y1(i).eq.-999.999) then
               iend(ilines)=i-1
               ilines=ilines+1
               istart(ilines)=i+1
            end if
      end do
      iend(ilines)=ijoin
c
      if (iseg.lt.ilines) then
         print*,' zcstfl: work arrays are too small ',iseg,ilines
         read(5,*)
         stop 1
      end if
c
c        Now print out results if necessary
c
      if (isilent.ge.1) then
         print*,' There are ',ilines,' lines and ',ijoin,' joins'
         do i=1,ilines
            print*,i,' istart = ',istart(i),' iend = ',iend(i)
            ic=istart(i)-18
            if (ic.le.0) ic=0
            do ii=1,37
               ic=ic+1
               print*,ic,x1(ic),y1(ic)
            end do
c
            ic=iend(i)-18
            if (ic.le.0) ic=0
            do ii=1,37
               ic=ic+1
               print*,ic,x1(ic),y1(ic)
            end do
c
            print*,' Start point of line ',i,' is ',istart(i),
     :                 x1(istart(i)),y1(istart(i))
            print*,' End   point of line ',i,' is ',iend(i),
     :                 x1(iend(i)),y1(iend(i))
            if (isilent.gt.1) read(5,*)
c
         end do
      end if
c
c     This is when it gets really hard. Loop over all the line breaks
c     until all are joined up
c
      do ibr=1,ilines
         iline(ibr)=1
c
c     For some reason, I thought that in some cases the
c     line could follow the outline. Get rid of this by
c     moving start of line to first point near boundary
c
         ic=istart(ibr)-1
  200    continue
         ic=ic+1
         xstart=x1(ic)
         ystart=y1(ic)
         distmn=1000.0
         do i=1,iout-1
            call xnear(xout,yout,i,xstart,ystart,xnr,ynr,dist)
            if (dist.lt.distmn) then
               distmn=dist
               if (ic.eq.istart(ibr)) then
                  xmin1(ibr)=xnr
                  ymin1(ibr)=ynr
                  imin1=i
               else
                  xmin=xnr
                  ymin=ynr
                  imin=i
               end if
            end if
         end do
         if (distmn.lt.distmin) then
            if (ic.eq.istart(ibr)) then
               istout(ibr)=imin1
               go to 200
            else if (ic.eq.iend(ibr)) then
               iline(ibr)=-1
            else
               istart(ibr)=istart(ibr)+1
               ic=ic-1
               go to 200
            end if
         else
            if (ic.eq.istart(ibr)) istout(ibr)=-imin1
         end if
         if (isilent.ge.1) then
            print*,' distmn for  istout ',ibr,distmn,distmin
         end if
c
c     For some reason, I thought that in some cases the
c     line could follow the outline. Get rid of this by
c     moving end of line to last point near boundary
c
         ic=iend(ibr)+1
  210    continue
         ic=ic-1
         xend=x1(ic)
         yend=y1(ic)
         distmn=1000.0
         do i=1,iout-1
            call xnear(xout,yout,i,xend,yend,xnr,ynr,dist)
            if (dist.lt.distmn) then
               distmn=dist
               if (ic.eq.iend(ibr)) then
                  xmin2(ibr)=xnr
                  ymin2(ibr)=ynr
                  imin2=i
               else
                  xmin=xnr
                  ymin=ynr
                  imin=i
               end if
            end if
         end do
         if (distmn.lt.distmin) then
            if (ic.eq.iend(ibr)) then
               iendout(ibr)=imin2
               go to 210
            else if (ic.eq.istart(ibr)) then
               iline(ibr)=-1
            else
               iend(ibr)=iend(ibr)-1
               ic=ic+1
               go to 210
            end if
         else
            if (ic.eq.iend(ibr)) iendout(ibr)=-imin2
         end if
c
         if (isilent.ge.1) then
            print*,' Seg start ',ibr,' crosses  ',istout(ibr)
            if (istout(ibr).gt.0) then
               print*,x1(istart(ibr)),y1(istart(ibr)),
     :                xout(istout(ibr)),yout(istout(ibr))
            else
               print*,x1(istart(ibr)),y1(istart(ibr))
            end if
c
            print*,' Seg end   ',ibr,' crosses  ',iendout(ibr)
            if (iendout(ibr).gt.0) then
               print*,x1(iend(ibr)),y1(iend(ibr)),
     :                xout(iendout(ibr)),yout(iendout(ibr))
            else
               print*,x1(iend(ibr)),y1(iend(ibr))
            end if
            if (isilent.gt.1) read(5,*)
         end if
      end do
c
c     
c
      do ibr=1,ilines
         if (iline(ibr).gt.0) then
            ilen=iend(ibr)-istart(ibr) + 1
            if (ilen.lt.3.and.iendout(ibr).eq.istout(ibr)) 
     :         iline(ibr)=-1
         end if
         if (iline(ibr).gt.0) then
c            totala=0.0
c            do i=istart(ibr),iend(ibr)-1
c               totala=totala+xdist(x1(i),x1(i+1),y1(i),y1(i+1))
c            end do
c            if (totala.lt.??) iline(ibr)=-1
         end if
      end do
c
c     Now check that we have the same number of starts and ends
c
      if (isilent.gt.0) print*,' Checking number of ends '
      ist=0
      ien=0
      do ibr=1,ilines
         if (iline(ibr).ne.-1) then
            if (istout(ibr).lt.0) ist=ist+1
            if (iendout(ibr).lt.0) ien=ien+1
         end if
      end do
      if (ist.ne.ien) then
         print*,' Error: wrong no starts and ends ',
     :          icount_seq,ist,ien
         do ibr=1,ilines
            print*,ibr,iline(ibr),istart(ibr),iend(ibr)
         end do
         stop 1
      end if
c
c     Calculate total distance around outline
c
      totald=0.0
      do i=1,iout-1
         totald=totald+xdist(xout(i),xout(i+1),yout(i),yout(i+1))
      end do
      if (isilent.gt.0) print*,' total distance = ',totald
c
c
c     Now find out how to join the lines. First do the simple
c     ones
c
      if (isilent.gt.0) print*,' processing simple intersect '
      do ibr=1,ilines
         if (iline(ibr).ne.-1) then
            if (istout(ibr).lt.0) then
               distmn=1000.0
               do ibr1=1,ilines
                  if (iline(ibr1).ne.-1) then
                     if (iendout(ibr1).lt.0) then
                        dist=xdist(x1(istart(ibr)),
     :                             x1(iend(ibr1)),
     :                             y1(istart(ibr)),
     :                             y1(iend(ibr1)))
                        if (dist.lt.distmn) then
                           distmn=dist
                           itemp=ibr1
                        end if
                     end if
                  end if
               end do
               if (itemp.eq.0) then
                  print*,' Error: itemp = 0 in simple '
                  print*,ibr,distmn
                  stop 1
               end if
               ijoine(itemp)=ibr
            end if
         end if
      end do
c
      if (isilent.gt.0) then
         print*,' intersects so far '
         do ibr=1,ilines
            print*,ibr,iline(ibr),ijoine(ibr)
         end do
         if (isilent.gt.1) read(5,*)
      end if
c
c      Now do the tricky bit
c
      if (isilent.gt.0) print*,' Tricky bit now '   
      do ibr=1,ilines
         if (iline(ibr).ne.-1.and.iendout(ibr).gt.0) then
            distmn=1000.0
            do ibr1=1,ilines
               if (iline(ibr1).ne.-1.and.
     :                         istout(ibr1).gt.0) then
                  xaim=xmin1(ibr1)
                  yaim=ymin1(ibr1)
                  xst=xmin2(ibr)
                  yst=ymin2(ibr)
                  ic=iendout(ibr)
                  dist=0.0
c
                  do while (istout(ibr1).ne.ic) 
                     ic=ic+1
                     if (ic.gt.iout) ic=1
                     xen=xout(ic)
                     yen=yout(ic)
                     dist=dist+xdist(xst,xen,yst,yen)
                     xst=xen
                     yst=yen
                  end do
c
                  if (ic.eq.iendout(ibr)) then
                     x1out=xout(ic)
                     y1out=yout(ic)
                     ic1=ic+1
                     if (ic1.gt.iout) ic1=1
                     x2out=xout(ic1)
                     y2out=yout(ic1)
                     ang1=atan2((x2out-x1out),(y2out-y1out))
                     ang2=atan2((xaim-xst),(yaim-yst))
                     if (isilent.gt.0) print*,' angles ',
     :                  ang1,ang2,ang1-ang2
                     if (ang1-ang2.ne.0) then
                        dist=totald-xdist(xst,xaim,yst,yaim)
                     else
                        dist=dist+xdist(xst,xaim,yst,yaim)
                     end if
                  else 
                     dist=dist+xdist(xst,xaim,yst,yaim)
                  end if
c
                  if (isilent.gt.0) print*,ibr,ibr1,dist
                  if (dist.lt.distmn) then
                     distmn=dist
                     itemp=ibr1
                  end if
               end if
            end do
            if (isilent.gt.0) print*,' final result ',ibr,itemp
            ijoine(ibr)=itemp
         end if
      end do                         
c
      if (isilent.gt.0) then
         do ibr=1,ilines
            print*,' ibr,iline,istart,iend,iout '
            print*,ibr,iline(ibr),istart(ibr),iend(ibr),iout
            print*,' x1(istart),y1(istart) and ends '
            print*,x1(istart(ibr)),y1(istart(ibr)),
     :             x1(iend(ibr)),y1(iend(ibr))
            print*,' istout, iendout, ijoine '
            print*,istout(ibr),iendout(ibr),ijoine(ibr)
            if (istout(ibr).gt.0.and.iendout(ibr).gt.0) then
               print*,' xmin1,ymin1,xmin2,ymin2 '
               print*,xmin1(ibr),ymin1(ibr),xmin2(ibr),ymin2(ibr)
            else if (istout(ibr).gt.0) then
               print*,' xmin1,ymin1 '
               print*,xmin1(ibr),ymin1(ibr)
            else if (iendout(ibr).gt.0) then
               print*,' xmin2,ymin2 '
               print*,xmin2(ibr),ymin2(ibr)
            end if
        end do
      end if
c
      iloop=0
      icheck=0
      do ibr=1,ilines
c
         if (ijoine(ibr).le.0.and.iline(ibr).gt.0) then
            print*,' Error: ijoine should be > 0 ',
     :                    ibr,ijoine(ibr)
            read(5,*)
            stop 1
         end if
c
         if (iline(ibr).gt.0) icheck=1
      end do 
c
c
      do while (icheck.eq.1)
c
c        Find starting line
c
         ibr=1
         do while (iline(ibr).lt.0) 
            ibr=ibr+1
            if (ibr.gt.ilines) go to 800
         end do 
         ibrst=ibr
         itest=1
c
         do while (ibr.ne.ibrst.or.itest.eq.1) 
            itest=0
c
            do i=istart(ibr),iend(ibr)
               if (iloop.eq.idim) go to 800
               iloop=iloop+1
               xloop(iloop)=x1(i)
               yloop(iloop)=y1(i)
            end do
            iline(ibr)=-1
c
            if (iendout(ibr).gt.0) then
c
               if (x1(iend(ibr)).ne.xmin2(ibr).or.
     :             y1(iend(ibr)).ne.ymin2(ibr) ) then
                  if (iloop.eq.idim) go to 800
                  iloop=iloop+1
                  xloop(iloop)=xmin2(ibr)
                  yloop(iloop)=ymin2(ibr)
               end if
c
               if (iendout(ibr).ne.istout(ijoine(ibr))) then
                  ist=iendout(ibr)
                  do while (ist.ne.istout(ijoine(ibr)))
                     ist=ist+1
                     if (ist.gt.iout) ist=1
                     if (iloop.eq.idim) go to 800
                     iloop=iloop+1
                     xloop(iloop)=xout(ist)
                     yloop(iloop)=yout(ist)
                  end do
               end if
               ibr=ijoine(ibr)
c               
               if (x1(istart(ibr)).ne.xmin1(ibr).or.
     :             y1(istart(ibr)).ne.ymin1(ibr) ) then
                  if (iloop.eq.idim) go to 800
                  iloop=iloop+1
                  xloop(iloop)=xmin1(ibr)
                  yloop(iloop)=ymin1(ibr)
               end if
c
            else if (iendout(ibr).lt.0) then
c
               ibr=ijoine(ibr)
c
            end if
c
         end do
c
         if (iloop.gt.0) then
            call zfilln(xloop,yloop,iloop)
            iloop=0
         end if
c
         icheck=0
         do ibr=1,ilines
            if (iline(ibr).gt.0) icheck=1
         end do 
c
      end do
c
 800  continue
c
      if (iloop.gt.0) then
         call zfilln(xloop,yloop,iloop)
         iloop=0
      end if
c
      return
      end
