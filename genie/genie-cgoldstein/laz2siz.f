c laz2siz.m matlab program to convert 3-D gridded data to goldstein grid 
c set up for Levitus data
c dum is the land mask value, values close to dum are assumed to be land
c far=.false. if don't look outside surrounding cell 

      implicit none
      integer imax,jmax,kmax,nx,ny,nz,i,j,k,is,js,ks,ii,ji,ki,int,near
     &       ,none,imin,jmin,kmin,iw,jw,kw,isgn,nn

c     parameter(imax = 36,jmax = 36,kmax = 8,nx = 360,ny = 180,nz = 3)
      parameter(imax = 36,jmax = 36,kmax = 8,nx = 360,ny = 180,nz = 33)

      real dsc,th0,th1,phix,dum,ez0,z1,tv1,tv2,tv3,tv4,tv5
     &    ,s0,s1,dphi,ds,pi,tmin,a,b,c,dmin,dw,tv,dx,dy
      real xlev(nx),ylev(ny),zlev(nz),xgg(imax),ygg(jmax),zgg(kmax)
      real dz(kmax),dza(kmax),tlev(nx,ny,nz),tgg(imax,jmax,kmax)
     &    ,dist(2,2)

      logical far

      parameter(far=.true. )

      pi=4*atan(1.0)

      dsc = 5000.0
      th0 = - pi/2
      th1 = pi/2
      phix = 2*pi

      dum = -99.9999

c positions for 1-degree data in degrees
      do i=1,nx
         xlev(i) = - 0.5 + 1.0*i
      enddo
      do j=1,ny
         ylev(j) = - 90.5 + 1.0*j
      enddo

c test at nz=3
c     data zlev/  0.0,  1000.0, 5500.0/

      data zlev/  0.0,  10.0,  20.0,  30.0,  50.0,  75.0, 100.0, 125.0,
     &          150.0, 200.0, 250.0, 300.0, 400.0, 500.0, 600.0, 700.0,
     &          800.0, 900.0,1000.0,1100.0,1200.0,1300.0,1400.0,1500.0,
     &         1750.0,2000.0,2500.0,3000.0,3500.0,4000.0,4500.0,5000.0,
     &         5500.0/

c convert to right-handed coordinates, vertical upwards
      do k=1,nz
         zlev(k) = - zlev(k)
         print*,'zlev ',zlev(k)
      enddo

cFor variable (exponential) dz use ez0 > 0, else use ez0 < 0

      ez0 = 0.1
c     ez0 = - 1.0
      z1 = ez0*((1.0 + 1/ez0)**(1.0/kmax) - 1.0)
      print*,'z1',z1
      tv4 = ez0*((z1/ez0+1)**0.5-1)
      tv2 = 0
      tv1 = 0
      zgg(kmax) = -tv4
c     zw(kmax) = tv2
      do k=1,kmax
         if(ez0.gt.0)then
            tv3 = ez0*((z1/ez0+1)**k-1)
            dz(kmax-k+1) = tv3 - tv2
            tv2 = tv3
            tv5 = ez0*((z1/ez0+1)**(k+0.5)-1)
            if(k.lt.kmax)dza(kmax-k) = tv5 - tv4
            tv4 = tv5
            tv1 = tv1 + dz(kmax-k+1)
c tv3 is the depth of the kth w level from the top
c tv5 is the depth of the k+1th density level from the top
         else
            dz(k) = 1d0/kmax
            dza(k) = 1d0/kmax
         endif
      enddo

      do k=kmax,2,-1
         zgg(k-1) = zgg(k) - dza(k-1)
c        zw(k-1) = zw(k) - dz(k)
      enddo
      write(6,'(i4,3e11.4)')(k,zgg(k),dz(k),dza(k),k=kmax,1,-1)

      do k=1,kmax   
         zgg(k) = dsc * zgg(k) 
         print*,'zgg ',k,zgg(k)
      enddo

      s0 = sin(th0)
      s1 = sin(th1)
      dphi = phix/imax
      ds = (s1-s0)/jmax

      do i=1,imax
         xgg(i) = 180 * (- 0.5 + i)*dphi / pi
c        print*,'golds. x-points ',i,xgg(i)
      enddo
      do j=1,jmax
         ygg(j) = 180 * asin(s0 + (- 0.5+j)*ds)/pi
         print*,'golds. y-points ',j,ygg(j)
     &         ,180 * asin(s0 + j*ds)/pi
      enddo

      open(1,file='tempann.data')
c     open(1,file='saliann.data')
      do k=1,nz    
        do j=1,ny  
          read (1,'(10f8.4)') (tlev(i,j,k),i=1,nx)
        enddo
        PRINT *,' Levitus data read for level ',k
      enddo
      close(1)
c write out SST for test

      open(2,file='sst.tmp')
      do j=2,ny,2
         write(2,'(f11.4)')(tlev(i,j,1),i=102,nx,2)
         write(2,'(f11.4)')(tlev(i,j,1),i=2,100,2)
      enddo
c     write(2,'(f11.4)')((tlev(i,j,1),i=10,nx,10),j=5,ny,5)
      close(2)

c interpolate from lat-long-z grid to goldstein grid
c first use trilinear interpolation if all 8 surrounding points wet

      int = 0
      near = 0
      none = 0
      do k=1,kmax
         do j=1,jmax
            do i=1,imax
c first locate goldstein point (i,j,k) on Levitus grid between is-1 and is
c              print*,i,j,k,xgg(i),ygg(j),zgg(k)
               is = 1
               dowhile(xlev(is).lt.xgg(i).and.is.le.nx)
                  is = is + 1
               enddo
               if(is.eq.1.or.is.gt.nx)stop 'x out of range'
               js = 1
               dowhile(ylev(js).lt.ygg(j).and.js.le.ny)
                  js = js + 1
               enddo
               if(js.eq.1.or.js.gt.ny)stop 'y out of range'
               ks = 1
c zlev decreases in k
               dowhile(zlev(ks).gt.zgg(k).and.ks.le.nz)
                  ks = ks + 1
               enddo
               if(ks.eq.1.or.ks.gt.nz)stop 'z out of range'
c              print*,is,js,ks,xlev(is),ylev(js),zlev(ks)
c              print*,is-1,js-1,ks-1,xlev(is-1),ylev(js-1),zlev(ks-1)
c test for land points 
               tmin = min(tlev(is,js,ks),tlev(is-1,js,ks)
     &              ,tlev(is,js-1,ks),tlev(is,js,ks-1)
     &              ,tlev(is-1,js-1,ks),tlev(is,js-1,ks-1)
     &              ,tlev(is-1,js,ks-1),tlev(is-1,js-1,ks-1))
               if(abs(tmin-dum).gt.1e-4)then
c assume no land in surrounding cell and interpolate
                  a = (xgg(i) - xlev(is-1))/(xlev(is) - xlev(is-1))
                  b = (ygg(j) - ylev(js-1))/(ylev(js) - ylev(js-1))
                  c = (zgg(k) - zlev(ks-1))/(zlev(ks) - zlev(ks-1))
                  tgg(i,j,k) = (1.0-c)*((1.0-a)*(
     &              (1.0-b)*tlev(is-1,js-1,ks-1) + b*tlev(is-1,js,ks-1))
     &            + a*((1.0-b)*tlev(is,js-1,ks-1) + b*tlev(is,js,ks-1)))
     &    + c*((1.0-a)*((1.0-b)*tlev(is-1,js-1,ks) + b*tlev(is-1,js,ks))
     &                + a*((1.0-b)*tlev(is,js-1,ks) + b*tlev(is,js,ks)))
                  int = int + 1
               else

c find nearest point
c better to always use same interpolated vertical level as vertical gradients 
c are likely to be much larger than horizontal

c                 print*,'land in cell at ',i,j,k
                  dmin = 360*360
                  dw = dmin
                  do ii=1,2 
                     do ji=1,2 
                        dist(ii,ji) = (xgg(i) - xlev(is-2+ii))**2
     &                              + (ygg(j) - ylev(js-2+ji))**2
                        tv1 = tlev(is-2+ii,js-2+ji,ks)
                        tv2 = tlev(is-2+ii,js-2+ji,ks-1)
                        tv3 = min(tv1,tv2)
                        if(dist(ii,ji).lt.dw.and.
     &                     abs(tv3-dum).gt.1e-4)then
                           dw = dist(ii,ji)
                           tgg(i,j,k) = (1.0-c)*tv2 + c*tv1
c                          iw = is-2+ii
c                          jw = js-2+ji
                        endif
                        if(dist(ii,ji).lt.dmin)then
                           dmin = dist(ii,ji)
                           imin = is-2+ii
                           jmin = js-2+ji
                        endif
                     enddo
                  enddo
                  if(dw.lt.359*359)then
c done
                     near = near + 1
c                    print*,'nearby wet point used'
                  elseif(far)then

c now know that imin,jmin,kmin is the nearest point and that it is dry 
c next look along x, y directions for a nearby wet point
c start at the nearest point (the next point may be out of range):
c then do leadfrog search

                     nn = 0
                     isgn = 1
                     ii = imin
                     tv1 = tlev(ii,jmin,ks)
                     tv2 = tlev(ii,jmin,ks-1)
                     tv3 = min(tv1,tv2)
                     dowhile(abs(tv3-dum).lt.1e-4.and.nn.le.nx)
                        nn = nn + 1
                        isgn = - isgn
                        ii = ii + isgn*nn
c                       print*,nn
c                       print*,'search for wet pt in x',ii,jmin,ks
                        if(ii.ge.1.and.ii.le.nx)then
                           tv1 = tlev(ii,jmin,ks)
                           tv2 = tlev(ii,jmin,ks-1)
                           tv3 = min(tv1,tv2)
                        endif
                     enddo
                     dx = (xgg(i) - xlev(ii))**2
     &                  + (ygg(j) - ylev(jmin))**2
                     tgg(i,j,k) = (1.0-c)*tv2 + c*tv1
                     dmin = dx
c y-direction
                     nn = 0
                     isgn = 1
                     ii = imin
                     ji = jmin 
                     tv1 = tlev(imin,ji,ks)
                     tv2 = tlev(imin,ji,ks-1)
                     tv3 = min(tv1,tv2)
                     dowhile(abs(tv3-dum).lt.1e-4.and.nn.le.ny)
                        nn = nn + 1
                        isgn = - isgn
                        ji = ji + isgn*nn
c                       print*,nn
c                       print*,'search for wet pt in y',imin,ji,ks
                        if(ji.ge.1.and.ji.le.ny)then
                           tv1 = tlev(imin,ji,ks)
                           tv2 = tlev(imin,ji,ks-1)
                           tv3 = min(tv1,tv2)
                        endif
                     enddo
                     dy = (xgg(i) - xlev(imin))**2
     &                  + (ygg(j) - ylev(ji))**2
                     if(dy.lt.dx)then
                        tgg(i,j,k) = (1.0-c)*tv2 + c*tv1
                     endif
                     none = none + 1
                  else
                     tgg(i,j,k) = 0.0
                  endif
               endif
c              print*,'totals ',int,near,none,int+near+none
c              print*,'total points in grid ',imax*jmax*kmax
            enddo
         enddo
      enddo
      print*,'interpolated, nearest adjacent, non-adjacent, total'
      print*,'totals ',int,near,none,int+near+none
      print*,'total points in grid ',imax*jmax*kmax
c write out SST for test

      open(2,file='gss.tmp')
      do j=1,jmax
         write(2,'(f11.4)')(tgg(i,j,kmax),i=11,imax)
         write(2,'(f11.4)')(tgg(i,j,kmax),i=1,10  )
      enddo
      close(2)

c write results

      open(3,file='tempann.silo')
c     open(3,file='saliann.silo')
      do j=1,jmax
         write(3,'(f11.4)')((tgg(i,j,k),k=1,kmax),i=11,imax)
         write(3,'(f11.4)')((tgg(i,j,k),k=1,kmax),i=1,10  )
      enddo
      close(3)
      end   
