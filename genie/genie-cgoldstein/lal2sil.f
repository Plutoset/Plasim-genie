c lal2sil.f program to convert 2-D gridded data to goldstein grid 
c dum is the land mask value, values close to dum are assumed to be land
c rotation corrected to 100 degrees E from 80 degrees E 22/10/2

      implicit none
      integer imax,jmax,nx,ny,i,j,is,js,ii,ji,int,near
     &       ,none,imin,jmin,iw,jw,isgn,nn,ind

      parameter(imax = 36,jmax = 36,nx = 144,ny = 73 )

      real th0,th1,phix,dum,xtv,ytv
     &    ,s0,s1,dphi,ds,pi,tmin,a,b
      real xlev(nx),ylev(ny),xgg(imax),ygg(jmax)
      real tlev(nx,ny),tgg(imax,jmax),offx,offy
      character(len=80) infile

      pi=4*atan(1.0)

      th0 = - pi/2
      th1 = pi/2
      phix = 2*pi

      dum = -99.9999

c positions for data in degrees
      do i=1,nx
         xlev(i) = 2.5*(i-1)
      enddo
      do j=1,ny
         ylev(j) = - 90 + 2.5*(j-1)
      enddo

      s0 = sin(th0)
      s1 = sin(th1)
      dphi = phix/imax
      ds = (s1-s0)/jmax

      print*,'rho (1) u (2) or v (3) data output?'
      read(5,*)ind
      if(ind.eq.1)then
         offx = 0.
         offy = 0.
      elseif(ind.eq.2)then
         offx = 0.5
         offy = 0.0
      elseif(ind.eq.3)then
         offx = 0.0
         offy = 0.5
      endif

      do i=1,imax
         xgg(i) = 180 * (- 0.5 + i + offx)*dphi / pi
c        print*,'golds. x-points ',i,xgg(i)
      enddo
      do j=1,jmax
         ygg(j) = 180 * asin(s0 + (- 0.5 + j + offy)*ds)/pi
         print*,'golds. y-points ',j,ygg(j)
     &         ,180 * asin(s0 + j*ds)/pi
      enddo

c test for pole
    
      if (abs(ygg(jmax)-90.0).lt.1e-9)then
         ygg(jmax) = 89.999
         print*,'unrealistic polar point shifted away from pole'
      endif

      print*,'input file name'
      read(5,'(a)')infile

      open(1,file=infile)
      do j=1,ny  
         do i=1,nx
            read (1,*)tlev(i,j)
c           read (1,*)xtv,ytv,tlev(i,j)
c           if(max(abs(xtv-xlev(i)),abs(ytv-ylev(j))).gt.1e-12)
c    1             stop 'error'
c           print*,xtv,ytv,xlev(i),ylev(j),tlev(i,j)
         enddo
      enddo
      PRINT *,'data read '
      close(1)

c write out for test

      open(2,file='tmp.dat')
      do j=1,ny
         write(2,'(f11.4)')(tlev(i,j),i=41,nx)
         write(2,'(f11.4)')(tlev(i,j),i=1,40)
      enddo
c     write(2,'(f11.4)')((tlev(i,j,1),i=10,nx,10),j=5,ny,5)
      close(2)

c interpolate from lat-long grid to goldstein grid
c first use  bilinear interpolation if all 8 surrounding points wet

      int = 0
      near = 0
      none = 0
      do j=1,jmax
         do i=1,imax
c first locate goldstein point (i,j) on Levitus grid between is-1 and is
c           print*,i,j,xgg(i),ygg(j)
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
c test for land points 
            tmin = min(tlev(is,js),tlev(is-1,js)
     &           ,tlev(is,js-1),tlev(is-1,js-1))
            if(abs(tmin-dum).gt.1e-4)then
c assume no land in surrounding cell and interpolate
               a = (xgg(i) - xlev(is-1))/(xlev(is) - xlev(is-1))
               b = (ygg(j) - ylev(js-1))/(ylev(js) - ylev(js-1))
               tgg(i,j) = (1.0-a)*(
     &           (1.0-b)*tlev(is-1,js-1) + b*tlev(is-1,js))
     &         + a*((1.0-b)*tlev(is,js-1) + b*tlev(is,js))
               int = int + 1
            else
               stop 'not coded this option'
            endif
         enddo
      enddo
      print*,'total points in grid ',imax*jmax

c write results

      open(2,file='tmp.silo')
      do j=1,jmax
         write(2,'(f11.4)')(tgg(i,j),i=11,imax)
         write(2,'(f11.4)')(tgg(i,j),i=1,10  )
      enddo
      close(2)
      end   
