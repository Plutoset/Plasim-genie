c
c program to average real data on lat-long grid onto coarser sin(lat)-
c long grid for goldstein 15/5/2
c
c modified from etopo5 integer data treatment program
c istep and jstep give the average number of nth degree averaged data into 
c each box eg 12 for 12th degree input and 180x360 output points
c offin = offset for input data = 0 if data at long-lat points, 0.5 if data
c         is at 1/2 degree points
c offx,offy = offset for output = (0,0) if data at rho points, 
c             (0.5,0) for u pts (0,0.5) for v points
c
      program lalo2silo

      implicit none
      integer nlat,nlon,nths,ind,i1,i2,j1,j2
      integer istep,jstep,iav,jav,maxj,maxi
      parameter (nths=1,nlat=180*nths, nlon=360*nths )
      parameter (maxi=36,maxj=36,istep=nlon/maxi,jstep=nlat/maxj)

      real topo(nlon,nlat), stopo(maxi,maxj), fact, fwet, offin
      real offx, offy, th0, th1, tmp

      integer jsv(0:maxj),js(0:maxj)

      real pi,s0,s1,ds,sv(0:maxj),s(0:maxj)

      integer iin, i, j
      character*80 datafile
      character*80 outfile

c test domain is integer no of averaging intervals

      if(istep*maxi.ne.nlon)stop 'bad domain'
      if(jstep*maxj.ne.nlat )stop 'bad domain'

      offin = 0.5
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
      print*,'offsets offin offx offy ',offin,offx,offy
      iin=52
      write(6,*) 'input  file'
      read(5,'(a80)') datafile
      write(6,*) datafile
      open(iin,file=datafile,status='old')
      read(iin,*)((topo(i,j),i=1,nlon),j=1,nlat)
      close(iin)

c set up sin factors at rho and v points (c grid)
c find nearest data point which is where 
c ''(offin + jdata)*1/nths = theta(j)''

      pi=4*atan(1.0)
      th0  = - 90.0
      th1  = 90.0
c     th0 = - pi*7/18
c     th1 = pi*7/18
      s0 = sin(th0 *pi/180)
      s1 = sin(th1 *pi/180)
      ds = (s1-s0)/maxj
      do 10 j=0,maxj
         sv(j) = s0 + j*ds
         s(j) = sv(j) - 0.5*ds
         jsv(j) = nint(180*asin(sv(j))/pi*nths-offin) - nint(th0 *nths)
         if(j.ge.1)
     &   js(j) = nint(180*asin(s(j))/pi*nths-offin) - nint(th0 *nths)
         print*,j,sv(j),jsv(j),s(j),js(j)
   10 continue

c average on to (coarser) sin theta grid

      do j=1,maxj
         do i=1,maxi
            fwet = 0.0
            tmp = 0.0
            if(offy.lt.0.25)then
               j1 = max(1,jsv(j-1))
               j2 = jsv(j)
            else
               j1 = js(j)
               j2 = min(nlat,js(j+1))
            endif
            i1 = max(0,nint((i-1+offx)*istep)) 
            i2 = min(nlon,nint((i+offx)*istep))
            do jav=j1,j2
               do iav=i1,i2
                  fact = 1.0
                  if(iav.eq.i1.or.iav.eq.i2.or.jav.eq.j1.or.jav.eq.j2)
     &               fact = 0.5
                  tmp = tmp + topo(iav,jav)*fact
                  fwet = fwet + fact
c                 print*,i,j,iav,jav,topo(iav,jav),tmp,fact,fwet
               enddo
            enddo
            if(fwet.gt.0.25)then
               stopo(i,j) = tmp/fwet
            else
               stopo(i,j) = 0
               print*,'no values in averaging interval at ',i,j 
            endif
            print*
            print*,i ,j,stopo(i,j),fwet
         enddo
      enddo

      write(6,*) 'output file'
      read(5,'(a80)') outfile
      write(6,*) outfile

      open(53,file=outfile)

      print*,'writing data e15.5 (appropriate for SOC winds)'
      print*,'rotating grid 8 gridpoints west'
      do j=1,maxj
         print*,j
c        write(53,'(1e15.5)') (stopo(i,j),i=1,maxi)
         write(53,'(1e15.5)') (stopo(maxi-8+i,j),i=1,8),
     &                        (stopo(i-8,j),i=9,maxi)
      enddo

      close(53)

      end
