c
c diag.f diagnostics for program goldstein variable depth
c lmax > 2 allowed, 27/5/2       
c
      subroutine diag

      include 'var.cmn'

      real*8 sum(maxl), sumsq(maxl), umax(3), cnmax, avp, avs, vol, tmax
c$$$     1   ,tmin, ubmax(2), enmax(maxk), rdmn, tv, tmax1, tmin1, tv1, tv2
     1   ,tmin, ubmax(2), tv, tmax1, tmin1, tv1, tv2

c$$$      integer i, j, k, l, imm(3,3), icp, icp2, isl, icp10, ird, jrd
c$$$     1      , ien(maxk), jen(maxk)
      integer i, j, k, l, imm(3,3), icp, icp2, isl, icp10

      vol = 0.0
      do l=1,lmax
         sum(l) = 0.0
         sumsq(l) = 0.0
      enddo       
      do i=1,3
         umax(i) = 0
         do j=1,3
            imm(i,j) = 0
         enddo
      enddo
      cnmax = 0
      tv = 0
      icp = 0
      icp2 = 0
      icp10 = 0
      avp = 0
      avs = 0
      cnmax = 0
      ubmax(1) = 0
      ubmax(2) = 0
      do 10 k=1,kmax
c        cnmax = 0
c        cnvax = 0
         do 20 j=1,jmax
            do 30 i=1,imax
               if(k.ge.k1(i,j))then
                  vol = vol + dphi*ds*dz(k)
                  do l=1,lmax
                     sum(l) = sum(l) + ts(l,i,j,k)*dphi*ds*dz(k)
                     sumsq(l) = sumsq(l) + ts(l,i,j,k)**2*dphi*ds*dz(k)
                  enddo   
                  do 40 l=1,3
c                    if(l.eq.1.and.ts(l,i,j,k).eq.0)print*,i,j,k,'zero'
                     if(abs(u(l,i,j,k)).gt.umax(l))then
                        umax(l)=abs(u(l,i,j,k))
                        imm(1,l) = i
                        imm(2,l) = j
                        imm(3,l) = k
                     endif
   40             continue
c                 tv = max(abs(u(1,i,j,k))/dphi,abs(u(2,i,j,k))/ds
c    1                 ,abs(u(3,i,j,k))/dz(k))*dt(k)
                  tv = max(abs(u(1,i,j,k))*rc(j)*rdphi
     1                 ,abs(u(2,i,j,k))*cv(j)*rds
     1                 ,abs(u(3,i,j,k))*rdz(k))*dt(k)
                  if(tv.gt.cnmax)cnmax = tv
c                 tv = abs(u(3,i,j,k))/dz(k)*dt(k)
c                 if(tv.gt.cnvax)cnvax = tv

c                 if(ts(1,i,j,k).lt.-0.001)then
c                    print*,'T<0 at',i,j,k
c                    stop
c                 endif

                  avs = avs + u(1,i,j,k)**2 + u(2,i,j,k)**2

c                 tv = abs(u(1,i,j,k)*dza(k)*dza(k)/c(j)/dphi/diff(2))
c                 tv = abs(u(2,i,j,k)*dza(k)*dza(k)*c(j)/ds/diff(2))
                  tv = abs(u(3,i,j,k)*dza(k)/diff(2))
                  avp = avp + tv
                  if(tv.gt.2)then
                     icp = icp + 1
                     if(tv.gt.4)icp2 = icp2 + 1
                     if(tv.gt.20)icp10 = icp10 + 1
                  endif
               endif
   30       continue
   20    continue
c        if(k.eq.1.or.k.eq.kmax)
c    1   print*,'k dt cn ',k,dt(k),cnmax
   10 continue
c     write(6,'(5e10.3)')(((ts(2,i,j,k),i=2,10,2),j=2,10,2),k=2,10,2)

      tmax1 = -1e10
      tmin1 = 1e10
      do i=1,imax
         do j=1,jmax
            if(k1(i,j).eq.1)then
               if(ts(1,i,j,1).gt.tmax1)tmax1=ts(1,i,j,1)
               if(ts(1,i,j,1).lt.tmin1)tmin1=ts(1,i,j,1)
            endif
         enddo
      enddo

      tmax = -1e10
      tmin = 1e10
      do i=1,imax
         do j=1,jmax
            if(k1(i,j).le.kmax)then
               if(ts(1,i,j,kmax).gt.tmax)tmax=ts(1,i,j,kmax)
               if(ts(1,i,j,kmax).lt.tmin)tmin=ts(1,i,j,kmax)
            endif
         enddo
      enddo

c find max barotropic velocity components

      do j=1,jmax
         do i=1,imax
            do l=1,2
               if(abs(ub(l,i,j)).gt.ubmax(l))ubmax(l) = abs(ub(l,i,j))
            enddo
         enddo
      enddo

c write out SST and SSS

      tv1 = 0
      tv2 = 0
      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax)then
               tv1 = tv1 + ts(1,i,j,kmax)
               tv2 = tv2 + ts(2,i,j,kmax)
            endif
         enddo
      enddo
      tv1 = tv1/(ntot - intot)
      tv2 = tv2/(ntot - intot)
      write(6,*)'average SST',tv1
      write(6,*)'average SSS',tv2

c find density differences 

c     rdmn = rho(1,1,k1(1,1))-rho(1,1,kmax)
c     do j=1,jmax
c        do i=1,imax
c          if(k1(i,j).le.kmax)then
c           if(rho(i,j,k1(i,j))-rho(i,j,kmax).lt.rdmn)then
c              rdmn = rho(i,j,k1(i,j))-rho(i,j,kmax)
c              ird = i
c              jrd = j
c           endif
c           endif
c        enddo 
c     enddo
c     do k=1,kmax-1
c        enmax(k) = -100.0                           
c        do j=1,jmax
c           do i=1,imax
c              en = (rho(i,j,k+1)-rho(i,j,k))/dza(k)
c              if(k.ge.k1(i,j).and.en.gt.enmax(k))then
c                 enmax(k) = en
c                 ien(k) = i
c                 jen(k) = j
c              endif
c           enddo
c        enddo 
c     enddo

      print*,'max and min T at kmax ',tmax,tmin
      print*,'max and min T at k=1  ',tmax1,tmin1
      print*,'<T>, <S> ..., <T**2>, <S**2> ...'
     1      ,(sum(l)/vol,l=1,lmax),(sumsq(l)/vol,l=1,lmax)
c     print*,'ts(1,3,3,4)'
      print*,'ts(1,1,1,1)'
     1      ,ts(1,1,1,1)  
      print*,'Cn Kh*dt/dx**2 Kv*dt/dz**2'
     1      ,cnmax,diff(1)/min(ds,dphi)**2 *dt(kmax),
     1             diff(2)/dz(kmax)**2 *dt(kmax)
      print*,'Dmax',dmax
      print*,'flux limited at ',limps,' points'
      print*,'max absolute velocities'
     1      ,((imm(i,l),i=1,3),umax(l),l=1,3)
      print*,'Peclet number exceeds 2  ',icp,' times'
c     print*,'Peclet number exceeds 4  ',icp2,' times'
      print*,'Peclet number exceeds 20 ',icp10,' times'
      print*,'average Peclet number ',avp/ntot          
      print*,'maximum horizontal Peclet nos. ',
     1 umax(1)*dphi/diff(1),umax(2)*ds/diff(1)
      print*,'r.m.s. horiz. flow speed ',sqrt(avs/ntot)          
      print*,'max barotropic velocities',(ubmax(i),i=1,2)
c     print*,'min top to bottom density diff',rdmn,ird,jrd
c     write(6,'(i4,e12.5,2i4)')(k,enmax(k),ien(k),jen(k),k=1,12)
c isl=isles+1 path is purely for testing consistency of scheme
c     do isl=1,isles+1
      do isl=1,isles
         call island(ub,tv,isl,1)
         print*,'path integral error on island ',isl,tv
      enddo

c test to find appropriate forcings

c     do k=16,14,-1
c        do j=1,jmax
c           tmp(k,j) = 0
c           do i=1,imax
c              tmp(k,j) = tmp(k,j) + ts(1,i,j,k)
c           enddo
c           tmp(k,j) = tmp(k,j)/imax
c           print*,k,j,tsa(1,1,j)-tmp(k,j),tmp(16,j)-tmp(k,j)
c        enddo
c     enddo
               
c check inversion for barotropic streamfunction

c     n = imax-1
c     do 50 i=1,imax-1
c        do 50 j=1,jmax-1
c           tv = 0
c           k = i + (j-1)*(imax-1)
c           if(j.gt.1)
c    1         tv = tv + gapold(k,2)*gb(k-n)
c           if(i.gt.1)
c    1         tv = tv + gapold(k,n+1)*gb(k-1)

c           tv = tv + gapold(k,n+2)*gb(k)

c           if(i.lt.imax-1)
c    1         tv = tv + gb(k+1)*gapold(k,n+3)
c           if(j.lt.jmax-1)
c    1         tv = tv + gb(k+n)*gapold(k,2*n+2)
c           tv = tv - gbold(k)
c           if(abs(tv).gt.1e-7)print*,i,j,k,tv,gbold(k)
c  50 continue

      end
