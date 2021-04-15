* 
* subroutine gseto, sets up geometry etc variable depth
* copied from v3_1 at 28/8/2 with coupled stuff added  
* eqn of state coeff error corrected 13/11/6 nre
*
      subroutine gseto

      include 'var.cmn'

      real phix, th0, th1, z1, tv, tv1, tv2, tv3, tv4, tv5,
     2     zro(maxk), zw(0:maxk), temp0, temp1, adrag,
     3     drgf, s0, s1
cAR
c     real ta0
      real h(3,0:maxi+1,0:maxj+1)

      integer i,j,k,l,kmxdrg,jeb

      logical getj(maxi,maxj)

      common /lars/getj

      character(len=6) world
      parameter(world='worbe2')
cAR   parameter(world='world1')

      pi = 4*atan(1.0)

c dimensional scale values

      usc = 0.05
      rsc = 6.37e6
      dsc = 5e3
      fsc = 2*7.2921e-5
      gsc = 9.81
      rh0sc = 1e3
      rhosc = rh0sc*fsc*usc*rsc/gsc/dsc
      cpsc = 3981.1
      tsc = rsc/usc
      syr = 86400.0*365.25
      daysperyear = syr/(86400.0)

c EMBM scaling for heat forcing of ocean

      rfluxsc = rsc/(dsc*usc*rh0sc*cpsc)

c EMBM reference salinity

      saln0 = 34.9

c EMBM scaling for freshwater forcing of ocean

      rpmesco = rsc*saln0/(dsc*usc)

c parameters for setting up grid
c th is latitude, coords are sin(th), longitude phi, and z

      th0 = - pi/2    
      th1 = pi/2 
c     th0 = - pi*7/18
c     th1 = pi*7/18
      s0 = sin(th0)    
      s1 = sin(th1)     
c     phix = pi/3
      phix = 2*pi

c grid dimensions must be no greater than array dimensions in var.cmn

      imax = 36
      jmax = 36 
      kmax = 8   
      lmax = 2
c ocean positions (may be overwritten later in this routine)
c     if (imax.eq.18) then
c        ips = 2
c        ipf = 7
c        ias = 10
c        iaf = 12
c        jsf = 3
c     else
c        print*,'unknown ocean positions'
c        stop
c     endif

      dphi = phix/imax
      ds = (s1-s0)/jmax
      dphi2 = dphi*2
      ds2 = ds*2
      rdphi = 1.0/dphi
      rds = 1.0/ds

c set time to zero (may be overwritten if continuing run)

      t0 = 0
      t = t0

c set timestep and initialize some 1-d arrays to zero

c     print*,'ocean dt in days and A/O dt ratio'
c     read(5,*)tv,ndta
c     tv = tv*86400/tsc
c v2 seasonality
      print*,'timesteps per year and A/O dt ratio'
      read(5,*)nyear,ndta
      if(nyear.gt.maxnyr)stop 'nyear > maxnyr' 
c     tv = 86400.0*365.25/(nyear*tsc)
      tv = syr/(nyear*tsc)

      dtatm = tv/ndta
c variable timestep option not recommended
      do k=1,kmax
         dt(k) = tv  
c initialize
         dzu(1,k) = 0
         dzu(2,k) = 0
      enddo
      print*,'dimensional ocean timestep',tv*tsc/86400
      print*,'dimensionless O/A timesteps',tv,dtatm

      rdtdim = 1.0/(tsc*dt(kmax))

c set up grid
c For variable (exponential) dz use ez0 > 0, else use ez0 < 0

      ez0 = 0.1
c     ez0 = - 1.0
      z1 = ez0*((1.0 + 1/ez0)**(1.0/kmax) - 1.0)
      print*,'z1',z1
      tv4 = ez0*((z1/ez0+1)**0.5-1)
      tv2 = 0
      tv1 = 0
      zro(kmax) = -tv4
      zw(kmax) = tv2
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

      do k=kmax,1,-1
         if(k.gt.1)zro(k-1) = zro(k) - dza(k-1)
         zw(k-1) = zw(k) - dz(k)
      enddo
c     write(6,'(i4,4e11.4)')(k,zw(k),zro(k),dz(k),dza(k),k=kmax,1,-1)
c     write(6,'(i4,e11.4)')k,zw(0)
c write dimensional vertical grid 
      write(6,'(i4,3e12.4)')(k,dsc*zw(k),dsc*zro(k),dsc*dz(k)
     1        ,k=kmax,1,-1)
         
      dzz = dz(kmax)*dza(kmax-1)/2  

c efficiency array

      do k=1,kmax-1
         rdz(k) = 1.0/dz(k)
         rdza(k) = 1.0/dza(k)
      enddo
      rdz(kmax) = 1.0/dz(kmax)

c dza(kmax) never referenced, set to 0 for Andy's biogeo-code

      dza(kmax) = 0.

c set up sin and cos factors at rho and v points (c grid)
c fix for global domain although only cv and cv2 are referred to at or beyond
c limits 24/6/2 if no flow out of N + S boundaries.

      do j=0,jmax
         sv(j) = s0 + j*ds
         if(abs(1.0 - abs(sv(j))).lt.1e-12)then
            cv(j) = 0.
            rcv(j) = 1e12
         else
            cv(j) = sqrt(1 - sv(j)*sv(j))
            rcv(j) = 1.0/cv(j)
         endif
         cv2(j) = cv(j)*cv(j)*rds
         s(j) = sv(j) - 0.5*ds
         if(s(j).lt.-1.0)s(j) = -2.0 - s(j)
         c(j) = sqrt(1 - s(j)*s(j))
         rc(j) = 1.0/c(j)
         rc2(j) = rc(j)*rc(j)*rdphi
c       print*,'s TEST f-PLANE FUDGE'
c        sv(j) = 0.5
c        s(j) = 0.5
c        print*,j,sv(j),s(j),c(j),cv(j)
      enddo   

c set up coeffs for state equation following WS 1993

c error: rhosc value assumed dsc=4 km corrected 13/11/6 nre
c     ec(1) = - 0.0559 /1.18376
c     ec(2) = 0.7968   /1.18376
c     ec(3) = - 0.0063 /1.18376
c     ec(4) = 3.7315e-5/1.18376
      ec(1) = - 0.0559 /rhosc
      ec(2) = 0.7968   /rhosc
      ec(3) = - 0.0063 /rhosc
      ec(4) = 3.7315e-5/rhosc

c read parameters 

      print*,'temp0 temp1 rel scf'
      read(5,*)temp0,temp1,rel,scf
      print*,temp0,temp1,rel,scf
      print*,'diff(1),diff(2)'
      read(5,*)diff(1),diff(2)    
      print*,diff(1),diff(2)
      print*,'inverse minimum drag in days'
      read(5,*)adrag
      print*,adrag

c define forcing

c read wind data

c taux,tauy at u-points
      open(96,file='taux_u.interp')
      open(97,file='tauy_u.interp')
c taux,tauy at v-points
      open(98,file='taux_v.interp')
      open(99,file='tauy_v.interp')

      do j=1,jmax
         do i=1,imax
c rotate grid to check b.c.s
c        do idum=1,imax
c           i=1+mod(idum+18-1,36)

            read(96,*)dztau(1,i,j)
            read(97,*)dztau(2,i,j)
            read(98,*)dztav(1,i,j)
            read(99,*)dztav(2,i,j)

c multiply by scaling factor scf (to drive correct gyre strengths)

            dztau(1,i,j) = scf*dztau(1,i,j)/(rh0sc*dsc*usc*fsc)/dzz
            dztau(2,i,j) = scf*dztau(2,i,j)/(rh0sc*dsc*usc*fsc)/dzz
            dztav(1,i,j) = scf*dztav(1,i,j)/(rh0sc*dsc*usc*fsc)/dzz
            dztav(2,i,j) = scf*dztav(2,i,j)/(rh0sc*dsc*usc*fsc)/dzz

            tau(1,i,j) = dztau(1,i,j)*dzz
            tau(2,i,j) = dztav(2,i,j)*dzz
         enddo
      enddo

      close(96)
      close(97)
      close(98)
      close(99)

cAR
cAR   ta0 = 0.5/(rh0sc*dsc*usc*fsc)

c parameters for (restricted) time-dependent forcing 
c set sda1 < 1e05 for steady forcing 

c     sda1 = 0.017
      sda1 = 0.000
      sdomg = 2*pi/10.0

c seabed depth h needed BEFORE forcing if coastlines are non-trivial
c note k1(i,j) must be periodic ; k1(0,j) - k1(imax,j) = 0 and
c k1(1,j) - k1(imax+1,j) = 0

      ntot = 0
      intot = 0

      open(13,file=world//'.k1')
c note k1(i,j) must be periodic ; k1(0,j) - k1(imax,j) = 0 and
c k1(1,j) - k1(imax+1,j) = 0, as enforced below;

      do j=jmax+1,0,-1
         read(13,*)(k1(i,j),i=0,imax+1)
c rotate grid to check b.c.s
c        read(13,*)xxx,(k1(i,j),i=19,36),(k1(i,j),i=1,18),xxx

         k1(0,j) = k1(imax,j)
         k1(imax+1,j) = k1(1,j)
         do i=0,imax+1
c boundary condition
c           if(i*j*(imax+1-i)*(jmax+1-j).eq.0)then
c              k1(i,j) = kmax+1
c           else
c               k1(i,j) = 1
c           endif

            do k=1,3
               h(k,i,j) = 0
               rh(k,i,j) = 0
            enddo
         enddo
         write(6,'(i4,40i3)')j,(k1(i,j),i=0,imax+1)
c        write(99,'(38i3)')(k1(i,j),i=0,imax+1)
      enddo

c read ips etc if possible

c     read(13,*,end=200)ips,ipf,ias,iaf,jsf
 200  close(13)
c     print*,'ocean positions ',ips,ipf,ias,iaf,jsf

c count wet cells

      do j=1,jmax
         do i=1,imax
            if(k1(i,j).le.kmax)then
               ntot = ntot + kmax - k1(i,j) + 1
               intot = intot + kmax - k1(i,j)
            endif
         enddo
      enddo

c find ocean positions semi-automatically, must start with a
c longitude i which is in the right ocean for all j, tricky in north

c     stop 'testing new code'
cAR   ias(jmax) = 10
cAR   ips(jmax) = 2 
      ias(jmax) = 24
      ips(jmax) = 10
      jsf = 1
      print*,'ips ipf ias iaf '
      do j=1,jmax-1
         ips(j) = ips(jmax)
         ipf(j) = ips(j)
         ias(j) = ias(jmax)
         iaf(j) = ias(j)
         if(j.eq.jmax-1)then
            ias(j) = 22
         endif
         do i=1,imax
            if(k1(ips(j)-1,j).le.kmax)ips(j) = ips(j) - 1
            if(k1(ipf(j)+1,j).le.kmax)ipf(j) = ipf(j) + 1
            if(k1(ias(j)-1,j).le.kmax)ias(j) = ias(j) - 1
            if(k1(iaf(j)+1,j).le.kmax)iaf(j) = iaf(j) + 1
            ips(j) = 1 + mod(ips(j)-1+imax,imax)
            ipf(j) = 1 + mod(ipf(j)-1+imax,imax)
            ias(j) = 1 + mod(ias(j)-1+imax,imax)
            iaf(j) = 1 + mod(iaf(j)-1+imax,imax)
         enddo
         if(ias(j).ge.iaf(j).and.j.le.jmax/2)jsf = j
         if(ips(j).ge.ipf(j).and.j.le.jmax/2)jsf = j
      enddo
      ips(jmax) = 1
      ipf(jmax) = 0 
      ips(jmax-1) = 1
      ipf(jmax-1) = 0 
      ias(jmax) = 1
      iaf(jmax) = imax
      print*,'jsf ',jsf
      write(6,'(5i4)')(j,ips(j),ipf(j),ias(j),iaf(j),j=1,jmax)

c initialize psi

      do j=0,jmax
         do i=0,imax
           psi(i,j)=0.0
         enddo
         do i=0,imax+1
            ub(1,i,j) = 0
            ub(2,i,j) = 0
         enddo
      enddo

c seabed depth h

      do j=jmax+1,0,-1
         do i=0,imax+1
            if(k1(i,j).le.kmax)then
               do k=k1(i,j),kmax
                  h(3,i,j) = h(3,i,j) + dz(k)
               enddo
               rh(3,i,j) = 1.0/h(3,i,j)
            endif
         enddo
c        write(6,'(i4,40f5.0)')j,(dsc*h(3,i,j),i=0,imax+1)
      enddo

      do j=0,jmax+1
         do i=0,imax
            h(1,i,j) = min(h(3,i,j),h(3,i+1,j))
            if(max(k1(i,j),k1(i+1,j)).le.kmax)rh(1,i,j) = 1.0/h(1,i,j)
         enddo
      enddo   

      do j=0,jmax
         do i=0,imax+1
            h(2,i,j) = min(h(3,i,j),h(3,i,j+1))
            if(max(k1(i,j),k1(i,j+1)).le.kmax)rh(2,i,j) = 1.0/h(2,i,j)
         enddo
      enddo   

      do 120 j=1,jmax
         do 120 i=1,imax
            ku(1,i,j) = max(k1(i,j),k1(i+1,j))
            ku(2,i,j) = max(k1(i,j),k1(i,j+1))
  120 continue
      tv2 = 0

c set up drag and diffusion values
c drag takes the value adrag in the interior, rising twice by factor
c drgf per gridpoint close to equator and in regions of
c shallow water (k1>kmxdrg) ie land in the case kmxdrg=kmax
c jeb = 1/2 width of equatorial region of maximum drag

c     adrag = 1.0/2.5/86400/fsc
      adrag = 1.0/(adrag*86400*fsc)
c cross equator need * 4 if drag is constant ie if drgf=1
      drgf = 3.0
      kmxdrg = kmax/2
      jeb = 1
c     adrag = adrag * 2.0 
c     drgf = 2.0
c     kmxdrg = kmax
c     jeb = 0
      call drgset(adrag,drgf,kmxdrg,jeb)
      print*,'drag ds',adrag,ds,adrag/ds

c     diff(1) = 2000./rsc/usc
      diff(1) = diff(1)/(rsc*usc)
c     diff(2) = 1e-4/usc/dsc/dsc*rsc
      diff(2) = diff(2)*rsc/(usc*dsc*dsc)

c arrays for efficiency

      do j=1,jmax
         do i=1,imax
            rtv(i,j) = 1.0/(s(j)*s(j) + drag(1,i,j)*drag(1,i,j))
            rtv3(i,j) = 1.0/(sv(j)*sv(j) + drag(2,i,j)*drag(2,i,j))
         enddo
      enddo

      print*,'dphi ds diff(1) diff(2)'
      print*,dphi,ds,diff(1),diff(2)

c initialize some arrays to zero

      do i=0,imax
         do j=0,jmax
            do k=1,kmax
               do l=1,3
                  u(l,i,j,k) = 0
                  u1(l,i,j,k) = 0
               enddo
            enddo
         enddo
      enddo

#ifdef dosc
c v2 seasonal. Annual averages

      do j=1,jmax
         do i=1,imax
            do k=1,kmax
               do l=1,lmax
                  tsavg(l,i,j,k) = 0.
               enddo
               do l=1,3
                  uavg(l,i,j,k) = 0.
               enddo
            enddo
         enddo
      enddo
#endif

c initial conditions   

      do i=0,imax+1
         do j=0,jmax+1
            do k=0,kmax+1
c initial uniform temperature T0 large favours thermally direct solutions
               if(j.le.jmax/2)then
                  ts(1,i,j,k) = temp0
     1                          *0.5*(1 + sign(1,k-k1(i,j)))
               else
                  ts(1,i,j,k) = temp1
     1                          *0.5*(1 + sign(1,k-k1(i,j)))
               endif
c              if(i.eq.1.and.k.eq.1)print*,ts(1,i,j,k)
c initial salinity
               ts(2,i,j,k) =  0.0
c              ts(2,i,j,k) =  saln0
               ts1(1,i,j,k) = ts(1,i,j,k)
               ts1(2,i,j,k) = ts(2,i,j,k)
            enddo
            do k=0,kmax
               rho(i,j,k) = ec(1)*ts(1,i,j,k) + ec(2)*ts(2,i,j,k)
     1                    + ec(3)*ts(1,i,j,k)**2 + ec(4)*ts(1,i,j,k)**3
            enddo
         enddo
      enddo

c forcing fields and some more initialisation

      do 20 i=1,imax
         do 30 j=1,jmax
c th at u and v points
            tv = asin(s(j)) 
            tv1 = asin(sv(j))
c convective frequency array
            cost(i,j) = 0

            rho(i,j,0) = 0
c rho(i,j,0) is never referenced but this is not easy to prove in co.f

c wind stress, tau(1,i,j) is the u component at a u point
c              tau(2,i,j) is the v component at a v point
c BUT NB this is confusing notation...
c dztau(l,i,j) are the deriv's of the u,v components at u points
c dztav(l,i,j) are the deriv's of the u,v components at v points
cAR 18x18
c           tau(1,i,j) = - ta0*cos(2*pi*tv/th1)
cc          tau(1,i,j) = 0.
c           if(j.eq.1)tau(1,i,j) = tau(1,i,j) + ta0
c           dztau(1,i,j) = tau(1,i,j)/dzz
c this is also needed at v points
c           tau(2,i,j) = 0
c           dztau(2,i,j) = 0
c           dztav(1,i,j) = - ta0*cos(2*pi*tv1/th1)
cc          dztav(1,i,j) = 0.
c    1                     /dzz
c           dztav(2,i,j) = 0
   30    continue
   20 continue
c
c array to determine limit of easy part of double p integral in J term
c use integer wetpoint indicator
c (1+sign(1,kmax-k1(i,j)))/2
c mk is largest of surrounding wet k1 values if i,j is wet, else 0
c
      do 130 j=1,jmax
         do 130 i=1,imax
            mk(i,j) = max(k1(i,j)*(1+sign(1,kmax-k1(i,j)))/2,
     1                k1(i+1,j)*(1+sign(1,kmax-k1(i+1,j)))/2,
     1                k1(i-1,j)*(1+sign(1,kmax-k1(i-1,j)))/2,
     1                k1(i,j+1)*(1+sign(1,kmax-k1(i,j+1)))/2,
     1                k1(i,j-1)*(1+sign(1,kmax-k1(i,j-1)))/2)
            mk(i,j) = mk(i,j) * (1+sign(1,kmax-k1(i,j)))/2
  130 continue

c initialize bp, only strictly needed at k=k1

      do i=1,imax+1
         do j=1,jmax
            do k=1,kmax
               bp(i,j,k) = 0.0
            enddo
         enddo
      enddo

c periodic b.c. required for Antarctic island integral

      do j=1,jmax
         mk(imax+1,j) = mk(1,j)
      enddo

c
c array to avoid J term in flat regions
c For non-trivial coasts essential to avoid adding J term at non-Psi points.
c Hence first condition ensures (i,j) is a wet Psi point, 2nd that bottom
c is not flat.
c
      do 140 j=1,jmax
         do 140 i=1,imax
            if( (max(k1(i,j),k1(i+1,j),k1(i,j+1),k1(i+1,j+1)).le.kmax)
     1          .and. (k1(i,j).ne.k1(i,j+1).or.k1(i,j).ne.k1(i+1,j)
     1                       .or.k1(i,j).ne.k1(i+1,j+1)))then
               getj(i,j) = .true.   
            else
               getj(i,j) = .false.  
            endif
  140 continue

c read island geometry file or write out for manual editing
c setting wet to zero and 1 on 1st landmass, 2 on 2nd landmass (1st island)
c etc nb narrow channels may have no wet psi points and hence not show up on 
c psi grid

      open(23,file=world//'.psiles',status='old')
c     open(23,file=world//'.psiles',status='new')
c use enquire stmt if you can remember how   
c     do j=jmax,0,-1
c        write(23,'(180i3)')((1-sign(1,kmax-
c    1      max(k1(i,j),k1(i+1,j),k1(i,j+1),k1(i+1,j+1))))/2,i=1,imax)
c        write(6 ,'(180i3)')((1-sign(1,kmax-
c    1      max(k1(i,j),k1(i+1,j),k1(i,j+1),k1(i+1,j+1))))/2,i=1,imax)
c     enddo
c     stop 'psi file written'
      do j=jmax,0,-1
         read(23,*)(gbold(i + j*imax),i=1,imax)
c rotate grid to check b.c.s
c        read(23,*)(gbold(i+j*imax),i=19,36),(gbold(i+j*imax),i=1,18)
      enddo
      close(23)
c 
c read island path integral data, read isles+1 paths only if want last path 
c for testing
c
      open(24,file=world//'.paths')
c     read(24,*)(npi(i),i=1,isles+1)
c     do i=1,isles+1
      read(24,*)(npi(i),i=1,isles)
      do i=1,isles
         read(24,*)
         if(npi(i).gt.mpi)stop 'path integral around island too long'
         do j=1,npi(i)
            read(24,*)lpisl(j,i), ipisl(j,i), jpisl(j,i)
c rotate grid to check b.c.s
c           ipisl(j,i) = 1 + mod(ipisl(j,i)-1+18,36)

            if(abs(lpisl(j,i)).ne.1.and.abs(lpisl(j,i)).ne.2)stop 
            if(ipisl(j,i).gt.imax.or.ipisl(j,i).lt.0)stop 'bad path'
            if(jpisl(j,i).gt.jmax.or.jpisl(j,i).lt.0)stop 'bad path'
            if(k1(ipisl(j,i),jpisl(j,i)).gt.kmax)stop 'dry path'
         enddo
      enddo
      close(24)

      print*,'horizontal diffusivity',diff(1)*rsc*usc,' m**2/s'
      print*,'vertical diffusivity',diff(2)*usc*dsc*dsc/rsc,' m**2/s'
      print*,'basic drag coefficient',adrag*fsc,' /s'
      print*,'wind stress scale',fsc*usc*dsc,' m**2/s**2'
      print*,'or',fsc*usc*dsc*rh0sc,' N/m**2'
      print*,'density variation scale',rhosc,' kg/m**3'
      print*,'vertical velocity scale',usc*dsc/rsc,' m/s'
c     print*,'time scale',tsc/86400/365.25,' yrs'
      print*,'time scale',tsc/syr         ,' yrs'
      print*,'overturning scale',dsc*usc*rsc*1e-6,' Sv'
      print*,'vertical heat flux scale',dsc*usc*rh0sc*cpsc/rsc,' W/m**2'
      print*,'integrated energy scale',rh0sc*fsc*usc*rsc**3*dsc,' J'
      print*,'integrated northward heat flux scale (W)'
      write(6,'(e15.5)') usc*rh0sc*cpsc*rsc*dsc

      end
