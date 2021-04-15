*
* mains.F main program for thermocline equation model goldstein
* variable depth 3/5/95 
* extra outputs for global version 19/4/00
* #ifdef's added so that mains.f -> mains.F 6/5/2
* 
      program goldstein
      include 'var.cmn'

c$$$      real*8 avn, avs, sum, sums(8*maxl), tv(maxl,maxi,maxj,maxk), rms
      real*8 avn, avs, sums(8*maxl), tv(maxl,maxi,maxj,maxk), rms
c$$$      real*8 sum1, sum2, cor, pacsf
      real*8 sum1, sum2
      real*8 ominpt, omaxpt, ominat, omaxat 

c to average osc runs
c     real ats(maxl,maxi,maxj,maxk), au(3,maxi,maxj,maxk)
c    1   , afn(maxl,maxi,maxj,maxk)

      real*8 opsi(0:maxj,0:maxk), ou(maxj,maxk)
      real*8 opsia(0:maxj,0:maxk), omina, omaxa
      real*8 opsip(0:maxj,0:maxk), ominp, omaxp

      real*8 zpsi(0:maxi,0:maxk), zu(maxi,maxk)

      integer nsteps, npstp, iwstp, itstp, iw, icount
c$$$     1      , i, j, k, l, istep, iterun, isol, isl, natm
     1      , i, j, k, l, istep, isol, isl, natm

      character ans,lout*4,lin*8,ext*3,conv*3

      logical flat, osc

      real*8 hft(3), hfp(3), hfa(3), phfmax, tv2, tv3
      real*8 psisl(0:maxi,0:maxj,isles), ubisl(2,0:maxi+1,0:maxj,isles)
ctest1    ,erisl(isles,isles+1), psibc(2)
     1    ,erisl(isles+1,isles+1), psibc(2)
c     ***** m.gulamali@imperial.ac.uk
      integer ifile
      real*8 binbit
c
c for repeated runs forward or backward
c
c     open(20,file='output.553R')
c     do iterun=10,250,10
c     do iterun=240,0,-10 
c     open(5,file='input.553')

      print*,'nsteps npstp iwstp itstp'
      read(5,*)nsteps,npstp,iwstp,itstp
      print*,nsteps,npstp,iwstp,itstp
      print*,'new or continuing run ?'
      read(5,'(a1)')ans
      print*,ans

      call gseto
c EMBM
      call gseta

      print*,'file extension for output (a4) ?'
      read(5,'(a4)')lout

c     ***** m.gulamali@imperial.ac.uk
      print*,'files to output (integer switch) ?'
      read(5,*)ifile
      
      if(ans.eq.'n'.or.ans.eq.'N')then
* define an i.c. done in gset
      else
         print*,'input file extension for input (a6)'
         read(5,'(a6)')lin
         open(1,file=lin)
         call inm(1)
         close(1)
c perturb the salinity
c        print*,'perturbing the i.c.'
c        do k=10 ,16
c           do i=9,11  
c              do j=1,1
c              ts(1, i,j ,k ) = ts(1, i,j ,k ) + 0.2*(1-0.5*abs(
c    1           real*8(i-10)))
c           enddo
c           enddo
c        enddo
         do k=1,kmax
            do j=1,jmax
               do i=1,imax
                  do l=1,lmax
                     ts1(l,i,j,k) = ts(l,i,j,k)
                  enddo
                  rho(i,j,k) = ec(1)*ts(1,i,j,k) + ec(2)*ts(2,i,j,k)
     1                    + ec(3)*ts(1,i,j,k)**2 + ec(4)*ts(1,i,j,k)**3
               enddo
            enddo
         enddo
c EMBM atm
         do j=1,jmax
            do i=1,imax
               tq1(1,i,j) = tq(1,i,j)
               tq1(2,i,j) = tq(2,i,j)
            enddo
         enddo
c EMBM sea-ice
         do j=1,jmax
            do i=1,imax
               varice1(1,i,j) = varice(1,i,j)
               varice1(2,i,j) = varice(2,i,j)
            enddo
         enddo
      endif

c periodic b.c. (required for implicit code)

      do k=1,kmax
         do j=1,jmax
            rho(0,j,k) = rho(imax,j,k)
            rho(imax+1,j,k) = rho(1,j,k)
            do l=1,lmax
               ts(l,0,j,k) = ts(l,imax,j,k)
               ts(l,imax+1,j,k) = ts(l,1,j,k)
c for cimp.ne.1 need
               ts1(l,0,j,k) = ts(l,imax,j,k)
               ts1(l,imax+1,j,k) = ts(l,1,j,k)
            enddo
         enddo
      enddo

c oscillating forcing 

c     if(abs(sda1).gt. 1e-5)then
c        osc = .true.
c        print*,'oscillatory forcing amplitude',sda1
c        do k=1,kmax
c           do j=1,jmax
c              do i=1,imax
c                 do l=1,lmax
c                    ats(l,i,j,k)= 0
c                    afn(l,i,j,k) = 0
c                 enddo
c                 do l=1,3
c                    au(l,i,j,k) = 0
c                 enddo
c              enddo
c           enddo
c        enddo
c     else
         osc = .false.
c     endif
      
      flat = .true.
      do i=1,imax
         do j=1,jmax
            if(k1(i,j).gt.1.and.k1(i,j).le.kmax)flat = .false.
         enddo
      enddo
      if(flat)then
         print*,'flat bottom'
      else
         print*,'topography present'
      endif

      call invert

#ifdef disle2
c code for multiple islands
      if(isles.ne.2)stop 'Makefile and var.cmn have different no. isles'
#endif

      do isol=1,isles

c set source term to 1 on the ith island (i+1th landmass) only

         do j=0,jmax
            do i=1,imax
               k=i + j*imax
               if(gbold(k).eq.isol + 1)then
                  gb(k) = 1.0
               else
                  gb(k) = 0.0
               endif
            enddo
         enddo
         call ubarsolv(ubisl(1,0,0,isol),psisl(0,0,isol))

c find island path integral due to unit source on boundary

         do isl=1,isles
            call island(ubisl(1,0,0,isol),erisl(isl,isol),isl,0)
         enddo
      enddo

      print*,'island path integrals due to unit sources',
     &       ((erisl(isl,isol),isl=1,isles),isol=1,isles)

      call wind

      iw = 1
      icount = 0

* time loop

      do istep=1,nsteps

c        if(osc)call oscer
         if(.not.flat.or.osc.or.istep.eq.1)then
            call jbar
            call ubarsolv(ub,psi)

c find island path integral due to wind and jbar terms 

            do isl=1,isles
               call island(ub,erisl(isl,isles+1),isl,1)
            enddo

#ifdef disle2
c solve system of simultaneous equations. Zero division here might 
c suggest not enough islands in the .psiles file

            psibc(1) = (- erisl(1,3)*erisl(2,2) + erisl(2,3)*erisl(1,2))
     1                  /(erisl(2,2)*erisl(1,1) - erisl(2,1)*erisl(1,2))
            psibc(2) =   (erisl(1,3)*erisl(2,1) - erisl(2,3)*erisl(1,1))
     1                  /(erisl(2,2)*erisl(1,1) - erisl(2,1)*erisl(1,2))

c to artificially set flows around islands to zero set psibc() to zero
c           psibc(1) = 0
c           psibc(2) = 0
#else
            psibc(1) = - erisl(1,2)/erisl(1,1)
#endif

            do j=1,jmax
               do i=0,imax+1
                  do isl=1,isles
                     ub(1,i,j) = ub(1,i,j) + ubisl(1,i,j,isl)*psibc(isl)
                     ub(2,i,j) = ub(2,i,j) + ubisl(2,i,j,isl)*psibc(isl)
                  enddo
               enddo
            enddo
c
c update diagnostic psi, not always necessary
c
            do j=0,jmax
               do i=0,imax
                  do isl=1,isles
                     psi(i,j) = psi(i,j) + psisl(i,j,isl)*psibc(isl)
                  enddo
               enddo
            enddo
         endif

c update velocities

         call velc

c EMBM change E-P forcing
c        extra0 = extra0 + ?? check def'n same

ccc HYSTERESIS option
c gradual increase/decrease P-E in N.Atlantic
c plus/minus range0 Sv @ 0.05 Sv/1000 yr [after Rahmstorf 1996]
c         extra0 = extra0 + (range0/real(nsteps))

ccc MELTWATER PULSE option
c reset extra0 to zero after 25/50/100 years:
c         if(istep.ge.nsteps_extra0) extra0 = 0.0

c EMBM surface fluxes
c EMBM sea ice now updated in surflux to allow adjustment of surface fluxes
         call surflux

#ifdef dimpo 
         call tstipo(istep)
#else 
         call tstepo(istep)
#endif

c EMBM update 1-layer atmosphere

         do natm = 1,ndta
#ifdef dimpa
            call tstipa
#else
            call tstepa
#endif
            if(mod(istep,npstp).lt.1) call diaga
         enddo

         t = istep*dt(kmax) + t0

         if(mod(istep,npstp).lt.2)then
            print*
            print*,'step ',istep,' t ',t,'  dt ',dt(kmax)
            print*,'psi on islands ',(psibc(isl),isl=1,isles)
            call diag
            if(mod(istep,npstp).eq.0)then
               do k=1,kmax
                  do j=1,jmax
                     do i=1,imax
                        do l=1,lmax
                           tv(l,i,j,k) = ts(l,i,j,k)
                        enddo
                     enddo
                  enddo
               enddo
            else if(mod(istep,npstp).eq.1.and.istep.gt.1)then
c              open(7,file='../results/tmp.1')
               rms = 0
               do j=1,jmax
                  do i=1,imax
                     do k=1,kmax
                        do l=1,lmax
                           rms = rms + (tv(l,i,j,k) - ts(l,i,j,k))**2
c               change = (tv(l,i,j,k) - ts(l,i,j,k))         
c               if(j.eq.3.and.k.eq.kmax-1)write(6,*)i,j,k,l,change
c            write(7,*)(tv(l,i,j,k) - ts(l,i,j,k))/dt(kmax)
                        enddo
                     enddo
                  enddo
               enddo
c            close (7)
               rms = sqrt(rms/lmax/ntot/dt(kmax)/dt(kmax))
               print*,'r.m.s. r.o.c.',rms
            endif
         endif

         if(mod(istep,iwstp).eq.0)then
            ext=conv(mod(iw,10))
            open(2,file='../results/'//lout//'.'//ext)
            rewind 2
            call outm(2)
            close(2)
c     write oscillating streamfunction
            if(.not.flat.or.osc)then
               open(2,file='../results/'//lout//'.psi.'//ext)
               do j=0,jmax
                  do i=0,imax
                     write(2,*)psi(i,j)
                  enddo
               enddo   
               close(2)
            endif
            iw = iw + 1
         endif

      if(mod(istep,itstp).eq.1)then
c           open(4,file=lout//'.'//'t'  
c    1          ,access='append')
c for ibm no access parameter and don't close file faster on sg too
            call diag2(sums,avn,avs,ominpt,omaxpt,ominat,omaxat)
c     ***** m.gulamali@imperial.ac.uk
            if (binbit(ifile,0).eq.1.) then
               open(4,file='../results/'//lout//'.'//'t')
               if (istep.eq.1) then
                  write(4,'(11(a11,3x))')
     $                 '%time      '
     $                 ,' Pac_T_d   ',' Atl_T_d   '
     $                 ,' Ind_T_d   ',' Sou_T_d   '
     $                 ,' Pac_T_u   ',' Atl_T_u   '
     $                 ,' Ind_T_u   ',' Sou_T_u   '
     $                 ,' drho/dz   ',' speed     '
               endif
               write(4,110)t,(sums(i),i=1,8),avn,avs
            endif
c     ***** m.gulamali@imperial.ac.uk
            if (binbit(ifile,1).eq.1.) then
               open(14,file='../results/'//lout//'.'//'s')
               if (istep.eq.1) then
                  write(14,'(11(a11,3x))')
     $                 '%time      '
     $                 ,' Pac_S_d   ',' Atl_S_d   '
     $                 ,' Ind_S_d   ',' Sou_S_d   '
     $                 ,' Pac_S_u   ',' Atl_S_u   '
     $                 ,' Ind_S_u   ',' Sou_S_u   '
     $                 ,' drho/dz   ',' speed     '
               endif
               write(14,110)t,(sums(i),i=9,16),avn,avs
            endif
c     ***** m.gulamali@imperial.ac.uk
            if (binbit(ifile,2).eq.1.) then
               open(40,file='../results/'//lout//'.'//'opsit')
               if(istep.eq.1) write(40,*) dfwx,diffamp(2)
               write(40,120)t,ominpt,omaxpt,ominat,omaxat
            endif
c EMBM
c     ***** m.gulamali@imperial.ac.uk
            call diag3(sum1,sum2)
            if (binbit(ifile,3).eq.1.) then
               open(41,file='../results/'//lout//'.'//'airt')
               write(41,110)t,sum1
            endif
            if (binbit(ifile,4).eq.1.) then
               open(42,file='../results/'//lout//'.'//'q')
               write(42,110)t,sum2
            endif

c           close(4)

c           if(osc)then
c              open(8,file=lout//'.osi')
c              write(8,100)(ts(2,i,20,7   ),i=1,imax) 
c              write(8,100)( u(2,i, 1,7   ),i=1,imax) 
c              open(9,file=lout//'.osj')
c              write(9,100)(ts(2,1 ,j, 7),j=1,jmax) 
c              open(12,file=lout//'.osk'
c    1             ,access='append')
c              write(12,100)(ts(1,10,10,k ),k=1,kmax) 
c              close(12)
c to average osc runs
c              if(osc)call oscav(ats,au,afn,icount)
c           endif
         endif

      enddo

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,0).eq.1.) then
         close(4)
      endif
      if (binbit(ifile,1).eq.1.) then
         close(14)
      endif
      if (binbit(ifile,2).eq.1.) then
         close(40)
      endif
      if (binbit(ifile,3).eq.1.) then
         close(41)
      endif
      if (binbit(ifile,4).eq.1.) then
         close(42)
      endif
c     close(8)
c     close(9)

      call diagend(lout,ifile)

c put all the following junk into diagend when get round to it.??

c write out convective frequency array. Divide by 2*nsteps if call co twice

c     **** m.gulamali@imperial.ac.uk
      if (binbit(ifile,5).eq.1.) then
         open(3,file='../results/'//lout//'.cost')
         if(nsteps.gt.0)write(3,*) dfwx,diffamp(2)
         if(nsteps.gt.0)write(3,'(e15.8)')((cost(i,j)/nsteps
c     if(nsteps.gt.0)write(3,'(e15.8)')((0.5*cost(i,j)/nsteps
     1        ,i=1,imax),j=1,jmax)
         close(3)
      endif

c write out barotropic streamfunction

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,6).eq.1.) then
         open(3,file='../results/'//lout//'.psi')
         do 60 j=0,jmax
            do 60 i=0,imax
               write(3,*)psi(i,j)
 60      continue
         close(3)
      endif

c Calculate meridional overturning streamfunction opsi on C grid only

      do j=0,jmax
         do k=0,kmax
            opsi(j,k) = 0
            opsia(j,k) = 0
            opsip(j,k) = 0
         enddo
      enddo

      do 70 j=1,jmax-1
         do 80 k=1,kmax-1
            ou(j,k) = 0
            do 90 i=1,imax
               ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
   90       continue
            opsi(j,k) = opsi(j,k-1) - dz(k)*ou(j,k)
   80    continue
   70 continue

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,7).eq.1.) then
         open(10,file='../results/'//lout//'.opsi')
         write(10,*) dfwx,diffamp(2)
         write(10,100)((opsisc*opsi(j,k),j=0,jmax),k=0,kmax)
         close(10)
      endif

c
c Pacific and Atlantic overturning streamfunctions
c
      ominp = 0
      omaxp = 0
      do j=jsf+1,jmax-1
         do k=1,kmax-1
            ou(j,k) = 0
            do i=ips(j),ipf(j)
               ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
            enddo    
            opsip(j,k) = opsip(j,k-1) - dz(k)*ou(j,k)
            if(opsip(j,k).lt.ominp)ominp = opsip(j,k)
            if(opsip(j,k).gt.omaxp)omaxp = opsip(j,k)
         enddo   
      enddo    

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,8).eq.1.) then
         open(10,file='../results/'//lout//'.opsip')
         write(10,*) dfwx,diffamp(2)
         write(10,100)((opsisc*opsip(j,k),j=0,jmax),k=0,kmax)
         close(10)
      endif

      omina = 0
      omaxa = 0
      do j=jsf+1,jmax-1
         do k=1,kmax-1
            ou(j,k) = 0
            do i=ias(j),iaf(j)
               ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
            enddo
            opsia(j,k) = opsia(j,k-1) - dz(k)*ou(j,k)
            if(opsia(j,k).lt.omina)omina = opsia(j,k)
            if(opsia(j,k).gt.omaxa)omaxa = opsia(j,k)
         enddo   
      enddo    

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,9).eq.1.) then
         open(10,file='../results/'//lout//'.opsia')
         write(10,*) dfwx,diffamp(2)
         write(10,100)((opsisc*opsia(j,k),j=0,jmax),k=0,kmax)
         close(10)
      endif

c
c zonal overturning streamfunction
c
      do i=0,imax
         do k=0,kmax
            zpsi(i,k) = 0
         enddo
      enddo

      do i=1,imax-1
         do k=1,kmax-1
            zu(i,k) = 0
            do j=1,jmax
               zu(i,k) = zu(i,k) + u(1,i,j,k)/c(j)*ds
            enddo
            zpsi(i,k) = zpsi(i,k-1) - dz(k)*zu(i,k)
         enddo
      enddo

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,10).eq.1.) then
         open(10,file='../results/'//lout//'.zpsi')
         write(10,100)((zpsi(i,k),i=0,imax),k=0,kmax)
         close(10)
      endif

  100 format(e14.7)
  110 format(11e14.6)
  120 format(5e14.6)

c write poleward heat flux in Atlantic and Pacific and total

      pi=4*atan(1.0)
c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,11).eq.1.) then
         open(15,file='../results/'//lout//'.fofy')
         write(15,*) dfwx,diffamp(2)
         write(15,'(10(a11,3x))')
     $        ' latitude  '
     $        ,' tot_tot   ',' Pac_tot   '
     $        ,' Atl_tot   ',' tot_adv   '
     $        ,' Pac_adv   ',' Atl_adv   '
     $        ,' tot_dif   ',' Pac_dif   '
     $        ,' Atl_dif   '
      endif
      phfmax = 0
      do j=1,jmax-1
         do l=1,3
            hft(l) = 0
            hfp(l) = 0
            hfa(l) = 0
         enddo
         do i=1,imax
            if(k1(i,j).le.kmax.and.k1(i,j+1).le.kmax)then
               tv2 = 0
               tv3 = 0
               do k=k1(i,j),kmax
                  tv2 = tv2 + 0.5*cv(j)*u(2,i,j,k)*(ts(1,i,j+1,k) +
     1                       ts(1,i,j,k))*dz(k)*dphi
                  tv3 = tv3 - cv(j)*cv(j)*(ts(1,i,j+1,k) -
     1                       ts(1,i,j,k))/ds*diff(1)*dz(k)*dphi
               enddo
               hft(1) = hft(1) + tv2 + tv3
               hft(2) = hft(2) + tv2
               hft(3) = hft(3) + tv3
               if(i.ge.ips(j).and.i.le.ipf(j))then
                  hfp(1) = hfp(1) + tv2 + tv3
                  hfp(2) = hfp(2) + tv2
                  hfp(3) = hfp(3) + tv3
               elseif(i.ge.ias(j).and.i.le.iaf(j))then
                  hfa(1) = hfa(1) + tv2 + tv3
                  hfa(2) = hfa(2) + tv2
                  hfa(3) = hfa(3) + tv3
               endif
            endif
         enddo
c     ***** m.gulamali@imperial.ac.uk
         if (binbit(ifile,11).eq.1.) then
            write(15,110)180.0/pi*asin(s(j)),
     $           (hft(l),hfp(l),hfa(l),l=1,3)
         endif
         if(abs(hft(3)).gt.phfmax)phfmax = abs(hft(3))
      enddo

      write(6,*)'max poleward heat flux ',phfmax
               
c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,11).eq.1.) then
         close(15)
      endif

c write out potential vorticity or density

c     open(11,file=lout//'.pv')

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,12).eq.1.) then
         open(11,file='../results/'//lout//'.rho')
c     print*,'input k for Sz plot'
c     read (5,*)k
         do j=1,jmax
            do i=1,imax
c     do k=1,kmax-1
               do k=1   ,kmax
                  if(k.ge.k1(i,j))then
c                 tmp = s(j)*(rho(i,j,k+1)-rho(i,j,k))/dza(k)
c                 tmp = (ts(2,i,j,k+1)-ts(2,i,j,k))/dza(k)
c                 write(11,*)tmp
                     write(11,*)rho(i,j,k)
                  else
                     write(11,*)0.0
                  endif
               enddo
            enddo
         enddo
         close(11)
      endif

c call routine to write terms in psi equation, gb is subsequently wrong

c     call goose(lout)

c ropsi calculates overturning on steady density surfaces

c     call ropsi

c     open(11,file='../results/tmp.w')
c     do k=kmax,1,-1
c        tmp = 0
c        do i=1,imax
c           do j=1,jmax/2
c              tmp =  u(3,5,8,k)
c           enddo
c        enddo
c        write(11,*)tmp
c     enddo
c     close(11)

c write average osc data

c     if(osc.and.icount.gt.0)then
c        open(10,file=lout//'.av')
c        write(10,100)((((ats(l,i,j,k)/icount
c    1             ,l=1,lmax),k=1,kmax),i=1,imax),j=1,jmax)
c        close(10)
c        open(10,file=lout//'.avu')
c        write(10,100)((((au(l,i,j,k)/icount,l=1,3)
c    1             ,k=1,kmax),i=1,imax),j=1,jmax)
c        close(10)
c        open(10,file=lout//'.avf')
c        write(10,100)(((afn(1,i,j,k)/icount,k=1,kmax),i=1,imax)
c    1                ,j=1,jmax)
c        close(10)
c     endif
c
c for repeated runs
c
c     write(20,'(8e15.5)')0.01*iterun,ominp,omaxp,omina,omaxa
c    1                   ,phfmax,avn,rms
      tv2 = dsc*usc*rsc*1e-6
      write(6,'(a)')'overturning extrema in Sv'
      write(6,'(a)')'ominp,omaxp,omina,omaxa,avn,rms'
      write(6,'(6e15.5)')ominp*tv2,omaxp*tv2,omina*tv2,omaxa*tv2,avn,rms
c     close(5)
c     enddo
c     close(20)
      
      end

      character*3 function conv(i)
      character*1 a, b, c
      integer i, i1, i2, i3, itemp
      if(i.lt.10)then
        a=char(i+48)
        conv=a//'  '
      else if(i.lt.100)then
        i1=i/10
        i2=i-i1*10
        a=char(i1+48)
        b=char(i2+48)
        conv=a//b//' '
      else
        i1=i/100
        itemp=i-100*i1
        i2=itemp/10
        i3=itemp-10*i2
        a=char(i1+48)
        b=char(i2+48)
        c=char(i3+48)
        conv=a//b//c
      endif
      end

