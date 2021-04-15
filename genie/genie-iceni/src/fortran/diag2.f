c
c diag2.f frequent diagnostics for program goldstein variable depth
c mo indexes the oceans 1=Pacific, 2=Atlantic, 3=Indian, 4=Southern
c ndep indexes the depth 0=deep, 1=upper
c ipf=end of Pacific, iaf=end of Atlantic, jsf=north end of Southern
c lmax.gt.2.allowed 22/5/2
c
      subroutine diag2(sum,avn,avs,ominpt,omaxpt,ominat,omaxat)

      include 'var.cmn'

      real*8 sum(8*maxl), avn,  avs, vol(8), tv

      real*8 ou(maxj,maxk)
      real*8 opsia(0:maxj,0:maxk), ominat, omaxat
      real*8 opsip(0:maxj,0:maxk), ominpt, omaxpt

      integer i,j,k,l,mo,ndep

      logical first

      save vol, first

      data first/.true./
      data vol/8*0.0/

      do i=1,8*lmax
         sum(i) = 0
      enddo      
      tv = 0
      avn = 0
      avs = 0
      do 10 k=1,kmax
         do 20 j=1,jmax
            do 30 i=1,imax
               if(k.ge.k1(i,j))then
                  ndep = (2*k-1)/kmax
                  if(j.le.jsf)then
                     mo = 4
                  elseif(i.ge.ips(j).and.i.le.ipf(j).and.j.ne.jmax)then
                     mo = 1
                  elseif(i.ge.ias(j).and.i.le.iaf(j).and.j.ne.jmax)then
                     mo = 2
                  else
                     mo = 3
                  endif
                  do 60 l=1,lmax
                     sum(mo + 4*ndep + 8*(l-1)) =
     1                  sum(mo + 4*ndep + 8*(l-1)) + 
     1                  ts(l,i,j,k)*dphi*ds*dz(k)
c                    print*,i,j,k,l,mo + 4*ndep + 8*(l-1)
c    2                   ,sum(mo + 4*ndep + 8*(l-1))
   60             continue
                  if(first)vol(mo + 4*ndep) = vol(mo + 4*ndep) 
     1                                      + dphi*ds*dz(k)
c                 print*,i,j,k,mo + 4*ndep,vol(mo + 4*ndep)
                  if(k.lt.kmax)
     1               avn = avn + abs(rho(i,j,k) - rho(i,j,k+1))/dza(k)

                  avs = avs + u(1,i,j,k)**2 + u(2,i,j,k)**2

               endif
   30       continue
   20    continue
   10 continue

c Calculate meridional overturning streamfunction opsi on C grid only
cbob for purpose of getting time series of ominpt, omaxpt, ominat, omaxat

      do j=0,jmax
         do k=0,kmax
            opsia(j,k) = 0
            opsip(j,k) = 0
         enddo
      enddo
c
c Pacific and Atlantic overturning streamfunctions
c
      ominpt = 0
      omaxpt = 0
      do j=jsf+1,jmax-1
         do k=1,kmax-1
            ou(j,k) = 0
            do i=ips(j),ipf(j)
               ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
            enddo
            opsip(j,k) = opsip(j,k-1) - dz(k)*ou(j,k)
            if(opsip(j,k).lt.ominpt)ominpt = opsip(j,k)
            if(opsip(j,k).gt.omaxpt)omaxpt = opsip(j,k)
         enddo
      enddo

      ominat = 0
      omaxat = 0
      do j=jsf+1,jmax-1
         do k=1,kmax-1
            ou(j,k) = 0
            do i=ias(j),iaf(j)
               ou(j,k) = ou(j,k) + cv(j)*u(2,i,j,k)*dphi
            enddo
            opsia(j,k) = opsia(j,k-1) - dz(k)*ou(j,k)
            if(opsia(j,k).lt.ominat)ominat = opsia(j,k)
            if(opsia(j,k).gt.omaxat)omaxat = opsia(j,k)
         enddo
      enddo

      ominpt = opsisc*ominpt 
      omaxpt = opsisc*omaxpt 
      ominat = opsisc*ominat 
      omaxat = opsisc*omaxat 

c crash barrier added 111099
      if(avs.lt.1e20)then
        continue
      else
        print*,'big avs , stopping'
        stop
      endif

      do i=1,8     
         do l=1,lmax
            sum(i + 8*(l-1)) = sum(i + 8*(l-1))/vol(i)
         enddo
      enddo

      avs = sqrt(avs/ntot)

      avn = avn/(intot)

      first = .false.

      end
