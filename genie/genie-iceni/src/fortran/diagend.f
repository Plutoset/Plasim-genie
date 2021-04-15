c     
c     diag.f end-of-run diagnostics for c-goldstein created 2/9/2 nre
c     
c     tsdata and tqdata contain observational estimates for ts and tq
c     err is the mismatch between model and data weighted by errw 
c     
      subroutine diagend(lout,ifile)

      include 'var.cmn'

      character lout*4

c     ***** m.gulamali@imperial.ac.uk
      integer ifile
      real*8 binbit

      real*8 sum, tv2, syr, err
      real*8 tsdata(maxl,maxi,maxj,maxk), tqdata(2,maxi,maxj)
      real*8 errwts(maxl,maxk), errwtq(2)

      parameter(syr = 365*86400)

      integer i, j, k, l, iwets
      iwets = 0

c     read interpolated Levitus and NCEP data

      open(30,file='../input/tempann.silo')
      read(30,*)(((tsdata(1,i,j,k),k=1,kmax),i=1,imax),j=1,jmax)
      close(30)
      open(31,file='../input/saliann.silo')
      read(31,*)(((tsdata(2,i,j,k),k=1,kmax),i=1,imax),j=1,jmax)
      close(31)
      open(32,file='../input/ta_ncep.silo')
      read(32,*)((tqdata(1,i,j),i=1,imax),j=1,jmax)
      close(32)
      open(33,file='../input/qa_ncep.silo')
      read(33,*)((tqdata(2,i,j),i=1,imax),j=1,jmax)
      close(33)

c     calculate some very ad-hoc weights 

      do k=1,kmax
c     errwts(1,k) = 1.0*k/kmax
c     errwts(2,k) = 1.0*k/kmax
         errwts(1,k) = 1.0
         errwts(2,k) = 1.0
      enddo
      errwtq(1) = 1.0
      errwtq(2) = 1.0*1000.

c     calculate error compared to observations (!)

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,13).eq.1.) then
         open(25,file='../results/'//lout//'.err')
c     open(25,file='tmp.err')
         err = 0.
         sum = 0.
         do j=1,jmax
            do i=1,imax
               do k=1,kmax
                  if(k.ge.k1(i,j))then
                     tsdata(2,i,j,k) = tsdata(2,i,j,k) - saln0
                     do l=1,2
                        err = err + errwts(l,k)
     &                       *(ts(l,i,j,k) - tsdata(l,i,j,k))**2
                        write(25,10)ts(l,i,j,k) - tsdata(l,i,j,k)
                        sum = sum + errwts(l,k)
                     enddo
                  else
                     write(25,10)0.0,0.0
                  endif
               enddo
               tqdata(2,i,j) = tqdata(2,i,j)*1e-3
               do l=1,2
                  err = err + errwtq(l)*(tq(l,i,j) - tqdata(l,i,j))**2
                  sum = sum + errwtq(l)
               enddo
               write(25,10)(tq(l,i,j) - tqdata(l,i,j),l=1,2)
            enddo
         enddo
 10      format(e15.5)
         close(25)
         err = sqrt(err/((ntot + imax*jmax)*2))
c     err = sqrt(err/sum)
         print*,'weighted r.m.s. model-data error ',err
      endif

c     write out SST

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,14).eq.1.) then
         open(20,file='../results/'//lout//'.sst')
         sum = 0
         iwets =0
         do j=1,jmax
            do i=1,imax
               if(k1(i,j).le.kmax)then
                  write(20,*) ts(1,i,j,kmax)
                  sum = sum +ts(1,i,j,kmax)
                  iwets = iwets + 1
               else
                  write(20,*)0.0
               endif
            enddo
         enddo
         write(20,*)sum/iwets
         write(6,*)'average SST',sum/iwets
         close(20)
      endif

c     write out SSS

c     ***** m.gulamali@imperial.ac.uk
      if (binbit(ifile,15).eq.1.) then
         open(20,file='../results/'//lout//'.sss')
         sum = 0
         do j=1,jmax
            do i=1,imax
               if(k1(i,j).le.kmax)then
                  write(20,*) saln0+ts(2,i,j,kmax)
                  sum = sum +(saln0+ts(2,i,j,kmax))
               else
                  write(20,*)0.0
               endif
            enddo
         enddo
         write(20,*)sum/iwets
         write(6,*)'average SSS',sum/iwets
         close(20)
      endif

c     write out atmos air temp

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,16).eq.1.) then
         open(20,file='../results/'//lout//'.tair')
         write(20,*) dfwx,diffamp(2)
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) tq(1,i,j)
               sum = sum +tq(1,i,j)
            enddo
         enddo
         write(20,*)sum/imax/jmax
         write(6,*)'average atmos tair',sum/imax/jmax
         close(20)
      endif

c     write out atmos specific humidity

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,17).eq.1.) then
         open(20,file='../results/'//lout//'.qair')
         write(20,*) dfwx,diffamp(2)
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) tq(2,i,j)
               sum = sum +tq(2,i,j)
            enddo
         enddo
         write(20,*)sum/imax/jmax
         write(6,*)'average atmos qair',sum/imax/jmax
         close(20)
      endif

c     write out sea ice thickness

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,18).eq.1.) then
         open(20,file='../results/'//lout//'.hice')
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) varice1(1,i,j)
               sum = sum +varice1(1,i,j)
            enddo
         enddo
         write(20,*)sum/imax/jmax
         write(6,*)'average hice',sum/imax/jmax
         close(20)
      endif

c     write out sea ice areal fraction

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,19).eq.1.) then
         open(20,file='../results/'//lout//'.aice')
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) varice1(2,i,j)
               sum = sum +varice1(2,i,j)
            enddo
         enddo
         write(20,*)sum/imax/jmax
         write(6,*)'average aice',sum/imax/jmax
         close(20)
      endif

c     write out sea ice temperature

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,20).eq.1.) then
         open(20,file='../results/'//lout//'.tice')
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) tice(i,j)
               sum = sum +tice(i,j)
            enddo
         enddo
         write(20,*)sum/imax/jmax
         write(6,*)'average tice',sum/imax/jmax
         close(20)
      endif

c     write out pptn

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,21).eq.1.) then
         open(20,file='../results/'//lout//'.pptn')
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) pptn(i,j)*syr 
               sum = sum + pptn(i,j)
            enddo
         enddo
         write(6,*)'average pptn m/yr',sum*syr/imax/jmax
         close(20)
      endif

c     write out evap

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,22).eq.1.) then
         open(20,file='../results/'//lout//'.evap')
         sum = 0
         do j=1,jmax
            do i=1,imax
               tv2 = (evap(i,j)*(1-varice1(2,i,j))
     1              + evapsic(i,j)*varice1(2,i,j))
               write(20,*)tv2*syr
               sum = sum + tv2
            enddo
         enddo
         write(6,*)'average evap m/yr',sum*syr/(imax*jmax)
         close(20)
      endif

c     write out runoff

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,23).eq.1.) then
         open(20,file='../results/'//lout//'.runoff')
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) runoff(i,j)*syr
               sum = sum +runoff(i,j)
            enddo
         enddo
         write(6,*)'average runoff m/yr',sum*syr/imax/jmax
         close(20)
      endif

c     Artic ice diag

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,24).eq.1.) then
         open(20,file='../results/'//lout//'.arcice')
         do i=1,imax
            write(20,'(3e15.5)')varice(1,i,jmax),varice(2,i,jmax)
     1           ,tice(i,jmax)
         enddo
         close(20)
      endif

c     write out net freshwater flux into ocean (P-E+R+freeze/melt)

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,25).eq.1.) then
         open(20,file='../results/'//lout//'.fwfxneto')
         write(20,*) dfwx,diffamp(2)
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*)fwfxneto(i,j)*syr
               sum = sum + fwfxneto(i,j)
            enddo
         enddo
         sum = sum*syr/(imax*jmax)
         write(6,*)'global average net fwflux into ocean',sum
         close(20)
      endif

c     write out net heat flux into ocean

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,26).eq.1.) then
         open(20,file='../results/'//lout//'.fx0neto')
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*)fx0neto(i,j)
               sum = sum + fx0neto(i,j)
            enddo
         enddo
         sum = sum/(imax*jmax)
         write(6,*)'global average net heat flux into ocean',sum
         close(20)
      endif

c     write out net surface heat flux into atmos

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,27).eq.1.) then
         open(20,file='../results/'//lout//'.fx0a')
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) fx0a(i,j)
               sum = sum + fx0a(i,j)
            enddo
         enddo
         sum = sum/(imax*jmax)
         write(6,*)'average net heat flux in atmos',sum
         close(20)
      endif

c     write out 3-d field of temperature

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,28).eq.1.) then
         open(20,file='../results/'//lout//'.temp')
         do j=1,jmax
            do i=1,imax
               do k=1,kmax
                  if(k.ge.k1(i,j))then
                     write(20,*)ts(1,i,j,k)
                  else
                     write(20,*)0.0
                  endif
               enddo
            enddo
         enddo
         close(20)
      endif

c     write out 3-d field of salinity

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,29).eq.1.) then
         open(20,file='../results/'//lout//'.saln')
         do j=1,jmax
            do i=1,imax
               do k=1,kmax
                  if(k.ge.k1(i,j))then
                     write(20,*)ts(2,i,j,k)
                  else
                     write(20,*)0.0
                  endif
               enddo
            enddo
         enddo
         close(20)
      endif

c     write out albedo

c     ***** mygulamali@imperial.ac.uk
      if (binbit(ifile,30).eq.1.) then
         open(20,file='../results/'//lout//'.albedo')
         sum = 0
         do j=1,jmax
            do i=1,imax
               write(20,*) albcl(i,j)
               sum = sum +albcl(i,j)
            enddo
         enddo
         write(20,*)sum/imax/jmax
         write(6,*)'average planetary albedo',sum/imax/jmax
         close(20)
      endif

      write(6,*) 'final CO-2 concentration',co2(1,1)
      return
      end
