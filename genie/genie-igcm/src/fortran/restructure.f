      subroutine restructure1(input,output,mg,jg,nhem,scale,offset,
     :     imode)
      implicit none
      integer mg,jg,nhem,imode
      real input((mg+2)*nhem,jg),output(mg,jg*nhem),scale,offset
c     
      integer i,ihem,ioff,jj,j
c     
      if (imode.eq.1) then
         do j=1,jg
            do ihem=1,nhem
               ioff=(ihem-1)*(mg+2)
               jj=j
               if (ihem.eq.2) then
                  jj=jg*nhem+1-j
               end if
               do i=1,mg
                  output(i,jj)=input(i+ioff,j)*scale-offset
               end do
            end do
         end do
      else
         do j=1,jg
            do ihem=1,nhem
               ioff=(ihem-1)*(mg+2)
               jj=j
               if (ihem.eq.2) then
                  jj=jg*nhem+1-j
               end if
               do i=1,mg
                  input(i+ioff,j)=(output(i,jj)+offset)/scale
               end do
            end do
         end do
      end if
c     
      return
      end
c     
      subroutine restructure2(input,output,mg,jg,nhem,nl,scale,offset,
     :     imode)
      implicit none
      integer mg,jg,nhem,nl,imode
      real input(nhem,jg,mg,nl),output(mg,jg*nhem,nl),scale,offset
c     
      integer i,ihem,jj,j,l
c     
      if (imode.eq.1) then
         do l=1,nl
            do j=1,jg
               do ihem=1,nhem
                  jj=j
                  if (ihem.eq.2) then
                     jj=jg*nhem+1-j
                  end if
                  do i=1,mg
                     output(i,jj,l)=input(ihem,j,i,l)*scale-offset
                  end do
               end do
            end do
         end do
      else
         do l=1,nl
            do j=1,jg
               do ihem=1,nhem
                  jj=j
                  if (ihem.eq.2) then
                     jj=jg*nhem+1-j
                  end if
                  do i=1,mg
                     input(ihem,j,i,l)=(output(i,jj,l)+offset)/scale
                  end do
               end do
            end do
         end do
      end if
c     
      return
      end
c     
      subroutine restructure3(input,output,mg,jg,nhem,nl,scale,offset,
     :     imode)
      implicit none
      integer mg,jg,nhem,nl,imode
      real input((mg+2)*nhem,jg,nl),output(mg,jg*nhem,nl),scale,offset
c     
      integer i,ihem,jj,j,l,iof,mgpp
c     
      mgpp=mg+2
      if (imode.eq.1) then
         do l=1,nl
            do j=1,jg
               do ihem=1,nhem
                  iof=(ihem-1)*mgpp
                  jj=j
                  if (ihem.eq.2) then
                     jj=jg*nhem+1-j
                  end if
                  do i=1,mg
                     output(i,jj,l)=input(i+iof,j,l)*scale-offset
                  end do
               end do
            end do
         end do
      else
         do l=1,nl
            do j=1,jg
               do ihem=1,nhem
                  iof=(ihem-1)*mgpp
                  jj=j
                  if (ihem.eq.2) then
                     jj=jg*nhem+1-j
                  end if
                  do i=1,mg
                     input(i+iof,j,l)=(output(i,jj,l)+offset)/scale
                  end do
               end do
            end do
         end do
      end if
c     
      return
      end
