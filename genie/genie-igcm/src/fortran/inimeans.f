c
      subroutine inimeans(amean,mg,jgg,nl)
      implicit none
c
      integer mg,jgg,nl
      real amean(mg,jgg,nl)
c
      integer i,j,l
c
      do l=1,nl
         do j=1,jgg
            do i=1,mg
               amean(i,j,l)=0.0
            end do
         end do
      end do
c
      return
      end
