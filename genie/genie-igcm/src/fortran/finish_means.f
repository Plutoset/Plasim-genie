c
      subroutine finish_means(amean,mg,jgg,nl,icount)
      implicit none
c
      integer mg,jgg,nl,icount
      real amean(mg,jgg,nl)
c
      real rcount
      integer i,j,l
c
      rcount=1./real(icount)
      do l=1,nl
         do j=1,jgg
            do i=1,mg
               amean(i,j,l)=rcount*amean(i,j,l)
            end do
         end do
      end do
c
      return
      end
