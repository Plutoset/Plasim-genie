c
      subroutine accum_means(afield,amean,mg,jg,nhem,nl,scale,roff)
      implicit none
c
      integer mg,nl,jg,nhem
      real afield((mg+2)*nhem,nl,jg)
      real amean(mg,jg*nhem,nl),scale,roff
c
      integer i,j,jj,ihem,iof,l
c
      do l=1,nl
         do j=1,jg
            do ihem=1,nhem
               iof=(ihem-1)*(mg+2)
               if (ihem.eq.1) then
                  jj=j
               else
                  jj=jg*nhem+1-j
               end if
               do i=1,mg
                  amean(i,jj,l)=amean(i,jj,l)+
     :                          scale*afield(i+iof,l,j)-roff
               end do
            end do
         end do
      end do
c
      return
      end
