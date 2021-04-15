c
      subroutine maxmin(zg,idim,fmin1,fmax1)

      implicit none

      integer idim
      real    fmin1,fmax1

      integer i

      real zg(idim)
c
      fmin1=zg(1)
      fmax1=zg(1)
      do i=2,idim
         if (zg(i).lt.fmin1) fmin1=zg(i)
         if (zg(i).gt.fmax1) fmax1=zg(i)
      end do
c
      return
      end
