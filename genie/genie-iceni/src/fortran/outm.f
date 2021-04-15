c subroutine outm.f writes out data for goldstein last change  6/6/95
c expanded to write out atmos and sea ice data (Bob 10/5/02)
c
      subroutine outm(unit)

      include 'var.cmn'

      integer i, j, k, l, unit

      do 20 j=1,jmax
         do 20 i=1,imax
            do 20 k=1,kmax
               do 30 l=1,lmax  
                  if(k.ge.k1(i,j))then
                     write(unit,* )ts(l,i,j,k)
                  else
                     write(unit,* )0.0      
                  endif
   30          continue
               do l=1,2
                  write(unit,* )u(l,i,j,k)
               enddo
   20 continue

c EMBM

      do 120 j=1,jmax
         do 120 i=1,imax
            do 120 l=1,2
               write(unit,* )tq(l,i,j)
  120 continue
      do 220 j=1,jmax
         do 220 i=1,imax
            do 220 l=1,2
               write(unit,* )varice(l,i,j)
  220 continue

c EMBM for exact continuation need

      do j=1,jmax
         do i=1,imax
            write(unit,* )tice(i,j)
         enddo
      enddo
         
      write(unit,*)t
   10 format(10f10.4)
      end
