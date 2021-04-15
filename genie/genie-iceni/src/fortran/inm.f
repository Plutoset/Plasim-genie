c subroutine inm.f reads in data for goldstein last change  1/5/95
c expanded to read in atmos and sea ice data (Bob 10/5/02)
c
      subroutine inm(unit)

      include 'var.cmn'

      integer i, j, k, l, unit

c     read (unit,*)imax,jmax,kmax

      read (unit,*)((((ts(l,i,j,k),l=1,lmax),(u1(l,i,j,k),l=1,2),
     1             k=1,kmax),i=1,imax),j=1,jmax)

c extra read statement for embm atmos
      read (unit,*)(((tq(l,i,j),l=1,2),i=1,imax),j=1,jmax)

c extra read statement for sea ice
      read (unit,*)(((varice(l,i,j),l=1,2),i=1,imax),j=1,jmax)

c extra read statement for exact continuation
      read (unit,*)((tice(i,j),i=1,imax),j=1,jmax)

      read (unit,*,end=10) t0
      t = t0
      print*,'t = ',t
 10   continue
      end
