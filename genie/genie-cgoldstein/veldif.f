c 
c veldif.f  reordered to sensible order
c
      program veldif 
      character lin*30
      real tmp(6,40,40,40), u(6,40,40,40)
      print*,'lmax imax jmax kmax'
      read(5,*)lmax,imax,jmax,kmax
      print*,'1st filename for input?'
      read(5,'(a)')lin
      write(6,'(a)')lin
      open(1,file=lin)
      read(1,*)((((tmp(l,i,j,k),l=1,lmax),i=1,imax),j=1,jmax),k=1,kmax)
      close(1)
      print*,'2nd filename for input?'
      read(5,'(a)')lin
      write(6,'(a)')lin
      open(1,file=lin)
      read(1,*)((((u(l,i,j,k),l=1,lmax),i=1,imax),j=1,jmax),k=1,kmax)
      close(1)
      print*,'filename for output (2-1)'
      read(5,'(a)')lin
      open(1,file=lin)
      write(1,10)((((u(l,i,j,k)-tmp(l,i,j,k),l=1,lmax),i=1,imax)
     1 ,j=1,jmax),k=1,kmax)
c    1 ,i=10,10 ),j=10,10 )
      close(1)
  10  format(e16.9)
c 10  format(6e15.5)
      end
