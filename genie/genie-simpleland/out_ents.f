cmsw
cmsw Writes output for restarts
cmsw
      subroutine out_ents(unit)

      include '../genie-cgoldstein/var.cmn'
      include '../genie-simpleland/var_ents.cmn' 

      integer i,j,m,unit
     
      write(unit,*)((photo(i,j),i=1,imax),j=1,jmax) 
      write(unit,*)((respveg(i,j),i=1,imax),j=1,jmax)
      write(unit,*)((leaf(i,j),i=1,imax),j=1,jmax)
      write(unit,*)((respsoil(i,j),i=1,imax),j=1,jmax)

      write(unit,*)((Cveg(i,j),i=1,imax),j=1,jmax)
      write(unit,*)((Csoil(i,j),i=1,imax),j=1,jmax)
      write(unit,*)((fv(i,j),i=1,imax),j=1,jmax)

      write(unit,*)((tqld(1,i,j),i=1,imax),j=1,jmax)
      write(unit,*)((tqld(2,i,j),i=1,imax),j=1,jmax)

      write(unit,*)((snow(i,j),i=1,imax),j=1,jmax)

      write(unit,*)pco2ld

      end
