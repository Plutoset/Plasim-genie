! ******************************************************************
! $Id: ichem_var.f90 2985 2006-08-17 11:17:59Z cvs-gw $
! common module for atmospheric geochemistry -
! should take from ocean biogenie instead ???
! JSS
!
! ******************************************************************

module ichem_var

      implicit none

#ifndef GENIENX
#define GENIENX 64
#endif

#ifndef GENIENY
#define GENIENY 32
#endif

#ifndef GENIENL
#define GENIENL 7
#endif

#ifndef GENIENLEVRF
#define GENENLEVRF 1
#endif

#ifndef GENIENTRAC
#define GENIENTRAC 2
#endif


      integer,parameter :: mg=GENIENX
      integer,parameter :: jgg=GENIENY
      integer,parameter :: nl=GENIENL 
      integer,parameter :: ntrac=GENIENTRAC

      real,parameter:: const_lamda_14C = 8267.0 
!                                         e-folding time of decay
!                                 note: half life = 5730.0 years (GOSAC)

      integer chemcount,iyear,idoy
      real doy,year
      real l14c_annmean(mg,jgg,nl)

end module ichem_var
