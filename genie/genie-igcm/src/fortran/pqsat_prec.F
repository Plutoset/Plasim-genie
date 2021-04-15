*DECK PQSAT
C**********************************************************
C             SUBROUTINE PQSAT
C**********************************************************
      real function PQSAT_prec(T)

      implicit none

      include 'qstabs.cmn'
#include "precision.inc"
      real(rk_in) :: T

#ifdef IGCMPQSATTEST
      if (int(T*1e5).gt.nqstab) then
         pqsat_prec=pqsval(nqstab)
      else if (int(T*1e5).lt.1) then
         pqsat_prec=pqsval(1)
      else
         PQSAT_prec=PQSVAL(int(T*1e5))
      endif
#else
         PQSAT_prec=PQSVAL(int(T*1e5))
#endif

      END
