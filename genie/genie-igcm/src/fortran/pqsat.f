*DECK PQSAT
C**********************************************************
C             SUBROUTINE PQSAT
C**********************************************************
      real function PQSAT(T)

      implicit none

      include 'qstabs.cmn'
      real T

#ifdef IGCMPQSATTEST
      if (int(T*1e5).gt.nqstab) then
         PQSAT=PQSVAL(nqstab)
      else if (int(T*1e5).lt.1) then
         PQSAT=PQSVAL(1)
      else
         PQSAT=PQSVAL(int(T*1e5))
      end if
#else
         PQSAT=PQSVAL(int(T*1e5))
#endif

      END
