*DECK INIQS
C**********************************************************
C             SUBROUTINE INIQS
C**********************************************************
       subroutine INIQS

       implicit none

      integer i
      real    t

#include "param1.cmn"
      include 'param2.cmn'
      include 'qstabs.cmn'
      include 'physca.cmn'

      do i=1,nqstab
        t=REAL(i)/1e5
        PQSVAL(i)=ESCONA*EXP(-ESCONB/T)
      enddo

      END
