*DECK INISP
C**********************************************************
C             SUBROUTINE INISP
C**********************************************************
      SUBROUTINE INISP

      IMPLICIT NONE

C
C     Initialise spectral arrays
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'outcon.cmn'
      include 'restij.cmn'

      INTEGER I,L,IL

C
      I=1
      DO 170 L=1,NL
         T(I)=T(I)+SQR2*(TRS(L)-T0(L))
         TMI(I)=TMI(I)+SQR2*(TRS(L)-T0(L))
         I=I+IGA
 170  CONTINUE
      IL=1
      DO 174 L=1,NL
         Z(IL)=Z(IL)+EZ
         ZMI(IL)=ZMI(IL)+EZ
         IL=IL+IGA
 174  CONTINUE
C
      END
