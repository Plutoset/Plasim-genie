      SUBROUTINE DSTEP

      IMPLICIT NONE

C     
C     Diabatic part of timestep. Completion of time-filter.
C     Note that only Z,D,T have diabatic tendencies at present.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'spectr.cmn'
      include 'bats.cmn'

      INTEGER I,KK

C     
C     Ordinary centred timestep
C     
      IF (KOUNT.GT.KITS) THEN
         DO I=1,IGB
            Z(I)=Z(I)+DELT2*ZT(I)
            T(I)=T(I)+DELT2*TT(I)
            D(I)=D(I)+DELT2*DT(I)
            ZMI(I)=ZMI(I)+PNU*Z(I)
            TMI(I)=TMI(I)+PNU*T(I)
            DMI(I)=DMI(I)+PNU*D(I)
         END DO
         DO KK=1,NTRAC
            DO I=1,IGB
               TRA(I,KK)=TRA(I,KK)+DELT2*TRAT(I,KK)
               TRAMI(I,KK)=TRAMI(I,KK)+PNU*TRA(I,KK)
            END DO
         END DO
         DO I=1,IGA
            SPMI(I)=SPMI(I)+PNU*SP(I)
         END DO
         RETURN
      ELSE
C     
C     Short initial timestep
C     
         DO I=1,IGB
            Z(I)=Z(I)+DELT2*ZT(I)
            T(I)=T(I)+DELT2*TT(I)
            D(I)=D(I)+DELT2*DT(I)
         END DO
         DO KK=1,NTRAC
            DO I=1,IGB
               TRA(I,KK)=TRA(I,KK)+DELT2*TRAT(I,KK)
            END DO
         END DO
         DELT=DELT2
         DELT2=DELT2+DELT2
         RETURN
      ENDIF
C     
      END
