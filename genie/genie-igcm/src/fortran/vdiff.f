      SUBROUTINE VDIFF

      IMPLICIT NONE

C     
C     VERTICAL DIFFUSION OF MOMENTUM, HEAT AND MOISTURE.
C     CONSTANT DIFFUSION COEFFICIENTS SET IN INITAL.
C     ASSUMES ZERO FLUX AT UPPER AND LOWER BOUNDARIES.
C     SURFACE FLUX DEALT WITH SEPARATELY IN BLAYER.
C     
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'legau.cmn'
      include 'gridpp.cmn'
      include 'physca.cmn'
      include 'igcm_vdiff.cmn'
      REAL QG(IGC,NL)
      EQUIVALENCE (QG(1,1),TRAG(1,1,1))

      INTEGER L,IHEM,IOFM,I,J,LP,LM
      REAL    TLPH,FTSQ

C     
      REAL BVP(MG),BQP(MG),BTP(MG),BVM(MG),BQM(MG),BTM(MG)
      REAL FA(NL)
C     
c      THESE ARE NOW IN NAMELIST and igcm_vdiff.cmn
c      real increaseu
c      parameter(increaseu=5.)
c      real increaseq
c      parameter(increaseq=1.)
c      real increaset
c      parameter(increaset=1.)

      DO L=1,NL
         FA(L)=1.0/DSIGMA(L)
      END DO
C     
      DO 800 IHEM=1,NHEM
         IOFM=(IHEM-1)*MGPP
         DO 20 I=1,MG
            J=I+IOFM
            TLPH=TG(J,2)+TG(J,1)
            FTSQ=FB(1)/(TLPH*TLPH)
            BVP(I)=AKVV*FTSQ
            BQP(I)=AKQV*FTSQ
            BTP(I)=AKTV*FTSQ
            UTVD(J,1)=FA(1)*BVP(I)*(UG(J,2)-UG(J,1))
            VTVD(J,1)=FA(1)*BVP(I)*(VG(J,2)-VG(J,1))
            QTVD(J,1)=FA(1)*BQP(I)*(QG(J,2)-QG(J,1))
            TTVD(J,1)=FA(1)*BTP(I)*(SK(1)*TG(J,2)-TG(J,1))
 20      CONTINUE
         DO L=2,NLM
            LP=L+1
            LM=L-1
            DO I=1,MG
               J=I+IOFM
               BVM(I)=BVP(I)
               BQM(I)=BQP(I)
               BTM(I)=BTP(I)
               TLPH=TG(J,LP)+TG(J,L)
               FTSQ=FB(L)/(TLPH*TLPH)
               BVP(I)=AKVV*FTSQ
               BQP(I)=AKQV*FTSQ
               BTP(I)=AKTV*FTSQ
               IF (L.EQ.NLM) THEN
                  if (.not.LOLDBL) THEN
                     BVP(I)=BVP(I)*increaseu
                     BQP(I)=BQP(I)*increaseq
                     BTP(I)=BTP(I)*increaset
                  endif
               ENDIF
               UTVD(J,L)=FA(L)*( BVP(I)*(UG(J,LP)-UG(J,L))
     :              -BVM(I)*(UG(J,L)-UG(J,LM)))
               VTVD(J,L)=FA(L)*( BVP(I)*(VG(J,LP)-VG(J,L))
     :              -BVM(I)*(VG(J,L)-VG(J,LM)))
               QTVD(J,L)=FA(L)*( BQP(I)*(QG(J,LP)-QG(J,L))
     :              -BQM(I)*(QG(J,L)-QG(J,LM)))
               TTVD(J,L)=FA(L)*( BTP(I)*(SK(L)*TG(J,LP)-TG(J,L))
     :              -BTM(I)*(TG(J,L)-TG(J,LM)/SK(LM)))
            END DO
         END DO
         DO I=1,MG
            J=I+IOFM
            UTVD(J,NL)=-FA(NL)*BVP(I)*(UG(J,NL)-UG(J,NLM))
            VTVD(J,NL)=-FA(NL)*BVP(I)*(VG(J,NL)-VG(J,NLM))
            QTVD(J,NL)=-FA(NL)*BQP(I)*(QG(J,NL)-QG(J,NLM))
            TTVD(J,NL)=-FA(NL)*BTP(I)*(TG(J,NL)-TG(J,NLM)/SK(NLM))
         END DO
         DO L=1,NL
            DO I=1,MG
               J=I+IOFM
               UG(J,L)=UG(J,L)+DELT2C*UTVD(J,L)
               VG(J,L)=VG(J,L)+DELT2C*VTVD(J,L)
               QG(J,L)=QG(J,L)+DELT2C*QTVD(J,L)
               TG(J,L)=TG(J,L)+DELT2C*TTVD(J,L)
            END DO
         END DO
 800  CONTINUE
C     
      RETURN
      END
