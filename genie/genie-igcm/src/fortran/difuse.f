*DECK DIFUSE
C**********************************************************
C             SUBROUTINE DIFUSE
C**********************************************************
      SUBROUTINE DIFUSE

      IMPLICIT NONE

C
C     Calculates spectral tendencies from restoration (if included)
C     and biharmonic diffusion.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'spectr.cmn'
      include 'bats.cmn'
      include 'restor.cmn'
      include 'restij.cmn'
      include 'igcm_nlevs.cmn'
      real rfcoeff(nlevrf)
      logical ifirstrf
      save ifirstrf, rfcoeff
      data ifirstrf /.true./

      INTEGER IHEM,L,I,J,IR,JOFF,MP,IN,KK
      REAL    AKZ,AKDT

C
C Add newtonian cooling and surface Rayleigh friction.
C
      IF (LRESTIJ) THEN
      DO 21 IHEM=1,NHEM
         DO 22 L=1,NL
            I=(IHEM-1)*NWJ2+(L-1)*IGA
            DO 23 J=1,NWJ2
               TT(I+J)=TT(I+J)-DDAMP(L)*(T(I+J)-TTRES(I+J))
               ZT(I+J)=ZT(I+J)-TFRC(L)*Z(I+J)
               DT(I+J)=DT(I+J)-TFRC(L)*D(I+J)
23         CONTINUE
22       CONTINUE
21    CONTINUE
C
C       No friction on planetary vorticity
C
      DO 24 L=1,NL
         I=(L-1)*IGA+1
         ZT(I)=ZT(I)+TFRC(L)*EZ
 24   CONTINUE
      ELSE
      IF(DAMP.GT.0.0) THEN
         I=0
         IR=0
         DO 800 IHEM=1,NHEM
            DO 20 L=1,NL
               DO 10 J=1,IDM
                  I=I+1
                  IR=IR+1
                  ZT(I)=ZT(I)-DAMP*(Z(I)-ZRES(IR))
                  DT(I)=DT(I)-DAMP*(D(I)-DRES(IR))
                  TT(I)=TT(I)-DAMP*(T(I)-TRES(IR))
   10          CONTINUE
               I=I+IGA-IDM
               IR=IR+IGM-IDM
   20       CONTINUE
            I=NWJ2
            IR=IDM
  800    CONTINUE
      ENDIF
      ENDIF
C
C     Add in biharmonic diffusion if required
C
      IF (AK(2).GT.0.0) THEN
         DO 820 IHEM=1,NHEM
            DO 821 L=1,NL
               JOFF=(L-1)*IGA+(IHEM-1)*NWJ2
               J=JOFF
               DO 822 MP=1,MFP,MOCT
                  DO 823 IN=MP,NFP,MH
                     J=J+1
                     AKZ =AK(IN+2-IHEM)
                     AKDT=AK(IN-1+IHEM)
                     ZT(J)=ZT(J)-AKZ *Z(J)
                     DT(J)=DT(J)-AKDT*D(J)
                     TT(J)=TT(J)-AKDT*T(J)
823               CONTINUE
822            CONTINUE
               DO 824 KK=1,NTRAC
                  J=JOFF
                  DO 825 MP=1,MFP,MOCT
                     DO 826 IN=MP,NFP,MH
                        J=J+1
                        AKDT=AK(IN-1+IHEM)
                        TRAT(J,KK)=TRAT(J,KK)-AKDT*TRA(J,KK)
826                  CONTINUE
825               CONTINUE
824            CONTINUE
821         CONTINUE
820      CONTINUE
C
C        No diffusion on EZ (planetary vorticity)
C
         I=1
         DO 30 L=1,NL
            ZT(I)=ZT(I)+AK(2)*EZ
            I=I+IGA
30       CONTINUE
      ENDIF
C
c
c 11-08-97 Very crude Rayleigh Friction in the top NLEVRF levels.
c          NLEVRF is set in the parameter line.
c 29-06-98 Changed to use RFCOEFF instead of TFRC.  SMR
C 11-05-00 NB RFCOEFF AND TFRC ARE BOTH RAYLEIGH FRICTION
C          COEFFICIENT ARRAYS.                      SH
c
      if (ifirstrf) then
        ifirstrf=.false.
        do l=1,nlevrf
! The 0.0625 is a tunable parameter. 0.1875 recommended for L26.
! Now in igcm_nlevs.cmn and ININLEVS namelist
          rfcoeff(l)=((1.0-((l-1)*(1.0/float(nlevrf))))/pi2)*
     :                             difusestrength
        enddo
      endif
      do ihem=1,nhem
        do l=1,nlevrf
          i=(ihem-1)*nwj2+(l-1)*iga
          do j=1,nwj2
            zt(i+j)=zt(i+j)-rfcoeff(l)*z(i+j)
            dt(i+j)=dt(i+j)-rfcoeff(l)*d(i+j)
          enddo
        enddo
      enddo
      do l=1,nlevrf
        i=(l-1)*iga+1
        zt(i)=zt(i)+rfcoeff(l)*ez
      enddo
      RETURN
      END
