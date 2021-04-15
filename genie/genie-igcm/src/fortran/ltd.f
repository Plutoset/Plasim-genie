*DECK LTD
C**********************************************************
C             SUBROUTINE LTD
C**********************************************************
      SUBROUTINE LTD

      IMPLICIT NONE

C
C     Direct Legendre transform for the adiabatic part of the timestep.
C     Transforms from Fourier to spectral space at the current latitude
C     (pair).  In a global run the input arrays are complete (even+odd)
C     Fourier coefficients at the northern & southern hemisphere rows.
C
C     Includes the option either to call the modular routine HANAL for
C     each field to be transformed, or to call the fast vectorising
C     routine HANALV to perform all transforms together.  The choice
C     is controlled by logical LTVEC.
C
C     Each call to HANAL transforms fields having the same symmetry
C     and type of Legendre Function.  HANAL1 is a separate routine
C     with improved efficiency for single-level transforms.
C
C     The Fourier work array passed to HANAL must be dimensioned with
C     (at least) the maximum number of levels used in the HANAL calls.
C
C     Version for RSGUP3.                     Mike Blackburn,  12.01.95.
C     ANSI work arrays for HANAL,SPDEL2.      Mike Blackburn,  04.09.96.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'gridp3.cmn'
      include 'legau.cmn'
      include 'polyno.cmn'
      include 'spectr.cmn'
      REAL    FILT(0:NN)
      COMPLEX GWORK(IGL,(3+NTRAC)*NL+2)

      INTEGER L,I,KK
      REAL    AFAC,GRPAD

C
C     Prepare Fourier arrays:
C     - change sign of terms which contribute negatively to tendency,
C     - apply (1-mu**2) weighting,
C     - take zonal derivatives,
C     - make copies of effective momentum tendencies.
C
      DO 10 L=1,NL
         DO 10 I=1,IGL
            EG(I,L)=-0.5*EG(I,L)*SECSQ(JH)
            VTG(I,L)=-VTG(I,L)
            TNLG(I,L)=TNLG(I,L)-CMPA(I)*UTG(I,L)*SECSQ(JH)
            FVGT(I,L)=FVG(I,L)
            FUGT(I,L)=-FUG(I,L)
            FUG(I,L)=CMPA(I)*FUG(I,L)*SECSQ(JH)
            FVG(I,L)=CMPA(I)*FVG(I,L)*SECSQ(JH)
   10 CONTINUE
C
C     Tracer transforms depend on the form of advection used.
C
      IF (LFLUX) THEN
         AFAC=-1.0
      ELSE
         AFAC=1.0
      ENDIF
      DO 11 KK=1,NTRAC
         DO 11 L=1,NL
            DO 11 I=1,IGL
               TRANLG(I,L,KK)=AFAC*TRANLG(I,L,KK)
     *                        -CMPA(I)*UTRAG(I,L,KK)*SECSQ(JH)
               VTRAG(I,L,KK)=-VTRAG(I,L,KK)
   11 CONTINUE
C
      IF (LTVEC) THEN
C
C        Call single routine to perform all transforms with maximum
C        vector efficiency.
C        *** NOTE : THE INPUT FOURIER FIELDS ARE MODIFIED IF GLOBAL ***
C
         CALL HANALV(SPA,VP,DTE,TT,TRAT,DT,ZT,SPG,VPG,EG
     +              ,TNLG,TRANLG,FUG,FVG,VTG,VTRAG,FVGT,FUGT)
C
      ELSE
C
C        Main transform of even fields:
C        SPG to FUG (and SPA to DT) must be contiguous in common.
C
         CALL HANAL(SPG,GWORK,SPA,(3+NTRAC)*NL+2,2)
C
C        Remaining transforms: VTG,FVGT (and TT,DT) must be contiguous.
C
         CALL HANAL(FVG,GWORK,ZT,NL,1)
         CALL HANAL(VTG,GWORK,TT,(2+NTRAC)*NL,4)
         CALL HANAL(FUGT,GWORK,ZT,NL,3)
C
      ENDIF
C
C     At the last latitude, take del**2 of the energy term and add to
C     the divergence tendency.
C
      IF (JH.EQ.JG) THEN
         CALL SPDEL2(DTE,FILT,NWJ2,NN,MM,MOCT,NHEM,NL,0,2)
         DO 20 I=1,IGB
            DT(I)=DT(I)+DTE(I)
   20    CONTINUE
      ENDIF
C
      RETURN
      END
