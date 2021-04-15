*DECK LTDDIA
C**********************************************************
C             SUBROUTINE LTDDIA
C**********************************************************
         SUBROUTINE LTDDIA

         IMPLICIT NONE

C
C     Direct Legendre transform for the diabatic part of the timestep.
C     Only transform the required fields: forcing of temperature and
C     tracers and spatial derivatives of the momentum forcing.
C
C     Transforms from Fourier to spectral space at the current latitude
C     (pair).  In a global run the input arrays are complete (even+odd)
C     Fourier coefficients at the northern & southern hemisphere rows.
C
C     Includes the option either to call the modular routine HANAL for
C     each field to be transformed, or to call the fast vectorising
C     routine DANALV to perform all transforms together.  The choice
C     is controlled by logical LTVEC.
C
C     Each call to HANAL transforms fields having the same symmetry
C     and type of Legendre Function.  HANAL1 is a separate routine
C     with improved efficiency for single-level transforms.
C
C     The Fourier work array passed to HANAL must be dimensioned with
C     (at least) the maximum number of levels used in the HANAL calls.
C
C     Original version in IGCM2.              Piers Forster,   25.10.96.
C     LTVEC option for vector machines.       Mike Blackburn,  04.09.98.
C     Optimisation (/CSSQ to *SECSQ).         Mike Blackburn,  22.12.99.
C
#include "param1.cmn"
      include 'param2.cmn'
      include 'blank.cmn'
      include 'gridp3.cmn'
      include 'legau.cmn'
      include 'polyno.cmn'
      include 'spectr.cmn'
      COMPLEX GWORK(IGL,(2+NTRAC)*NL)

      INTEGER L,I
      REAL    GRPAD

C
C     Prepare Fourier arrays:
C     - change sign of terms which contribute negatively to tendency,
C     - apply (1-mu**2) weighting,
C     - take zonal derivatives,
C     - make copies of effective momentum tendencies.
C
      DO 10 L=1,NL
         DO 10 I=1,IGL
            FVGT(I,L)=FVG(I,L)
            FUGT(I,L)=-FUG(I,L)
            FUG(I,L)=CMPA(I)*FUG(I,L)*SECSQ(JH)
            FVG(I,L)=CMPA(I)*FVG(I,L)*SECSQ(JH)
   10 CONTINUE
C
      IF (LTVEC) THEN
C
C        Call single routine to perform all transforms with maximum
C        vector efficiency.
C        *** NOTE : THE INPUT FOURIER FIELDS ARE MODIFIED IF GLOBAL ***
C
         CALL DANALV(TT,TRAT,DT,ZT,TNLG,TRANLG,FUG,FVG,FVGT,FUGT)
C
      ELSE
C
C        Main transform of even fields:
C        TNLG to FUG (and TT to DT) must be contiguous in common.
C
         CALL HANAL(TNLG,GWORK,TT,(2+NTRAC)*NL,2)
C   4th argument of call to HANAL must be smaller than or equal to
C   the second dimension of GWORK.
C
C        Remaining transforms.
C
         CALL HANAL(FVG,GWORK,ZT,NL,1)
         CALL HANAL(FVGT,GWORK,DT,NL,4)
         CALL HANAL(FUGT,GWORK,ZT,NL,3)
C
      ENDIF
C
      RETURN
      END
