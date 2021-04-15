

C***********************************************************************
C*                                                                     *
C*                     FUNCTION HALTRAN                                *
C*                                                                     *
C***********************************************************************

      REAL FUNCTION HALTRAN (IBND,UPATH)

C This function returns the halocarbon transmittance, calculated by the
C simple expression Tr=1-a*u, where u is the column amount per molecular
C weight The values of 'a' were chosed, so that the model can reproduce
C the radiative forcing of a typical halocarbon (average of CFC-11,
C CFC-12, CFC-113, HCFC-141b, HFC-134a, HCFC-22) computed by a
C narrow-band model with a mid-latitude summer profile, for different
C overlap cases
C
C INPUT:  Band index (IBND)
C         Absorber amount (UPATH)
C OUTPUT: Halocarbon Transmittance (HALTRAN)

      IMPLICIT NONE

C Input Variables
C-----------------

      INTEGER IBND
      REAL UPATH


C Internal Variables
C--------------------

      REAL ALPHA(4)

      DATA ALPHA(1), ALPHA(2), ALPHA(3), ALPHA(4) /
     $     1.300D+05, 1.200D+05, 1.200D+05, 1.133D+05/

      HALTRAN = 0.0

      IF (IBND.EQ.9) THEN
         HALTRAN=1.0-ALPHA(1)*UPATH
      END IF

      IF (IBND.EQ.10) THEN
         HALTRAN=1.0-ALPHA(2)*UPATH
      END IF

      IF (IBND.EQ.11) THEN
         HALTRAN=1.0-ALPHA(3)*UPATH
      END IF

      IF (IBND.EQ.12) THEN
         HALTRAN=1.0-ALPHA(4)*UPATH
      END IF

      END
