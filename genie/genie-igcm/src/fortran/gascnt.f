

C***********************************************************************
C*                                                                     *
C*                         SUBROUTINE GASCNT                           *
C*                                                                     *
C***********************************************************************

      SUBROUTINE GASCNT(IBND,WVEFF,TRCNT)

C Subroutine GASCNT calculates the self-broadened continuum
C transmittance in the bands of gases
C
C INPUT:  Band index (IBND)
C         Effective H2O amount for continuum absorption (WVEFF)
C         Effective temperature (TEFF)
C
C OUTPUT: Self-broadened continuum transmittance (TRCNT)

      IMPLICIT NONE

C-----------------
C Input Variables
C-----------------

      INTEGER IBND
      REAL WVEFF

C-----------------
C Output Variable
C-----------------

      REAL TRCNT

C--------------------
C Internal Variables
C--------------------

      REAL BND1    ! Abs. Xsection in band 1
      REAL BND2    ! Abs. Xection  in band 2
      REAL BND3    ! Abs. Xsection in band 3
      REAL BND4    ! Abs. Xsection in band 4
      REAL BND5    ! Abs. Xsection in band 5
      REAL BND6    ! Abs. Xsection in band 6
      REAL BND7    ! Abs. Xsection in band 7
      REAL BND8    ! Abs. Xsection in band 8
      REAL BND9    ! Abs. Xsection in band 9

      DATA BND1, BND2 / 5.47, 0.83 /
      DATA BND3, BND4 / 2.03, 0.13 /
      DATA BND5, BND6 / 1.33, 3.99 /
      DATA BND7, BND8, BND9 / 8.61, 0.65, 1.86 /

C--- Band 1
      IF (IBND.EQ.1) THEN
         TRCNT=EXP(-WVEFF*BND1)
      END IF

C--- Band 2
      IF (IBND.EQ.2) THEN
         TRCNT=EXP(-WVEFF*BND2)
      END IF

C--- Band 3
      IF (IBND.EQ.3) THEN
         TRCNT=EXP(-WVEFF*BND3)
      END IF

C--- Band 4
      IF (IBND.EQ.4) THEN
         TRCNT=EXP(-WVEFF*BND4)
      END IF

C--- Band 5
      IF (IBND.EQ.5) THEN
         TRCNT=EXP(-WVEFF*BND5)
      END IF

C--- Band 6
      IF (IBND.EQ.6) THEN
         TRCNT=EXP(-WVEFF*BND6)
      END IF

C--- Band 7
      IF (IBND.EQ.7) THEN
         TRCNT=EXP(-WVEFF*BND7)
      END IF

C--- Band 8
      IF (IBND.EQ.8) THEN
         TRCNT=EXP(-WVEFF*BND8)
      END IF
C--- Band 9
      IF (IBND.EQ.9) THEN
         TRCNT=EXP(-WVEFF*BND9)
      END IF

      END
