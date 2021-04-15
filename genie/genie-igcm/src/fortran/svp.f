c*************************************
      REAL FUNCTION SVP(T)

C Calculates the saturation vapour pressure given T, using Murray's
C formulae. If t < 253 K assume saturation w.r.t. ice
C SVP returned in Pascals

      IMPLICIT NONE

      REAL T

      IF(T.GE.253.0)THEN
        SVP=611*EXP(17.27*(T-273.16)/(T-35.86))
      ELSE
        SVP=611*EXP(21.87*(T-273.16)/(T-7.66))
      ENDIF

      END
