      SUBROUTINE XRCNTA(ZG,Z,MD,MG,JG,CVL,NC)
C
      INTEGER MD,K,NC,MG,JG
      REAL    CV,CVL,ZG
C
      DIMENSION ZG(MD,*),Z(MD,*),CVL(*)
      COMPLEX   Z
      DO 100 K=1,NC
      CV=CVL(K)
  100 CALL XRCNTR(ZG,Z,MD,MG,JG,CV)
      RETURN
      END
