      SUBROUTINE ZCONTA(ZG,Z,IWRK,MD,MG,JG,CVL,NC)
C
      INTEGER MD,MG,K,NC,IWRK
      REAL    CV,CVL,ZG,JG
C
      DIMENSION ZG(MD ,*),Z(MD ,*),IWRK(MG ,*),CVL(*)
      COMPLEX   Z
      DO 100 K=1,NC
      CV=CVL(K)
  100 CALL ZCONTR(ZG,Z,IWRK,MD,MG,JG,CV)
      RETURN
      END
