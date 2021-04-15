      SUBROUTINE ZRCNTB(ZG,Z,MD,MG,NG,CVL,NC)
      INTEGER MD
      REAL CV
      INTEGER JG
      INTEGER NG
      INTEGER I
      INTEGER MG
      INTEGER J
      REAL H3
      REAL ZG
      INTEGER M
      REAL H1
      REAL H2
      INTEGER K
      INTEGER NC
      REAL CVL
C Contouring on triangular grid
      DIMENSION ZG(MD,*),Z(MD,*),CVL(*)
      COMPLEX   Z,B1,B2,ZA,ZB,Z1,Z2,Z3,D
      D(P1,P2,B1,B2   )=B1+(CV-P1)*(B2-B1)/(P2-P1)
      JG=ABS(NG)
      DO  50 I=1,MG-1
      DO  50 J=1,JG-1
      H3=0.25*(ZG(I,J)+ZG(I,J+1)+ZG(I+1,J+1)+ZG(I+1,J))
      Z3=0.25*( Z(I,J)+ Z(I,J+1)+ Z(I+1,J+1)+ Z(I+1,J))
      DO  50 M=1,4
      H1=ZG(I+MOD(M  ,4)/2,J+MOD(M-1,4)/2)
      Z1=Z (I+MOD(M  ,4)/2,J+MOD(M-1,4)/2)
      H2=ZG(I+MOD(M+1,4)/2,J+MOD(M  ,4)/2)
      Z2=Z (I+MOD(M+1,4)/2,J+MOD(M  ,4)/2)
      DO  50 K=1,NC
      CV=CVL(K)
      IF(H1-CV) 1, 2, 2
    1 IF(H2-CV) 3, 4, 4
    3 IF(H3-CV)50,30,30
    4 IF(H3-CV)20,10,10
    2 IF(H2-CV) 5, 6, 6
    5 IF(H3-CV)10,20,20
    6 IF(H3-CV)30,50,50
   10 ZA=D(H3,H1,Z3,Z1)
      ZB=D(H1,H2,Z1,Z2)
      GOTO 40
   20 ZA=D(H1,H2,Z1,Z2)
      ZB=D(H2,H3,Z2,Z3)
      GOTO 40
   30 ZA=D(H2,H3,Z2,Z3)
      ZB=D(H3,H1,Z3,Z1)
   40 IF(ZA.EQ.ZB) GOTO 50
      IF(NG.GT.0) THEN
       CALL XPENUP(REAL(ZA),AIMAG(ZA))
       CALL XPENDN(REAL(ZB),AIMAG(ZB))
      ELSE
       ZB=ZA+0.7*(ZB-ZA)
       CALL XPENUP(REAL(ZA),AIMAG(ZA))
       CALL XPENDN(REAL(ZB),AIMAG(ZB))
      ENDIF
   50 CONTINUE
      RETURN
      END
