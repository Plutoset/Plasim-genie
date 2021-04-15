      SUBROUTINE XHAT01(XP,YP,N)
C arrange data sequence in xp and yp in ascending order of XP element
      INTEGER N
      INTEGER I
      INTEGER K
      INTEGER KK
C
      REAL XP(N), YP(N), XPT(20),YPT(20)
      INTEGER IN(20)
      XPT(1)=XP(1)
      YPT(1)=YP(1)
      DO 4 I=1,N
 4      IN(I)=0
      K=1
      KK=1
 3    DO 1 I=1,N
      IF( XP(I).LT.XPT(K).AND.IN(I).EQ.0) THEN
        KK=I
        XPT(K)=XP(I)
        YPT(K)=YP(I)
      ENDIF
  1   CONTINUE
      IN(KK)=1
      K=K+1
      IF(K.GT.N) GOTO 6
      DO 5 I=1,N
      IF( IN(I).EQ.0) THEN
        YPT(K)=YP(I)
        XPT(K)=XP(I)
        KK=I
        GOTO 3
      ENDIF
 5    CONTINUE
      GOTO 3
 6    DO 2 I=1,N
      XP(I)=XPT(I)
 2      YP(I)=YPT(I)
      END
