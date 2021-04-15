C* COLOUR FILLING ROUTINES
      SUBROUTINE ZCONTB(ZG,Z,MD,MG,JG,C1,C2)
C
      INTEGER MD,I,MG,J,JG,M,IWS,K,IWA,NP
      REAL    CV,CV1,C1,C2,CV2,H5,ZG,H1,H2,H3,H4
      REAL    X,Y
C
C Colour filling of triangular blocks
      DIMENSION ZG(MD,*),Z(MD,*),X(5),Y(5)
      COMPLEX   Z,B1,B2,ZA,ZB,ZC,ZD,Z1,Z2,Z3,Z4,Z5,D
            D(P1,P2,B1,B2   )=B1+(CV-P1)*(B2-B1)/(P2-P1)
      CV1=MIN(C1,C2)
      CV2=MAX(C1,C2)
      DO 101 I=1,MG-1
      DO 102 J=1,JG-1
      H5=0.25*(ZG(I,J)+ZG(I,J+1)+ZG(I+1,J+1)+ZG(I+1,J))
      Z5=0.25*( Z(I,J)+ Z(I,J+1)+ Z(I+1,J+1)+ Z(I+1,J))
      DO 103 M=1,4
      H1=ZG(I+MOD(M  ,4)/2,J+MOD(M-1,4)/2)
      Z1=Z (I+MOD(M  ,4)/2,J+MOD(M-1,4)/2)
      H2=ZG(I+MOD(M+1,4)/2,J+MOD(M  ,4)/2)
      Z2=Z (I+MOD(M+1,4)/2,J+MOD(M  ,4)/2)
      H3=H5
      Z3=Z5
      IF(H2.LT.H1) THEN
        H4=H2
        H2=H1
        H1=H4
        Z4=Z2
        Z2=Z1
        Z1=Z4
      ENDIF
      IF(H3.LT.H1) THEN
        H4=H3
        H3=H1
        H1=H4
        Z4=Z3
        Z3=Z1
        Z1=Z4
      ENDIF
      IF(H3.LT.H2) THEN
        H4=H3
        H3=H2
        H2=H4
        Z4=Z3
        Z3=Z2
        Z2=Z4
      ENDIF
      IWS=0
      DO 104 K=1,2
      CV=CV1*(2-K)+CV2*(K-1)
      IF(H1-CV) 1, 3, 3
    1 IF(H2-CV) 2,10,10
    2 IF(H3-CV)30,20,20
    3 IWA=0
      GOTO 104
   10 IWA=1
      ZA=D(H3,H1,Z3,Z1)
      ZB=D(H1,H2,Z1,Z2)
      IF(K.EQ.1) GOTO 60
      IF(IWS.EQ.0) THEN
        X(1)= REAL(Z1)
        X(2)= REAL(ZA)
        X(3)= REAL(ZB)
        Y(1)=AIMAG(Z1)
        Y(2)=AIMAG(ZA)
        Y(3)=AIMAG(ZB)
        NP=3
      ELSEIF(IWS.EQ.1) THEN
        X(1)= REAL(ZA)
        X(2)= REAL(ZB)
        X(3)= REAL(ZD)
        X(4)= REAL(ZC)
        Y(1)=AIMAG(ZA)
        Y(2)=AIMAG(ZB)
        Y(3)=AIMAG(ZD)
        Y(4)=AIMAG(ZC)
        NP=4
      ENDIF
      GOTO 60
   20 IWA=2
      ZA=D(H2,H3,Z2,Z3)
      ZB=D(H3,H1,Z3,Z1)
      IF(K.EQ.1) GOTO 60
      IF(IWS.EQ.0) THEN
        X(1)= REAL(ZA)
        X(2)= REAL(ZB)
        X(3)= REAL(Z1)
        X(4)= REAL(Z2)
        Y(1)=AIMAG(ZA)
        Y(2)=AIMAG(ZB)
        Y(3)=AIMAG(Z1)
        Y(4)=AIMAG(Z2)
        NP=4
      ELSEIF(IWS.EQ.1) THEN
        X(1)= REAL(ZA)
        X(2)= REAL(ZB)
        X(3)= REAL(ZC)
        X(4)= REAL(ZD)
        X(5)= REAL(Z2)
        Y(1)=AIMAG(ZA)
        Y(2)=AIMAG(ZB)
        Y(3)=AIMAG(ZC)
        Y(4)=AIMAG(ZD)
        Y(5)=AIMAG(Z2)
        NP=5
      ELSEIF(IWS.EQ.2) THEN
        X(1)= REAL(ZA)
        X(2)= REAL(ZB)
        X(3)= REAL(ZD)
        X(4)= REAL(ZC)
        Y(1)=AIMAG(ZA)
        Y(2)=AIMAG(ZB)
        Y(3)=AIMAG(ZD)
        Y(4)=AIMAG(ZC)
        NP=4
      ENDIF
      GOTO 60
   30 IWA=3
      IF(K.EQ.1) GOTO 103
      IF(IWS.EQ.0) THEN
        X(1)= REAL(Z1)
        X(2)= REAL(Z2)
        X(3)= REAL(Z3)
        Y(1)=AIMAG(Z1)
        Y(2)=AIMAG(Z2)
        Y(3)=AIMAG(Z3)
        NP=3
      ELSEIF(IWS.EQ.1) THEN
        X(1)= REAL(ZC)
        X(2)= REAL(ZD)
        X(3)= REAL(Z2)
        X(4)= REAL(Z3)
        Y(1)=AIMAG(ZC)
        Y(2)=AIMAG(ZD)
        Y(3)=AIMAG(Z2)
        Y(4)=AIMAG(Z3)
        NP=4
      ELSEIF(IWS.EQ.2) THEN
        X(1)= REAL(ZC)
        X(2)= REAL(ZD)
        X(3)= REAL(Z3)
        Y(1)=AIMAG(ZC)
        Y(2)=AIMAG(ZD)
        Y(3)=AIMAG(Z3)
        NP=3
      ENDIF
   60 IF(K.NE.1) CALL ZFILLN(X,Y,NP)
      IF(IWA.EQ.3) GOTO 103
      ZC=ZA
      ZD=ZB
      IWS=IWA
  104 CONTINUE
  103 CONTINUE
  102 CONTINUE
  101 CONTINUE
      RETURN
      END
