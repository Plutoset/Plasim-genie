      SUBROUTINE XCONTS(Z,X,Y,MD,ND, ZINC)
C
      INTEGER MD
      INTEGER ND
      REAL ZINC
      INTEGER MODE
      INTEGER M
      INTEGER N
      INTEGER IST
      INTEGER JST
      INTEGER NCL
      REAL ZMAX
      REAL ZINC1
C
      REAL Z(MD,ND),X(MD,ND),Y(MD,ND),CL(150)
      INTEGER IWRK(10000)
      CL(1)=0.0
      CL(2)=ZINC
      MODE=1
      M =MD
      N =ND
      IST=1
      JST=1
      CALL XINTSY(0.80)
      CALL XCONTA(Z(IST,JST),X(IST,JST),Y(IST,JST)
     :           ,IWRK,MD,M,N,CL,NCL,MODE)

      CALL XINTSY(0.8)
      IF( NCL.EQ.0) THEN
         ZMAX= CL(1)
         ZINC1= ZMAX
      ELSE
         ZMAX=CL(NCL)
         ZINC1=CL(2)-CL(1)
       ENDIF
      CALL XCLIMT(ZMAX, CL(1),ZINC1)

      RETURN
      END
